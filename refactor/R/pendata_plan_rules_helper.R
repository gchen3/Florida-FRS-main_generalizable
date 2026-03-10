# pendata_plan_rules_helper.R
#
# Replaces plan_rule_table_helpers.R (xlsx-based).
# Loads plan rule tables directly from pendata and builds:
#   params$plan_rule_tables  - list matching Gang's original structure
#   params$tier_table_       - pre-computed tier/status table (same as before)
#   params$ben_mult_lookup   - replaces legacy lookup; built from
#                              pendata::frs$benefit_multipliers via
#                              pendata::benmult_lookup()
#
# Requires pendata >= experimental branch (benefit_multipliers + benmult_lookup).

message("Loading plan rule tables from pendata...")

# ---- 1. Assemble params$plan_rule_tables from pendata objects ---------------
# These replace the 5 sheets of plan_rule_tables.xlsx.
# Column names are preserved to match what FRS_benefit_model_functions_2.R expects.

params$plan_rule_tables <- list(
  plan_overview   = data.frame(
    item  = c("plan_name", "new_year_"),
    value = c("FRS", as.character(params$new_year_))
  ),
  class_group_map = params$class_groups,          # class -> class_group
  tier_map        = params$tier_map,              # tier_id, entry_year_min/max
  status_paths    = params$status_paths,          # tier x class_group x status eligibility
  status_priority = params$status_priority        # norm > early > vested > non_vested
)

message("  \u2713 Built params$plan_rule_tables from pendata")

# ---- 2. Build params$tier_table_ (same logic as plan_rule_table_helpers.R) -
# This mirrors Gang's pre-computed tier/status table; the NULL guard ensures
# we don't overwrite params$tier_table if it is already set (e.g., by
# build_tier_table() in refactor branches).

tier_base <- tidyr::expand_grid(
  class      = as.character(params$class_names_no_drop_frs_),
  entry_year = as.integer(params$entry_year_range_),
  yos        = as.integer(params$yos_range_),
  age        = as.integer(params$age_range_)
) |>
  dplyr::mutate(row_id__ = dplyr::row_number()) |>
  dplyr::left_join(params$plan_rule_tables$class_group_map, by = "class") |>
  dplyr::mutate(class_group = dplyr::coalesce(class_group, "GEN"))

tier_lookup <- tier_base |>
  dplyr::cross_join(params$plan_rule_tables$tier_map) |>
  dplyr::filter(entry_year >= entry_year_min, entry_year <= entry_year_max) |>
  dplyr::select(row_id__, tier_id)

status_lookup <- tier_base |>
  dplyr::left_join(tier_lookup, by = "row_id__") |>
  dplyr::inner_join(
    params$plan_rule_tables$status_paths,
    by = c("tier_id", "class_group"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(yos >= min_yos, age >= min_age) |>
  dplyr::left_join(params$plan_rule_tables$status_priority, by = "status") |>
  dplyr::arrange(row_id__, priority) |>
  dplyr::group_by(row_id__) |>
  dplyr::summarise(status = dplyr::first(status), .groups = "drop")

params$tier_table_ <- tier_base |>
  dplyr::left_join(tier_lookup, by = "row_id__") |>
  dplyr::left_join(status_lookup, by = "row_id__") |>
  dplyr::mutate(
    status     = dplyr::coalesce(status, "non_vested"),
    new_year   = as.integer(params$new_year_),
    tier       = paste0(tier_id, "_", status),
    is_norm_retire_elig = status == "norm",
    vested_at_term      = status == "vested"
  ) |>
  dplyr::arrange(class, entry_year, yos, age) |>
  dplyr::select(class, entry_year, yos, age, new_year, tier,
                is_norm_retire_elig, vested_at_term)

if (is.null(params$tier_table)) {
  params$tier_table <- params$tier_table_
}

message("  \u2713 Built params$tier_table_")

# ---- 3. Build params$ben_mult_lookup from pendata::frs$benefit_multipliers --
# Strategy: pre-compute benmult at every (class, tier, status, dist_age, yos)
# using pendata::benmult_lookup(), producing one row per point so the
# inequality join in get_benefit_table_s matches at most one row (no duplicates).
#
# Status mapping:
#   norm/vested/non_vested -> use "norm" rules (same multiplier; reduction
#                             factors and benefit eligibility handle the rest)
#   early                  -> use "early" rules
#
# Ranges: age 0-120, yos 0-75. Zero-benmult rows are dropped to keep size down.

message("  Building ben_mult_lookup from pendata benefit_multipliers (may take a moment)...")

bm_rules <- params$benefit_multipliers

# Norm/vested/non_vested share the same multiplier rules
norm_combos <- tidyr::expand_grid(
  class    = unique(bm_rules$class),
  tier     = unique(bm_rules$tier),
  status   = "norm",       # look up as "norm"
  dist_age = 0L:120L,
  yos      = 0L:75L
) |>
  dplyr::mutate(
    ben_mult = pendata::benmult_lookup(
      dplyr::pick(class, tier, status, dist_age, yos),
      bm_rules
    )
  ) |>
  dplyr::filter(ben_mult > 0) |>
  # Expand to all three non-early statuses
  dplyr::cross_join(data.frame(status_suffix = c("norm", "vested", "non_vested"))) |>
  dplyr::mutate(tier_at_dist_age = paste(tier, status_suffix, sep = "_")) |>
  dplyr::select(-status, -status_suffix)

early_combos <- tidyr::expand_grid(
  class    = unique(bm_rules$class),
  tier     = unique(bm_rules$tier),
  status   = "early",
  dist_age = 0L:120L,
  yos      = 0L:75L
) |>
  dplyr::mutate(
    ben_mult = pendata::benmult_lookup(
      dplyr::pick(class, tier, status, dist_age, yos),
      bm_rules
    )
  ) |>
  dplyr::filter(ben_mult > 0) |>
  dplyr::mutate(tier_at_dist_age = paste(tier, "early", sep = "_")) |>
  dplyr::select(-status)

params$ben_mult_lookup <- dplyr::bind_rows(norm_combos, early_combos) |>
  dplyr::mutate(
    dist_age_min_ge  = dist_age,
    dist_age_max_lt  = dist_age + 1L,
    yos_min_ge       = yos,
    yos_max_lt       = yos + 1L,
    dist_year_min_ge = 2011L,
    dist_year_max_lt = 9999L,
    system           = "pendata"            # kept so select(-system) still works
  ) |>
  dplyr::select(class, tier_at_dist_age,
                dist_age_min_ge, dist_age_max_lt,
                yos_min_ge, yos_max_lt,
                dist_year_min_ge, dist_year_max_lt,
                ben_mult, system)

message("  \u2713 Built ben_mult_lookup: ", nrow(params$ben_mult_lookup), " rows")
