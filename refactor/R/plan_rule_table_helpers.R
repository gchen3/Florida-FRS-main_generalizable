# Load plan rule tables from the Excel workbook into params
plan_rule_tables_path <- here::here("refactor", "R", "plan_rule_tables.xlsx")

params$plan_overview <- readxl::read_excel(plan_rule_tables_path, sheet = "plan_overview")
params$class_group_map <- readxl::read_excel(plan_rule_tables_path, sheet = "class_group_map")
params$tier_map <- readxl::read_excel(plan_rule_tables_path, sheet = "tier_map")
params$status_paths <- readxl::read_excel(plan_rule_tables_path, sheet = "status_paths")
params$status_priority <- readxl::read_excel(plan_rule_tables_path, sheet = "status_priority")

params$plan_rule_tables <- list(
  plan_overview = params$plan_overview,
  class_group_map = params$class_group_map,
  tier_map = params$tier_map,
  status_paths = params$status_paths,
  status_priority = params$status_priority
)

tier_base <- tidyr::expand_grid(
  class = as.character(params$class_names_no_drop_frs_),
  entry_year = as.integer(params$entry_year_range_),
  yos = as.integer(params$yos_range_),
  age = as.integer(params$age_range_)
) |>
  dplyr::mutate(row_id__ = dplyr::row_number()) |>
  dplyr::left_join(params$class_group_map, by = "class") |>
  dplyr::mutate(class_group = dplyr::coalesce(class_group, "GEN"))

tier_lookup <- tier_base |>
  dplyr::cross_join(params$tier_map) |>
  dplyr::filter(entry_year >= entry_year_min, entry_year <= entry_year_max) |>
  dplyr::select(row_id__, tier_id)

status_lookup <- tier_base |>
  dplyr::left_join(tier_lookup, by = "row_id__") |>
  dplyr::inner_join(
    params$status_paths,
    by = c("tier_id", "class_group"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(yos >= min_yos, age >= min_age) |>
  dplyr::left_join(params$status_priority, by = "status") |>
  dplyr::arrange(row_id__, priority) |>
  dplyr::group_by(row_id__) |>
  dplyr::summarise(status = dplyr::first(status), .groups = "drop")

params$tier_table_ <- tier_base |>
  dplyr::left_join(tier_lookup, by = "row_id__") |>
  dplyr::left_join(status_lookup, by = "row_id__") |>
  dplyr::mutate(
    status = dplyr::coalesce(status, "non_vested"),
    new_year = as.integer(params$new_year_),
    tier = paste0(tier_id, "_", status),
    is_norm_retire_elig = status == "norm",
    vested_at_term = status == "vested"
  ) |>
  dplyr::arrange(class, entry_year, yos, age) |>
  dplyr::select(class, entry_year, yos, age, new_year, tier, is_norm_retire_elig, vested_at_term)

if (is.null(params$tier_table)) {
  params$tier_table <- params$tier_table_
}
