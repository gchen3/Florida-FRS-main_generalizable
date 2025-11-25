#!/usr/bin/env Rscript

# Minimal comparison: V3 vs s for selected classes (default: all)
# Runs both implementations in isolated envs, writes to temp dirs,
# aligns on union keys with NA->0, and reports max absolute diffs.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(fs)
  library(here)
  library(pendata)
  library(pentools)
  library(data.table)
})

# Paths and minimal envs
rdir  <- here::here("refactor", "R")
wddir <- here::here("refactor", "working_data")
stopifnot(file.exists(fs::path(wddir, "frs_data_env.rds")))

params <- pendata::frs$params_env
# Prefer cached params from working_data if available (ensures start_year_ and model_period_ are set)
params_rdata <- fs::path(wddir, "params.RData")
if (file.exists(params_rdata)) {
  holder <- new.env()
  load(params_rdata, holder)
  if (exists("params", envir = holder, inherits = FALSE)) {
    params <- get("params", envir = holder)
    cat("Using cached params from refactor/working_data/params.RData\n")
  } else if (exists("params_env", envir = holder, inherits = FALSE)) {
    params <- get("params_env", envir = holder)
    cat("Using cached params_env from refactor/working_data/params.RData\n")
  }
}

# Sanity check for required fields
req <- c("start_year_", "model_period_", "pop_growth_", "retire_refund_ratio_")
missing <- req[!vapply(req, function(nm) !is.null(params[[nm]]) && length(params[[nm]]) == 1, logical(1))]
if (length(missing)) {
  stop(sprintf("Missing required params fields: %s. Rebuild working_data/params.RData or ensure pendata::frs$params_env provides them.", paste(missing, collapse = ", ")))
}
frs_data_env <- list2env(readRDS(fs::path(wddir, "frs_data_env.rds")), parent = emptyenv())

# Try to load a precomputed bm_env to avoid heavy rebuilds and schema issues
bm_env <- NULL
bm_env_rdata <- fs::path(wddir, "bm_env.RData")
if (file.exists(bm_env_rdata)) {
  holder <- new.env()
  load(bm_env_rdata, holder)
  if (exists("bm_env", envir = holder, inherits = FALSE)) {
    bm_env <- get("bm_env", envir = holder)
  }
}
if (is.null(bm_env)) {
  # Fallback: source the benefit function and build on the fly
  bm_env <- new.env()
  source(fs::path(rdir, "FRS_benefit_model_get_benefit_data_function_GC_s.R"), local = bm_env)
}

# If a precomputed benefit_data_s exists, wrap a lightweight provider
if (exists("benefit_data_s", envir = bm_env, inherits = TRUE)) {
  cached <- get("benefit_data_s", envir = bm_env)
  bm_env2 <- new.env(parent = emptyenv())
  bm_env2$get_benefit_data_s <- function(...) cached
  bm_env <- bm_env2
}

# Helper to run one implementation and read results
run_one <- function(wf_file, iddir, class_name = "regular") {
  if (!dir_exists(iddir)) dir_create(iddir, recurse = TRUE)
  parent_env <- new.env(parent = globalenv())
  assign("bm_env", bm_env, envir = parent_env)
  assign("frs_data_env", frs_data_env, envir = parent_env)
  assign("params", params, envir = parent_env)
  assign("iddir", iddir, envir = parent_env)
  wfm_env <- new.env(parent = parent_env)
  source(fs::path(rdir, wf_file), local = wfm_env)
  invisible(wfm_env$get_wf_data_s(class_name = class_name, params = params))
  readRDS(fs::path(iddir, paste0(class_name, "_wf_data.rds")))
}

# Which classes to test
classes <- c("regular", "special", "admin", "eco", "eso", "judges", "senior_management")
wf_classes <- Sys.getenv("WF_CLASSES", unset = NA_character_)
if (!is.na(wf_classes) && nchar(wf_classes) > 0) {
  classes <- strsplit(wf_classes, ",")[[1]] |> trimws()
}

# Align+compare helper
align_compare <- function(df_s, df_v, keys, val_col) {
  a <- as.data.frame(df_s); b <- as.data.frame(df_v)
  key_vals <- lapply(keys, function(k) sort(unique(c(a[[k]], b[[k]]))))
  names(key_vals) <- keys
  grid <- do.call(expand.grid, c(key_vals, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  join_side <- function(df) {
    z <- suppressWarnings(dplyr::right_join(df, grid, by = keys))
    if (!(val_col %in% names(z))) z[[val_col]] <- 0
    v <- z[[val_col]]; v[is.na(v)] <- 0; as.numeric(v)
  }
  vs <- join_side(a); vv <- join_side(b)
  list(max_abs = if (length(vs)==length(vv)) max(abs(vs - vv)) else NA_real_, equal = isTRUE(all.equal(vs, vv, tol = 1e-10)))
}

# Components and report
comps <- list(
  wf_active_df = list(keys = c("entry_age","age","year"),                      val = "n_active"),
  wf_term_df   = list(keys = c("entry_age","age","year","term_year"),         val = "n_term"),
  wf_refund_df = list(keys = c("entry_age","age","year","term_year"),         val = "n_refund"),
  wf_retire_df = list(keys = c("entry_age","age","year","term_year","retire_year"), val = "n_retire")
)

summary_rows <- list()
cat("\nCompare V3 vs s\n")
for (cls in classes) {
  id_s  <- fs::path(tempdir(), paste0("wf_s_", cls, "_min"));  dir_create(id_s, recurse = TRUE)
  id_v3 <- fs::path(tempdir(), paste0("wf_v3_", cls, "_min")); dir_create(id_v3, recurse = TRUE)
  res_s  <- run_one("FRS_workforce_model_functions_s.R",  id_s,  cls)
  res_v3 <- run_one("FRS_workforce_model_functions_V3.R", id_v3, cls)
  gc()
  cat(sprintf("\nClass: %s\n", cls))
  for (nm in names(comps)) {
    cfg <- comps[[nm]]
    cmp <- align_compare(res_s[[nm]], res_v3[[nm]], cfg$keys, cfg$val)
    status <- if (isTRUE(cmp$equal)) "PASS" else "FAIL"
    cat(sprintf("  - %-12s : %-4s  max|diff|=%s\n", nm, status, format(cmp$max_abs, digits = 6)))
    summary_rows[[length(summary_rows) + 1]] <- data.frame(class = cls, component = nm, status = status, max_abs = cmp$max_abs)
  }
}

cat("\nSummary:\n"); print(do.call(rbind, summary_rows))
