## --- Reload funding code, rebuild baseline, run tests ---

# 1) (Re)load only the funding pieces into a fresh env
fm_env <- new.env()
source(fs::path(rdir, "FRS_funding_amort.R"),                                local = fm_env)
source(fs::path(rdir, "FRS_funding_model_functions_loop_without_drop_V6.R"), local = fm_env)
source(fs::path(rdir, "FRS_funding_model_functions_drop_only.R"),            local = fm_env)  # optional; harmless if not used

# 2) Toggle DROP here if you want
params$enable_drop_ <- TRUE   # set FALSE to run without DROP

# 3) Rebuild baseline (uses existing params, wf/liability already in memory)
baseline_funding <- fm_env$get_funding_data(params = params, out = "stacked")

# 4) (Optional) save just the result instead of the whole workspace
saveRDS(baseline_funding, here::here("refactor", "new_results", "baseline_funding.rds"))

# 5) Run your tests
source(fs::path(tooldir, "run_allobjects_tests.R"))