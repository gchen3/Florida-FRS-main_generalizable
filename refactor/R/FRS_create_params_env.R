extend_params <- function(params){
  
  # class groupings
  params$class_names_ <- params$init_funding_data$class
  params$class_names_no_frs_ <- params$class_names_[!params$class_names_ %in% c("frs")]
  params$class_names_no_drop_frs_ <- params$class_names_[!params$class_names_ %in% c("drop", "frs")]
 
  
  # create a tibble that has nc_cal_ values for each class
  var_names <- ls(pattern = "_nc_cal_$", envir=params)
  classes <- gsub("_nc_cal_$", "", var_names) # Extract the class prefix
  values <- mget(var_names, envir = params)
  params$nc_cal_ <- tibble(class = classes, nc_cal_ = unlist(values))
  
  params$salary_growth_table_ <- params$salary_growth_table_ %>% 
    bind_rows(tibble(yos = (max(params$salary_growth_table_$yos)+1):max(yos_range_))) %>% 
    fill(everything(), .direction = "down") %>% 
    mutate(across(contains("salary"), ~ cumprod(1 + lag(.x, default = 0)), .names = "cumprod_{.col}"), .keep = "unused")
  
  return(params)
}


get_params <- function(frs_data_env, modparm_data_env){
  
  frs_names <- ls(envir = frs_data_env) |> sort()
  frs_underscore <- frs_names[grepl("_$", frs_names)]
  # setdiff(frs_names, frs_underscore)
  frs_extras <- c("eco_eso_judges_active_member_adjustment_ratio", "retiree_distribution", "init_funding_data", "return_scenarios")
  frs_keep <- c(frs_underscore, frs_extras)
  frs_objects <- mget(frs_keep, envir = frs_data_env)
  
  modparm_names <- ls(envir = modparm_data_env) |> sort()
  modparm_underscore <- modparm_names[grepl("_$", modparm_names)]
  # setdiff(modparm_names, modparm_underscore) # no names that don't end in non-underscore
  modparm_keep <- modparm_underscore
  modparm_objects <- mget(modparm_keep, envir = modparm_data_env)
  
  params <- new.env()
  params_objects <- c(frs_objects, modparm_objects)
  list2env(params_objects, envir = params) 
  
  params <- extend_params(params)
  
  # it is possible to make the names in params sorted, but work, and it won't
  # be maintained if we modify params, so I don't do it
  # create a temporary environment from which we will copy objects, sorted by name
  # temp_env <- new.env()
  # list2env(c(frs_objects, modparm_objects), envir = temp_env) 
  # sorted_names <- ls(envir=temp_env)
  # 
  # params <- new.env()
  # for (name in sorted_names) {
  #   # use assign rather than list2env so we can control sort order
  #   assign(name, temp_env[[name]], envir = params)
  # }
  return(params)
}