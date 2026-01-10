# Description: Get and save liability data for each class

liab_all <- lm_env$get_liability_data_s(bf_data_env, wf_data_env, params)
classes <- params$class_names_no_drop_frs_

liability_list <- purrr::map(classes, ~ liab_all %>% 
                               dplyr::filter(class == .x) %>% 
                               dplyr::select(-class)) %>% 
  purrr::set_names(classes)