

get_class_from_sheetname <- function(class_sheetname){
  # map Excel sheet names that represent classes to the class names that Reason
  # uses in the model example: get_class(c("Sen Man", "Judge"))

  # we need this for the HeadCount Distribution and Salary Distribution tables

  class_mapping <- c(regular="Regular", special="Special", admin="Admin",
                     eco="Eco", eso="Eso", judges="Judge",
                     senior_management="Sen Man")

  indexes <- match(class_sheetname, class_mapping)

  model_names <- names(class_mapping)[indexes]
  model_names
}


