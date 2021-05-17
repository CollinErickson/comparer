par_discrete <- R6::R6Class(
  classname="par_log10",
  inherit = par_hype,
  public=list(
    name=NULL,
    # lower=NULL,
    # upper=NULL,
    values=NULL,
    # partrans="log",
    #' @description Function to convert from raw scale to transformed scale
    #' @param x Value of raw scale
    fromraw=function(x) {x},
    #' @description Function to convert from transformed scale to raw scale
    #' @param x Value of transformed scale
    toraw= function(x) {x},
    ggtrans="identity", # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
    initialize = function(name, values) {
      self$name <- name
      # self$lower <- lower #log(lower, 10)
      # self$upper <- upper #log(upper, 10)
      # stopifnot(lower>0, upper>lower)
      self$values <- values
    }
  )
)

