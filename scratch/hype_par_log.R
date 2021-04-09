
# par_log ----
par_log <- R6::R6Class(
  classname="par_log",
  inherit = par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    partrans="log",
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
    initialize = function(name, lower, upper) {
      self$name <- name
      self$lower <- log(lower, 10)
      self$upper <- log(upper, 10)
    }
  )
)

