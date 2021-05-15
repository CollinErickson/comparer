
# par_log ----
par_log10 <- R6::R6Class(
  classname="par_log10",
  inherit = par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    # partrans="log",
    fromraw=function(x) {log(x, 10)},
    toraw= function(x) {10 ^ x},
    ggtrans="log10", # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
    initialize = function(name, lower, upper) {
      self$name <- name
      self$lower <- lower #log(lower, 10)
      self$upper <- upper #log(upper, 10)
    }
  )
)

