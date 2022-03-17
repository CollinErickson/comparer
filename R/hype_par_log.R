
#' Hyperparameter on log10 scale
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field lower Lower bound of the parameter
#' @field upper Upper bound of the parameter
# @field fromraw Function to convert from raw scale to transformed scale
# @field toraw Function to convert from transformed scale to raw scale
#' @field ggtrans Transformation for ggplot, see ggplot2::scale_x_continuous()
#' @examples
#' p1 <- par_log10$new('x1', 1e-4, 1e4)
#' class(p1)
#' print(p1)
# par_log ----
par_log10 <- R6::R6Class(
  classname="par_log10",
  inherit = par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    # partrans="log",
    #' @description Function to convert from raw scale to transformed scale
    #' @param x Value of raw scale
    fromraw=function(x) {log(x, 10)},
    #' @description Function to convert from transformed scale to raw scale
    #' @param x Value of transformed scale
    toraw= function(x) {10 ^ x},
    #' @description Generate values in the raw space based on quantiles.
    #' @param q In [0,1].
    generate = function(q) {
      self$toraw(self$fromraw(self$lower) + q * (self$fromraw(self$upper) - self$fromraw(self$lower)))
    },
    ggtrans="log10", # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
    initialize = function(name, lower, upper) {
      self$name <- name
      self$lower <- lower #log(lower, 10)
      self$upper <- upper #log(upper, 10)
      stopifnot(lower>0, upper>lower)
    }
  )
)
if (F) {
  p1 <- par_log10$new('x1', 1e-4, 1e4)
  p1$generate(0)
  p1$generate((0:8)/8)
  curve(p1$generate(x))
}

