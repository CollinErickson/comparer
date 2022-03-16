#' Parameter with uniform distribution for hyperparameter optimization
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field lower Lower bound of the parameter
#' @field upper Upper bound of the parameter
# @field fromraw Function to convert from raw scale to transformed scale
# @field toraw Function to convert from transformed scale to raw scale
#' @field ggtrans Transformation for ggplot, see ggplot2::scale_x_continuous()
#' @examples
#' p1 <- par_unif$new('x1', 0, 2)
#' class(p1)
#' print(p1)
par_unif <- R6::R6Class(
  # par_unif ----
  classname="par_unif",
  inherit = par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    #' @description Function to convert from raw scale to transformed scale
    #' @param x Value of raw scale
    fromraw=function(x) {x}, #identity,
    #' @description Function to convert from transformed scale to raw scale
    #' @param x Value of transformed scale
    toraw= function(x) {x}, #identity,
    ggtrans="identity", # ggplot trans to give to scale_x_continuous
    # fromraw=NULL,
    # toraw= NULL,
    # ggtrans=NULL, # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
    initialize = function(name, lower, upper) {
      self$name <- name
      self$lower <- lower
      self$upper <- upper
      # These can't be defined above, gave error, has to be in init
      # self$fromraw <- identity
      # self$toraw <- identity
      # self$ggtrans <- "identity" # ggplot trans to give to scale_x_continuous
    }
  )
)
if (F) {
  p1 <- par_unif$new('x1', 0, 2)
  class(p1)
}
