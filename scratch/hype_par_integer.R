#' Parameter with uniform distribution for hyperparameter optimization
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field lower Lower bound of the parameter
#' @field upper Upper bound of the parameter
# @field fromraw Function to convert from raw scale to transformed scale
# @field toraw Function to convert from transformed scale to raw scale
#' @field ggtrans Transformation for ggplot, see ggplot2::scale_x_continuous()
#' @examples
#' p1 <- par_integer$new('x1', 0, 2)
#' class(p1)
#' print(p1)
par_integer <- function(name, lower, upper) {
  R6_par_integer$new(
    name=name,
    lower=lower,
    upper=upper
  )
}

R6_par_integer <- R6::R6Class(
  # par_unif ----
  classname="par_integer",
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
    #' @description Generate values in the raw space based on quantiles.
    #' @param q In [0,1].
    # generate = function(q) {
    #   self$toraw(self$fromraw(self$lower) + q * (self$fromraw(self$upper) - self$fromraw(self$lower)))
    # },
    generate = function(q) {
      # self$lower + q * (self$upper - self$lower)
      self$lower + floor(q*(1-1e-16) * (self$upper + 1 - self$lower))
    },
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
      lowerint <- as.integer(lower)
      upperint <- as.integer(upper)
      stopifnot(abs(lower - lowerint) < 1e-8)
      stopifnot(abs(upper - upperint) < 1e-8)
      stopifnot(lower < upper)
      self$lower <- lowerint
      self$upper <- upperint
      # These can't be defined above, gave error, has to be in init
      # self$fromraw <- identity
      # self$toraw <- identity
      # self$ggtrans <- "identity" # ggplot trans to give to scale_x_continuous
    }
  )
)
if (F) {
  p1 <- par_integer$new('x1', 4, 9)
  class(p1)
  p1$generate(runif(22))
  table(p1$generate(runif(1e5)))
}

if (F) {
  par_integer2 <- function(name, lower, upper) {
    par_integer$new(
      name=name,
      lower=lower,
      upper=upper
    )
  }
  par_integer
}
