par_discrete <- R6::R6Class(
  classname="par_discrete",
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
    #' @description Generate values in the raw space based on quantiles.
    #' @param q In [0,1].
    generate = function(q) {
      # self$toraw(self$fromraw(self$lower) + q * (self$fromraw(self$upper) - self$fromraw(self$lower)))
      inds <- 1 + floor(q*.999999999*length(self$values))
      stopifnot(inds>=1, inds <= length(self$values))
      self$values[inds]
    },
    ggtrans="identity", # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param values The values the variable can take on.
    initialize = function(name, values) {
      self$name <- name
      # self$lower <- lower #log(lower, 10)
      # self$upper <- upper #log(upper, 10)
      # stopifnot(lower>0, upper>lower)
      self$values <- values
    }
  )
)
if (F) {
  pd <- par_discrete$new('disc', letters[1:4])
  pd
  pd$generate((0:10)/10)
  pd$generate(runif(10))
}

