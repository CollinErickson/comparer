#' Hyperparameter of discrete (factor) variable
#' @export
#' @param name Name of the parameter, must match the input to `eval_func`.
#' @param values Vector of values
#' @examples
#' p1 <- par_unordered('x1', c('a', 'b', 'c'))
#' class(p1)
#' print(p1)
# par_unordered ----
par_unordered <- function(name, values) {
  R6_par_unordered$new(
    name=name,
    values=values
  )
}

#' R6 class for hyperparameter of discrete (factor) variable
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field values Vector of values
# @field fromraw Function to convert from raw scale to transformed scale
# @field toraw Function to convert from transformed scale to raw scale
#' @field ggtrans Transformation for ggplot, see ggplot2::scale_x_continuous()
#' @field lower Lower bound of the parameter
#' @field upper Upper bound of the parameter
#' @examples
#' p1 <- par_unordered('x1', c('a', 'b', 'c'))
#' class(p1)
#' print(p1)
# R6_par_unordered ----
R6_par_unordered <- R6::R6Class(
  classname="par_unordered",
  inherit = par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    values=NULL,
    # partrans="log",
    #' @description Function to convert from raw scale to transformed scale
    #' @param x Value of raw scale
    fromraw=function(x) {self$toint(x)},
    #' @description Function to convert from transformed scale to raw scale
    #' @param x Value of transformed scale
    toraw= function(x) {self$fromint(x)},
    #' @description Convert from integer index to actual value
    #' @param x Integer index
    fromint=function(x) {self$values[x]},
    #' @description Convert from value to integer index
    #' @param x Value
    toint=function(x) {
      if (length(x) > 1) {
        return(sapply(x, self$toint))
      }
      w <- which(self$values==x)
      stopifnot(length(w) == 1)
      w
    },
    #' @description Generate values in the raw space based on quantiles.
    #' @param q In [0,1].
    generate = function(q) {
      inds <- 1 + floor(q*(1-1e-12)*length(self$values))
      stopifnot(inds>=1, inds <= length(self$values))
      self$values[inds]
    },
    #' @description Get a sequence, uniform on the transformed scale
    #' @param n Number of points. Ignored for discrete.
    getseq = function(n) {
      list(
        trans=1:length(self$values),
        raw=self$values
      )
    },
    ggtrans="identity", # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param values The values the variable can take on.
    initialize = function(name, values) {
      self$name <- name
      stopifnot(!anyDuplicated(values))
      self$values <- values
      self$lower <- values[1]
      self$upper <- values[length(values)]
    },
    #' @description Print details of the object.
    #' @param ... not used
    print = function(...) {
      s <- paste0("hype par_unordered(name = ", self$name,
                  ", values = ", paste(self$values, collapse=" "),
                  ")")
      cat(s)
      invisible(self)
    }
  )
)
if (F) {
  pd <- par_unordered('disc', letters[1:4])
  pd
  pd$generate((0:10)/10)
  pd$generate(runif(10))
  pd$toint('c')
  pd$toint(letters[4:1])
  pd$fromint(3)
  pd$fromint(5:1)
  par_unordered('a', c('a','b','b'))
}
