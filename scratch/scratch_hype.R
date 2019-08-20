par_hype <- R6::R6Class(
  classname="par_hype",
  public=list(

  )
)

par_unif <- R6::R6Class(
  classname="par_unif",
  inherit = par_hype,
  public=list(
    name=NULL,
    low=NULL,
    high=NULL,
    initialize = function(name, low, high) {
      self$name <- name
      self$low <- low
      self$high <- high
    }
  )
)
if (F) {
  p1 <- par_unif$new('x1', 0, 2)
  class(p1)
}

hype <- R6::R6Class(
  classname="hype",
  inherit=ffexp,
  public=list(
    initialize = function(eval_func, ...) { # ... is params
      self$eval_func <- eval_func
      dots <- list(...)
      for (pari in dots) {
        if (!("par_hype" %in% class(pari))) {
          stop("All ... should be par_hype objects")
        }
      }

    },
    add_data = function(X, Y) {

    }
  )
)

hype$new()
