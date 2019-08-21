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
    exp = NULL,
    eval_func = NULL,
    initialize = function(eval_func, ...) { # ... is params
      self$eval_func <- eval_func
      dots <- list(...)
      parlist <- data.frame()
      parnames <- c()
      parlow <- c()
      parhigh <- c()
      for (pari in dots) {
        if (!("par_hype" %in% class(pari))) {
          stop("All ... should be par_hype objects")
        }
        parlist <- c(parlist, pari)
        parnames <- c(parnames, pari$name)
        parlows <- c(parlows, pari$low)
        parhighs <- c(parhighs, pari$high)
      }

    },
    add_data = function(X, Y) {

    },
    add_X = function(X) {

    },
    add_LHS = function(n) {

    },
    add_EI = function(n) {

    },
    run_all = function() {

    }
  )
)

hype$new()
