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
    lower=NULL,
    upper=NULL,
    initialize = function(name, lower, upper) {
      self$name <- name
      self$lower <- lower
      self$upper <- upper
    }
  )
)
if (F) {
  p1 <- par_unif$new('x1', 0, 2)
  class(p1)
}

hype <- R6::R6Class(
  classname="hype",
  # inherit=ffexp,
  public=list(
    X=NULL,
    Z=NULL,
    params=NULL,
    parnames=NULL,
    parlower=NULL,
    parupper=NULL,
    ffexp = NULL,
    eval_func = NULL,
    initialize = function(eval_func, ..., X0=NULL, n_lhs) { # ... is params
      self$eval_func <- eval_func
      dots <- list(...)
      parlist <- data.frame()
      self$parnames <- c()
      self$parlower <- c()
      self$parupper <- c()
      for (pari in dots) {
        if (!("par_hype" %in% class(pari))) {
          stop("All ... should be par_hype objects")
        }
        parlist <- c(parlist, pari)
        self$parnames <- c(self$parnames, pari$name)
        self$parlower <- c(self$parlower, pari$lower)
        self$parupper <- c(self$parupper, pari$upper)
      }
      if (!missing(n_lhs)) {
        Xlhs <- lhs::maximinLHS(n=n_lhs, k=length(parnames))
        Xlhs <- sweep(sweep(Xlhs,
          2, parhighs - parlows, "*"
        ), 2, parlows, "+")
        Xlhs <- as.data.frame(Xlhs)
        names(Xlhs) <- parnames
        X0 <- rbind(X0, Xlhs)
      }
      if (is.null(X0)) {stop('X0 is null')}
      if (!is.data.frame(X0)) {browser(); stop("X0 is not a df?")}
      # Use an ffexp object to manage simulations
      self$ffexp <- ffexp$new(eval_func = eval_func,
                              Xdf <- X0
      )
      invisible(self)
    },
    add_data = function(X, Y) {

    },
    add_X = function(X) {

    },
    add_LHS = function(n) {

    },
    add_EI = function(n) {
      browser()
      mod <- DiceKriging::km(formula = ~1,
                             design = self$X,
                             response = self$Z)
      DiceOptim::max_EI(model=self$mod,
                        lower=self$parlows,
                        upper=self$parhighs)
    },
    run_all = function(...) {
      self$ffexp$run_all(...)
      self$X <- self$ffexp$rungrid2()
      self$Z <- self$ffexp$outlist
      invisible(self)
    }
  )
)

h1 <- hype$new(
  eval_func = function(a, b) {a+b},
  a = par_unif$new('a', 1, 2),
  b = par_unif$new('b', -10, 10),
  n_lhs = 20
)
h1
h1$ffexp
h1$run_all()
h1$ffexp
h1$add_EI(1)
