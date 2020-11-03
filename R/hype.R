#' Parameter for hyperparameter optimization
#' @export
#' @examples
#' p1 <- par_hype$new()
#' class(p1)
#' print(p1)
par_hype <- R6::R6Class(
  classname="par_hype",
  public=list(

  )
)

#' Parameter with uniform distribution for hyperparameter optimization
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field lower Lower bound of the parameter
#' @field upper Upper bound of the parameter
#' @examples
#' p1 <- par_unif$new('x1', 0, 2)
#' class(p1)
#' print(p1)
par_unif <- R6::R6Class(
  classname="par_unif",
  inherit = par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
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

#' Hyperparameter optimization
#' @export
#' @field X Data frame of inputs that have been evaluated or will be evaluated
#' next.
#' @field Z Output at X
#' @field mod Gaussian process model used to predict what the output will be.
#' @field parnames Names of the parameters
#' @field parlower Lower bounds for each parameter
#' @field parupper Upper bounds for each parameter
#' @field ffexp An ffexp R6 object used to run the experiment and store
#' the results.
#' @field eval_func The function we evaluate.
#' @field extract_output_func A function that takes in the output from
#' `eval_func` and returns the value we are trying to minimize.
#' @examples
#'
#' # Have df output, but only use one value from it
#' h1 <- hype$new(
#'   eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
#'   extract_output_func = function(odf) {odf$c[1]},
#'   a = par_unif$new('a', -1, 2),
#'   b = par_unif$new('b', -10, 10),
#'   n_lhs = 10
#' )
#' h1$run_all()
#' h1$add_EI(n = 1)
#' h1$run_all()
#' #system.time(h1$run_EI_for_time(sec=3, batch_size = 1))
#' #system.time(h1$run_EI_for_time(sec=3, batch_size = 3))
#' h1$plotorder()
#' h1$plotX()
hype <- R6::R6Class(
  classname="hype",
  # inherit=ffexp,
  # active=list(
  #   X = function(value) {
  #     if (!missing(value)) {
  #       stop("You can't set X in a hype object")
  #     }
  #     self$ffexp$rungrid2()
  #   }
  # ),
  public=list(
    X=NULL,
    Z=NULL,
    mod=NULL,
    # params=NULL,
    parnames=NULL,
    parlower=NULL,
    parupper=NULL,
    ffexp = NULL,
    eval_func = NULL,
    extract_output_func = NULL,
    #' @description Create hype R6 object.
    #' @param eval_func The function used to evaluate new points.
    #' @param ... Hyperparameters to optimize over.
    #' @param X0 Data frame of initial points to run.
    #' @param n_lhs The number that should initially be run using
    #' a maximin Latin hypercube.
    #' @param extract_output_func A function that takes in the output from
    #' `eval_func` and returns the value we are trying to minimize.
    initialize = function(eval_func,
                          ..., # ... is params
                          X0=NULL, n_lhs,
                          extract_output_func
    ) {
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
        Xlhs <- lhs::maximinLHS(n=n_lhs, k=length(self$parnames))
        Xlhs <- sweep(sweep(Xlhs,
                            2, self$parupper - self$parlower, "*"
        ), 2, self$parlower, "+")
        Xlhs <- as.data.frame(Xlhs)
        names(Xlhs) <- self$parnames
        X0 <- rbind(X0, Xlhs)
      }
      if (is.null(X0)) {
        stop(paste('Give in n_lhs, the number of initial points to evaluate.',
                   '(X0 is null.)'))
      }
      if (!is.data.frame(X0)) {browser(); stop("X0 is not a df?")}
      # Use an ffexp object to manage simulations
      self$ffexp <- ffexp$new(eval_func=eval_func,
                              Xdf=X0
      )
      if (!missing(extract_output_func)) {
        self$extract_output_func <- extract_output_func
      }
      invisible(self)
    },
    #' @description Add data to the experiment results.
    #' @param X Data frame with names matching the input parameters
    #' @param Y Output at rows of X matching the experiment output.
    add_data = function(X, Y) {
      stop("Not yet implemented")
      self$ffexp <- updatedffexp
      nameoflevel <- "Xdf" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, X, suppressMessage=TRUE)
      stop("need to add Y too")
      invisible(self)
    },
    #' @description Add new inputs to run. This allows the user to specify
    #' what they want run next.
    #' @param X Data frame with names matching the input parameters.
    add_X = function(X) {
      stopifnot(is.data.frame(X))
      stopifnot(all(colnames(X) == colnames(self$X)))
      nameoflevel <- "Xdf" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, X, suppressMessage=T)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    #' @description Add new input points using a maximin
    #' Latin hypercube.
    #' Latin hypercubes are usually more spacing than randomly picking points.
    #' @param n Number of points to add.
    add_LHS = function(n) {
      Xlhs <- lhs::maximinLHS(n=n, k=length(self$parnames))
      Xlhs <- sweep(sweep(Xlhs,
                          2, self$parupper - self$parlower, "*"
      ), 2, self$parlower, "+")
      Xlhs <- as.data.frame(Xlhs)
      names(Xlhs) <- self$parnames
      self$add_X(Xlhs)
      invisible(self)
    },
    #' @description Add new inputs to run using the expected information
    #' criteria
    #' @param n Number of points to add.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    add_EI = function(n, covtype="matern5_2", nugget.estim=TRUE) {
      # If unevaluated points, set lowest value.
      X <- self$ffexp$rungrid2()
      if (nrow(X) == length(self$Z)) { # All have been evaluated
        Z <- self$Z
      } else { # Unevaluated points exist, set to constant liar
        Z <- c(self$Z, rep(min(self$Z), nrow(X) - length(self$Z)))
        message("Unevaluated points already exist, using constant liar")
      }

      # Just update mod? Set covtype?
      if (covtype == "random") {
        covtype <- sample(c("matern5_2", "matern3_2", "exp", "powexp", "gauss"), 1)
      }
      if (is.null(self$X)) {
        stop('X is null, you need to run_all first.')
      }
      self$mod <- DiceKriging::km(formula = ~1,
                                  covtype=covtype,
                                  design = self$X,
                                  response = self$Z,
                                  nugget.estim=nugget.estim,
                                  control=list(trace=FALSE))
      if (n==1) {
        EIout <- DiceOptim::max_EI(model=self$mod,
                                   lower=self$parlower,
                                   upper=self$parupper,
                                   control=list(print.level=0))
      } else {
        EIout <- DiceOptim::max_qEI(model=self$mod,
                                    npoints=n,
                                    lower=self$parlower,
                                    upper=self$parupper)
      }
      newX <- EIout$par
      nameoflevel <- "Xdf" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, newX, suppressMessage=TRUE)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    #' @description Run all unevaluated input points.
    #' @param ... Passed into `ffexp$run_all`.
    run_all = function(...) {
      self$ffexp$run_all(...)
      if (is.null(self$extract_output_func)) {
        self$Z <- self$ffexp$outlist
        if (!all(sapply(self$ffexp$outlist, length) == 1)) {
          print(self$ffexp$outlist)
          stop(paste("output from function must either all have length one",
                     " or else you must give extract_output_func"))
        }
        self$Z <- unlist(self$ffexp$outlist)
      } else {
        self$Z <- sapply(self$ffexp$outlist, self$extract_output_func)
      }
      self$X <- self$ffexp$rungrid2()
      invisible(self)
    },
    #' @description Add points using the expected information criteria,
    #' evaluate them, and repeat until a specified amount of time has passed.
    #' @param sec Number of seconds to run for. It will go over this time
    #' limit, finish the current iteration, then stop.
    #' @param batch_size Number of points to run at once.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    #' @param ... Passed into `ffexp$run_all`.
    run_EI_for_time = function(sec, batch_size, covtype="matern5_2",
                               nugget.estim=TRUE, ...) {
      start_time <- proc.time()
      while(proc.time()[3] - start_time[3] < sec) {
        self$add_EI(n=batch_size, covtype=covtype, nugget.estim=nugget.estim)
        self$run_all(...)
      }
      invisible(self)
    },
    #' @description Make a plot to summarize the experiment.
    plot = function() {
      self$plotorder()
    },
    #' @description Plot pairs of inputs and output
    pairs = function() {
      GGally::ggpairs(cbind(self$X, Z=self$Z))
    },
    #' @description Plot the output of the points evaluated in order.
    plotorder = function() {
      ggplot2::ggplot(data.frame(index=1:length(self$Z), Z=self$Z,
                                 col=ifelse(self$Z<=min(self$Z),'red','black')),
                      ggplot2::aes(index, Z, color=col)) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_manual(values = c("black" = "black", "red" = "red")) +
        ggplot2::theme(legend.position = "none")

    },
    #' @description Plot the output as a function of each input.
    plotX = function() {
      stopifnot(nrow(self$X) == length(self$Z))
      tdf <- cbind(self$X, Z=self$Z, Zorder=order(order(self$Z)))
      ggplot2::ggplot(reshape2::melt(tdf, id.vars=c('Z', 'Zorder')),
                      ggplot2::aes(value, Z, color=Zorder)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(. ~ variable, scales='free_x') +
        ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
    },
    #' @description Print details of the object.
    #' @param ... not used
    print = function(...) {
      ts <- paste0(
        "hype object:",
        "\n\td = ", if (is.null(self$X)) {ncol(self$ffexp$rungrid2())} else {ncol(self$X)},
        "\n\tn = ", if (is.null(self$X)) {nrow(self$ffexp$rungrid2())} else {nrow(self$X)},
        if (!all(self$ffexp$completed_runs)) {
          paste0(" (", sum(!self$ffexp$completed_runs)," unevaluated)")
        } else {''},
        "\n\tTo add data: $add_data(X, Y)",
        "\n\tTo add points using EI: $add_EI",
        "\n\tTo access underlying experiment: $ffexp",
        "\n\tTo access model: $mod",
        "\n\tTo access params: $params",
        "\n\tTo plot output values in order: $plotorder",
        "\n\tTo plot output vs each input: $plotX",
        "\n\tTo run unevaluated points: $run_all",
        "\n\tTo access inputs and output: $X and $Z",
        "\n"
      )
      cat(ts)
      invisible(self)
    }
  )
)
if (F) {
  h1 <- hype$new(
    eval_func = function(a, b) {a^2+b^2},
    a = par_unif$new('a', -1, 2),
    b = par_unif$new('b', -10, 10),
    n_lhs = 10
  )
  h1
  h1$ffexp
  h1$run_all()
  h1$ffexp
  h1$add_EI(1)
  h1$ffexp
  h1$run_all()
  h1$ffexp
  h1$add_EI(4)
  h1$ffexp
  h1$run_all()
  h1$ffexp
  h1
  h1$plotorder()
  h1$plotX()
  h1$add_X(data.frame(a=1.111, b=2.222))
  h1$add_LHS(3)
  h1$add_EI(1)
}
if (F) {
  # Have df output, but only use one value from it
  h1 <- hype$new(
    eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
    extract_output_func = function(odf) {odf$c[1]},
    a = par_unif$new('a', -1, 2),
    b = par_unif$new('b', -10, 10),
    n_lhs = 10
  )
  h1$run_all()
  h1$add_EI(n = 1)
  system.time(h1$run_EI_for_time(sec=3, batch_size = 1))
  system.time(h1$run_EI_for_time(sec=3, batch_size = 3))
  h1$plotorder()
  h1$plotX()
}
