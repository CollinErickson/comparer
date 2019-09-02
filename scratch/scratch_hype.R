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
    mod=NULL,
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
        Xlhs <- lhs::maximinLHS(n=n_lhs, k=length(self$parnames))
        Xlhs <- sweep(sweep(Xlhs,
                            2, self$parupper - self$parlower, "*"
        ), 2, self$parlower, "+")
        Xlhs <- as.data.frame(Xlhs)
        names(Xlhs) <- self$parnames
        X0 <- rbind(X0, Xlhs)
      }
      if (is.null(X0)) {stop('X0 is null')}
      if (!is.data.frame(X0)) {browser(); stop("X0 is not a df?")}
      # Use an ffexp object to manage simulations
      self$ffexp <- ffexp$new(eval_func=eval_func,
                              Xdf=X0
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
      # browser()
      # Just update mod? Set covtype?
      self$mod <- DiceKriging::km(formula = ~1,
                                  covtype="matern5_2",
                                  design = self$X,
                                  response = self$Z)
      if (n==1) {
        EIout <- DiceOptim::max_EI(model=self$mod,
                                   lower=self$parlower,
                                   upper=self$parupper)
      } else {
        EIout <- DiceOptim::max_qEI(model=self$mod,
                                    npoints=n,
                                    lower=self$parlower,
                                    upper=self$parupper)
      }
      newX <- EIout$par
      updatedffexp <- self$ffexp$add_level("Xdf", newX)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    run_all = function(...) {
      self$ffexp$run_all(...)
      self$X <- self$ffexp$rungrid2()
      self$Z <- self$ffexp$outlist
      invisible(self)
    },
    plotorder = function() {
      ggplot2::ggplot(data.frame(index=1:length(self$Z), Z=self$Z,
                                 col=ifelse(self$Z<=min(self$Z),'red','black')),
                      ggplot2::aes(index, Z, color=col)) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_manual(values = c("black" = "black", "red" = "red")) +
        ggplot2::theme(legend.position = "none")

    },
    plotX = function() {
      tdf <- cbind(self$X, Z=self$Z, Zorder=order(order(self$Z)))
      ggplot2::ggplot(reshape2::melt(tdf, id.vars=c('Z', 'Zorder')),
                      ggplot2::aes(value, Z, color=Zorder)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(. ~ variable, scales='free_x') +
        ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
    },
    print = function(...) {
      ts <- paste0(
        "hype object:",
        "\n\td = ", ncol(self$X),
        "\n\tn = ", nrow(self$X),
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

h1 <- hype$new(
  eval_func = function(a, b) {a^2+b^2},
  a = par_unif$new('a', -1, 2),
  b = par_unif$new('b', -10, 10),
  n_lhs = 20
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
