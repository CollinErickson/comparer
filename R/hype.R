#' Parameter for hyperparameter optimization
#' @field partrans The transformation type.
#' @export
#' @examples
#' p1 <- par_hype$new()
#' class(p1)
#' print(p1)
# par_hype ----
par_hype <- R6::R6Class(
  classname="par_hype",
  public=list(
    partrans=""
  )
)

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


# hype ----
#' Hyperparameter optimization
#' @export
#' @field X Data frame of inputs that have been evaluated or will be evaluated
#' next.
#' @field Z Output at X
#' @field mod Gaussian process model used to predict what the output will be.
#' @field parnames Names of the parameters
#' @field parlowerraw Lower bounds for each parameter on raw scale
#' @field parupperraw Upper bounds for each parameter on raw scale
#' @field parlowertrans Lower bounds for each parameter on transformed scale
#' @field paruppertrans Upper bounds for each parameter on transformed scale
#' @field parlist List of all parameters
#' @field partrans Transformation for each parameter
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
  # hype ----
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
    parlowerraw=NULL,
    parupperraw=NULL,
    parlowertrans=NULL,
    paruppertrans=NULL,
    partrans=NULL,
    parlist=NULL,
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
                          X0=NULL, Z0=NULL,
                          n_lhs,
                          extract_output_func
    ) {
      self$eval_func <- eval_func
      dots <- list(...)
      if (length(dots) == 0) {
        stop("No hyperparameters given. Give a par_unif$new to hype$new.")
      }
      parlist <- list()
      self$parnames <- c()
      self$parlowerraw <- c()
      self$parupperraw <- c()
      self$parlowertrans <- c()
      self$paruppertrans <- c()
      self$partrans <- c()
      for (pari in dots) {
        if (!("par_hype" %in% class(pari))) {
          stop("All ... should be par_hype objects")
        }
        parlist <- c(parlist, pari)
        self$parnames <- c(self$parnames, pari$name)
        self$parlowerraw <- c(self$parlowerraw, pari$lower)
        self$parupperraw <- c(self$parupperraw, pari$upper)
        self$parlowertrans <- c(self$parlowertrans, pari$fromraw(pari$lower))
        self$paruppertrans <- c(self$paruppertrans, pari$fromraw(pari$upper))
        self$partrans <- c(self$partrans, pari$partrans)
      }
      stopifnot(length(self$parnames) == c(length(self$parlowerraw),
                                           length(self$parupperraw),
                                           length(self$parlowertrans),
                                           length(self$paruppertrans),
                                           length(self$partrans)))
      self$parlist <- parlist
      if (!is.null(X0)) {
        # browser("X0 not working yet, need to check with raw/trans")
        stopifnot(is.data.frame(X0), nrow(X0) > .5,
                  colnames(X0) == self$parnames)
        X0trans <- X0
      } else {
        X0trans <- NULL
      }
      if (!missing(n_lhs) && n_lhs > .5) {
        # Use add_LHS here?
        Xlhstrans <- lhs::maximinLHS(n=n_lhs, k=length(self$parnames))
        Xlhstrans <- sweep(sweep(Xlhstrans,
                                 2, self$paruppertrans - self$parlowertrans, "*"
        ), 2, self$parlowertrans, "+")
        Xlhstrans <- as.data.frame(Xlhstrans)
        names(Xlhstrans) <- self$parnames
        X0trans <- rbind(X0trans, Xlhstrans)
      }
      if (is.null(X0trans)) {
        stop(paste('Give in n_lhs, the number of initial points to evaluate.',
                   '(X0 is null.)'))
      }
      if (!is.data.frame(X0trans)) {stop("X0 is not a df?")}
      # Convert transformed back to raw
      # X0raw <- X0trans
      X0raw <- self$convert_trans_to_raw(X0trans)
      # for (i in 1:ncol(X0trans)) {
      #   X0raw[, i] <- parlist[[i]]$toraw(X0trans[, i])
      # }
      # Use an ffexp object to manage simulations
      self$ffexp <- ffexp$new(eval_func=eval_func,
                              Xdftrans=X0raw
      )
      # If Y0 given in with X0, put it in
      if (!is.null(Z0)) {
        stopifnot(is.numeric(Z0), length(Z0) == nrow(X0))
        for (i in 1:nrow(X0)) {
          self$ffexp$run_one(i, Z0[[i]])
        }
      }
      if (!missing(extract_output_func)) {
        self$extract_output_func <- extract_output_func
      }
      invisible(self)
    },
    #' @description Add data to the experiment results.
    #' @param X Data frame with names matching the input parameters
    #' @param Z Output at rows of X matching the experiment output.
    add_data = function(X, Z) {
      newffexp <- self$ffexp$add_level('Xdftrans', X)
      stopifnot(is.data.frame(X), nrow(X) > .5,
                ncol(X) == ncol(self$X),
                colnames(X) == colnames(self$X),
                is.numeric(Z), nrow(X) == length(Z))
      for (i in 1:nrow(X)) {
        newffexp$run_one(self$ffexp$number_runs + i, Z[[i]])
      }
      self$ffexp <- newffexp

      # Update this object
      self$X <- self$ffexp$rungrid2()
      self$Z <- unlist(self$ffexp$outlist)

      invisible(self)
    },
    #' @description Add new inputs to run. This allows the user to specify
    #' what they want run next.
    #' @param X Data frame with names matching the input parameters.
    add_X = function(X) {
      stopifnot(is.data.frame(X))
      stopifnot(all(colnames(X) == colnames(self$X)))
      nameoflevel <- "Xdftrans" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, X, suppressMessage=T)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    #' @description Add new input points using a maximin
    #' Latin hypercube.
    #' Latin hypercubes are usually more spacing than randomly picking points.
    #' @param n Number of points to add.
    add_LHS = function(n) {
      Xlhstrans <- lhs::maximinLHS(n=n, k=length(self$parnames))
      Xlhstrans <- sweep(sweep(Xlhstrans,
                               2, self$paruppertrans - self$parlowertrans, "*"
      ), 2, self$parlowertrans, "+")
      Xlhstrans <- as.data.frame(Xlhstrans)
      names(Xlhstrans) <- self$parnames
      # Convert trans to raw
      Xlhsraw <- self$convert_trans_to_raw(Xlhstrans)
      self$add_X(Xlhsraw)
      invisible(self)
    },
    #' @description Convert parameters from transformed scale to raw scale.
    #' @param Xtrans Parameters on the transformed scale
    convert_trans_to_raw = function(Xtrans) {
      convert_back <- FALSE
      if (is.vector(Xtrans)) {
        convert_back <- TRUE
        Xtrans <- matrix(Xtrans, nrow=1)
      }
      Xraw <- Xtrans
      for (i in 1:ncol(Xtrans)) {
        Xraw[, i] <- self$parlist[[i]]$toraw(Xtrans[, i])
      }
      if (convert_back) {
        Xraw <- Xraw[1, , drop=TRUE]
      }
      Xraw
    },
    #' @description Convert parameters from raw scale to transformed scale.
    #' @param Xraw Parameters on the raw scale
    convert_raw_to_trans = function(Xraw) {
      convert_back <- FALSE
      if (is.vector(Xraw)) {
        convert_back <- TRUE
        Xraw <- matrix(Xraw, nrow=1)
      }
      Xtrans <- Xraw
      for (i in 1:ncol(Xtrans)) {
        Xtrans[, i] <- self$parlist[[i]]$fromraw(Xraw[, i])
      }
      if (convert_back) {
        Xtrans <- Xtrans[1, , drop=TRUE]
      }
      Xtrans
    },
    #' @description Change lower/upper bounds of a parameter
    #' @param parname Name of the parameter
    #' @param lower New lower bound. Leave empty if not changing.
    #' @param upper New upper bound. Leave empty if not changing.
    change_par_bounds = function(parname, lower, upper) {
      # browser()
      stopifnot(parname %in% self$parnames)
      parind <- which(parname == self$parnames)
      stopifnot(length(parind) == 1)
      if (!missing(lower)) {
        stopifnot(!is.null(lower), !is.na(lower), length(lower) == 1, is.numeric(lower))
        self$parlist[[parind]]$lower <- lower
        self$parlowerraw[[parind]] <- lower
        self$parlowertrans[[parind]] <- self$parlist[[parind]]$fromraw(lower)
      }
      if (!missing(upper)) {
        stopifnot(!is.null(upper), !is.na(upper), length(upper) == 1, is.numeric(upper))
        self$parlist[[parind]]$upper <- upper
        self$parupperraw[[parind]] <- upper
        self$paruppertrans[[parind]] <- self$parlist[[parind]]$fromraw(upper)
      }
      stopifnot(self$parlist[[parind]]$lower < self$parlist[[parind]]$upper)
      invisible(self)
    },
    #' @description Add new inputs to run using the expected information
    #' criteria
    #' @param n Number of points to add.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    add_EI = function(n, covtype="matern5_2", nugget.estim=TRUE) {
      if (is.null(self$X)) {
        stop('X is null, you need to run_all first.')
      }
      # If unevaluated points, set lowest value.
      Xraw <- self$ffexp$rungrid2()
      Xtrans <- self$convert_raw_to_trans(Xraw)
      if (nrow(Xtrans) == length(self$Z)) { # All have been evaluated
        Z <- self$Z
      } else { # Unevaluated points exist, set to constant liar
        Z <- c(self$Z, rep(min(self$Z), nrow(Xtrans) - length(self$Z)))
        message("Unevaluated points already exist, using constant liar")
      }

      # Just update mod? Set covtype?
      if (covtype == "random") {
        covtype <- sample(c("matern5_2", "matern3_2", "exp", "powexp", "gauss"), 1)
      }
      self$mod <- DiceKriging::km(formula = ~1,
                                  covtype=covtype,
                                  design = Xtrans,
                                  response = Z,
                                  nugget.estim=nugget.estim,
                                  control=list(trace=FALSE))
      if (n==1) {
        # Suppress "Stopped because hard maximum generation limit was hit"
        EIout <- suppressWarnings(DiceOptim::max_EI(model=self$mod,
                                                    lower=self$parlowertrans,
                                                    upper=self$paruppertrans,
                                                    control=list(print.level=0)))
      } else {
        # Select multiple points to be evaluated, useful when running in parallel
        # Suppress "Stopped because hard maximum generation limit was hit."
        EIout <- suppressWarnings(DiceOptim::max_qEI(model=self$mod,
                                                     npoints=n,
                                                     crit="CL", # exact was very slow for more than a couple
                                                     lower=self$parlowertrans,
                                                     upper=self$paruppertrans))
      }
      newXtrans <- EIout$par
      newXraw <- self$convert_trans_to_raw(newXtrans)
      nameoflevel <- "Xdftrans" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, newXraw, suppressMessage=TRUE)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    #' @description Run all unevaluated input points.
    #' @param ... Passed into `ffexp$run_all`. Can set 'parallel=TRUE'
    #' to evaluate multiple points simultaneously as long as all needed
    #' variables have been passed to 'varlist'
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
      # self$Xtrans <- s
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
    #' @param verbose Verbose parameter to pass to ffexp$
    #' @param ... Passed into `ffexp$run_all`.
    run_EI_for_time = function(sec, batch_size, covtype="matern5_2",
                               nugget.estim=TRUE, verbose=0, ...) {
      pb <- progress::progress_bar$new(
        format=paste0("  Running for time (:spin) [:bar] :elapsed / ", sec, "s"),
        total=sec)
      pb$tick(0)
      start_time <- Sys.time() #proc.time()
      ncompleted <- 0
      minbefore <- min(self$Z)
      # while(proc.time()[3] - start_time[3] < sec) {
      while(as.numeric(Sys.time() - start_time, units='secs') < sec) {
        # Only add EI once all existing are run
        if (sum(!h1$ffexp$completed_runs) < .5) {
          self$add_EI(n=batch_size, covtype=covtype, nugget.estim=nugget.estim)
        }
        # Run it
        self$run_all(verbose=0, ...)
        # Increment
        ncompleted <- ncompleted + batch_size
        pb$update(ratio=min(1, as.numeric(Sys.time() - start_time, units='secs') / sec))
      }
      pb$terminate()
      message(paste0("Completed ", ncompleted, " new points in ",
                     round(as.numeric(Sys.time() - start_time, units='secs'), 1), " seconds\n",
                     "Reduced minimum from ", signif(minbefore,5), " to ", signif(min(self$Z),5)))
      invisible(self)
    },
    #' @description Make a plot to summarize the experiment.
    plot = function() {
      self$plotorder()
    },
    #' @description Plot pairs of inputs and output
    pairs = function() {
      # Before changing to transformed coordinates
      # GGally::ggpairs(cbind(self$X, Z=self$Z))
      df <- cbind(self$X, Z=self$Z)
      ggs <- list()
      for (i in 1:ncol(df)) {
        for (j in 1:ncol(df)) {
          si <- colnames(df)[i]
          sj <- colnames(df)[j]
          if (i == j) {
            p <- ggplot2::ggplot(df, ggplot2::aes_string(si)) + ggplot2::geom_histogram(bins = 30)
            if (i < ncol(df)) {
              p <- p + ggplot2::scale_x_continuous(trans = self$parlist[[i]]$ggtrans)
            }
          } else {
            p <- ggplot2::ggplot(df, ggplot2::aes_string(si, sj, color=colnames(df)[ncol(df)])) +
              ggplot2::geom_point() +
              ggplot2::theme(legend.position = "none") +
              ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
            if (i < ncol(df)) {
              p <- p + ggplot2::scale_x_continuous(trans = self$parlist[[i]]$ggtrans)
            }
            if (j < ncol(df)) {
              p <- p + ggplot2::scale_y_continuous(trans = self$parlist[[j]]$ggtrans)
            }
          }
          if (i > 1) {
            p <- p + ggplot2::ylab(NULL)
          }
          if (i==1 && j==1) {
            p <- p + ggplot2::ylab(colnames(df)[1])
          }
          if (j < ncol(df)) {
            p <- p + ggplot2::xlab(NULL)
          }
          # ggs <- c(ggs, p)
          ggs[[(j-1) * ncol(df) + (i-1) + 1]] <- p
        }
      }
      # ggpubr
      do.call(ggpubr::ggarrange, ggs) #+ ggplot2::ylab("Outer ylab")
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
    #' @param addlines Should prediction mean and 95\% interval be plotted?
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    plotX = function(addlines=TRUE, covtype="matern5_2", nugget.estim=TRUE) {
      if (is.null(self$X) || is.null(self$Z)) {
        stop("Nothing has been evaluated yet. Call $run_all() first.")
      }
      stopifnot(!is.null(self$X), !is.null(self$Z), nrow(self$X) == length(self$Z))
      tdf <- cbind(self$X, Z=self$Z, Rank=order(order(self$Z)))
      Xtrans <- self$convert_raw_to_trans(self$X)
      if (addlines) {
        min_ind <- which.min(self$Z)[1]
        min_Xraw <- self$X[min_ind,,drop=TRUE]
        min_Xtrans <- Xtrans[min_ind,,drop=TRUE]
        preddf <- NULL
        npts <- 30
        mod <- DiceKriging::km(formula = ~1,
                               covtype=covtype,
                               design = Xtrans,
                               response = self$Z,
                               nugget.estim=nugget.estim,
                               control=list(trace=FALSE))
        for (i in 1:ncol(self$X)) {
          # Predict at points that are the same for all other components
          predXtrans <- matrix(rep(unlist(min_Xtrans), npts), ncol=ncol(self$X), byrow=T)
          # predZ <- mod
          predXtrans[, i] <- seq(self$parlowertrans[i], self$paruppertrans[i],l=npts)
          predXtransdf <- as.data.frame(predXtrans)
          names(predXtransdf) <- names(self$X)
          predout <- DiceKriging::predict.km(mod, predXtransdf, type="SK", light.return = T)
          predout$mean
          df_i <- data.frame(valuetrans=predXtrans[, i],
                             valueraw=self$parlist[[i]]$toraw((predXtrans[, i])),
                             mean=predout$mean,
                             lower95=predout$lower95, upper95=predout$upper95,
                             index=i, variable=colnames(self$X)[i])
          preddf <- rbind(preddf, df_i)
        }
      }
      # p <- ggplot2::ggplot(reshape2::melt(tdf, id.vars=c('Z', 'Rank')),
      #                      ggplot2::aes(value, Z, color=Rank))
      # if (addlines) {
      #   p <- p +
      #     ggplot2::geom_line(data=preddf, ggplot2::aes(value,    mean,color=NULL), alpha=.1) +
      #     ggplot2::geom_line(data=preddf, ggplot2::aes(value, lower95,color=NULL), alpha=.1) +
      #     ggplot2::geom_line(data=preddf, ggplot2::aes(value, upper95,color=NULL), alpha=.1)
      # }
      # p <- p + ggplot2::geom_point() +
      #   ggplot2::facet_wrap(. ~ variable, scales='free_x') +
      #   ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
      # p

      # New with transformations on x axis

      ggs <- list()
      for (i in 1:ncol(Xtrans)) {
        dfi <- data.frame(value=self$X[, i], Z=self$Z, Rank=order(order(self$Z)))
        # ggi <- ggplot2::ggplot(reshape2::melt(tdf, id.vars=c('Z', 'Rank')),
        #                        ggplot2::aes(value, Z, color=Rank))
        ggi <- ggplot2::ggplot(dfi,
                               ggplot2::aes(value, Z, color=Rank))
        # Add prediction lines
        # ggi <- ggi + preddf[preddf$index==i, ]
        if (addlines) {
          preddfi <-  preddf[preddf$index==i, ]
          ggi <- ggi +
            ggplot2::geom_line(data=preddfi, ggplot2::aes(valueraw,    mean,color=NULL), alpha=.1) +
            ggplot2::geom_line(data=preddfi, ggplot2::aes(valueraw, lower95,color=NULL), alpha=.1) +
            ggplot2::geom_line(data=preddfi, ggplot2::aes(valueraw, upper95,color=NULL), alpha=.1)
        }
        # Add points
        ggi <- ggi + ggplot2::geom_point() +
          #ggplot2::facet_wrap(. ~ variable, scales='free_x') +
          ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
        ggi <- ggi + ggplot2::scale_x_continuous(trans=self$parlist[[i]]$ggtrans)
        ggi <- ggi + ggplot2::xlab(self$parnames[i])
        if (i > 1) {
          ggi <- ggi + ggplot2::ylab(NULL)
        }
        ggs[[i]] <- ggi
      }
      # ggpubr::ggarrange(ggs[[1]], ggs[[2]], common.legend=T, legend="right")

      ggs$common.legend <- T
      ggs$legend <- "right"
      do.call(ggpubr::ggarrange, ggs) + ggplot2::ylab("Outer ylab")

    },
    plotXorder = function() {
      # browser()
    },
    #' @description Plot the 2D plots from inputs to the output.
    #' All other variables are held at their values for the best input.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    plotinteractions = function(covtype="matern5_2", nugget.estim=TRUE) {
      if (is.null(self$X) || is.null(self$Z)) {
        stop("Nothing has been evaluated yet. Call $run_all() first. Use $plotX instead.")
      }
      if (ncol(self$X) == 1) {
        stop("Can't plot interactions with single input.")
      }

      Xtrans <- self$convert_raw_to_trans(self$X)
      mod <- DiceKriging::km(formula = ~1,
                             covtype=covtype,
                             design = Xtrans,
                             response = self$Z,
                             nugget.estim=nugget.estim,
                             control=list(trace=FALSE))
      # predict(mod, self$X, type='sk', light.compute=T, se.compute=F)
      min_ind <- which.min(self$Z)[1]
      min_X <- self$X[min_ind,,drop=TRUE]
      min_Xvec <- unlist(min_X)
      Xtrans <- self$convert_raw_to_trans(self$X)
      min_Xtrans <- Xtrans[min_ind,,drop=TRUE]
      min_Xvectrans <- unlist(min_Xtrans)
      predfunc <- function(X) {
        Xdf <- as.data.frame(X)
        colnames(Xdf) <- colnames(self$X)
        pred <- DiceKriging::predict.km(mod, Xdf, type="sk", light.return = T, se.compute = F)
        pred$mean
      }
      if (ncol(self$X) < 2.5) {
        ContourFunctions::cf_func(predfunc, batchmax = Inf, bar=T,
                                  xlim = c(self$parlowertrans[1], self$paruppertrans[1]),
                                  ylim = c(self$parlowertrans[2], self$paruppertrans[2]),
                                  pts=self$X, gg=TRUE) #+ ggplot2::xlab("xxxx")
      } else {
        ContourFunctions::cf_highdim(predfunc, D=ncol(self$X), baseline=min_Xvectrans,
                                     batchmax = Inf,
                                     pts=matrix(min_Xvectrans, nrow=1), #pts=as.matrix(self$X),
                                     var_names = colnames(self$X),
                                     low = self$parlowertrans, high=self$paruppertrans)
      }
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
if (F) { # Error when no par given
  h1 <- hype$new(
    eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
    extract_output_func = function(odf) {odf$c[1]},
    n_lhs = 10
  )
}

if (F) {
  h1 <- hype$new(
    eval_func = function(a) {print(a); (log(a) - log(1e-4))^2},
    n_lhs = 10,
    par_log$new("a", 1e-8, 1e-1)
  )
  h1
  h1$run_all()
  h1$X
  h1$plotX()
}
