# function(inputs, eval_func, method="epsgreedy") {
#   inputs
# }

..MAB_R6 <- R6::R6Class(
  classname="MAB",
  public=list(
    inputs=NULL,
    ninputs=NULL,
    # inputs2=NULL,
    inputdf=NULL,
    method=NULL,
    eps=NULL, # Prob to select a random input
    exp=NULL, # FFexp experiment object
    eval_func=NULL,
    minimize=NULL,
    parallel=NULL,
    parallel_cores=NULL,
    initialize = function(inputs, n0=1, eval_func, method="UCB", eps=.1, minimize=TRUE,
                          parallel=FALSE,
                          parallel_cores="detect") {
      cat('initializing mab\n')
      # inputs is vector
      self$inputs <- inputs
      self$ninputs <- length(inputs)
      self$method <- method
      self$eps <- eps
      # print(inputs); print(n0)
      stopifnot(is.numeric(n0), length(n0) == 1)
      inputs2 <- expand.grid(input=inputs, K=1:n0)
      self$eval_func <- eval_func
      eval_func2 <- function(input, K) {do.call(self$eval_func, list(self$inputs[input]))}
      # self$exp <- comparer::ffexp$new(input=inputs2, eval_func=eval_func)
      self$exp <- comparer::ffexp$new(input=expand.grid(input=1:self$ninputs, K=1:n0), eval_func=eval_func2)
      self$minimize <- minimize
      self$parallel <- parallel
      self$parallel_cores <- parallel_cores
    },
    run = function(verbose=0, parallel=self$parallel, parallel_cores=self$parallel_cores, ...) {
      self$exp$run_all(verbose=verbose, parallel=parallel, parallel_cores=parallel_cores, ...)
    },
    add1 = function(run=TRUE, method=self$method, eps=self$eps, Nmin=2) {
      stopifnot(length(eps)==1, eps>=0, eps<=1, length(Nmin)==1, Nmin>=1,
                length(method)==1, length(run)==1, is.logical(run))
      method <- tolower(method)
      minmult <- if (self$minimize) {-1} else {1}
      ocdf <- self$exp$outcleandf
      # Count of all allocated
      countdf_all <- self$exp$rungrid2() %>% group_by(input) %>% tally(name="Ntotal")
      # Countdf can include some trials not evaluated yet.
      countdf_completed <- ocdf %>% group_by(input) %>% filter(!is.na(input)) %>%
        summarize(N=n(),mn=mean(V1, na.rm=T), std=sd(V1, na.rm=T),
                  UCB=mn+2*std, LCB=mn-2*std)
      countdf <- full_join(countdf_all, countdf_completed, "input")
      if (min(countdf$Ntotal) < Nmin) {
        nextind <- countdf %>% arrange(N) %>% .$input %>% .[[1]]
        nextK <- min(countdf$N) + 1
      } else if (runif(1) < eps || method=="random") {
        # if () {
        nextind <- sample(1:self$ninputs, 1)
        nextK <- (ocdf %>% filter(input==nextind) %>% nrow) + 1
        # }
      } else if (method=="greedy") {
        nextdf <- countdf %>% arrange(-minmult*mn)
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else if (method %in% c("cb", "ucb", "lcb")) {
        if (self$minimize) {
          nextdf <- countdf %>% arrange(LCB)
        } else {
          nextdf <- countdf %>% arrange(-UCB)
        }
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else if (method=="ts") {
        countdf <- countdf %>% mutate(samp=mn+std*rt(n=n(), df=N-1))
        if (self$minimize) {
          nextdf <- countdf %>% arrange(samp)
        } else {
          nextdf <- countdf %>% arrange(-samp)
        }
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else if (method=="fewest") {
        nextdf <- countdf %>% arrange(N)
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else {
        stop(paste0("Bad method given (", method,"), pick one of epsgreedy, CB, TS, or fewest"))
      }
      # cat('next ind is', nextind, 'nextK', nextK, '\n')
      if (is.na(nextind)) {stop("Next ind is NA")}
      # Add new level to experiment, returns new object
      expnew <- self$exp$add_level("input", list(input=nextind, K=nextK), suppressMessage = T)
      self$exp <- expnew
      # Run the new one
      # self$exp$run_all(verbose=0)
      if (run) {
        self$run(verbose=0, parallel=FALSE)
      }
    },
    addn = function(n, run=TRUE, method=self$method, eps=self$eps, Nmin=2,
                    parallel=self$parallel && n>1, parallel_cores=self$parallel_cores) {
      stopifnot(as.integer(n) == n, length(n)==1)
      for (i in 1:n) {
        self$add1(run=FALSE, method=method, eps=eps, Nmin=Nmin)
      }
      if (run) {
        self$run(verbose=0, parallel=parallel, parallel_cores=parallel_cores)
      }
    },
    plot = function(flip=F) {
      df <- self$exp$outcleandf
      dfsum <- df %>% group_by(input) %>%
        summarize(N=n(), mn=mean(V1), std=sd(V1),
                  LCB=mn-2*std, UCB=mn+2*std,
                  LCBmn=mn-2*std/sqrt(N), UCBmn=mn+2*std/sqrt(N))
      # df$inputx <- factor(df$input) %>% as.numeric
      df$inputorig <- factor(self$inputs[df$input], self$inputs)
      dfsum$inputorig <- factor(self$inputs[dfsum$input], self$inputs)
      # p <- ggplot2::ggplot(df, ggplot2::aes(inputorig, V1)) +
      #   ggplot2::geom_rect(data=dfsum %>% filter(!is.na(std)),
      #                      aes(xmin=input-.05,xmax=input+.05, ymin=LCB, ymax=UCB, y=NULL), fill="gray77") +
      #   ggplot2::geom_rect(data=dfsum %>% filter(!is.na(std)),
      #                      aes(xmin=input-.05,xmax=input+.05, ymin=LCBmn, ymax=UCBmn, y=NULL), fill="gray67") +
      #   ggplot2::geom_point(data=dfsum, aes(input, mn), color='green', size=3) +
      #   ggplot2::geom_point()
      dfsum2 <- dfsum %>% filter(!is.na(std))
      p <- ggplot2::ggplot(df, ggplot2::aes(inputorig, V1)) +
        ggplot2::geom_rect(data=dfsum %>% filter(!is.na(std)),
                           aes(xmin=input-.05,xmax=input+.05, ymin=LCB, ymax=UCB, y=NULL),
                           xmin=dfsum$input-.05,xmax=dfsum$input+.05, fill="gray77") +
        ggplot2::geom_rect(data=dfsum %>% filter(!is.na(std)),
                           aes(ymin=LCBmn, ymax=UCBmn, y=NULL),
                           xmin=dfsum$input-.05,xmax=dfsum$input+.05, fill="gray67") +
        ggplot2::geom_point(data=dfsum, aes(input, mn), color='green', size=3) +
        ggplot2::geom_point() +
        ylab(paste0("Output (",if (self$minimize) {"min"} else {"max"},")"))
      if (flip) {p <- p + coord_flip()}
      p
    },
    print = function() {
      s <- c("Multi-armed bandit experiment from the comparer package")
      s <- c(s, paste0("    # of arms: ", self$ninputs, ',    using method: ', self$method))
      s <- c(s, paste0("    # of trials completed: ", sum(self$exp$completed_runs), "/", length(self$exp$completed_runs)))
      if (sum(!self$exp$completed_runs) > 0) {
        s <- c(s, paste0("    Use $run() to evaluate all remaining trials"))
      }
      s <- c(s, "    Use $add1() to add a trial and run it")
      s <- c(s, "    Use $plot() to view results")
      s <- paste(s, collapse="\n")
      cat(s, "\n")
    }
  )
)

if (F) {
  e1 <- ..MAB_R6$new(1:5, n0=1, eval_func=function(input, ...) {rnorm(1, input, 2)}, minimize=F, method="TS")
  e1$exp
  e1$run() #exp$run_all()
  e1$exp$outcleandf
  library(dplyr); library(ggplot2)
  e1$add1()
  e1$exp
  for (i in 1:10) {
    e1$add1()
    # print(e1$exp$outcleandf)
    # plot(e1$exp$outcleandf$input, e1$exp$outcleandf$V1)
    e1$plot() %>% print
  }
  print(e1)
}


if (F) {

  e1 <- ..MAB_R6$new(1:5, n0=2, eval_func=function(input, ...) {print(input);rnorm(1, input, 2)}, minimize=F, method="TS")
  e1
  e1$exp$rungrid2()
  e1$run()
  e1$plot()
  e1$add1(run=F)
  e1$add1(run=F)
  e1$add1(run=F)
  e1
  e1$run()
  e1$plot()
}

if (F) {

  e1 <- ..MAB_R6$new(1:5, n0=2, eval_func=function(input, ...) {rnorm(1, input, (6-input)^2)}, minimize=F, method="TS")
  e1
  e1$exp$rungrid2()
  system.time(e1$run())
  e1$plot()
  e1$add1(run=F)
  e1$add1(run=F)
  e1$add1(run=F, eps=1)
  e1
  e1$run()
  e1$plot()
  e1$exp$plot_run_times()
  e1$addn(10L, parallel=T)
  e1$plot()
  e1$exp$plot_run_times()
}



if (F) {
  # Character input ----
  e1 <- ..MAB_R6$new(c("a","b","c"), n0=2,
                     eval_func=function(input, ...) {
                       input2 <- which(c("a","b","c") == input);
                       rnorm(1, input2^2, (4-input2)^2)},
                     minimize=F, method="TS")
  e1
  e1$exp$rungrid2()
  system.time(e1$run())
  e1$plot()
  e1$add1(run=F)
  e1$add1(run=F)
  e1$add1(run=F, eps=1)
  e1
  e1$run()
  e1$plot()
  e1$exp$plot_run_times()
  e1$addn(10L, parallel=T)
  e1$plot()
  e1$exp$plot_run_times()
}

