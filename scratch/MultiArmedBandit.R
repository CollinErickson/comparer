function(inputs, eval_func, method="epsgreedy") {
  inputs
}

..MAB_R6 <- R6::R6Class(
  classname="MAB",
  public=list(
    inputs=NULL,
    ninputs=NULL,
    inputs2=NULL,
    method=NULL,
    exp=NULL,
    minimize=NULL,
    initialize = function(inputs, n0=1, eval_func, method="epsgreedy", minimize=TRUE) {
      cat('initializing mab\n')
      # inputs is vector
      self$inputs <- inputs
      self$ninputs <- length(inputs)
      self$method <- method
      # print(inputs); print(n0)
      self$inputs2 <- data.frame(input=inputs, K=n0)
      self$exp <- comparer::ffexp$new(input=self$inputs2, eval_func=eval_func)
      self$minimize <- minimize
    },
    run = function(verbose=0) {
      self$exp$run_all(verbose=verbose)
    },
    add1 = function() {
      minmult <- if (self$minimize) {-1} else {1}
      ocdf <- self$exp$outcleandf
      countdf <- ocdf %>% group_by(input) %>%
        summarize(N=n(),mn=mean(t.output.), std=sd(t.output.),
                  UCB=mn+2*std, LCB=mn-2*std)
      if (min(countdf$N) < 2) {
        nextind <- countdf %>% arrange(N) %>% .$input %>% .[[1]]
        nextK <- min(countdf$N) + 1
      } else if (self$method=="epsgreedy") {
        if (runif(1) < .2) {#browser()
          nextind <- sample(1:self$ninputs, 1)
          nextK <- (ocdf %>% filter(input==nextind) %>% nrow) + 1
        } else {
          nextdf <- countdf %>% arrange(-minmult*mn)
          nextind <- nextdf$input[1]
          nextK <- nextdf$N[1] + 1
        }
      } else if (self$method=="CB") {
        if (self$minimize) {
          nextdf <- countdf %>% arrange(LCB)
        } else {
          nextdf <- countdf %>% arrange(-UCB)
        }
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else if (self$method=="TS") {
        countdf <- countdf %>% mutate(samp=mn+std*rt(n=n(), df=N-1))
        if (self$minimize) {
          nextdf <- countdf %>% arrange(samp)
        } else {
          nextdf <- countdf %>% arrange(-samp)
        }
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else if (self$method=="fewest") {
        nextdf <- countdf %>% arrange(N)
        nextind <- nextdf$input[1]
        nextK <- nextdf$N[1] + 1
      } else {
        stop(paste0("Bad method given (", self$method,"), pick one of epsgreedy, CB, TS, or fewest"))
      }
      # cat('next ind is', nextind, 'nextK', nextK, '\n')
      # Add new level to experiment, returns new object
      expnew <- self$exp$add_level("input", list(input=nextind, K=nextK), suppressMessage = T)
      self$exp <- expnew
      # Run the new one
      # self$exp$run_all(verbose=0)
      self$run(verbose=0)
    },
    plot = function(flip=F) {
      df <- self$exp$outcleandf
      # browser()
      dfsum <- df %>% group_by(input) %>%
        summarize(N=n(), mn=mean(t.output.), std=sd(t.output.),
                  LCB=mn-2*std, UCB=mn+2*std,
                  LCBmn=mn-2*std/sqrt(N), UCBmn=mn+2*std/sqrt(N))
      p <- ggplot2::ggplot(df, ggplot2::aes(input, t.output.)) +
        ggplot2::geom_rect(data=dfsum %>% filter(!is.na(std)),
                           aes(xmin=input-.05,xmax=input+.05, ymin=LCB, ymax=UCB, y=NULL), fill="gray77") +
        ggplot2::geom_rect(data=dfsum %>% filter(!is.na(std)),
                           aes(xmin=input-.05,xmax=input+.05, ymin=LCBmn, ymax=UCBmn, y=NULL), fill="gray67") +
        ggplot2::geom_point(data=dfsum, aes(input, mn), color='green', size=3) +
        ggplot2::geom_point()
      if (flip) {p <- p + coord_flip()}
      p
    },
    print = function() {
      s <- c("Multi-armed bandit experiment from the comparer package")
      s <- c(s, paste0("    # of arms: ", self$ninputs))
      s <- c(s, paste0("    # of trials completed: ", nrow(self$exp$outcleandf)))
      s <- c(s, "    Use $add1() to add a trial and run it")
      s <- c(s, "    Use $plot() to view results")
      s <- paste(s, collapse="\n")
      cat(s, "\n")
    }
  )
)

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
  # plot(e1$exp$outcleandf$input, e1$exp$outcleandf$t.output.)
  e1$plot() %>% print
}
print(e1)
