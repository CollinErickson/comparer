#' Microbenchmark compare
#'
#' @param ... Functions to run
#' @param times Number of times to run
#' @param input Object to be passed as input to each function
#' @param inputi Function to be called with the replicate number then passed to each function.
#' @param evaluator An expression that the ... expressions will be passed as "." for evaluation.
#' @param post Function to post-process results.
#' @param target Values the functions are expected to (approximately) return.
#' @param targetin Values that will be given to the result of the run to produce output.
#' @param metric Metric used to compare output values to target.
#' @param paired Should the results be paired for comparison?
#' @importFrom stats median predict t.test
#'
#' @return Data frame of comparison results
#' @export
#'
#' @examples
#' # input given, no post
#' m1 <- mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100))
#' m1
#' # input given with post
#' mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100),
#'   post=function(x){c(x+1, 12)})
#' # input given with post, 30 times
#' mbc(function(x) {Sys.sleep(rexp(1, 3));mean(x)+runif(1)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)+runif(1)}, input=runif(100),
#'   post=function(x){c(x+1, 12)}, times=30)
#' # Name one function and post
#' mbc(function(x) {mean(x)+runif(1)},  a1=function(x) {median(x)+runif(1)},
#'   input=runif(100),  post=function(x){c(rr=x+1, gg=12)}, times=30)
#' # No input
#' m1 <- mbc(function() {x <- runif(100);Sys.sleep(rexp(1, 30));mean(x)},
#'   function() {x <- runif(100);Sys.sleep(rexp(1, 5));median(x)})
mbc <- function(..., times=5, input, inputi, evaluator, post, target, targetin, metric="rmse", paired) {#browser()
  if (!missing(input) && !missing(inputi)) {
    stop("input and inputi should not both be given in")
  }
  # dots are the functiosn to run, n is the number of functions
  # dots <- list(...)
  dots <- as.list(match.call(expand.dots = FALSE)$`...`)
  n <- length(dots)

  fnames <- names(dots)
  if (is.null(fnames)) {fnames <- rep("", n)}
  fnoname <- which(fnames == "")
  if (length(fnoname) > 0) {fnames[fnoname] <- paste0("f", fnoname)}
  if (length(fnames) == 1) {fnames <- list(fnames)}

  # Create objects to hold output data
  runtimes <- matrix(NA, n, times)
  dimnames(runtimes)[[1]] <- fnames
  outs <- rep(list(rep(list(NA),times)), n)
  # if (!missing(post)) {postout <- array(data = NA, dim = c(n, times, post_length))} #rep(list(rep(list(NA),times)), n)}

  # Create a progress bar to show progress
  pb <- progress::progress_bar$new(total=times*n)
  pb$tick(0)
  # Loop over each replicate
  for (j in 1:times) {
    # Get input for replicate if inputi given
    if (!missing(inputi)) {
      if (missing(paired)) {paired <- TRUE} # Same inputs so pair them
      if (is.function(inputi)) {
        input <- inputi(j)
      } else {
        input <- inputi[[j]]
      }
    } else {
      if (missing(paired)) {paired <- FALSE} # Inputs not paired so don't pair unless told to
    }

    # Make input a list/env, this will cause trouble
    if (!missing(input) && !is.list(input) && !is.environment(input)) {
      input <- list(x=input)
    }

    # Loop over each function
    for (i in 1:n) {#browser()
      # See if there is input to each
      if (!missing(input)) { # Single input for all
        if (missing(evaluator)) { # Run as normal
          runtime <- system.time(
            # out <- dots[[i]](input) # Old version, required functions
            out <- eval(dots[[i]], envir=input)
          )
          if (is.function(out)) {print("Trying second time")
            runtime <- system.time(
              # out <- out(input)
              out <- do.call(out, input)
            )
          }
        } else { # dots are input to evaluator to be evaluated
          # browser()
          expr_evaluator <- match.call(expand.dots = FALSE)$`evaluator`
          input$. <- eval(dots[[i]], envir=input)
          runtime <- system.time(
            # out <- dots[[i]](input) # Old version, required functions
            out <- eval(expr_evaluator, envir=input)
          )
          if (is.function(out)) {print("Trying second time")
            runtime <- system.time(
              # out <- out(input)
              out <- do.call(out, input)
            )
          }
        }
      # } else if (!missing(inputi)) { # Different input for each rep
      #   runtime <- system.time(
      #     out <- dots[[i]](inputi(j))
      #   )
      } else { # No input at all
        # runtime <- system.time(
        #   out <- dots[[i]]()
        # )
        runtime <- system.time(
          # out <- dots[[i]](input) # Old version, required functions
          out <- eval(dots[[i]], envir=parent.frame())
        )
        if (is.function(out)) {print("Trying second time 2")
          runtime <- system.time(
            # out <- out(input)
            out <- out() #do.call(out, input)
          )
        }
      }
      runtimes[i, j] <- runtime['elapsed']
      outs[[i]][[j]] <- out
      if (!missing(post) || !missing(target)) {
        # Run post if given
        if (!missing(post)) {
          po <- post(out)
        } else {
          po <- out
        }
        if (!missing(targetin)) { # If targetin in given, use predict function
          # if (any(exists(paste0("predict.",class(po))))) {
            if (is.function(targetin)) {
              targetinj <- targetin(j)
            } else if (is.list(targetin) && !is.data.frame(targetin)) {
              targetinj <- targetin[[j]]
            } else {
              targetinj <- targetin
            }
            po <- predict(po, targetinj)
          # }
        }
        # Run
        if (!missing(target)) {
          if (metric == "rmse") {#browser()
            targetj <- if (is.function(target)) {target(j)}
                       else if (is.list(target)) {target[[j]]}
                       else if (is.character(target) && !is.character(po)) {input[[target]]}
                       else {target}
            po <- c(rmse=sqrt(mean((po - targetj)^2)))
          } else if (metric == "t") {#browser()
              targetj <- if (is.function(target)) {target(j)}
              else if (is.list(target)) {target[[j]]}
              else if (is.character(target) && !is.character(po)) {input[[target]]}
              else {target}
              po.t <- (po$mean - targetj) / po$se
              po <- summary(po.t) #c(mean=mean(po.t)) # rmse=sqrt(mean((po - targetj)^2)))
              names(po) <- paste0("", names(po), " t")
          } else {stop("Only metric recognized are rmse and t")}
        }
        if (i==1 && j==1) { # Initialize once we know length
          postout <- array(data = NA, dim = c(n, times, length(po)))
          dimnames(postout)[[1]] <- fnames
          if (!is.null(names(po))) {dimnames(postout)[[3]] <- names(po)}
        }
        postout[i,j,] <- po
      }
      pb$tick() # tick progress bar
    } # end for i in 1:n
  }
  # browser()

  # Create list to return, set to class mbc so S3 methods can be used
  out_list <- list()
  class(out_list) <- c("mbc", class(out_list))

  # Process run times
  out_list$Run_times <-
    if (times > 5) {
      # plyr::adply(runtimes, 1, function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = 'Function')
      plyr::adply(runtimes, 1, summary, .id = 'Function')
    } else {
      plyr::adply(runtimes, 1, function(x) {sx <- sort(x); c(Sort=(sx), mean=mean(x))}, .id = 'Function')
    }

  # Run post to post process
  if (!missing(post) || !missing(target)) {
    if (times > 5) {
      post_df_disp <- plyr::adply(postout, c(1,3), function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = c('Func','Stat'))
      if (paired) {
        if (n > 1) {
          for (i1 in 1:(n-1)) {
            for (i2 in (i1+1):n) {
              for (istat in 1:(dim(postout)[3])) {
                labeli <- paste0(dimnames(postout)[[1]][i1],'-',dimnames(postout)[[1]][i2])
                diffs <- postout[i1,,istat] - postout[i2,,istat]
                ttest_diffs <- t.test(diffs)
                statname <- dimnames(postout)[[3]][istat]
                comp12 <- data.frame(Func=labeli, Stat=statname, min=min(diffs), med=median(diffs), mean=mean(diffs), max=max(diffs), t=ttest_diffs$statistic, p=ttest_diffs$p.value)
                # names(comp12)[3:(3+length(diffs)-1)] <- paste0("V", 1:(length(diffs)))
                # print(comp12)
                # post_df_disp <- rbind(post_df_disp, comp12)
                if (i1 == 1 && i2 == 2) {
                  comp_df <- comp12
                } else {
                  comp_df <- rbind(comp_df, comp12)
                }
              }
            }
          }
          out_list$Compare <- comp_df
        }
      } else {

      }
    } else {#browser()
      if (paired) { # Don't sort if paired, get differences
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {c(x, mean=mean(x))}, .id = c('Func','Stat'))
        if (n > 1) {
          for (i1 in 1:(n-1)) {
            for (i2 in (i1+1):n) {
              for (istat in 1:(dim(postout)[3])) {
                labeli <- paste0(dimnames(postout)[[1]][i1],'-',dimnames(postout)[[1]][i2])
                diffs <- postout[i1,,istat] - postout[i2,,istat]
                ttest_diffs <- t.test(diffs)
                statname <- dimnames(postout)[[3]][istat]
                comp12 <- data.frame(Func=labeli, Stat=statname, t(diffs), mean=mean(diffs), t=ttest_diffs$statistic, p=ttest_diffs$p.value)
                names(comp12)[3:(3+length(diffs)-1)] <- paste0("V", 1:(length(diffs)))
                # print(comp12)
                # post_df_disp <- rbind(post_df_disp, comp12)
                if (i1 == 1 && i2 == 2) {
                  comp_df <- comp12
                } else {
                  comp_df <- rbind(comp_df, comp12)
                }
              }
            }
          }
          out_list$Compare <- comp_df
        }

      } else { # If not paired/ordered, then sort them
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c(Sort=(sx), mean=mean(x))}, .id = c('Func','Stat'))
      }
    }
    out_list$RawOutput <- outs
    out_list$Output <- postout
    out_list$Output_disp <- post_df_disp
  } else {#browser()
    # Check if all outs have same length, then convert to df if small enough just as postprocessing would
    #  in case single return value already is post
    lengths <- sapply(outs, function(listi) {sapply(listi, length)})
    len <- lengths[[1]]
    if (all(lengths == len) && any(class(outs[[1]][[1]]) %in% c("numeric", "character", "logical"))) { # Try to auto-post-process
      # Convert data to array
      postout <- array(data = NA, dim = c(n, times, len))
      dimnames(postout)[[1]] <- fnames
      # if (class(outs[[1]][[1]]) %in% c("numeric", "character", "logical")) {
        for (i in 1:n) {
          for (j in 1:times) {
            postout[i, j, ] <- outs[[i]][[j]]
          }
        # }
      }
      # Post-process
      if (is.numeric(postout)) {
        if (times > 5) {
          post_df_disp <- plyr::adply(postout, c(1,3), function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = c('Func','Stat'))
        } else {
          post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c(Sort=(sx), mean=mean(x))}, .id = c('Func','Stat'))
        }
      } else if (is.logical(postout)) {
        # post_df_disp <- plyr::adply(postout, c(1,3), table, .id = c('Func','Stat'))
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {tr <- sum(x);c('TRUE'=tr,'FALSE'=times-tr)}, .id = c('Func','Stat'))

      } else {
        post_df_disp <- postout
      }
      # Set outputs
      out_list$RawOutput <- outs
      out_list$Output <- postout
      out_list$Output_disp <- post_df_disp
    } else { # Can't auto-post-process
      out_list$Output <- outs
    }
  }
  # browser()
  if (is.array(out_list$Output)) {
    dimnames(out_list$Output)[[1]] <- fnames
  }
  out_list
}


#' Plot mbc class
#'
#' @param x Object of class mbc
#' @param ... Additional parameters
#' @importFrom graphics stripchart
#'
#' @return None
#' @export
#'
#' @examples
#' m1 <- mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100))
#' plot(m1)
plot.mbc <- function(x, ...) {
  stripchart(x$Run_times)
}

#' Print mbc class
#'
#' @param x Object of class mbc
#' @param ... Additional parameters
#'
#' @return None
#' @export
#'
#' @examples
#' m1 <- mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100))
#' print(m1)
print.mbc <- function(x, ...) {#browser()
  nam <- names(x)
  if ('Run_times' %in% nam) {
    cat("Run times (sec)\n")
    print(x$Run_times)
  }
  if ('Output_disp' %in% nam) {
    cat("Output summary\n")
    if (length(unique(x$Output_disp$Stat)) > 1) { # If more than 1 stat, print separately by function
      tp <- plyr::dlply(x$Output_disp, 'Func', identity)
      suppress <- lapply(tp, print)
    } else {
      print(x$Output_disp)
    }
  } else if ('Output' %in% nam) {
    if (is.data.frame(x$Output) || is.numeric(x$Output)) { # Only print df, not list
      cat("\nOutput \n")
      print(x$Output)
    }
  }
  if ("Compare" %in% nam) {
    cat("\nCompare\n")
    print(x$Compare)
  }
}
