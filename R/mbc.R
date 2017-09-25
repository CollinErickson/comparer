#' Microbenchmark compare
#'
#' @param ... Functions to run
#' @param times Number of times to run
#' @param input Object to be passed as input to each function
#' @param inputi Function to be called with the replicate number then passed to each function.
#' @param post Function to post-process results.
#' @param target Values the functions are expected to (approximately) return.
#' @importFrom stats median
#'
#' @return Data frame of comparison results
#' @export
#'
#' @examples
#' m1 <- mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100))
#' mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100),
#'   post=function(x){c(x+1, 12)})
#' mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)+runif(1)},
#'   function(x) {Sys.sleep(rexp(1, 5));median(x)+runif(1)}, input=runif(100),
#'   post=function(x){c(x+1, 12)}, times=3)
#' m1 <- mbc(function() {x <- runif(100);Sys.sleep(rexp(1, 30));mean(x)},
#'   function() {x <- runif(100);Sys.sleep(rexp(1, 5));median(x)})
mbc <- function(..., times=5, input=NULL, inputi=NULL, post, target) {#browser()
  if (!is.null(input) && !is.null(inputi)) {
    stop("input and inputi should not both be given in")
  }
  # dots are the functiosn to run, n is the number of functions
  dots <- list(...)
  n <- length(dots)

  fnames <- names(dots)
  fnoname <- which(fnames == "")
  fnames[fnoname] <- paste0("f", fnoname)

  # Create objects to hold output data
  runtimes <- matrix(NA, n, times)
  dimnames(runtimes)[[1]] <- fnames
  outs <- rep(list(rep(list(NA),times)), n)
  # if (!missing(post)) {postout <- array(data = NA, dim = c(n, times, post_length))} #rep(list(rep(list(NA),times)), n)}

  # Loop over each replicate
  for (j in 1:times) {
    # Get input for replicate if inputi given
    if (!is.null(inputi)) {
      if (is.function(inputi)) {
        input <- inputi(j)
      } else {
        input <- inputi[[j]]
      }
    }
    # Loop over each function
    for (i in 1:n) {
      # See if there is input to each
      if (!missing(input)) { # Single input for all
        runtime <- system.time(
          out <- dots[[i]](input)
        )
      # } else if (!missing(inputi)) { # Different input for each rep
      #   runtime <- system.time(
      #     out <- dots[[i]](inputi(j))
      #   )
      } else { # No input at all
        runtime <- system.time(
          out <- dots[[i]]()
        )
      }
      runtimes[i, j] <- runtime['elapsed']
      outs[[i]][[j]] <- out
      if (!missing(post)) {
        po <- post(out)
        if (i==1 && j==1) { # Initialize once we know length
          postout <- array(data = NA, dim = c(n, times, length(po)))
        }
        postout[i,j,] <- po
      }
    }
  }
  # browser()

  # Create list to retun, set to class mbc so S3 methods can be used
  out_list <- list()
  class(out_list) <- c("mbc", class(out_list))
  out_list$Run_times <-
    if (times > 5) {
      plyr::adply(runtimes, 1, function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = 'Function')
    } else {
      plyr::adply(runtimes, 1, function(x) {sx <- sort(x); c((sx), mean=mean(x))}, .id = 'Function')
    }
  if (!missing(post)) {
    if (times > 5) {
      post_df_disp <- plyr::adply(postout, c(1,3), function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = c('Func','Stat'))
    } else {
      post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c((sx), mean=mean(x))}, .id = c('Func','Stat'))
    }
    out_list$RawOutput <- outs
    out_list$Output <- postout
    out_list$Output_disp <- post_df_disp
  } else {
    # Check if all outs have same length, then convert to df if small enough just as postprocessing would
    #  in case single return value already is post
    lengths <- sapply(outs, function(listi) {sapply(listi, length)})
    len <- lengths[[1]]
    if (all(lengths == len)) { # Try to auto-post-process
      # Convert data to array
      postout <- array(data = NA, dim = c(n, times, len))
      for (i in 1:n) {
        for (j in 1:times) {
          postout[i, j, ] <- outs[[i]][[j]]
        }
      }
      # Post-process
      if (times > 5) {
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = c('Func','Stat'))
      } else {
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c((sx), mean=mean(x))}, .id = c('Func','Stat'))
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
  if ('Output' %in% nam) {
    if (is.data.frame(x$Output) || is.numeric(x$Output)) { # Only print df, not list
      cat("\nOutput \n")
      print(x$Output)
    }
  }
}
