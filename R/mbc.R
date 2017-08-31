#' Microbenchmark compare
#'
#' @param ... Functions to run
#' @param times Number of times to run
#'
#' @return Data frame of comparison results
#' @export
#'
#' @examples
#' m1 <- mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)}, function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100))
#' mbc(function(x) {Sys.sleep(rexp(1, 30));mean(x)}, function(x) {Sys.sleep(rexp(1, 5));median(x)}, input=runif(100), post=function(x){c(x+1, 12)})
mbc <- function(..., times=5, input, post, target) {#browser()
  dots <- list(...)
  n <- length(dots)
  runtimes <- matrix(NA, n, times)
  outs <- rep(list(rep(list(NA),times)), n)
  # if (!missing(post)) {postout <- array(data = NA, dim = c(n, times, post_length))} #rep(list(rep(list(NA),times)), n)}
  for (i in 1:n) {
    for (j in 1:times) {
      runtime <- system.time(
        out <- dots[[i]](input)
      )
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
  browser()
  times_df <-
    if (times > 5) {
      plyr::adply(runtimes, 1, function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = 'Func')
    } else {
      plyr::adply(runtimes, 1, function(x) {sx <- sort(x); c((sx), mean=mean(x))}, .id = 'Func')
    }
  if (!missing(post)) {
    if (times > 5) {
      post_df <- plyr::adply(postout, c(1,3), function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = c('Func','Stat'))
    } else {
      post_df <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c((sx), mean=mean(x))}, .id = c('Func','Stat'))
    }
    return(list('Run times'=times_df, 'Output'=post_df))
  }
  times_df
}
