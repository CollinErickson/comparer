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
mbc <- function(..., times=5, input) {#browser()
  dots <- list(...)
  n <- length(dots)
  runtimes <- matrix(NA, n, times)
  outs <- rep(list(rep(list(NA),times)), n)
  for (i in 1:n) {
    for (j in 1:times) {
      runtime <- system.time(
        out <- dots[[i]](input)
      )
      runtimes[i, j] <- runtime['elapsed']
      outs[[i]][[j]] <- out
    }
  }
  plyr::adply(runtimes, 1, function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = 'Func')
}
