#' Microbenchmark compare
#'
#' @param ... Functions to run
#' @param times Number of times to run
#'
#' @return Data frame of comparison results
#' @export
#'
#' @examples
#' m1 <- mbc()
mbc <- function(..., times=1) {
  dots <- list(...)
  n <- length(dots)
  runtimes <- matrix(NA, n, times)
  outs <- list()
  for (i in 1:n) {
    for (j in 1:times) {
      runtime <- system.time(
        out <- dots[[i]]()
      )
      runtimes[i, j] <- runtime$elapsed
      outs[[i]][[j]] <- out
    }
  }
}
