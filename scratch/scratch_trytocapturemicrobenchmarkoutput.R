tf <- function(...) {

  dots <- as.list(match.call(expand.dots = FALSE)$`...`)
  print(dots)
  o1 <- list()
  f1 <- function(i) {o1[[i]] <<- eval(dots[[1]])}
  browser()
  f1(1)
  mb1 <- list()
  f2 <- function(i) {mb1[[i]] <<- microbenchmark::microbenchmark(o1[[i]] <<- eval(dots[[1]]))}
  print(o1)
}
a <- 12
tf(2*a)
