I got an email from Brian Ripley saying that I had to fix my usage of microbenchmark. It is in suggests but not used conditionally. I only had it once in my vignette, I changed it to be

```

if (requireNamespace("microbenchmark", quietly = TRUE)) {
  x <- runif(100)
  microbenchmark::microbenchmark(sqrt(x), x ^ .5)
} else {
  "microbenchmark not available on your computer"
}
```

This should correct the problem.

## Test environments
* local Window 7 install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note



## Reverse dependencies

None.
