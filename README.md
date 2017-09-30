
<!-- README.md is generated from README.Rmd. Please edit that file -->
comparer
========

[![Travis-CI Build Status](https://travis-ci.org/CollinErickson/comparer.svg?branch=master)](https://travis-ci.org/CollinErickson/comparer)

The goal of comparer is to make it easy to compare the results of different code chunks that are trying to do the same thing. The R package `microbenchmark` is great for comparing the speed of code, but there's no way to compare their ouput to see which is more accurate.

Installation
------------

You can install comparer from github with:

``` r
# install.packages("devtools")
devtools::install_github("CollinErickson/comparer")
```

Example
-------

The main function of this package is `mbc`, for "model benchmark compare". It is designed to be similar to the package `microbenchmark`, allow for fast comparisons except including the output/accuracy of the code evaluated instead of just timing.

Suppose you want to see how the mean and median of a sample of 100 randomly generated data points from an exponential compare. Then, as demonstrated below, you can use the function `mbc`, with the functions mean and median, and then `input=rexp(100)`. It outputs the run times of each, and then the results from the five trials, where five is the default setting for `times`. The run times aren't useful because they are all fast. For more precise timing (&lt;0.01 seconds), you should use `microbenchmark`. The trials all have the same output since there is no randomness, the same data is used for each trial. The "Output summary" shows that the mean is near 1, while the median is near 0.6.

``` r
## basic example code
library(comparer)
mbc(mean, median, input=rexp(100))
#> Run times (sec)
#>   Function Sort1 Sort2 Sort3 Sort4 Sort5 mean
#> 1       f1     0     0     0     0     0    0
#> 2       f2     0     0     0     0     0    0
#> Output summary
#>   Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5      mean
#> 1   f1    1 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470
#> 2   f2    1 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696
```

To get the data to be generated for each trial, use the `inputi` argument to set a variable that the functions call. The arguments `mean(x)` and `median(x)` are captured as expressions, as is `{x=rexp(100)}` for `inputi`. Note that you must put brackets around the value of `inputi` if it is an expression or it will give an error. You can see that the values are now different for each trial.

``` r
## Regenerate the data each time.
mbc(mean(x), median(x), inputi={x=rexp(100)})
#> Run times (sec)
#>   Function Sort1 Sort2 Sort3 Sort4 Sort5 mean
#> 1       f1     0     0     0     0     0    0
#> 2       f2     0     0     0     0     0    0
#> Output summary
#>   Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5      mean
#> 1   f1    1 0.8813966 0.9069863 0.9513831 0.9890381 1.2063718 0.9870352
#> 2   f2    1 0.6404516 0.6488801 0.6836623 0.7563764 0.7901115 0.7038964
```
