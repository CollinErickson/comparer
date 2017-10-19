
<!-- README.md is generated from README.Rmd. Please edit that file -->
comparer
========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/comparer)](https://cran.r-project.org/package=comparer) [![Travis-CI Build Status](https://travis-ci.org/CollinErickson/comparer.svg?branch=master)](https://travis-ci.org/CollinErickson/comparer) <!-- [![Coverage Status](https://img.shields.io/codecov/c/github/CollinErickson/comparer/master.svg)](https://codecov.io/github/CollinErickson/comparer?branch=master) --> [![Coverage Status](https://codecov.io/gh/CollinErickson/comparer/branch/master/graph/badge.svg)](https://codecov.io/github/CollinErickson/comparer?branch=master) <!-- [![Coverage Status](https://img.shields.io/coveralls/CollinErickson/comparer.svg)](https://coveralls.io/r/CollinErickson/comparer?branch=master) --> [![Coverage Status](https://coveralls.io/repos/github/CollinErickson/comparer/badge.svg?branch=master)](https://coveralls.io/github/CollinErickson/comparer?branch=master)

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
#>   Function Sort1 Sort2 Sort3 Sort4 Sort5 mean sd
#> 1     mean     0     0     0     0     0    0  0
#> 2   median     0     0     0     0     0    0  0
#> 
#> Output summary
#>     Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5      mean
#> 1   mean    1 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470
#> 2 median    1 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696
#>   sd
#> 1  0
#> 2  0
```

To get the data to be generated for each trial, use the `inputi` argument to set a variable that the functions call. The arguments `mean(x)` and `median(x)` are captured as expressions, as is `{x=rexp(100)}` for `inputi`. Note that `x` is stored in that code chunk, and the functions both call `x` in the evaluation. You can see that the values are now different for each trial.

``` r
## Regenerate the data each time
mbc(mean(x), median(x), inputi={x=rexp(100)})
#> Run times (sec)
#>    Function Sort1 Sort2 Sort3 Sort4 Sort5 mean sd
#> 1   mean(x)     0     0     0     0     0    0  0
#> 2 median(x)     0     0     0     0     0    0  0
#> 
#> Output summary
#>        Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5
#> 1   mean(x)    1 0.8813966 0.9069863 0.9513831 0.9890381 1.2063718
#> 2 median(x)    1 0.6404516 0.6488801 0.6836623 0.7563764 0.7901115
#>        mean         sd
#> 1 0.9870352 0.12937442
#> 2 0.7038964 0.06642411
```

If the code chunks to compare are all simple functions that take a single input, then the value in `inputi` does not need to be saved as any variable, and the code chunks can just be the functions. For example, the previous code can be simplified as below.

``` r
## Simplify the call when the input is single object
mbc(mean, median, inputi=rexp(100))
#> Run times (sec)
#>   Function Sort1 Sort2 Sort3 Sort4 Sort5 mean sd
#> 1     mean     0     0     0     0     0    0  0
#> 2   median     0     0     0     0     0    0  0
#> 
#> Output summary
#>     Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5      mean
#> 1   mean    1 0.8781278 1.0184794 1.1029084 1.1511791 1.1535740 1.0608537
#> 2 median    1 0.6119180 0.6358414 0.7141539 0.7832192 0.9887379 0.7467741
#>          sd
#> 1 0.1158757
#> 2 0.1511878
```
