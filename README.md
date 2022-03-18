
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comparer

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/comparer)](https://cran.r-project.org/package=comparer)
[![Travis-CI Build
Status](https://travis-ci.org/CollinErickson/comparer.svg?branch=master)](https://travis-ci.org/CollinErickson/comparer)
<!-- [![Coverage Status](https://img.shields.io/codecov/c/github/CollinErickson/comparer/master.svg)](https://codecov.io/github/CollinErickson/comparer?branch=master) -->
[![Coverage
Status](https://codecov.io/gh/CollinErickson/comparer/branch/master/graph/badge.svg)](https://codecov.io/github/CollinErickson/comparer?branch=master)
<!-- [![Coverage Status](https://img.shields.io/coveralls/CollinErickson/comparer.svg)](https://coveralls.io/r/CollinErickson/comparer?branch=master) -->
[![Coverage
Status](https://coveralls.io/repos/github/CollinErickson/comparer/badge.svg?branch=master)](https://coveralls.io/github/CollinErickson/comparer?branch=master)
<!-- badges: start -->
[![R-CMD-check](https://github.com/CollinErickson/comparer/workflows/R-CMD-check/badge.svg)](https://github.com/CollinErickson/comparer/actions)
<!-- badges: end -->

The goal of comparer is to make it easy to compare the results of
different code chunks that are trying to do the same thing. The R
package `microbenchmark` is great for comparing the speed of code, but
there’s no way to compare their output to see which is more accurate.

## Installation

You can install comparer from GitHub with:

``` r
# install.packages("devtools")
# devtools::install_github("CollinErickson/comparer")
```

## `mbc`

One of the two main functions of this package is `mbc`, for “model
benchmark compare.” It is designed to be similar to the package
`microbenchmark`, allow for fast comparisons except including the
output/accuracy of the code evaluated instead of just timing.

Suppose you want to see how the mean and median of a sample of 100
randomly generated data points from an exponential distribution compare.
Then, as demonstrated below, you can use the function `mbc`, with the
functions mean and median, and then `input=rexp(100)`. The value of
`input` will be stored as `x`, so `mean(x)` will find the mean of that
data. It outputs the run times of each, and then the results from the
five trials, where five is the default setting for `times`. The run
times aren’t useful because they are all fast. For more precise timing
(\<0.01 seconds), you should use `microbenchmark`. The trials all have
the same output since there is no randomness, the same data is used for
each trial. The “Output summary” shows that the mean is near 1, while
the median is near 0.6.

``` r
## basic example code
library(comparer)
mbc(mean(x), median(x), input=rexp(100))
#> Run times (sec)
#>    Function        Sort1        Sort2        Sort3        Sort4        Sort5
#> 1   mean(x) 1.072884e-05 1.096725e-05 1.120567e-05 1.454353e-05 0.0001285076
#> 2 median(x) 4.792213e-05 4.911423e-05 5.006790e-05 6.866455e-05 0.0002224445
#>           mean           sd neval
#> 1 3.519058e-05 5.218906e-05     5
#> 2 8.764267e-05 7.583821e-05     5
#> 
#> Output summary
#>        Func Stat     Sort1     Sort2     Sort3     Sort4     Sort5      mean sd
#> 1   mean(x)    1 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470 1.0321470  0
#> 2 median(x)    1 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696 0.8087696  0
```

To get the data to be generated for each trial, use the `inputi`
argument to set a variable that the functions call. The arguments
`mean(x)` and `median(x)` are captured as expressions. `rexp(100)` will
be stored as `x` by default. You can see that the values are now
different for each trial.

``` r
## Regenerate the data each time
mbc(mean(x), median(x), inputi=rexp(100))
#> Run times (sec)
#>    Function        Sort1        Sort2        Sort3        Sort4        Sort5
#> 1   mean(x) 9.775162e-06 1.072884e-05 1.168251e-05 2.121925e-05 3.576279e-05
#> 2 median(x) 6.175041e-05 6.675720e-05 6.937981e-05 8.654594e-05 1.425743e-04
#>           mean           sd neval
#> 1 1.783371e-05 1.102464e-05     5
#> 2 8.540154e-05 3.329309e-05     5
#> 
#> Output summary
#>        Func Stat        V1        V2        V3        V4        V5     mean
#> 1   mean(x)    1 0.9890381 0.9069863 0.8813966 1.2063718 1.0568761 1.008134
#> 2 median(x)    1 0.6836623 0.6488801 0.6404516 0.7901115 0.7825493 0.709131
#>          sd
#> 1 0.1307018
#> 2 0.0723598
```

The variable name, or multiple variables, can be set in `inputi` by
using braces `{}` In the example below, values are set for `a` and `b`,
which can then be called by the expressions to be compared.

``` r
mbc(mean(a+b), mean(a-b), inputi={a=rexp(100);b=runif(100)})
#> Run times (sec)
#>      Function        Sort1        Sort2        Sort3        Sort4        Sort5
#> 1 mean(a + b) 1.096725e-05 1.120567e-05 1.478195e-05 1.573563e-05 1.788139e-05
#> 2 mean(a - b) 9.775162e-06 9.775162e-06 1.335144e-05 1.549721e-05 1.621246e-05
#>           mean           sd neval
#> 1 1.411438e-05 2.984521e-06     5
#> 2 1.292229e-05 3.059757e-06     5
#> 
#> Output summary
#>          Func Stat        V1        V2        V3       V4        V5      mean
#> 1 mean(a + b)    1 1.4851116 1.5601898 1.3481168 1.600197 1.4810187 1.4949268
#> 2 mean(a - b)    1 0.5518472 0.5843345 0.4168324 0.628536 0.4586843 0.5280469
#>           sd
#> 1 0.09641584
#> 2 0.08805201
```

## `ffexp`

The other main function of the package is `ffexp`, an abbreviation for
full-factorial experiment. It will run a function using all possible
combinations of input parameters given. It is useful for running
experiments that take a long time to complete.

The first arguments given to `ffexp$new` should give the possible values
for each input parameter. In the example below, `a` can be 1, 2, or 3,
and `b` can “a”, “b”, or “c”. Then `eval_func` should be given that can
operate on these parameters. For example, using `eval_func = paste` will
paste together the value of `a` with the value of `b`.

``` r
f1 <- ffexp$new(
  a=1:3,
  b=c("a","b","c"),
  eval_func=paste
)
```

After creating the `ffexp` object, we can call `f1$run_all` to run
`eval_func` on every combination of `a` and `b`.

``` r
f1$run_all()
#> Running 1, completed 0/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 1
#> 
#> $b
#> [1] "a"
#> 
#> Running 2, completed 1/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 2
#> 
#> $b
#> [1] "a"
#> 
#> Running 3, completed 2/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 3
#> 
#> $b
#> [1] "a"
#> 
#> Running 4, completed 3/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 1
#> 
#> $b
#> [1] "b"
#> 
#> Running 5, completed 4/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 2
#> 
#> $b
#> [1] "b"
#> 
#> Running 6, completed 5/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 3
#> 
#> $b
#> [1] "b"
#> 
#> Running 7, completed 6/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 1
#> 
#> $b
#> [1] "c"
#> 
#> Running 8, completed 7/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 2
#> 
#> $b
#> [1] "c"
#> 
#> Running 9, completed 8/9 Fri Mar 18 09:05:50 AM 2022
#> $a
#> [1] 3
#> 
#> $b
#> [1] "c"
```

Now to see the results in a clean format, look at `f1$outcleandf`.

``` r
f1$outcleandf
#>   a b  V1 runtime          start_time            end_time run_number
#> 1 1 a 1 a       0 2022-03-18 09:05:50 2022-03-18 09:05:50          1
#> 2 2 a 2 a       0 2022-03-18 09:05:50 2022-03-18 09:05:50          2
#> 3 3 a 3 a       0 2022-03-18 09:05:50 2022-03-18 09:05:50          3
#> 4 1 b 1 b       0 2022-03-18 09:05:50 2022-03-18 09:05:50          4
#> 5 2 b 2 b       0 2022-03-18 09:05:50 2022-03-18 09:05:50          5
#> 6 3 b 3 b       0 2022-03-18 09:05:50 2022-03-18 09:05:50          6
#> 7 1 c 1 c       0 2022-03-18 09:05:50 2022-03-18 09:05:50          7
#> 8 2 c 2 c       0 2022-03-18 09:05:50 2022-03-18 09:05:50          8
#> 9 3 c 3 c       0 2022-03-18 09:05:50 2022-03-18 09:05:50          9
```
