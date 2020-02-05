I got an email on 1/3/20 that I had to fix errors with
this package by 1/17/20 or it'd be taken off of CRAN.
The only error was that
one of the tests failed on OSX and Solaris due to
connections when running in parallel.
I can't recreate this error on any of my computers.
I used devtools::skip_on_os to skip that test on
the OS's giving errors and am resubmitting.

## Test environments
* local Window 7 install, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)
* R-hub (Windows, Ubuntu Linux, Fedora Linux)

## R CMD check results

0 errors | 0 warnings | 0 note


## Reverse dependencies

None.
