I got an email on 1/3/20 that I had to fix errors with
this package by 1/17/20 or it'd be taken off of CRAN.
The only error was that
one of the tests failed on OSX and Solaris due to
connections when running in parallel.
After it was taken off of CRAN, I tried to resubmit
after just skipping the test on non-Windows OS,
but it wasn't approved since I didn't reply to an email
in a timely manner.

I debugged the package on Linux and found the error
(I was using the wrong kind of slashes in a file path).
I fixed it and have tested on my personal Linux computer,
my personal Windows computer,
Travis, win-builder, and R-hub.
I apologize for the delay in fixing this.

I made a couple of other minor changes, including fixing
an error with lists, adding documentation, and
adding tests.

## Test environments
* local Window 7 install, R 3.6.2
* local Ubuntu 18.04, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)
* R-hub (Windows, Ubuntu Linux, Fedora Linux)

## R CMD check results

0 errors | 0 warnings | 0 note


## Reverse dependencies

None.
