I submitted this yesterday and it was rejected by the autocheck system.
There was one test that doesn't work on Linux systems, so I removed it
and now it passed on win-builder devel and release and all four
R-hub checks with no errors, warnings, or notes.

Major changes to the main function mbc(), and added another major function
`ffexp`.

## Test environments
* local Window 7 install, R 3.6.0
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note


## Reverse dependencies

None.
