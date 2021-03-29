I made some minor updates to the existing functions in the package.

## Test environments
* local Ubuntu 20.04.2 LTS, R 4.0.3
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (devel and release)
* R-hub (Windows, Ubuntu Linux, Fedora Linux)

## R CMD check results

On local Ubuntu and Ubuntu on Travis:

0 errors | 0 warnings | 0 note

From R-hub, there are 0 errors or warnings,
only a few notes:

"
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Collin Erickson <collinberickson@gmail.com>'
Possibly mis-spelled words in DESCRIPTION:
  ffexp (13:18)

Uses the superseded package: 'snow'
"

The first isn't an issue.
For 'snow', I tried removing it from Suggests since I never actually
call it (I only use the package 'parallel'), but then it gives an error.
Apparently parallel::makeCluster using type="SOCK" calls snow::makeSOCKcluster,
requiring 'snow' to be in Suggests.
Thus I do not think this is an issue and I am leaving 'snow' in Suggests.

On winbuilder, there is one NOTE:
"
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Collin Erickson <collinberickson@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  ffexp (13:18)

Uses the superseded package: 'snow'
"

This is the same as on R-hub and I don't think it's a problem.

## Reverse dependencies

None.
