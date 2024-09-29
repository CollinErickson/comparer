This packaged was removed from CRAN because of the issue with Suggests.
I have either moved those packages to Depends, put them behind requireNamespace,
or rewrote the code to eliminate the Suggests package.

## Test environments
* local Windows 11, R 4.4.1 (regular and _R_CHECK_DEPENDS_ONLY_)
* Ubuntu 22.04.5 LTS, R 4.4.1 (GitHub Actions)
* win-builder (devel and release)
* R-hub (5 systems, see below)
* mac-builder

## R CMD check results

Windows 11 (9/28/24): 0 errors 0 warnings 0 notes

Windows 11, _R_CHECK_DEPENDS_ONLY_ (9/28/24): 0 errors 0 warnings 0 notes

R-hub linux (R-devel) (9/28/24): OK
R-hub macos (R-devel) (9/28/22): OK (1 slow example)
R-hub windows (R-devel) (9/28/24): OK
R-hub mkl (9/28/24): OK
R-hub ubuntu-release (9/28/24): OK

On win-builder (release, 9/28/24): 1 NOTE for new submission, previous version
was archived, possibly misspelled word that is fine, and a superceded package
(snow, see note I added below).
On win-builder (devel, 9/28/24): 1 NOTE for new submission, previous version
was archived, possibly misspelled word that is fine.

On Ubuntu 22.04.5 LTS, R 4.4.1 (GitHub Actions, 9/28/24): OK

On mac-builder (9/28/24): 1 WARNING, not an issue.
Found the following significant warnings:
  Warning: package ‘GauPro’ was built under R version 4.4.1
  Warning: package ‘mixopt’ was built under R version 4.4.1


Regarding the note for "Uses the superseded package: 'snow'"

I tried removing snow from Suggests since I never actually
call it (I only use the package 'parallel'), but then it gives an error.
Apparently parallel::makeCluster using type="SOCK" calls snow::makeSOCKcluster,
requiring 'snow' to be in Suggests.
Thus I do not think this is an issue and I am leaving 'snow' in Suggests.



## Reverse dependencies

None.
