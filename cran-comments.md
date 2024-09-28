This packaged was removed from CRAN because of the issue with Suggests.
I have either moved those packages to Depends, put them behind requireNamespace,
or rewrote the code to eliminate the Suggests package.

## Test environments
* local Windows 11, R 4.4.1 (regular and _R_CHECK_DEPENDS_ONLY_)
* Ubuntu 22.04.1 LTS (GitHub Actions)
* win-builder (devel and release)
* R-hub (Windows, Ubuntu Linux, Fedora Linux)
* mac-builder

## R CMD check results

Windows 11 (9/28/24): 0 errors 0 warnings 0 notes

Windows 11, _R_CHECK_DEPENDS_ONLY_ (9/28/24): 0 errors 0 warnings 0 notes

R-hub Windows Server 2022 (2/18/23): NOTE
R-hub Fedora Linux (2/18/23): 3 NOTEs
R-hub Ubuntu Linux (2/18/23): 2 NOTEs

On win-builder (release, 2/18/23): 2 NOTEs
On win-builder (devel, 2/18/23): NOTE

On Ubuntu (GitHub Actions, 2/18/23) :OK

On mac-builder (9/28/24): 1 WARNING, not an issue.
Found the following significant warnings:
  Warning: package ‘GauPro’ was built under R version 4.4.1
  Warning: package ‘mixopt’ was built under R version 4.4.1

The NOTEs are all fine:

There is a note that the package was archived on CRAN.

There is a note for spelling, but it's fine.

There is a note for "Uses the superseded package: 'snow'"

I tried removing snow from Suggests since I never actually
call it (I only use the package 'parallel'), but then it gives an error.
Apparently parallel::makeCluster using type="SOCK" calls snow::makeSOCKcluster,
requiring 'snow' to be in Suggests.
Thus I do not think this is an issue and I am leaving 'snow' in Suggests.

There is a NOTE for a slow example, but only on some systems.


## Reverse dependencies

None.
