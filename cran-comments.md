This packaged was removed from CRAN on 1/17/23 because of unresolved issues.

## Test environments
* local Windows 11, R 4.2.2
* Ubuntu 22.04.1 LTS (GitHub Actions), R 4.2.2
* win-builder (devel and release)
* R-hub (Windows, Ubuntu Linux, Fedora Linux)
* mac-builder

## R CMD check results

R-hub Windows Server 2022 (2/18/23): NOTE
R-hub Fedora Linux (2/18/23): 3 NOTEs
R-hub Ubuntu Linux (2/18/23): 2 NOTEs

On win-builder (release, 2/18/23): 2 NOTEs
On win-builder (devel, 2/18/23): NOTE

On Ubuntu (GitHub Actions, 2/18/23) :OK

On mac-builder (2/18/23): OK

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
