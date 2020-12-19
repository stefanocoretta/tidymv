## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- Travis CI (r-release)
- local macOS (r-release)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Stefano Coretta <stefano.coretta@gmail.com>'
  
    GPL (>= 3) + file LICENSE
  File 'LICENSE':
    YEAR: 2020
  License components with restrictions and base license permitting such:
    COPYRIGHT HOLDER: Stefano Coretta, Jacolien van Rij, Martijn Wieling

0 errors ✓ | 0 warnings ✓ | 1 note x

The package contains both code licensed under the GPL and under the MIT license, so that the GPL needs to be the primary license.

## Downstream dependencies

There are currently no downstream dependencies for this package.
