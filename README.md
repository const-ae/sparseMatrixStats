
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseMatrixStats <a href='https://github.com/const-ae/sparseMatrixStats'><img src='man/figures/logo.png' align="right" height="209" /></a>

<!-- badges: start -->

[![codecov](https://codecov.io/gh/const-ae/sparseMatrixStats/branch/master/graph/badge.svg)](https://codecov.io/gh/const-ae/sparseMatrixStats)

<!-- badges: end -->

The goal of `sparseMatrixStats` is to make the API of
[matrixStats](https://github.com/HenrikBengtsson/matrixStats) available
for sparse matrices.

## Installation

You can install the release version of
*[sparseMatrixStats](https://bioconductor.org/packages/sparseMatrixStats)*
from BioConductor:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("sparseMatrixStats")
```

Alternatively, you can get the development version of the package from
[GitHub](https://github.com/const-ae/sparseMatrixStats) with:

``` r
# install.packages("devtools")
devtools::install_github("const-ae/sparseMatrixStats")
```

If you have trouble with the installation, see the end of the README.

## Example

``` r
library(sparseMatrixStats)
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
#> 
#> Attaching package: 'MatrixGenerics'
#> The following objects are masked from 'package:matrixStats':
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
```

``` r
mat <- matrix(0, nrow=10, ncol=6)
mat[sample(seq_len(60), 4)] <- 1:4
# Convert dense matrix to sparse matrix
sparse_mat <- as(mat, "dgCMatrix")
sparse_mat
#> 10 x 6 sparse Matrix of class "dgCMatrix"
#>                  
#>  [1,] 4 . . . . .
#>  [2,] . . . . . .
#>  [3,] . . . . . .
#>  [4,] 2 . . . . .
#>  [5,] . . . . . .
#>  [6,] . . . . . .
#>  [7,] . . . . . 1
#>  [8,] . . . . . .
#>  [9,] . . . 3 . .
#> [10,] . . . . . .
```

The package provides an interface to quickly do common operations on the
rows or columns. For example calculate the variance:

``` r
apply(mat, 2, var)
#> [1] 1.822222 0.000000 0.000000 0.900000 0.000000 0.100000
matrixStats::colVars(mat)
#> [1] 1.822222 0.000000 0.000000 0.900000 0.000000 0.100000
sparseMatrixStats::colVars(sparse_mat)
#> [1] 1.822222 0.000000 0.000000 0.900000 0.000000 0.100000
```

On this small example data, all methods are basically equally fast, but
if we have a much larger dataset, the optimizations for the sparse data
start to show.

I generate a dataset with 10,000 rows and 50 columns that is 99% empty

``` r
big_mat <- matrix(0, nrow=1e4, ncol=50)
big_mat[sample(seq_len(1e4 * 50), 5000)] <- rnorm(5000)
# Convert dense matrix to sparse matrix
big_sparse_mat <- as(big_mat, "dgCMatrix")
```

I use the `bench` package to benchmark the performance difference:

``` r
bench::mark(
  sparseMatrixStats=sparseMatrixStats::colVars(big_sparse_mat),
  matrixStats=matrixStats::colVars(big_mat),
  apply=apply(big_mat, 2, var)
)
#> # A tibble: 3 x 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 sparseMatrixStats  36.72µs   42.9µs   22895.     2.93KB    13.7 
#> 2 matrixStats         1.43ms   1.55ms     641.    156.8KB     2.04
#> 3 apply               9.02ms  10.64ms      94.8    9.54MB    56.9
```

As you can see `sparseMatrixStats` is ca. 35 times fast than
`matrixStats`, which in turn is 7 times faster than the `apply()`
version.

# API

The package now supports all functions from the `matrixStats` API for
column sparse matrices (`dgCMatrix`). And thanks to the
[`MatrixGenerics`](https://bioconductor.org/packages/MatrixGenerics/) it
can be easily integrated along-side
[`matrixStats`](https://cran.r-project.org/package=matrixStats) and
[`DelayedMatrixStats`](https://bioconductor.org/packages/DelayedMatrixStats/).
Note that the `rowXXX()` functions are called by transposing the input
and calling the corresponding `colXXX()` function. Special optimized
implementations are available for `rowSums2()`, `rowMeans2()`, and
`rowVars()`.

| Method               | matrixStats | sparseMatrixStats | Notes                                                                                    |
| :------------------- | :---------- | :---------------- | :--------------------------------------------------------------------------------------- |
| colAlls()            | ✔           | ✔                 |                                                                                          |
| colAnyMissings()     | ✔           | ❌                 | Not implemented because it is deprecated in favor of `colAnyNAs()`                       |
| colAnyNAs()          | ✔           | ✔                 |                                                                                          |
| colAnys()            | ✔           | ✔                 |                                                                                          |
| colAvgsPerRowSet()   | ✔           | ✔                 |                                                                                          |
| colCollapse()        | ✔           | ✔                 |                                                                                          |
| colCounts()          | ✔           | ✔                 |                                                                                          |
| colCummaxs()         | ✔           | ✔                 |                                                                                          |
| colCummins()         | ✔           | ✔                 |                                                                                          |
| colCumprods()        | ✔           | ✔                 |                                                                                          |
| colCumsums()         | ✔           | ✔                 |                                                                                          |
| colDiffs()           | ✔           | ✔                 |                                                                                          |
| colIQRDiffs()        | ✔           | ✔                 |                                                                                          |
| colIQRs()            | ✔           | ✔                 |                                                                                          |
| colLogSumExps()      | ✔           | ✔                 |                                                                                          |
| colMadDiffs()        | ✔           | ✔                 |                                                                                          |
| colMads()            | ✔           | ✔                 |                                                                                          |
| colMaxs()            | ✔           | ✔                 |                                                                                          |
| colMeans2()          | ✔           | ✔                 |                                                                                          |
| colMedians()         | ✔           | ✔                 |                                                                                          |
| colMins()            | ✔           | ✔                 |                                                                                          |
| colOrderStats()      | ✔           | ✔                 |                                                                                          |
| colProds()           | ✔           | ✔                 |                                                                                          |
| colQuantiles()       | ✔           | ✔                 |                                                                                          |
| colRanges()          | ✔           | ✔                 |                                                                                          |
| colRanks()           | ✔           | ✔                 |                                                                                          |
| colSdDiffs()         | ✔           | ✔                 |                                                                                          |
| colSds()             | ✔           | ✔                 |                                                                                          |
| colsum()             | ✔           | ❌                 | Base R function                                                                          |
| colSums2()           | ✔           | ✔                 |                                                                                          |
| colTabulates()       | ✔           | ✔                 |                                                                                          |
| colVarDiffs()        | ✔           | ✔                 |                                                                                          |
| colVars()            | ✔           | ✔                 |                                                                                          |
| colWeightedMads()    | ✔           | ✔                 | Sparse version behaves slightly differently, because it always uses `interpolate=FALSE`. |
| colWeightedMeans()   | ✔           | ✔                 |                                                                                          |
| colWeightedMedians() | ✔           | ✔                 | Only equivalent if `interpolate=FALSE`                                                   |
| colWeightedSds()     | ✔           | ✔                 |                                                                                          |
| colWeightedVars()    | ✔           | ✔                 |                                                                                          |
| rowAlls()            | ✔           | ✔                 |                                                                                          |
| rowAnyMissings()     | ✔           | ❌                 | Not implemented because it is deprecated in favor of `rowAnyNAs()`                       |
| rowAnyNAs()          | ✔           | ✔                 |                                                                                          |
| rowAnys()            | ✔           | ✔                 |                                                                                          |
| rowAvgsPerColSet()   | ✔           | ✔                 |                                                                                          |
| rowCollapse()        | ✔           | ✔                 |                                                                                          |
| rowCounts()          | ✔           | ✔                 |                                                                                          |
| rowCummaxs()         | ✔           | ✔                 |                                                                                          |
| rowCummins()         | ✔           | ✔                 |                                                                                          |
| rowCumprods()        | ✔           | ✔                 |                                                                                          |
| rowCumsums()         | ✔           | ✔                 |                                                                                          |
| rowDiffs()           | ✔           | ✔                 |                                                                                          |
| rowIQRDiffs()        | ✔           | ✔                 |                                                                                          |
| rowIQRs()            | ✔           | ✔                 |                                                                                          |
| rowLogSumExps()      | ✔           | ✔                 |                                                                                          |
| rowMadDiffs()        | ✔           | ✔                 |                                                                                          |
| rowMads()            | ✔           | ✔                 |                                                                                          |
| rowMaxs()            | ✔           | ✔                 |                                                                                          |
| rowMeans2()          | ✔           | ✔                 |                                                                                          |
| rowMedians()         | ✔           | ✔                 |                                                                                          |
| rowMins()            | ✔           | ✔                 |                                                                                          |
| rowOrderStats()      | ✔           | ✔                 |                                                                                          |
| rowProds()           | ✔           | ✔                 |                                                                                          |
| rowQuantiles()       | ✔           | ✔                 |                                                                                          |
| rowRanges()          | ✔           | ✔                 |                                                                                          |
| rowRanks()           | ✔           | ✔                 |                                                                                          |
| rowSdDiffs()         | ✔           | ✔                 |                                                                                          |
| rowSds()             | ✔           | ✔                 |                                                                                          |
| rowsum()             | ✔           | ❌                 | Base R function                                                                          |
| rowSums2()           | ✔           | ✔                 |                                                                                          |
| rowTabulates()       | ✔           | ✔                 |                                                                                          |
| rowVarDiffs()        | ✔           | ✔                 |                                                                                          |
| rowVars()            | ✔           | ✔                 |                                                                                          |
| rowWeightedMads()    | ✔           | ✔                 | Sparse version behaves slightly differently, because it always uses `interpolate=FALSE`. |
| rowWeightedMeans()   | ✔           | ✔                 |                                                                                          |
| rowWeightedMedians() | ✔           | ✔                 | Only equivalent if `interpolate=FALSE`                                                   |
| rowWeightedSds()     | ✔           | ✔                 |                                                                                          |
| rowWeightedVars()    | ✔           | ✔                 |                                                                                          |

# Installation Problems

`sparseMatrixStats` uses features from C++14 and as the standard is more
than 6 years old, I thought this wouldn’t cause problems. In most
circumstances this is true, but there are reoccuring reports, that the
installation fails for some people and that is of course annoying. The
typical error message is:

    Error: C++14 standard requested but CXX14 is not defined

The main reason that the installation fails is that the compiler is too
old. Sufficient support for C++14 came in

  - `clang` version 3.4
  - `gcc` version 4.9

Accordingly, you must have a compiler available that is at least that
new. If you run on the command line

``` bash
$ gcc --version
```

and it says 4.8, you will have to install a newer compiler. At the end
of the section, I have collected a few tips to install an appropriate
version on different distributions.

If you have recent version of `gcc` (\>=4.9) or `clang` (\>= 3.4)
installed, but you still see the error message

    Error: C++14 standard requested but CXX14 is not defined

the problem is that R doesn’t yet know about it.

The solution is to either create a `~/.R/Makevars` file and define

    CXX14 = g++
    CXX14FLAGS = -g -O2 $(LTO)
    CXX14PICFLAGS = -fpic
    CXX14STD = -std=gnu++11

or simply call

``` r
withr::with_makevars(
 new = c(CXX14 = "g++", CXX14FLAGS = "-g -O2 $(LTO)", 
         CXX14PICFLAGS = "-fpic", CXX14STD = "-std=gnu++14"), 
 code = {
   BiocManager::install("sparseMatrixStats")
 })
```

### Update Compiler

#### CentOS / Scientic Linux / RHEL

One of the main culprits causing trouble is CentOS 7. It is popular in
scientific computing and is still supported until 2024. It does,
however, by default come with a very old version of `gcc` (4.8.5).

To install a more recent compiler, we can use
[devtoolset](https://www.softwarecollections.org/en/scls/rhscl/devtoolset-7/).
First, we enable the Software Collection Tools and then install for
example `gcc` version 7:

``` bash
$ yum install centos-release-scl
$ yum install devtoolset-7-gcc*
```

We can now either activate the new compiler for an R session

``` bash
$ scl enable devtoolset-7 R
```

and then call

``` r
withr::with_makevars(
 new = c(CXX14 = "g++", CXX14FLAGS = "-g -O2 $(LTO)", 
         CXX14PICFLAGS = "-fpic", CXX14STD = "-std=gnu++14"), 
 code = {
   BiocManager::install("sparseMatrixStats")
 })
```

or we refer to the full path of the newly installed g++ from a standard
R session

``` r
withr::with_makevars(
 new = c(CXX14 = "/opt/rh/devtoolset-7/root/usr/bin/g++", 
         CXX14FLAGS = "-g -O2 $(LTO)", CXX14PICFLAGS = "-fpic",
         CXX14STD = "-std=gnu++14"), 
 code = {
   BiocManager::install("sparseMatrixStats")
 })
```

Note, that our shenanigans are only necessary once, when we install
`sparseMatrixStats`. After the successful installation of the package,
we can use R as usual.

#### Debian

All Debian releases later than Jessie (i.e. Stretch, Buster, Bullseye)
are recent enough and should install sparseMatrixStats without problems.

I was able to install `sparseMatrixStats` on Debian Jessie (which comes
with `gcc` version 4.9.2) by providing the necessary Makefile arguments

``` r
withr::with_makevars(
 new = c(CXX14 = "g++", 
         CXX14FLAGS = "-g -O2 $(LTO)", CXX14PICFLAGS = "-fpic",
         CXX14STD = "-std=gnu++14"), 
 code = {
   BiocManager::install("sparseMatrixStats")
 })
```

Debian Wheezy comes with `gcc` 4.7, which does not support C++14. On the
other hand, the last R release that was backported to Wheezy is 3.2.5
(see information on
[CRAN](https://cloud.r-project.org/bin/linux/debian/#debian-wheezy-oldoldoldstable)).
Thus, if you are still on Wheezy, I would encourage you to update your
OS.

#### Ubuntu

Since 16.04, Ubuntu comes with a recent enough compiler.

Ubuntu 14.04 comes with `gcc` 4.8.5, but updating to `gcc-5` is easy:

``` bash
$ sudo add-apt-repository ppa:ubuntu-toolchain-r/test
$ sudo apt-get update
$ sudo apt-get install gcc-5 g++-5
```

After that, you can install `sparseMatrixStats` with a custom Makevars
variables that refer to the new compiler

``` r
withr::with_makevars(
 new = c(CXX14 = "g++-5", 
         CXX14FLAGS = "-g -O2 $(LTO)", CXX14PICFLAGS = "-fpic",
         CXX14STD = "-std=gnu++14"), 
 code = {
   BiocManager::install("sparseMatrixStats")
 })
```

#### MacOS

No trouble reported so far. Just do:

``` r
BiocManager::install("sparseMatrixStats")
```

#### Windows

It is important that you have
[RTools40](https://cran.r-project.org/bin/windows/Rtools/) installed.
After that, you shouldn’t have any troubles installing
`sparseMatrixStats` directly from Bioconductor:

``` r
BiocManager::install("sparseMatrixStats")
```

### But I still have a problems

1.  *Please* make sure to carefully read the full problem section.
2.  Make sure that you are using at least R 4.0.0.
3.  Make sure your compiler is new enough to support C++14 (ie. `gcc`
    \>= 4.9 and `clang` \>= 3.4)

If your problems nonetheless persist, please file an
[issue](https://github.com/const-ae/sparseMatrixStats/issues/) including
the following information:

  - Operating system with exact version (e.g. ‘Linux Ubuntu 18.04’)
  - Compiler and compiler version (e.g. ‘gcc version 7.2.5’)
  - The output of `sessionInfo()`
  - Information if you have a `~/.R/Makevars` file and what it contains
  - The exact call that you use to install `sparseMatrixStats` including
    the full error message
