
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseMatrixStats

<!-- badges: start -->

[![codecov](https://codecov.io/gh/const-ae/sparseMatrixStats/branch/master/graph/badge.svg)](https://codecov.io/gh/const-ae/sparseMatrixStats)

<!-- badges: end -->

The goal of `sparseMatrixStats` is to make the API of the
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

## Example

``` r
library(sparseMatrixStats)
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
  sparseMatrixStats=sparseMatrixStats::colMedians(big_sparse_mat),
  matrixStats=matrixStats::colMedians(big_mat),
  apply=apply(big_mat, 2, median)
)
#> # A tibble: 3 x 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 sparseMatrixStats  30.14µs  36.34µs   26377.     7.76KB    23.8 
#> 2 matrixStats         2.21ms   2.35ms     422.   162.31KB     2.04
#> 3 apply              17.77ms  18.47ms      53.3   17.23MB   122.
```

As you can see `sparseMatrixStats` is ca. 60 times fast than
`matrixStats`, which in turn is 7 times faster than the `apply()`
version.

# API

The package now supports all relevant functions from the `matrixStats`
API. And thanks to the
[`MatrixGenerics`](https://bioconductor.org/packages/MatrixGenerics/) it
can be easily integrated along-side
[`matrixStats`](https://cran.r-project.org/package=matrixStats) and
[`DelayedMatrixStats`](https://bioconductor.org/packages/DelayedMatrixStats/).

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
