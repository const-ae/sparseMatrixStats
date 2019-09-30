test_that("quantile_sparse method works", {
  for(i in 1:100){
    N <- rpois(1, lambda=2)
    vec <- rnorm(N)
    nz <- rpois(1, lambda=3)
    compl_vec <- sort(c(vec, rep(0, nz)))
    prob <- runif(1)
    q1 <- quantile(compl_vec, prob)
    q2 <- quantile_sparse(vec, nz, prob)
    testthat::expect_equal(unname(q1), q2)
  }
})
