testthat::context("test--errors--d_from_t_in")

testthat::test_that("sample size has to be indicated in some way", {
  testthat::expect_error(d_from_t_in(t = 2.85, proportion=.5))
})

testthat::test_that("users don't specify both df and n", {
  testthat::expect_error(d_from_t_in(t = 2.85, df=100, n=100, proportion=.5))
})

testthat::test_that("users don't specify impossible df values", {
  testthat::expect_error(d_from_t_in(t = 2.85, df=1, proportion=.5))
})

testthat::test_that("users don't specify impossible proportion/df combinations", {
  testthat::expect_error(d_from_t_in(t = 2.85, df=4, proportion=.2))
})
