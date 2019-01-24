context("test-d_from_t_in_errors")

test_that("sample size has to be indicated in some way", {
  expect_error(d_from_t_in(t = 2.85))
})

test_that("users don't specify both df and n", {
  expect_error(d_from_t_in(t = 2.85, df=100, n=100))
})

