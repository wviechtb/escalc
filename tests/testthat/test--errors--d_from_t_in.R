context("test--errors--d_from_t_in")

test_that("sample size has to be indicated in some way", {
  expect_error(d_from_t_in(t = 2.85, proportion=.5))
})

test_that("users don't specify both df and n", {
  expect_error(d_from_t_in(t = 2.85, df=100, n=100, proportion=.5))
})

test_that("users don't specify impossible df values", {
  expect_error(d_from_t_in(t = 2.85, df=1, proportion=.5))
})

test_that("users don't specify impossible proportion/df combinations", {
  expect_error(d_from_t_in(t = 2.85, df=4, proportion=.2))
})
