library(testthat)
context("test for auxiliary function")

test_that("check_mean", {
  expect_equal(aux_mean(n=4, p=0.3), 1.2)
  expect_equal(aux_mean(n=5, p=0.8), 4)
  expect_error(expect_equal(aux_mean(n=4, p=0.3), 1.5))
  expect_error(aux_mean(n=4, p=5))
})




test_that("check_variance", {
  expect_equal(aux_variance(n=4, p=0.3), 0.84)
  expect_equal(aux_variance(n=5, p=0.8), 0.8)
  expect_error(aux_variance(n=4, p=5))
})


test_that("check_mode", {
  expect_equal(aux_mode(n=4, p=0.3), 1)
  expect_equal(aux_mode(n=5, p=0.8), 4)
  expect_error(aux_mode(n=4, p=5))
})



test_that("check_skewness", {
  expect_equal(aux_skewness(n=5, p=0.8), -0.6708204)
  expect_error(aux_skewness(n=4, p=5))
  expect_error(aux_skewness(n=4, p=7))
})


test_that("check_kurtosis", {
  expect_equal(aux_kurtosis(n=4, p=0.3),-0.3095238)
  expect_equal(aux_kurtosis(n=5, p=0.8), 0.05)
  expect_error(aux_kurtosis(n=4, p=5))
})
