library(testthat)
context("test for check function")

test_that("check the number of ways we can choose success out of total trials", {
  expect_type(bin_choose(n = 5, k = 4 ), "double")
  expect_length(bin_choose(n = 5 , k = 4 ), 1)
  expect_equal(bin_choose(n= 5, k=4), 5)
})


test_that("check the probability of success out of the total number of trial", {

  expect_equal(bin_probability(success=2, trials = 5, prob=0.5), 0.3125)
  expect_equal(bin_probability(success=2, trials = 6, prob=0.5), 0.234375)
  expect_equal(bin_probability(success=2, trials = 10, prob=0.5), 0.04394531)
})


bin_distribution = function(trials, prob){
  result2 =  data.frame(success = c(0:trials), probability = bin_probability(c(0:trials), trials, prob))
  class(result2) = c("bindis","data.frame")
  return(result2)
}

test_that("check probability in binomial distribution", {
  expect_length(bin_distribution(trials = 3, prob = 0.7)$success,4)
  expect_length(bin_distribution(trials = 5, prob = 0.7)$success,6)
  expect_length(bin_distribution(trials = 7, prob = 0.7)$success,8)
})


test_that("check cumulative probability", {
  expect_length(bin_cumulative(trials = 3, prob = 0.7)$success,4)
  expect_length(bin_cumulative(trials = 5, prob = 0.7)$success,6)
  expect_length(bin_cumulative(trials = 7, prob = 0.7)$success,8)
})
