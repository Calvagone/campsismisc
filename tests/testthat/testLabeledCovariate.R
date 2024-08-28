library(testthat)

context("Test all methods from the labeled covariate class")

test_that("Continuous labeled covariate works as expected", {
  covariate <- LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")
  expect_equal(covariate@label, "Weight")
  expect_equal(covariate@unit, "kg")
  expect_equal(covariate %>% getName(), "WT")
})

test_that("Dicrete labeled covariate works as expected", {
  covariate <- CategoricalLabeledCovariate(name="FOOD", default_value=0, label="Food", categories=c(FASTED=0, FED=1))
  expect_equal(covariate@label, "Food")
  expect_equal(covariate@categories, c(FASTED=0, FED=1))
  expect_equal(covariate %>% getName(), "FOOD")
})
