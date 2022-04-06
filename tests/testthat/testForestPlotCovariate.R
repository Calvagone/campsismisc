library(testthat)

context("Test all methods from the forest plot covariate class")

test_that("Numeric forest plot covariate", {
  covariate <- ForestPlotCovariate(name="WT", value=65, label="Weight", unit="kg")
  expect_equal(covariate@label, "Weight")
  expect_equal(covariate@unit, "kg")
})

test_that("Dicrete forest plot covariate", {
  covariate <- ForestPlotCovariate(name="FOOD", value=1, label="Food", value_label="Fasted")
  expect_equal(covariate@label, "Food")
  expect_equal(covariate@value_label, "Fasted")
})
