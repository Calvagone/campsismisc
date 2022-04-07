library(testthat)

context("Test all methods from the labeled covariate class")

test_that("Numeric labeled covariate", {
  covariate <- LabeledCovariate(name="WT", value=65, label="Weight", unit="kg")
  expect_equal(covariate@label, "Weight")
  expect_equal(covariate@unit, "kg")
})

test_that("Dicrete labeled covariate", {
  covariate <- LabeledCovariate(name="FOOD", value=1, label="Food", value_label="Fasted")
  expect_equal(covariate@label, "Food")
  expect_equal(covariate@value_label, "Fasted")
})
