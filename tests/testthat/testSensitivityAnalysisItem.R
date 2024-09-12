library(testthat)

context("Test all methods from the sensitivity analysis item class")

test_that("Method getLabel is working as expected", {
  labeledParameters <- LabeledParameters(c(DUR="Infusion duration"))
  item <- SensitivityAnalysisItem(Change("DUR", down=3, up=2))
  
  expect_equal(item %>% getLabel(labeled_parameters=labeledParameters), "Infusion duration (÷3, ×2)")
})