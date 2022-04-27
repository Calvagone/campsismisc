library(testthat)

context("Test all methods from the sensitivity analysis item class")

test_that("Method getLabel is working as expected", {
  labeledParameters <- LabeledParameters(c(DUR="Infusion duration"))
  item <- SensitivityAnalysisItem(Change("DUR", up=2, down=3))
  
  assertthat::are_equal(item %>% getLabel(labeled_parameters=labeledParameters), "Infusion duration (×2,÷3)")
})