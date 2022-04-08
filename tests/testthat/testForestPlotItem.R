library(testthat)

context("Test all methods from the forest plot class")

test_that("Method getLabel is working as expected with continuous labeled covariates", {
  labeledCovariates <- LabeledCovariates() %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg"))
  item <- ForestPlotItem(Covariate("WT", 60))
  
  assertthat::are_equal(item %>% getLabel(labeled_covariates=labeledCovariates), "Weight: 60 kg")
})

test_that("Method getLabel is working as expected with categorical labeled covariates", {
  labeledCovariates <- LabeledCovariates() %>%
    add(CategoricalLabeledCovariate(name="FOOD", default_value=0, label="Food", categories=c(Fasted=0, Fed=1)))
  item <- ForestPlotItem(Covariate("FOOD", 0))
  
  assertthat::are_equal(item %>% getLabel(labeled_covariates=labeledCovariates), "Food: Fasted")
})
