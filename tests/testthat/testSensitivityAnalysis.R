library(testthat)

context("Test the sensitivity analysis feature")

source(paste0("C:/prj/campsismisc/tests/testthat/", "testUtils.R"))

test_that("Sensitivity analysis: effect of KA, CL, V on AUC", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "effect_of_parameters_on_auc"
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24))
  
  object <- SensitivityAnalysis(model=model, dataset=dataset,
                       output=NcaMetricOutput(campsisnca::Auc(variable="CONC")), replicates=10) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))
  
  # RxODE
  object <- object %>% prepare()
  forestPlotRegressionTest(object=object, filename=regFilename)
  object %>% getPlot()
})