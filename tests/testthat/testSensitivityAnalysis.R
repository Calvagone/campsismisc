library(testthat)

context("Test the sensitivity analysis feature")

source(paste0("C:/prj/campsismisc/tests/testthat/", "testUtils.R"))

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on AUC", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "effect_of_parameters_on_auc"
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24))
  
  object <- SensitivityAnalysis(model=model, dataset=dataset,
                       output=NcaMetricOutput(campsisnca::Auc(variable="CONC")), replicates=10) %>%
    add(SensitivityAnalysisItem(Factor("DUR", 2))) %>%
    add(SensitivityAnalysisItem(Factor("VC", 2))) %>%
    add(SensitivityAnalysisItem(Factor("VP", 2))) %>%
    add(SensitivityAnalysisItem(Factor("Q", 2))) %>%
    add(SensitivityAnalysisItem(Factor("CL", 2)))
    
  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))
  
  # RxODE
  object <- object %>% prepare()
  forestPlotRegressionTest(object=object, filename=regFilename)
  object %>% getPlot()
})