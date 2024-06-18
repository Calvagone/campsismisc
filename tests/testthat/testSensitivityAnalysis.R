library(testthat)

context("Test the sensitivity analysis feature")

source(paste0("", "testUtils.R"))

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on AUC (several replicates)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "effect_of_parameters_on_auc"
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24)) %>%
    add(Covariate("METAB", 0)) %>%
    add(Covariate("WT", 70))
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset)
  spaghettiPlot(results, "CONC")
  
  object <- SensitivityAnalysis(model=model, dataset=dataset,
                       outputs=NcaMetricOutput(campsisnca::AUC(variable="CONC")), replicates=10) %>%
    add(SensitivityAnalysisItem(Change("DUR", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VP", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("Q", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2)))
    
  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))
  
  # Mrgsolve only
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot(relative=FALSE)
  expect_warning(object %>% getTornadoPlot(), regexp="Multiple replicates detected") 
})

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on AUC (single replicate)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "effect_of_parameters_on_auc_1rep"
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24)) %>%
    add(Covariate("METAB", 0)) %>%
    add(Covariate("WT", 70))
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset)
  spaghettiPlot(results, "CONC")
  
  object <- SensitivityAnalysis(model=model, dataset=dataset,
                                outputs=NcaMetricOutput(campsisnca::AUC(variable="CONC"))) %>%
    add(LabeledParameters(c(DUR="Duration", VC="Central volume", VP="Peripheral volume", Q="Inter-compartmental clearance", CL="Clearance"))) %>%
    add(SensitivityAnalysisItem(Change("DUR", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VP", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("Q", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2)))
  
  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))
  
  # Mrgsolve only
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot(relative=FALSE)
  object %>% getTornadoPlot(relative=TRUE)
  object %>% getTornadoPlot(relative=FALSE)
})

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on Cmax", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "effect_of_parameters_on_cmax"
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24)) %>%
    add(Covariate("METAB", 0)) %>%
    add(Covariate("WT", 70))
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset)
  spaghettiPlot(results, "CONC")
  
  object <- SensitivityAnalysis(model=model, dataset=dataset,
                                outputs=NcaMetricOutput(campsisnca::Cmax(variable="CONC")), replicates=10) %>%
    add(LabeledParameters(c(DUR="Duration", VC="Central volume", VP="Peripheral volume", Q="Inter-compartmental clearance", CL="Clearance"))) %>%
    add(SensitivityAnalysisItem(Change("DUR", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VP", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("Q", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2)))
  
  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))
  
  # Mrgsolve only
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot()
  expect_warning(object %>% getTornadoPlot(), regexp="Multiple replicates detected") 
})