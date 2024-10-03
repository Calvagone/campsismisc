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
    add(SensitivityAnalysisItem(Change("DUR", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VP", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("Q", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2, log=TRUE)))

  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    oatAnalysisRegressionTest(object=object, filename=regFilename),
    object %>% getForestPlot(relative=FALSE),
    expect_warning(object %>% getTornadoPlot(), regexp="Multiple replicates detected")
  )
  campsismiscTest(simulation, test, env=environment())

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
    add(SensitivityAnalysisItem(Change("DUR", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VP", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("Q", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2, log=TRUE)))

  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    oatAnalysisRegressionTest(object=object, filename=regFilename),
    object %>% getForestPlot(relative=FALSE),
    object %>% getTornadoPlot(relative=TRUE),
    object %>% getTornadoPlot(relative=FALSE)
  )
  campsismiscTest(simulation, test, env=environment())
})

test_that("Sensitivity analysis: effect of DUR, VC, VP, Q, CL on Cmax (rxode2/mrgsolve)", {
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
    add(SensitivityAnalysisItem(Change("DUR", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("VP", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("Q", up=2, down=2, log=TRUE))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2, log=TRUE)))

  expect_equal(object@items %>% getNames(), c("DUR", "VC", "VP", "Q", "CL"))

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    oatAnalysisRegressionTest(object=object, filename=regFilename),
    object %>% getForestPlot(),
    expect_warning(object %>% getTornadoPlot(), regexp="Multiple replicates detected")
  )
  campsismiscTest(simulation, test, env=environment())
})

test_that("Tornado plot: sensitivity analysis of the 1-cpt-fo model parameters on AUC (rxode2/mrgsolve, change applied on THETAS)", {
  model <- model_suite$pk$'1cpt_fo'
  regFilename <- "sensitivity_bio_ka_cl_vc_on_auc"

  arm1 <- Arm(subjects=10, label="Arm 1") %>%
    add(Bolus(time=0, amount=1000, compartment=1, ii=24, addl=0)) %>%
    add(Observations(seq(0,48,by=0.1))) %>%
    setSubjects(1) %>%
    setLabel(as.character(NA))

  outputs <- OATOutputs() %>%
    add(NcaMetricOutput(AUC(variable="CONC", method=1, name="AUC"), filter=~campsisnca::timerange(x=.x, min=0, max=Inf)))

  dataset_sens <- Dataset() %>% add(arm1) %>% add(DatasetConfig(exportTSLD=TRUE, exportTDOS=TRUE))

  sens <- SensitivityAnalysis(model=model, dataset=dataset_sens, outputs=outputs) %>%
    add(LabeledParameters(c(BIO="Bioavailability", KA="Absorption rate", CL="Clearance", VC="Volume of central compartment"))) %>%
    add(SensitivityAnalysisItem(Change("BIO", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("KA", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2)))
  
  simulation <- expression(
    tictoc::tic(),
    sens@dest <- destEngine,
    sens <- progressr::with_progress(sens %>% prepare()),
    tictoc::toc(),  # rxode2: 15.43 sec, mrgsolve: 7.19 sec
    sens
  )

  test <- expression(
    oatAnalysisRegressionTest(object=sens, filename=regFilename),
    sens %>% getTornadoPlot(), # Default plot
    sens %>% getTornadoPlot(limits=c(-15, 100), show_labels=FALSE) # Limits can be set, labels can be hidden, etc.
  )
  campsismiscTest(simulation, test, env=environment())
})

test_that("Tornado plot: sensitivity analysis of the 1-cpt-fo model parameters on AUC(rxode2/mrgsolve, change applied on equations)", {
  model <- model_suite$pk$'1cpt_fo'
  regFilename <- "sensitivity_bio_ka_cl_vc_on_auc"
  
  arm1 <- Arm(subjects=10, label="Arm 1") %>%
    add(Bolus(time=0, amount=1000, compartment=1, ii=24, addl=0)) %>%
    add(Observations(seq(0,48,by=0.1))) %>%
    setSubjects(1) %>%
    setLabel(as.character(NA))
  
  outputs <- OATOutputs() %>%
    add(NcaMetricOutput(AUC(variable="CONC", method=1, name="AUC"), filter=~campsisnca::timerange(x=.x, min=0, max=Inf)))
  
  dataset_sens <- Dataset() %>% add(arm1) %>% add(DatasetConfig(exportTSLD=TRUE, exportTDOS=TRUE))
  
  sens <- SensitivityAnalysis(model=model, dataset=dataset_sens, outputs=outputs) %>%
    add(LabeledParameters(c(BIO="Bioavailability", KA="Absorption rate", CL="Clearance", VC="Volume of central compartment"))) %>%
    add(SensitivityAnalysisItem(Change("BIO", up=2, down=2, where="equation"))) %>%
    add(SensitivityAnalysisItem(Change("KA", up=2, down=2, where="equation"))) %>%
    add(SensitivityAnalysisItem(Change("CL", up=2, down=2, where="equation"))) %>%
    add(SensitivityAnalysisItem(Change("VC", up=2, down=2, where="equation")))
  
  simulation <- expression(
    tictoc::tic(),
    sens@dest <- destEngine,
    sens <- progressr::with_progress(sens %>% prepare()),
    tictoc::toc(), # rxode2: 40.75 sec, mrgsolve: 35.78 sec
    sens
  )
  
  test <- expression(
    oatAnalysisRegressionTest(object=sens, filename=regFilename),
    sens %>% getTornadoPlot(), # Default plot
    sens %>% getTornadoPlot(limits=c(-15, 100), show_labels=FALSE) # Limits can be set, labels can be hidden, etc.
  )
  campsismiscTest(simulation, test, env=environment())
})

