library(testthat)

context("Test the tornado plot feature")

source(paste0("", "testUtils.R"))

test_that("Tornado plot: sensitivity analysis of the 1-cpt-fo model parameters on AUC (rxode2/mrgsolve)", {
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
    sens@dest <- destEngine,
    sens <- progressr::with_progress(sens %>% prepare())
  )
  
  test <- expression(
    oatAnalysisRegressionTest(object=sens, filename=regFilename),
    sens %>% getTornadoPlot()
  )
  campsismiscTest(simulation, test, env=environment())
})
