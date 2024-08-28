library(testthat)

context("Test the forest plot feature")

source(paste0("", "testUtils.R"))

test_that("Forest plot: effect of METAB (0/1) and WT on CL (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "meta_effect_cl_output"

  object <- ForestPlot(model=model, outputs=ModelParameterOutput("CL"), replicates=10) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  # RxODE
  object@dest <- "rxode2"
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot() +
    ggplot2::scale_y_continuous(breaks=c(0.8,1,1.25), limits=c(0.5, 1.5))
  # A tornado plot should work as well but a warning is thrown because it has more than 1 replicate
  expect_warning(object %>% getTornadoPlot(), regexp="Multiple replicates detected")

  # Mrgsolve
  object@dest <- "mrgsolve"
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot()
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC0-24 (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "meta_effect_auc_output"

  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24))

  object <- ForestPlot(model=model, dataset=dataset,
                       outputs=NcaMetricOutput(campsisnca::AUC(variable="CONC")), replicates=10) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  # RxODE
  object@dest <- "rxode2"
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot()

  # Mrgsolve
  object@dest <- "mrgsolve"
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot()
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC144-168 (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "meta_effect_aucd7_output"

  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1, ii=24, addl=6)) %>%
    add(Observations(times=0:168))

  object <- ForestPlot(model=model, dataset=dataset,
                       outputs=NcaMetricOutput(campsisnca::AUC(variable="CONC"),
                                              filter=~campsisnca::timerange(.x, min=144, max=168)),
                       replicates=2) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  # RxODE
  object@dest <- "rxode2"
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot()

  # Mrgsolve
  object@dest <- "mrgsolve"
  object <- object %>% prepare()
  oatAnalysisRegressionTest(object=object, filename=regFilename)
  object %>% getForestPlot()
})

test_that("Forest plots must work when only 1 item is simulated", {
  model <- getModel("metaboliser_effect_on_cl")

  object <- ForestPlot(model=model, outputs=ModelParameterOutput("CL"), replicates=2) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight")) %>% # Units have been removed
    add(ForestPlotItem(Covariate("WT", 60)))
  
  expect_equal(object@items %>% getNames(), c("WT:60"))

  # Requires campsis v1.5.4 since only 1 scenario is used
  # See https://github.com/Calvagone/campsis/issues/152
  object@dest <- "rxode2"
  object <- object %>% prepare() #
  results1 <- object@results@list[[1]]@results
  
  expect_equal(object@results %>% length(), 1)
  expect_equal(results1$replicate, c(1, 2))
  expect_true(is.numeric(results1$VALUE))
  expect_equal(as.character(results1$SCENARIO), rep("Weight: 60", 2))
})
