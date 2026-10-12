library(testthat)
library(campsis)
library(campsisnca)

context("Test the forest plot feature")

source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Forest plot: effect of METAB (0/1) and WT on CL (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "meta_effect_cl_output"

  object <- ForestPlot(model = model, outputs = "CL", replicates = 10) %>%
    add(CategoricalLabeledCovariate(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    )) %>%
    add(LabeledCovariate(name = "WT", default_value = 70, label = "Weight", unit = "kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% get_names(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    oatAnalysisRegressionTest(object = object, filename = regFilename),
    object %>% get_forest_plot(), # Default forest plot
    object %>% get_forest_plot(limits = c(0.7, 1.5)) + ggplot2::scale_y_continuous(breaks = c(0.8, 1, 1.25)),
    suppressWarnings(object %>% getForestPlot(limits = c(0.7, 1.5))) + ggplot2::scale_y_continuous(breaks = c(0.8, 1, 1.25)) # Deprecated method
    # class(object) <- "sensitivity_analysis",
    # # A tornado plot should work as well but a warning is thrown because it has more than 1 replicate
    # expect_warning(object %>% get_tornado_plot(), regexp="Multiple replicates detected")
  )
  campsismiscTest(simulation, test, env = environment())
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC0-24 (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "meta_effect_auc_output"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = 0:24))

  object <- ForestPlot(
    model = model,
    dataset = dataset,
    outputs = AUC(variable = "CONC"),
    replicates = 10
  ) %>%
    add(CategoricalLabeledCovariate(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    )) %>%
    add(LabeledCovariate(name = "WT", default_value = 70, label = "Weight", unit = "kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% get_names(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    oatAnalysisRegressionTest(object = object, filename = regFilename),
    object %>% get_forest_plot() # Default forest plot
  )
  campsismiscTest(simulation, test, env = environment())
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC144-168 (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  regFilename <- "meta_effect_aucd7_output"

  dataset <- Dataset(1) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1, ii = 24, addl = 6)) %>%
    add(Observations(times = 0:168))

  object <- ForestPlot(
    model = model,
    dataset = dataset,
    outputs = AUC(variable = "CONC", window = TimeWindow(144, 168)),
    replicates = 2
  ) %>%
    add(CategoricalLabeledCovariate(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    )) %>%
    add(LabeledCovariate(name = "WT", default_value = 70, label = "Weight", unit = "kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))

  expect_equal(object@items %>% get_names(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    oatAnalysisRegressionTest(object = object, filename = regFilename),
    object %>% get_forest_plot() # Default forest plot
  )
  campsismiscTest(simulation, test, env = environment())
})

test_that("Forest plots must work when only 1 item is simulated (rxode2/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")

  object <- ForestPlot(model = model, outputs = "CL", replicates = 2) %>%
    add(CategoricalLabeledCovariate(
      name = "METAB",
      default_value = 0,
      label = "Metaboliser",
      categories = c(Slow = 0, Fast = 1)
    )) %>%
    add(LabeledCovariate(name = "WT", default_value = 70, label = "Weight")) %>% # Units have been removed
    add(ForestPlotItem(Covariate("WT", 60)))

  expect_equal(object@items %>% get_names(), c("WT:60"))

  object@dest <- "mrgsolve"
  object <- progressr::with_progress(object %>% prepare())

  simulation <- expression(
    object@dest <- destEngine,
    object <- progressr::with_progress(object %>% prepare())
  )

  test <- expression(
    results1 <- object@results@list[[1]]@results,
    expect_equal(object@results %>% length(), 1),
    expect_equal(results1$replicate, c(1, 2)),
    expect_true(is.numeric(results1$value)),
    expect_equal(as.character(results1$SCENARIO), rep("Weight: 60", 2))
  )
  campsismiscTest(simulation, test, env = environment())
})
