library(testthat)

context("Test the forest plot feature")

source(paste0("C:/prj/campsismisc/tests/testthat/", "testUtils.R"))

test_that("Forest plot: effect of METAB (0/1) and WT on CL (RxODE/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  
  object <- ForestPlot(model=model, output=ModelParameterOutput("CL"), replicates=20) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))
  
  object <- object %>% prepare()
  plot1 <- object %>% getPlot()
  plot1
  
  # object@dest <- "mrgsolve"
  # object <- object %>% prepare()
  # plot2 <- object %>% getPlot()
  # plot2
})

test_that("Forest plot: effect of METAB (0/1) and WT on AUC (RxODE/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24))
  
  object <- ForestPlot(model=model, dataset=dataset, 
                       output=NcaMetricOutput(campsisnca::Auc(variable="CONC")), replicates=20) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1", "WT:60", "WT:80"))
  
  object <- object %>% prepare()
  plot1 <- object %>% getPlot()
  plot1
  
  # object@dest <- "mrgsolve"
  # object <- object %>% prepare()
  # plot2 <- object %>% getPlot()
  # plot2
})
