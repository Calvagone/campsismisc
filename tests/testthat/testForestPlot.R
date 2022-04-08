library(testthat)

context("Test the forest plot feature")

source(paste0("C:/prj/campsismisc/tests/testthat/", "testUtils.R"))

test_that("Forest plot: effect of METAB (0/1) on CL (RxODE/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  
  object <- ForestPlot(model=model, output=ModelParameterOutput("CL"), replicates=10) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1)))
  
  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1"))
  
  object <- object %>% prepare()
  plot1 <- object %>% getPlot(limits=c(0.5, 1.6))
  plot1
  
  object@dest <- "mrgsolve"
  object <- object %>% prepare()
  plot2 <- object %>% getPlot(limits=c(0.5, 1.6))
  plot2
})

test_that("Forest plot: effect of METAB (0/1) on AUC (RxODE/mrgsolve)", {
  model <- getModel("metaboliser_effect_on_cl")
  
  dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=0:24))
  
  object <- ForestPlot(model=model, dataset=dataset, 
                       output=NcaMetricOutput(campsisnca::Auc(variable="CONC")), replicates=10) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1)))
  
  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1"))
  
  object <- object %>% prepare()
  plot1 <- object %>% getPlot(limits=c(0.5, 1.6))
  plot1
  
  object@dest <- "mrgsolve"
  object <- object %>% prepare()
  plot2 <- object %>% getPlot(limits=c(0.5, 1.6))
  plot2
})
