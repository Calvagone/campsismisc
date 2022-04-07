library(testthat)

context("Test the forest plot feature")

source(paste0("C:/prj/campsismisc/tests/testthat/", "testUtils.R"))

test_that("Forest plot: effect of METAB (0-1) on CL", {
  model <- getModel("metaboliser_effect_on_cl")
  
  object <- ForestPlot(model=model, output="CL", replicates=100) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1)))
  
  expect_equal(object@items %>% getNames(), c("METAB:0", "METAB:1"))
})
