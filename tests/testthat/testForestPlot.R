library(testthat)

context("Test the forest plot feature")

source(paste0("C:/prj/campsismisc/tests/testthat/", "testUtils.R"))

test_that("Forest plot: effect of METAB (0-1) on CL", {
  model <- getModel("metaboliser_effect_on_cl")
  
  object <- ForestPlot(model=model, output="CL") %>%
    add(ForestPlotItem(LabeledCovariate(name="METAB", value=0, label="Metaboliser", value_label="slow"))) %>%
    add(ForestPlotItem(LabeledCovariate(name="METAB", value=1, label="Metaboliser", value_label="fast")))
  
  expect_equal(fp@items %>% getNames(), c("METAB:0", "METAB:1"))
})
