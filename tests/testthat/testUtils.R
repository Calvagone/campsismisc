
# setwd("C:/prj/campsismisc/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismisc/tests/")
# testFolder <- "C:/prj/campsismisc/tests/testthat/"

testFolder <- "C:/prj/campsismisc/tests/testthat/"
overwriteNonRegressionFiles <- FALSE

getModel <- function(name) {
  return(read.campsis(paste0(testFolder, "campsis_models", "/", name)))
}

#' Test there is no regression on the results of the OAT analysis.
#' 
#' @param object OAT analysis object
#' @param filename filename of expected results
#'
oatAnalysisRegressionTest <- function(object, filename) {
  results <- object@results %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.numeric, round, digits=2)
  
  file <- paste0(testFolder, "non_regression/", "oat_analysis/", paste0(filename, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(results, file=file, sep=",", row.names=FALSE)
  }
  expected <- read.csv(file=file) %>% tibble::as_tibble()
  expect_equal(results, expected)
}
