
# setwd("C:/prj/campsismisc/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismisc/tests/")
# testFolder <- "C:/prj/campsismisc/tests/testthat/"

testFolder <- ""
overwriteNonRegressionFiles <- FALSE

getModel <- function(name) {
  model <- read.campsis(paste0(testFolder, "campsis_models", "/", name))
  
  # See strange regression in R base: #21
  model@parameters@varcov <- signif(model@parameters@varcov, 5)
  
  return(model)
}

#' Test there is no regression on the results of the OAT analysis.
#' 
#' @param object OAT analysis object
#' @param filename filename of expected results
#' @param index output index to be checked
#'
oatAnalysisRegressionTest <- function(object, filename, index=1) {
  iResults <- object@results@list[[index]]
  results <- iResults@results %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.numeric, round, digits=2)
  
  file <- paste0(testFolder, "non_regression/", "oat_analysis/", paste0(filename, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(results, file=file, sep=",", row.names=FALSE)
  }
  expected <- read.csv(file=file) %>% tibble::as_tibble()
  expect_equal(results, expected)
}
