
# setwd("C:/prj/campsismisc/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismisc/tests/")
# testFolder <- "C:/prj/campsismisc/tests/testthat/"

testFolder <- ""
overwriteNonRegressionFiles <- FALSE
testEngines <- c("rxode2", "mrgsolve")

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

noEngineInstalled <- function() {
  cond1 <- engineInstalled("RxODE")
  cond2 <- engineInstalled("rxode2")
  cond3 <- engineInstalled("mrgsolve")
  return(!(cond1 || cond2 || cond3))
}

engineInstalled <- function(name) {
  return(find.package(name, quiet=TRUE) %>% length() > 0)
}

campsismiscTest <- function(simulation, test, env) {
  # Iteration over all test engines to be tested
  for (testEngine in testEngines) {
    env$destEngine <-  testEngine
    # Check if package exists (as test engines are suggested packages)
    # This is needed for CRAN when package is tested with `_R_CHECK_DEPENDS_ONLY_`=TRUE
    if (engineInstalled(testEngine)) {
      env$results <- eval(simulation, envir=env)
      eval(test, envir=env)
    }
  }
}
