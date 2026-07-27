overwriteNonRegressionFiles <- FALSE
testEngines <- c("rxode2", "mrgsolve")

getModel <- function(name) {
  model <- read.campsis(file.path(getwd(), test_path(), "campsis_models", "/", name))
  
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
  results <- iResults@results  %>%
    stripMetadata() %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.numeric, round, digits=2)
  
  file <- file.path(getwd(), test_path(), "non_regression", "oat_analysis", paste0(filename, ".csv"))
  
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

convertCampsisTest <- function(env = parent.frame(), debug_engine = "mrgsolve") {
  if (!exists("simulation", envir = env) || !exists("test", envir = env)) {
    stop("Could not find 'simulation' or 'test' expressions in the provided environment.")
  }
  
  # Get the expressions from the specified environment
  sim_expr <- get("simulation", envir = env)
  test_expr <- get("test", envir = env)
  
  # Modify the simulation call
  sim_call <- sim_expr[[1]]
  sim_call$dest <- debug_engine
  sim_text <- paste0("results <- ", deparse1(sim_call))
  
  # Extract all lines from the test expression
  test_text_lines <- vapply(as.list(test_expr), deparse1, character(1))
  
  # Combine and return
  final_script <- c(sim_text, test_text_lines)
  cat(paste(final_script, collapse = "\n"))
}

stripMetadata <- function(x) {
  # Revert class back to standard tibble components
  class(x) <- c("tbl_df", "tbl", "data.frame")
  
  # Delete the attribute
  attr(x, "metadata") <- NULL
  
  return(x)
}

