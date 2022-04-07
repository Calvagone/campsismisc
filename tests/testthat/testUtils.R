
# setwd("C:/prj/campsismisc/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismisc/tests/")
# testFolder <- "C:/prj/campsismisc/tests/testthat/"

testFolder <- "C:/prj/campsismisc/tests/testthat/"

getModel <- function(name) {
  return(read.campsis(paste0(testFolder, "campsis_models", "/", name)))
}
