#_______________________________________________________________________________
#----                        labeled_covariates class                         ----
#_______________________________________________________________________________

#' Labeled covariates class.
#' 
#' @export
setClass(
  "labeled_covariates",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="labeled_covariate")
)

#' Create a list of labeled covariates.
#' 
#' @return a list of labeled covariates
#' @export
LabeledCovariates <- function() {
  return(new("labeled_covariates"))
}
