#_______________________________________________________________________________
#----                   sensitivity_analysis_factors class                  ----
#_______________________________________________________________________________

#' Sensitivity analysis factors class.
#' 
#' @export
setClass(
  "sensitivity_analysis_factors",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="sensitivity_analysis_factor")
)

#' Create a list of sensitivity analysis factors
#' 
#' @return a list of sensitivity analysis factors
#' @export
SensitivityAnalysisFactors <- function() {
  return(new("sensitivity_analysis_factors"))
}
