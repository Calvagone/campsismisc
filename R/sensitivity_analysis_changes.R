#_______________________________________________________________________________
#----                   sensitivity_analysis_changes class                  ----
#_______________________________________________________________________________

#' Sensitivity analysis changes class.
#' 
#' @export
setClass(
  "sensitivity_analysis_changes",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="sensitivity_analysis_change")
)

#' Create a list of sensitivity analysis changes
#' 
#' @return a list of sensitivity analysis changes
#' @export
SensitivityAnalysisChanges <- function() {
  return(new("sensitivity_analysis_changes"))
}
