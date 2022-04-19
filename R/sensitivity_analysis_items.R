#_______________________________________________________________________________
#----                     sensitivity_analysis_items class                  ----
#_______________________________________________________________________________

#' Sensitivity analysis items class.
#' 
#' @export
setClass(
  "sensitivity_analysis_items",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="sensitivity_analysis_item")
)

#' Create a list of sensitivity analysis items
#' 
#' @return a list of sensitivity analysis items
#' @export
SensitivityAnalysisItems <- function() {
  return(new("sensitivity_analysis_items"))
}
