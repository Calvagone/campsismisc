#_______________________________________________________________________________
#----                       oat_analysis_results class                      ----
#_______________________________________________________________________________

#' Forest plot items class.
#' 
#' @export
setClass(
  "oat_analysis_results",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="oat_analysis_result")
)

#' Create a list of OAT results.
#' 
#' @return a list of OAT results
OATResults <- function() {
  return(new("oat_analysis_results"))
}
