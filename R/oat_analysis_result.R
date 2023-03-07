#_______________________________________________________________________________
#----                       oat_analysis_result class                       ----
#_______________________________________________________________________________

#' OAT result structure.
#' 
#' @export
setClass(
  "oat_analysis_result",
  representation(
    output="oat_analysis_output",
    baseline="numeric",
    results="data.frame"
  ),
  contains="pmx_element"
)

#' Create OAT result structure.
#' 
#' @param output corresponding output
#' @param baseline baseline value
#' @param results post-processed results
#' @return OAT result structure
OATResult <- function(output, baseline, results) {
  return(new("oat_analysis_result", output=output, baseline=baseline, results=results))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("oat_analysis_result"), definition=function(x) {
  return(x@output %>% getName())
})
