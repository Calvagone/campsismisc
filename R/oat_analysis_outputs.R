#_______________________________________________________________________________
#----                       oat_analysis_outputs class                      ----
#_______________________________________________________________________________

#' Forest plot items class.
#' 
#' @export
setClass(
  "oat_analysis_outputs",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="oat_analysis_output")
)

#' Create a list of forest plot items
#' 
#' @return a list of forest plot items
#' @export
OATOutputs <- function() {
  return(new("oat_analysis_outputs"))
}

# #_______________________________________________________________________________
# #----                                add                                    ----
# #_______________________________________________________________________________
# 
# setMethod("add", signature=c("oat_analysis_outputs", "oat_analysis_output"), definition=function(object, x) {
#   print("yep")
#   return(object)
# })


