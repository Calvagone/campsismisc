#_______________________________________________________________________________
#----                   sensitivity_analysis_factor class                   ----
#_______________________________________________________________________________

#' Sensitivity analysis factor.
#' 
#' @export
setClass(
  "sensitivity_analysis_factor",
  representation(
    parameter="character",
    multiplier="numeric"
  ),
  contains="pmx_element",
  validity=function(object) {
    return(c(expectOne(object, "parameter"),
             expectOne(object, "multiplier")))
  }
)

#' Create a sensibility analysis factor.
#' 
#' @param parameter parameter name, like "CL", must be part of the model
#' @param multiplier multiplication factor to be analysed
#' @export
Factor <- function(parameter, multiplier) {
  return(new("sensitivity_analysis_factor", parameter=parameter, multiplier=multiplier))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("sensitivity_analysis_factor"), definition=function(x) {
  return(x@parameter)
})
