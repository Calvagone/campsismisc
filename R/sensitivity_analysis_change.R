#_______________________________________________________________________________
#----                   sensitivity_analysis_change class                   ----
#_______________________________________________________________________________

#' Sensitivity analysis change.
#' 
#' @export
setClass(
  "sensitivity_analysis_change",
  representation(
    parameter="character",
    up="numeric",
    down="numeric",
    up_down_as_factor="logical"
  ),
  contains="pmx_element",
  validity=function(object) {
    return(c(expectOne(object, "parameter"),
             expectOne(object, "up"),
             expectOne(object, "down"),
             expectOne(object, "up_down_as_factor")))
  }
)

#' Create a sensibility analysis change.
#' 
#' @param parameter parameter name, like "CL", must be part of the model
#' @param up model parameter will be multiplied by this value if upDownAsFactor is TRUE,
#' otherwise the model parameter will take this value as is
#' @param down model parameter will be divided by this value if upDownAsFactor is TRUE,
#' otherwise the model parameter will take this value as is
#' @param upDownAsFactor if TRUE, up and down are respectively used as multiplier and divider,
#' if FALSE, their value is used as is
#' @export
Change <- function(parameter, up, down, upDownAsFactor=TRUE) {
  return(new("sensitivity_analysis_change", parameter=parameter, up=up, down=down, up_down_as_factor=upDownAsFactor))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("sensitivity_analysis_change"), definition=function(x) {
  return(x@parameter)
})
