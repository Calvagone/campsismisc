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
    up_down_as_factor="logical",
    where="character"
  ),
  contains="pmx_element",
  validity=function(object) {
    return(c(expectOne(object, "parameter"),
             expectOne(object, "up"),
             expectOne(object, "down"),
             expectOne(object, "up_down_as_factor"),
             expectOne(object, "where"),
             checkWhereArgument(object@where)))
  }
)

#' Create a sensibility analysis change.
#' 
#' @param parameter theta name (e.g. "CL") or parameter/equation name (e.g. "CL") in the model code,
#' see also argument `where`.
#' @param up model parameter will be multiplied by this value if upDownAsFactor is TRUE,
#' otherwise the model parameter will take this value as is
#' @param down model parameter will be divided by this value if upDownAsFactor is TRUE,
#' otherwise the model parameter will take this value as is
#' @param upDownAsFactor if TRUE, up and down are respectively used as multiplier and divider,
#' if FALSE, their value is used as is
#' @param where where to apply the change, either "theta" (the corresponding theta is changed in the model parameters) 
#' or "equation" (the corresponding equation is changed in the model code)
#' @export
Change <- function(parameter, up, down, upDownAsFactor=TRUE, where="theta") {
  return(new("sensitivity_analysis_change", parameter=parameter, up=up, down=down, up_down_as_factor=upDownAsFactor, where=where))
}

checkWhereArgument <- function(x) {
  if (length(x)==1 &&  (x %in% c("theta", "equation"))) {
    return(character(0))
  }
  return("where must be either 'theta' or 'equation'")
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("sensitivity_analysis_change"), definition=function(x) {
  return(x@parameter)
})
