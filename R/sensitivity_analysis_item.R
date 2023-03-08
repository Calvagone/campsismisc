#_______________________________________________________________________________
#----                         forest_plot_item class                        ----
#_______________________________________________________________________________

#' Forest plot item: one or more labeled covariates.
#' 
#' @export
setClass(
  "sensitivity_analysis_item",
  representation(
    changes="sensitivity_analysis_changes",
    label="character"
  ),
  contains="pmx_element"
)

#' 
#' Create a sensitivity analysis item.
#' 
#' @param ... all changes
#' @return a sensitivity analysis item
#' @export
SensitivityAnalysisItem <- function(...) {
  args <- list(...)
  areChanges <- args %>% purrr::map_lgl(~is(.x, "sensitivity_analysis_change"))
  assertthat::assert_that(all(areChanges), msg="Sensitivity analysis item can only accept one or multiple changes")
  changes <- SensitivityAnalysisChanges() %>% add(args)
  return(new("sensitivity_analysis_item", changes=changes))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("sensitivity_analysis_item"), definition=function(x) {
  names <- x@changes@list %>% purrr::map_chr(~.x@parameter)
  return(paste0(names, collapse="/"))
})

#_______________________________________________________________________________
#----                              getLabel                                 ----
#_______________________________________________________________________________

#' @rdname getLabel
#' @param labeled_parameters labeled parameters object
setMethod("getLabel", signature=c("sensitivity_analysis_item"), definition=function(object, labeled_parameters) {
  if (!is(labeled_parameters, "labeled_parameters")) {
    stop("Please provide the labeled parameters")
  }
  individualLabels <- object@changes@list %>% purrr::map_chr(.f=function(change) {
    parameterName <- change %>% getName()
    parameterLabel <- labeled_parameters[parameterName]
    if (is.na(parameterLabel)) {
      parameterLabel <- parameterName
    }
    if (change@up_down_as_factor) {
      return(paste0(parameterLabel, " (×", change@up, ",", "÷", change@down, ")"))
    } else {
      return(paste0(parameterLabel, " (", change@up, ",", change@down, ")"))
    }
  })
  
  return(paste0(individualLabels, collapse=" / "))
})

