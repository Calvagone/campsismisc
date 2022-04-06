#_______________________________________________________________________________
#----                     forest_plot_covariate class                       ----
#_______________________________________________________________________________

#' Forest plot covariate.
#' 
#' @export
setClass(
  "forest_plot_covariate",
  representation(
    label="character",
    value_label="character",
    unit="character"
  ),
  contains="fixed_covariate",
  validity=function(object) {
    return(c(expectOne(object, "label"),
             expectOne(object, "value_label"),
             expectOne(object, "unit")))
  }
)

#' 
#' Create a forest plot covariate.
#' 
#' @param name covariate name, single character value
#' @param value covariate value, single numeric value to use in CAMPSIS model
#' @param label covariate label, e.g. 'Weight'
#' @param value_label value label (useful for discrete covariate), e.g. 'Fasted'
#' @param unit value unit, e.g. 'kg'
#' @return a forest plot covariate
#' @export
ForestPlotCovariate <- function(name, value, label=NULL, value_label=NULL, unit=NULL) {
  if (is.null(label)) {
    label <- as.character(NA)
  }
  if (is.null(value_label)) {
    value_label <- as.character(NA)
  }
  if (is.null(unit)) {
    unit <- as.character(NA)
  }
  return(new("forest_plot_covariate", name=name, distribution=ConstantDistribution(value),
             label=label, value_label=value_label, unit=unit))
}

