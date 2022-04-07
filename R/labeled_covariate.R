#_______________________________________________________________________________
#----                     labeled_covariate class                       ----
#_______________________________________________________________________________

#' Labeled covariate.
#' 
#' @export
setClass(
  "labeled_covariate",
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
#' Create a labeled covariate.
#' 
#' @param name covariate name, single character value
#' @param value covariate value, single numeric value to use in CAMPSIS model
#' @param label covariate label, e.g. 'Weight'
#' @param value_label value label (useful for discrete covariate), e.g. 'Fasted'
#' @param unit value unit, e.g. 'kg'
#' @return a labeled covariate
#' @export
LabeledCovariate <- function(name, value, label=NULL, value_label=NULL, unit=NULL) {
  if (is.null(label)) {
    label <- as.character(NA)
  }
  if (is.null(value_label)) {
    value_label <- as.character(NA)
  }
  if (is.null(unit)) {
    unit <- as.character(NA)
  }
  return(new("labeled_covariate", name=name, distribution=ConstantDistribution(value),
             label=label, value_label=value_label, unit=unit))
}

