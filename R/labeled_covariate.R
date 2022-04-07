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
    unit="character"
  ),
  contains="fixed_covariate",
  validity=function(object) {
    return(c(expectOne(object, "label"),
             expectOne(object, "unit")))
  }
)

#' 
#' Create a labeled covariate.
#' 
#' @param name covariate name, single character value
#' @param default_value default covariate value, single numeric value
#' @param label covariate label, e.g. 'Weight'
#' @param unit value unit, e.g. 'kg'
#' @return a labeled covariate
#' @export
LabeledCovariate <- function(name, default_value, label=NULL, unit=NULL) {
  if (is.null(label)) {
    label <- as.character(NA)
  }
  if (is.null(unit)) {
    unit <- as.character(NA)
  }
  return(new("labeled_covariate", name=name, distribution=ConstantDistribution(default_value),
             label=label, unit=unit))
}

#_______________________________________________________________________________
#----                     categorical_labeled_covariate class               ----
#_______________________________________________________________________________

#' Categorical labeled covariate.
#' 
#' @export
setClass(
  "categorical_labeled_covariate",
  representation(
    categories="numeric" # Named numeric vector, e.g. c(FASTED=0, FED=1)
  ),
  contains="labeled_covariate",
  validity=function(object) {
    isUnnamed <- is.null(names(object@categories))
    checkNames <- character(0)
    if (isUnnamed) {
      checkNames <- "Categories must be a named vector"
    }
    return(c(checkNames, expectOneOrMore(object, "categories")))
  }
)

#' 
#' Create a categorical labeled covariate.
#' 
#' @param name covariate name, single character value
#' @param default_value default covariate value, single numeric value
#' @param label covariate label, e.g. 'Weight'
#' @param unit value unit, e.g. 'kg'
#' @param categories named vector describing the categories, e.g. c(FED=0, FASTED=1)
#' @return a labeled covariate
#' @export
CategoricalLabeledCovariate <- function(name, default_value, label=NULL, unit=NULL, categories) {
  if (is.null(label)) {
    label <- as.character(NA)
  }
  if (is.null(unit)) {
    unit <- as.character(NA)
  }
  return(new("categorical_labeled_covariate", name=name, distribution=ConstantDistribution(default_value),
             label=label, unit=unit, categories=categories))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("labeled_covariate"), definition=function(x) {
  return(x@name)
})

