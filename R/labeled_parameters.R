#_______________________________________________________________________________
#----                          labeled_parameters class                     ----
#_______________________________________________________________________________

#' Labeled covariates class.
#' 
#' @export
setClass(
  "labeled_parameters",
  representation(
  ),
  contains="character"
)

#' Add labels to your parameters.
#' 
#' @param x named vector of parameters (names are the parameter names, values are the corresponding labels)
#' @return a labeled_parameters object
#' @export
LabeledParameters <- function(x) {
  return(new("labeled_parameters", x))
}
