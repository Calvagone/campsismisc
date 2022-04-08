#_______________________________________________________________________________
#----                              prepare                                  ----
#_______________________________________________________________________________

#' Prepare.
#' 
#' @param object generic object
#' @return same object
#' @export
#' @rdname prepare
prepare <- function(object) {
  stop("No default function is provided")
}

setGeneric("prepare", function(object) {
  standardGeneric("prepare")
})

#_______________________________________________________________________________
#----                              getLabel                                 ----
#_______________________________________________________________________________

#' Get label.
#' 
#' @param object generic object
#' @param ... extra arguments
#' @return a label
#' @export
#' @rdname getLabel
getLabel <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getLabel", function(object, ...) {
  standardGeneric("getLabel")
})

#_______________________________________________________________________________
#----                              getPlot                                  ----
#_______________________________________________________________________________

#' Get plot.
#' 
#' @param object generic object
#' @param ... extra arguments
#' @return ggplot
#' @export
#' @rdname getPlot
getPlot <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getPlot", function(object, ...) {
  standardGeneric("getPlot")
})

