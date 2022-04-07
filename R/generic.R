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

