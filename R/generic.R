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
#----                             getForestPlot                             ----
#_______________________________________________________________________________

#' Get forest plot.
#' 
#' @param object generic object
#' @param index output number, 1 by default
#' @param relative relative change or absolute value, logical
#' @param show_labels show the numeric labels
#' @param show_ref show vertical reference (at 1 or at the baseline) 
#' @param show_range show vertical dashed lines, logical
#' @param range vertical dashed lines range, default is c(0.8, 1.25)
#' @param ci confidence interval, default is 0.9, which corresponds to a 90\% CI
#' @param limits limits of the forest plot, numeric vector (2 values)
#' @param ... extra arguments like 'geom_label_vjust', 'geom_label_nudge_x', 'geom_label_nudge_y', 'geom_label_size', 'label_nsig', 'geom_hline_color'
#' @return ggplot
#' @export
#' @rdname getForestPlot
getForestPlot <- function(object, index, relative, show_labels, show_ref, show_range, range, ci, ...) {
  stop("No default function is provided")
}

setGeneric("getForestPlot", function(object, index=NULL, relative=NULL, show_labels=NULL, show_ref=NULL, show_range=NULL, range=NULL, ci=NULL, limits=NULL, ...) {
  if (is.null(index)) {
    index <- 1L
  } else {
    index <- as.integer(index)
  }
  if (is.null(relative)) {
    relative <- TRUE
  }
  if (is.null(show_labels)) {
    show_labels <- TRUE
  }
  if (is.null(show_ref)) {
    show_ref <- TRUE
  }
  if (is.null(show_range)) {
    show_range <- TRUE
  }
  if (is.null(range)) {
    range <- c(0.8, 1.25)
  }
  if (is.null(ci)) {
    ci <- 0.9
  }
  if (is.null(limits)) {
    limits <- as.numeric(NA)
  }
  standardGeneric("getForestPlot")
})

#_______________________________________________________________________________
#----                             getTornadoPlot                             ----
#_______________________________________________________________________________

#' Get tornado plot.
#' 
#' @param object generic object
#' @param index output number, 1 by default
#' @param relative relative change or absolute value, logical
#' @param show_labels show the numeric labels
#' @param show_ref show vertical reference (at 0 or at the baseline)
#' @param limits limits of the tornado plot, numeric vector (2 values)
#' @param ... extra arguments like 'geom_bar_width', 'geom_text_nudge_y', 'label_nsig', 'geom_hline_color', 'geom_text_size'
#' @return ggplot
#' @export
#' @rdname getTornadoPlot
getTornadoPlot <- function(object, index, relative, show_labels, show_ref, ...) {
  stop("No default function is provided")
}

setGeneric("getTornadoPlot", function(object, index=NULL, relative=NULL, show_labels=NULL, show_ref=NULL, limits=NULL, ...) {
  if (is.null(index)) {
    index <- 1L
  } else {
    index <- as.integer(index)
  }
  if (is.null(relative)) {
    relative <- TRUE
  }
  if (is.null(show_labels)) {
    show_labels <- TRUE
  }
  if (is.null(show_ref)) {
    show_ref <- TRUE
  }
  if (is.null(limits)) {
    limits <- as.numeric(NA)
  }
  standardGeneric("getTornadoPlot")
})

