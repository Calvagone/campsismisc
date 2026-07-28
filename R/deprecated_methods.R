#' Get tornado plot (deprecated).
#' 
#' `getTornadoPlot()` is deprecated in favor of `get_tornado_plot()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_tornado_plot
#' @return a tornado plot
#' @export
#' @rdname getTornadoPlot
getTornadoPlot <- function(object, index, relative, show_labels, show_ref, limits, ...) {
  lifecycle::deprecate_warn("1.0.0", "getTornadoPlot()", "get_tornado_plot()")
  get_tornado_plot(object = object, index = index, relative = relative, show_labels = show_labels, show_ref = show_ref, limits = limits, ...)
}

setGeneric("getTornadoPlot", function(object, index=NULL, relative=NULL, show_labels=NULL, show_ref=NULL, limits=NULL, ...) {
  lifecycle::deprecate_warn("1.0.0", "getTornadoPlot()", "get_tornado_plot()")
  get_tornado_plot(object = object, index = index, relative = relative, show_labels = show_labels, show_ref = show_ref, limits = limits, ...)
})

#' Get forest plot (deprecated).
#' 
#' `getForestPlot()` is deprecated in favor of `get_forest_plot()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_forest_plot
#' @return a forest plot
#' @export
#' @rdname getForestPlot
getForestPlot <- function(object, index, relative, show_labels, show_ref, show_range, range, ci, limits, ...) {
  lifecycle::deprecate_warn("1.0.0", "getForestPlot()", "get_forest_plot()")
  get_forest_plot(object = object, index = index, relative = relative, show_labels = show_labels, show_ref = show_ref, show_range = show_range, range = range, ci = ci, limits = limits, ...)
}

setGeneric("getForestPlot", function(object, index=NULL, relative=NULL, show_labels=NULL, show_ref=NULL, show_range=NULL, range=NULL, ci=NULL, limits=NULL, ...) {
  lifecycle::deprecate_warn("1.0.0", "getForestPlot()", "get_forest_plot()")
  get_forest_plot(object = object, index = index, relative = relative, show_labels = show_labels, show_ref = show_ref, show_range = show_range, range = range, ci = ci, limits = limits, ...)
})
