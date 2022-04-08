#_______________________________________________________________________________
#----                       forest_plot_output class                        ----
#_______________________________________________________________________________

#' Forest plot output, see this class as an interface.
#' 
#' @export
setClass(
  "forest_plot_output",
  representation(
  )
)

#_______________________________________________________________________________
#----                      model_parameter_output class                     ----
#_______________________________________________________________________________

#' Model parameter output.
#' 
#' @export
setClass(
  "model_parameter_output",
  representation(
    name="character" # E.g. "CL"
  ),
  contains="forest_plot_output",
  validity=function(object) {
    return(c(expectOne(object, "name")))
  }
)

#' Create a model parameter output.
#' 
#' @param name model parameter name
#' @export
ModelParameterOutput <- function(name) {
  return(new("model_parameter_output", name=name))
}

#_______________________________________________________________________________
#----                        nca_metric_output class                        ----
#_______________________________________________________________________________

#' NCA metric output.
#' 
#' @export
setClass(
  "nca_metric_output",
  representation(
    metric="nca_metric" # E.g. "Auc()"
  ),
  contains="forest_plot_output",
  validity=function(object) {
    return(TRUE)
  }
)

#' Create a model parameter output.
#' 
#' @param name model parameter name
#' @export
NcaMetricOutput <- function(metric) {
  return(new("nca_metric_output", metric=metric))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("model_parameter_output"), definition=function(x) {
  return(x@name)
})

setMethod("getName", signature=c("nca_metric_output"), definition=function(x) {
  return(x@metric %>% getName())
})
