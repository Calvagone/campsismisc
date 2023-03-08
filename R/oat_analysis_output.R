#_______________________________________________________________________________
#----                      oat_analysis_output class                        ----
#_______________________________________________________________________________

#' OAT analysis output, see this class as an interface.
#' 
#' @export
setClass(
  "oat_analysis_output",
  representation(
  ),
  contains="pmx_element"
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
  contains="oat_analysis_output",
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
    metric="nca_metric", # E.g. "Auc()"
    filter="function",
    suffix="character"
  ),
  contains="oat_analysis_output",
  validity=function(object) {
    return(expectOneForAll(object, c("suffix")))
  }
)

#' Create a model parameter output.
#' 
#' @param metric NCA metric coming from campsisnca
#' @param filter function to be applied on the simulation output before the
#'  NCA analysis, either a function or a lambda formula
#' @param suffix a possible suffix that will be added to the NCA metric name
#'  (e.g. useful when the same metric appears more than once in the outputs list), default is NULL
#' @export
NcaMetricOutput <- function(metric, filter=NULL, suffix=NULL) {
  filter <- preprocessFunction(fun=filter, name="filter function")
  if (is.null(suffix)) {
    suffix <- as.character(NA)
  }
  return(new("nca_metric_output", metric=metric, filter=filter, suffix=suffix))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("model_parameter_output"), definition=function(x) {
  return(x@name)
})

setMethod("getName", signature=c("nca_metric_output"), definition=function(x) {
  name <- x@metric %>% getName()
  if (!is.na(x@suffix)) {
    name <- paste(name, x@suffix)
  }
  return(name)
})

#_______________________________________________________________________________
#----                             getOutvars                                ----
#_______________________________________________________________________________

#' Get out variables to be simulated.
#' 
#' @param object output
#' @return variables to be simulated
#' @export
#' @rdname getOutvars
getOutvars <- function(object) {
  stop("No default function is provided")
}

setGeneric("getOutvars", function(object) {
  standardGeneric("getOutvars")
})

#' @rdname getOutvars
setMethod("getOutvars", signature=c("model_parameter_output"), definition=function(object) {
  return(object %>% getName())
})

#' @rdname getOutvars
setMethod("getOutvars", signature=c("nca_metric_output"), definition=function(object) {
  return(object@metric@variable)
})

