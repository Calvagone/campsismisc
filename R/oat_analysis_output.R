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
    return(c(expect_one(object, "name")))
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
    metric="nca_metric", # E.g. "AUC()"
  ),
  contains="oat_analysis_output"
)

#' Create a model parameter output.
#' 
#' @param metric NCA metric coming from campsisnca
#' @export
NcaMetricOutput <- function(metric) {
  return(new("nca_metric_output", metric=metric))
}

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

setMethod("get_name", signature=c("model_parameter_output"), definition=function(x) {
  return(x@name)
})

setMethod("get_name", signature=c("nca_metric_output"), definition=function(x) {
  name <- x@metric %>% get_name()
  return(name)
})

#_______________________________________________________________________________
#----                            get_outvars                                ----
#_______________________________________________________________________________

#' Get out variables to be simulated.
#' 
#' @param object output
#' @return variables to be simulated
#' @rdname get_outvars
get_outvars <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_outvars", function(object) {
  standardGeneric("get_outvars")
})

#' @rdname get_outvars
setMethod("get_outvars", signature=c("model_parameter_output"), definition=function(object) {
  return(object %>% get_name())
})

#' @rdname get_outvars
setMethod("get_outvars", signature=c("nca_metric_output"), definition=function(object) {
  return(object@metric@variable)
})

