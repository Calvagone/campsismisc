
#_______________________________________________________________________________
#----                       sensitivity_analysis class                      ----
#_______________________________________________________________________________

#' Sensitivity analysis class.
#' 
#' @export
setClass(
  "sensitivity_analysis",
  representation(
    items="sensitivity_analysis_items",
    labeled_parameters="labeled_parameters"
  ),
  contains="oat_analysis",
  prototype=prototype(model=CampsisModel(), dataset=Dataset())
)

#' Create an OAT-based sensitivity analysis object.
#'
#' @param model CAMPSIS model
#' @param output output of interest, e.g. 'CL' or a PK metric from campsisnca
#' @param dataset CAMPSIS dataset, if NULL, minimalist dataset with 1 subject 
#' and single observation at time 0 is created
#' @param replicates number of replicates
#' @param dest destination engine: 'RxODE' or 'mrgsolve'
#' @return an empty OAT-based sensibility analysis object
#' @export
SensitivityAnalysis <- function(model, output, dataset=NULL, replicates=1L, dest="RxODE") {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  return(new("sensitivity_analysis", model=model, dataset=dataset, output=output,
             replicates=as.integer(replicates), dest=dest))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________


setMethod("add", signature=c("sensitivity_analysis", "sensitivity_analysis_item"), definition=function(object, x) {
  object@items <- object@items %>% add(x)
  return(object)
})

setMethod("add", signature=c("sensitivity_analysis", "labeled_parameters"), definition=function(object, x) {
  object@labeled_parameters <- x
  return(object)
})

#_______________________________________________________________________________
#----                           createScenarios                             ----
#_______________________________________________________________________________

#' @rdname createScenarios
setMethod("createScenarios", signature=c("sensitivity_analysis"), definition=function(object, model, ...) {
  scenarios <- Scenarios()
  items <- object@items
  
  for (item in items@list) {
    changes <-  item@changes
    modelUp <- model # First direction of tornado plot (direction up)
    modelDown <- model # Other direction of tornado plot (direction down)
    
    for (change in changes@list) {
      name <- change %>% getName()
      equation <- model %>% find(Equation(name))
      if (is.null(equation)) {
        stop(paste0("Equation ", name, " cannot be found in model"))
      }
      equationUp <- equation
      equationDown <- equation
      if (change@up_down_as_factor) {
        equationUp@rhs <- paste0("(", equationUp@rhs, ") * ", change@up) # Multiplication
        equationDown@rhs <- paste0("(", equationDown@rhs, ") / ", change@down) # Division
      } else {
        # Use values as is
        equationUp@rhs <- change@up
        equationDown@rhs <- change@down
      }

      modelUp <- modelUp %>% campsismod::replace(equationUp)
      modelDown <- modelDown %>% campsismod::replace(equationDown)
    }
    scenarios <- scenarios %>%
      add(Scenario(name=paste0(item %>% getName(), ", up"), model=modelUp)) %>%
      add(Scenario(name=paste0(item %>% getName(), ", down"), model=modelDown))
  }
  return(scenarios)
})
