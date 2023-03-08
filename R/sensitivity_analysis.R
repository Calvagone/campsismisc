
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
#' @param outputs outputs of interest, e.g. 'CL' or a PK metric from campsisnca or a list of them
#' @param dataset CAMPSIS dataset, if NULL, minimalist dataset with 1 subject 
#' and single observation at time 0 is created
#' @param replicates number of replicates, default is 1
#' @param seed simulation seed, default is 1
#' @param dest destination engine: rxode2, RxODE or mrgsolve (default)
#' @param settings simulation settings
#' @return an empty OAT-based sensibility analysis object
#' @export
SensitivityAnalysis <- function(model, outputs, dataset=NULL, replicates=1L, seed=1L, dest="mrgsolve", settings=Settings()) {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  if (is(outputs, "oat_analysis_output")) {
    # Add single output into multiple outputs object
    outputs <- OATOutputs() %>%
      add(outputs)
  }
  return(new("sensitivity_analysis", model=model, dataset=dataset, outputs=outputs,
             replicates=as.integer(replicates), seed=as.integer(seed), dest=dest, settings=settings))
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
  # Note, IIV, VARCOV_OMEGA, VARCOV_SIGMA was already disabled in the given model, see oat_analysis.R, prepare()
  # Because simulations are not replicated, variance-covariance matrix is not used by default in CAMPSIS
  # If sensitivity analysis is replicated, then theta uncertainty is taken into account.
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
        equationUp@rhs <- change@up %>% as.character()
        equationDown@rhs <- change@down %>% as.character()
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
