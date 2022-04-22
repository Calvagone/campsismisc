
#_______________________________________________________________________________
#----                       sensitivity_analysis class                      ----
#_______________________________________________________________________________

#' Sensitivity analysis class.
#' 
#' @export
setClass(
  "sensitivity_analysis",
  representation(
    items="sensitivity_analysis_items"
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

#_______________________________________________________________________________
#----                           createScenarios                             ----
#_______________________________________________________________________________

#' @rdname createScenarios
setMethod("createScenarios", signature=c("sensitivity_analysis"), definition=function(object, model, ...) {
  scenarios <- Scenarios()
  items <- object@items
  
  for (item in items@list) {
    factors <-  item@factors
    model_ <- model
    for (factor in factors@list) {
      name <- factor %>% getName()
      equation <- model %>% find(Equation(name))
      if (is.null(equation)) {
        stop(paste0("Equation ", name, " cannot be found in model"))
      }
      equation@rhs <- paste0("(", equation@rhs, ") * ", factor@multiplier)
      model_ <- model_ %>% campsismod::replace(equation)
    }
    scenarios <- scenarios %>%
      add(Scenario(name=item %>% getName(), model=model_))
  }
  return(scenarios)
})
