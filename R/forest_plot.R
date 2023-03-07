
#_______________________________________________________________________________
#----                           forest_plot class                           ----
#_______________________________________________________________________________

#' Forest plot class.
#' 
#' @export
setClass(
  "forest_plot",
  representation(
    labeled_covariates="labeled_covariates",
    items="forest_plot_items"
  ),
  contains="oat_analysis",
  prototype=prototype(model=CampsisModel(), dataset=Dataset())
)

#' Create a forest plot.
#'
#' @param model CAMPSIS model
#' @param outputs output(s) of interest, e.g. 'CL' or a PK metric from campsisnca or a list of them
#' @param dataset CAMPSIS dataset, if NULL, minimalist dataset with 1 subject 
#' and single observation at time 0 is created
#' @param replicates number of replicates
#' @param dest destination engine: rxode2, RxODE or mrgsolve (default)
#' @param settings simulation settings
#' @return an empty forest plot
#' @export
ForestPlot <- function(model, outputs, dataset=NULL, replicates=1L, dest="mrgsolve", settings=Settings()) {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  if (is(outputs, "oat_analysis_output")) {
    # Add single output into multiple outputs object
    outputs <- OATOutputs() %>%
      add(outputs)
  }
  return(new("forest_plot", model=model, dataset=dataset, outputs=outputs,
             replicates=as.integer(replicates), dest=dest, settings=settings))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature=c("forest_plot", "labeled_covariate"), definition=function(object, x) {
  object@labeled_covariates <- object@labeled_covariates %>% add(x)
  return(object)
})

setMethod("add", signature=c("forest_plot", "forest_plot_item"), definition=function(object, x) {
  object@items <- object@items %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                           createScenarios                             ----
#_______________________________________________________________________________

#' @rdname createScenarios
setMethod("createScenarios", signature=c("forest_plot"), definition=function(object, dataset, ...) {
  scenarios <- Scenarios()
  items <- object@items
  
  for (item in items@list) {
    covariates <-  item %>% getCovariates()
    dataset_ <- dataset
    for (covariate in covariates@list) {
      dataset_ <- dataset_ %>% campsismod::replace(covariate)
    }
    scenarios <- scenarios %>%
      add(Scenario(name=item %>% getLabel(object@labeled_covariates), dataset=dataset_))
  }
  return(scenarios)
})
