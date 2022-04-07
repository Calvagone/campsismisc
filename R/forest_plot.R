
#_______________________________________________________________________________
#----                           forest_plot class                           ----
#_______________________________________________________________________________

#' Forest plot class.
#' 
#' @export
setClass(
  "forest_plot",
  representation(
    model="campsis_model",
    labeled_covariates="labeled_covariates",
    dataset="dataset",
    items="forest_plot_items",
    output="ANY",
    replicates="integer",
    results="data.frame"
  ),
  prototype=prototype(model=CampsisModel(), dataset=Dataset())
)

#' Create a forest plot.
#'
#' @param model CAMPSIS model
#' @param output output of interest, e.g. 'CL' or a PK metric from campsisnca
#' @param dataset CAMPSIS dataset, if NULL, minimalist dataset with 1 subject 
#' and single observation at time 0 is created
#' @param replicates number of replicates
#' @return an empty forest plot
#' @export
ForestPlot <- function(model, output, dataset=NULL, replicates=1L) {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  return(new("forest_plot", model=model, dataset=dataset, output=output,
             replicates=as.integer(replicates)))
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
#----                               prepare                                 ----
#_______________________________________________________________________________

#' @rdname prepare
setMethod("prepare", signature=c("forest_plot"), definition=function(object) {
  model <- object@model
  base_dataset <- object@dataset %>% add(object@labeled_covariates@list)
  items <- object@items
  
  scenarios <- Scenarios()
  for (item in items@list) {
    covariates <-  item %>% getCovariates()
    dataset_ <- base_dataset
    for (covariate in covariates@list) {
      dataset_ <- dataset_ %>% replace(covariate)
    }
    scenarios <- scenarios %>%
      add(Scenario(dataset=dataset_))
  }
  
  results <- simulate(model=model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA")),
                      dataset=base_dataset, scenarios=scenarios, replicates=object@replicates)
  
  return(object)
})
