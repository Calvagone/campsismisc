
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
#' @param output output of interest, e.g. 'CL' or a PK metric from campsisnca
#' @param dataset CAMPSIS dataset, if NULL, minimalist dataset with 1 subject 
#' and single observation at time 0 is created
#' @param replicates number of replicates
#' @param dest destination engine: 'RxODE' or 'mrgsolve'
#' @return an empty forest plot
#' @export
ForestPlot <- function(model, output, dataset=NULL, replicates=1L, dest="RxODE") {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  formula <- ~(.x-.y)/.y + 1
  formula <- preprocessFunction(fun=formula, name="forest_plot_fct")
  return(new("forest_plot", model=model, dataset=dataset, output=output,
             replicates=as.integer(replicates), dest=dest, formula=formula))
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
setMethod("createScenarios", signature=c("forest_plot"), definition=function(object, dataset) {
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

#_______________________________________________________________________________
#----                               getPlot                                 ----
#_______________________________________________________________________________

#' @rdname getPlot
setMethod("getPlot", signature=c("forest_plot"), definition=function(object, limits=c(0.5,1.5), breaks=c(0.7,0.8,1,1.25,1.4),
                                                                     vjust=0, nudge_x=0.15, nudge_y=0, size=3) {
  
  # Note when hjust not set, geom_labels are automatically aligned with data
  summary <- object@results %>%
    dplyr::group_by(dplyr::across("SCENARIO")) %>%
    dplyr::summarise(CHANGE_LOW=quantile(.data$CHANGE, 0.05),
                     CHANGE_MED=median(.data$CHANGE),
                     CHANGE_UP=quantile(.data$CHANGE, 0.95))
  
  plot <- ggplot2::ggplot(summary, ggplot2::aes(x=SCENARIO, y=CHANGE_MED)) + 
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=CHANGE_LOW, ymax=CHANGE_UP), width=0.2) +
    ggplot2::geom_hline(yintercept=1, col="darkblue") +
    ggplot2::geom_hline(yintercept=c(0.8, 1.25), linetype=2) +
    ggplot2::geom_label(ggplot2::aes(label=paste0(round(CHANGE_MED,2), ' (', round(CHANGE_LOW,2), '-', round(CHANGE_UP,2), ')')),
                        vjust=vjust, nudge_x=nudge_x, nudge_y=nudge_y, size=size, label.size=NA, ) +
    ggplot2::scale_y_continuous(breaks=breaks) +
    ggplot2::coord_flip(ylim=limits) +
    ggplot2::ylab(paste("Relative", object@output %>% getName())) +
    ggplot2::xlab(NULL)
  
  return(plot)
})
