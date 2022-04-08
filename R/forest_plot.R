
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
  seed <- 1
  
  # Base scenario, no study replication needed
  base_scenario <- simulate(model=model %>% disable(c("IIV")),
                            dataset=base_dataset, seed=seed)
  
  # 1 scenario per forest plot item, replicated
  scenarios <- Scenarios()
  for (item in items@list) {
    covariates <-  item %>% getCovariates()
    dataset_ <- base_dataset
    for (covariate in covariates@list) {
      dataset_ <- dataset_ %>% campsismod::replace(covariate)
    }
    scenarios <- scenarios %>%
      add(Scenario(name=item %>% getLabel(object@labeled_covariates), dataset=dataset_))
  }
  
  results <- simulate(model=model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA")),
                      dataset=base_dataset, scenarios=scenarios, replicates=object@replicates, seed=seed)
  
  # Processing results
  object@results <- results %>%
    dplyr::select(c("replicate", "SCENARIO", object@output)) %>%
    dplyr::rename_at(.vars=object@output, .funs=~"VALUE") %>%
    dplyr::mutate(BASELINE_VALUE=base_scenario %>% dplyr::pull(object@output)) %>%
    dplyr::mutate(CFB=(.data$VALUE-.data$BASELINE_VALUE)/.data$BASELINE_VALUE + 1)

  return(object)
})

#_______________________________________________________________________________
#----                               getPlot                                 ----
#_______________________________________________________________________________

#' @rdname getPlot
setMethod("getPlot", signature=c("forest_plot"), definition=function(object, limits=c(0.5,1.5), breaks=c(0.7,0.8,1,1.25,1.4),
                                                                     vjust=1, nudge_x=0.3, nudge_y=0, size=3) {
  
  # Note when hjust not set, geom_labels are automatically aligned with data
  summary <- object@results %>%
    dplyr::group_by(dplyr::across("SCENARIO")) %>%
    dplyr::summarise(CFB_LOW=quantile(.data$CFB, 0.05),
                     CFB_MED=median(.data$CFB),
                     CFB_UP=quantile(.data$CFB, 0.95))
  
  plot <- ggplot2::ggplot(summary, ggplot2::aes(x=factor(SCENARIO, levels=unique(SCENARIO)), y=CFB_MED)) + 
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=CFB_LOW, ymax=CFB_UP), width=0.2) +
    ggplot2::geom_hline(yintercept=1, col="darkblue") +
    ggplot2::geom_hline(yintercept=c(0.8, 1.25), linetype=2) +
    ggplot2::geom_label(ggplot2::aes(label=paste0(round(CFB_MED,2), ' (', round(CFB_LOW,2), '-', round(CFB_UP,2), ')')),
                        vjust=vjust, nudge_x=nudge_x, nudge_y=nudge_y, size=size, label.size=NA, ) +
    ggplot2::scale_y_continuous(breaks=breaks) +
    ggplot2::coord_flip(ylim=limits) +
    ggplot2::ylab(paste("Relative", object@output)) +
    ggplot2::xlab(NULL)
  
  return(plot)
})
