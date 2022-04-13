
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
    output="forest_plot_output",
    replicates="integer",
    results="data.frame",
    dest="character"
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
#' @param dest destination engine: 'RxODE' or 'mrgsolve'
#' @return an empty forest plot
#' @export
ForestPlot <- function(model, output, dataset=NULL, replicates=1L, dest="RxODE") {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  return(new("forest_plot", model=model, dataset=dataset, output=output,
             replicates=as.integer(replicates), dest=dest))
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

outfunNCA <- function(metric, x) {
  metric@x <- x
  metric <- metric %>% calculate(level=0.9) # Level does not matter as we collect only individuals
  return(metric@individual %>% dplyr::rename_at(.vars="value", .funs=~"VALUE"))
}

#' @rdname prepare
setMethod("prepare", signature=c("forest_plot"), definition=function(object) {
  model <- object@model
  base_dataset <- object@dataset %>% add(object@labeled_covariates@list)
  items <- object@items
  output <- object@output
  replicates <- object@replicates
  dest <- object@dest
  seed <- 1
  
  # Base scenario, no study replication needed
  base_scenario <- simulate(model=model %>% disable(c("IIV")),
                            dataset=base_dataset, seed=seed)
  
  if (is(output, "model_parameter_output")) {
    outputName <- output %>% getName()
    baseline <- base_scenario %>% dplyr::pull(outputName)
  } else if (is(output, "nca_metric_output")) {
    baseline <- outfunNCA(metric=output@metric, x=base_scenario) %>% dplyr::pull(VALUE)
  } else {
    stop()
  }
  
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
  
  # Simulation of model parameter output
  outvars <- NULL
  if (is(output, "model_parameter_output")) {
    outputName <- output %>% getName()
    results <- simulate(model=model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA")),
                        dataset=base_dataset, scenarios=scenarios, replicates=replicates,
                        seed=seed, dest=dest, outvars=outputName) %>%
      dplyr::select(c("replicate", "SCENARIO", outputName)) %>%
      dplyr::rename_at(.vars=outputName, .funs=~"VALUE") %>%
      dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev()))
    
  } else if (is(output, "nca_metric_output")) {
    outfun <- function(x) {outfunNCA(metric=output@metric, x=x)}
    results <- simulate(model=model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA")),
                        dataset=base_dataset, scenarios=scenarios, replicates=replicates,
                        seed=seed, dest=dest, outvars=output@metric@variable, outfun=outfun) %>% 
      dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev()))
  } else {
    stop()
  }
  
  # Compute change from baseline
  # TODO: several formula's should be available
  object@results <- results %>% dplyr::mutate(BASELINE_VALUE=baseline) %>%
    dplyr::mutate(CHANGE=(.data$VALUE-.data$BASELINE_VALUE)/.data$BASELINE_VALUE + 1)
  
  return(object)
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
