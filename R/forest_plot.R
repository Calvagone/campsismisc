
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

#_______________________________________________________________________________
#----                               getForestPlot                           ----
#_______________________________________________________________________________

#' @rdname getForestPlot
setMethod("getForestPlot", signature=c("forest_plot", "logical", "logical", "logical", "logical", "numeric", "numeric", "numeric", "numeric"),
          definition=function(object, relative, show_labels, show_ref, show_range, range, ci, limits, breaks, ...) {
  
  # Note when hjust not set, geom_labels are automatically aligned with data
  vjust <- 0
  nudge_x=0.15
  nudge_y=0
  size=3
  
  formula <- preprocessFunction(fun=~.x/.y, name="forest_plot_fct")
  
  alpha <- (1-ci)/2
  # Recompute VALUE as relative value if relative is TRUE
  if (relative) {
    object@results$VALUE <- formula(object@results$VALUE, object@baseline)
  }
  summary <- object@results %>%
    dplyr::group_by(dplyr::across("SCENARIO")) %>%
    dplyr::summarise(LOW=quantile(.data$VALUE, alpha),
                     MED=median(.data$VALUE),
                     UP=quantile(.data$VALUE, 1-alpha))
  
  plot <- ggplot2::ggplot(summary, ggplot2::aes(x=SCENARIO, y=MED)) + 
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=LOW, ymax=UP), width=0.2)
  
  if (show_ref) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 1,  object@baseline), col="darkblue")
  }
  if (show_range) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 1,  object@baseline)*range, linetype=2)
  }
  if (show_labels) {
    plot <- plot + ggplot2::geom_label(ggplot2::aes(label=paste0(round(MED, 2), ' (', round(LOW, 2), '-', round(UP, 2), ')')),
                                       vjust=vjust, nudge_x=nudge_x, nudge_y=nudge_y, size=size, label.size=NA)
  }
  if (breaks %>% length()==0) {
    plot <- plot + ggplot2::scale_y_continuous()
  } else {
    plot <- plot + ggplot2::scale_y_continuous(breaks=breaks)
  }
  
  if (limits %>% length()==0) {
    limits <- NULL
  }
  
  plot <- plot +
    ggplot2::coord_flip(ylim=limits) +
    ggplot2::ylab(paste0(ifelse(relative, "Relative ", ""), object@output %>% getName())) +
    ggplot2::xlab(NULL)
  
  return(plot)
})
