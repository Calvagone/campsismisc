
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
#' @param replicates number of replicates, default is 100
#' @param seed simulation seed, default is 1
#' @param dest destination engine: rxode2, RxODE or mrgsolve (default)
#' @param settings simulation settings
#' @return an empty forest plot
#' @export
ForestPlot <- function(model, outputs, dataset=NULL, replicates=100L, seed=1L, dest="mrgsolve", settings=Settings()) {
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
             replicates=as.integer(replicates), seed=as.integer(seed), dest=dest, settings=settings))
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
#' @param dataset Campsis dataset
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
#' @param geom_label_vjust vjust argument of method geom_label
#' @param geom_label_nudge_x nudge_x argument of method geom_label
#' @param geom_label_nudge_y nudge_y argument of method geom_label
#' @param geom_label_size size argument of method geom_label
#' @param label_nsig number of significant digits in label
#' @param geom_hline_color color argument of method geom_hline
setMethod("getForestPlot", signature=c("forest_plot", "integer", "logical", "logical", "logical", "logical", "numeric", "numeric", "numeric"),
          definition=function(object, index, relative, show_labels, show_ref, show_range, range, ci, limits, 
                              geom_label_vjust=0, geom_label_nudge_x=0.15, geom_label_nudge_y=0, geom_label_size=3, label_nsig=3, geom_hline_color="darkblue", ...) {
  alpha <- (1-ci)/2
  formula <- preprocessFunction(fun=~.x/.y, name="forest_plot_fct")
  iResults <- object@results@list[[index]]
  output <- iResults@output
  results <- iResults@results
  baseline <- iResults@baseline
  
  # Check limits argument
  if (any(is.na(limits))) {
    limits <- NULL
  }
  
  # Recompute VALUE as relative value if relative is TRUE
  if (relative) {
    results$VALUE <- formula(results$VALUE, baseline)
  }
  summary <- results %>%
    dplyr::group_by(dplyr::across("SCENARIO")) %>%
    dplyr::summarise(LOW=quantile(.data$VALUE, alpha),
                     MED=median(.data$VALUE),
                     UP=quantile(.data$VALUE, 1-alpha))
  
  # Add label column
  summary$LABEL <- paste0(signif(summary$MED, label_nsig), ' (',
                          signif(summary$LOW, label_nsig), '-',
                          signif(summary$UP, label_nsig), ')')
  
  plot <- ggplot2::ggplot(summary, ggplot2::aes(x=SCENARIO, y=MED)) + 
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=LOW, ymax=UP), width=0.2)
  
  if (show_ref) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 1,  baseline), color=geom_hline_color)
  }
  if (show_range) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 1,  baseline)*range, linetype=2)
  }
  if (show_labels) {
    # Note when hjust not set, geom_labels are automatically aligned with data
    plot <- plot + ggplot2::geom_label(ggplot2::aes(label=LABEL),
                                       vjust=geom_label_vjust, nudge_x=geom_label_nudge_x, nudge_y=geom_label_nudge_y, size=geom_label_size, label.size=NA)
  }
  
  plot <- plot +
    ggplot2::coord_flip(ylim=limits) +
    ggplot2::ylab(paste0(ifelse(relative, "Relative ", ""), output %>% getName())) +
    ggplot2::xlab(NULL)
  
  return(plot)
})
