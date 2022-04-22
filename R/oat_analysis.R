
#_______________________________________________________________________________
#----                           oat_analysis class                          ----
#_______________________________________________________________________________

#' One-at-a-time analysis structure.
#' 
#' @export
setClass(
  "oat_analysis",
  representation(
    model="campsis_model",
    dataset="dataset",
    output="oat_analysis_output",
    replicates="integer",
    dest="character",
    baseline="numeric", # transient
    results="data.frame" # transient
  ),
  prototype=prototype(model=CampsisModel(), dataset=Dataset())
)

#_______________________________________________________________________________
#----                           computeBaseline                             ----
#_______________________________________________________________________________

#' Compute baseline.
#' 
#' @param object simulation results of the baseline, tibble
#' @param output plot output
#' @return the baseline, double
#' @export
#' @rdname computeBaseline
computeBaseline <- function(object, output) {
  stop("No default function is provided")
}

setGeneric("computeBaseline", function(object, output) {
  standardGeneric("computeBaseline")
})

#' @rdname computeBaseline
setMethod("computeBaseline", signature=c("tbl_df", "model_parameter_output"), definition=function(object, output) {
  outputName <- output %>% getName()
  baseline <- object %>% dplyr::pull(outputName)
  return(baseline)
})

#' @rdname computeBaseline
setMethod("computeBaseline", signature=c("tbl_df", "nca_metric_output"), definition=function(object, output) {
  baseline <- outfunNCA(metric=output@metric, x=output@filter(object)) %>% dplyr::pull(VALUE)
  return(baseline)
})

#_______________________________________________________________________________
#----                           createScenarios                             ----
#_______________________________________________________________________________

#' Create scenarios.
#' 
#' @param object OAT analysis, type forest plot or sensitivity analysis
#' @param ... extra arguments
#' @return a list of CAMPSIS scenarios to be run
#' @export
#' @rdname createScenarios
createScenarios <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("createScenarios", function(object, ...) {
  standardGeneric("createScenarios")
})

#_______________________________________________________________________________
#----                         postProcessScenarios                          ----
#_______________________________________________________________________________

#' postProcessScenarios
#' 
#' @param object simulation results, tibble
#' @param output plot output
#' @return tibble
#' @export
#' @rdname postProcessScenarios
postProcessScenarios <- function(object, output) {
  stop("No default function is provided")
}

setGeneric("postProcessScenarios", function(object, output) {
  standardGeneric("postProcessScenarios")
})

#' @rdname postProcessScenarios
setMethod("postProcessScenarios", signature=c("tbl_df", "model_parameter_output"), definition=function(object, output) {
  outputName <- output %>% getName()
  results <- object %>%
    dplyr::select(c("replicate", "SCENARIO", outputName)) %>%
    dplyr::rename_at(.vars=outputName, .funs=~"VALUE") %>%
    dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev()))
  return(results)
})

#' @rdname postProcessScenarios
setMethod("postProcessScenarios", signature=c("tbl_df", "nca_metric_output"), definition=function(object, output) {
  results <- object %>% 
    dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev()))
  return(results)
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
setMethod("prepare", signature=c("oat_analysis"), definition=function(object) {
  model <- object@model
  base_dataset <- object@dataset
  if (is(object, "forest_plot")) {
    base_dataset <- base_dataset %>% add(object@labeled_covariates@list)
  }
  items <- object@items
  output <- object@output
  replicates <- object@replicates
  dest <- object@dest
  seed <- 1
  
  # Compute and store baseline value
  base_scenario <- simulate(model=model %>% disable(c("IIV")), dataset=base_dataset, seed=seed)
  baseline <- base_scenario %>% computeBaseline(output=output)
  object@baseline <- baseline
  
  # Generate scenarios
  scenarios <- object %>% createScenarios(dataset=base_dataset, model=model)
  
  # Simulation all scenarios
  outvars <- output %>% getOutvars()
  outfun <- NULL
  if (is(output, "nca_metric_output")) {
    outfun <- function(x) {outfunNCA(metric=output@metric, x=output@filter(x))}
  }
  results <- simulate(model=model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA")),
                      dataset=base_dataset, scenarios=scenarios, replicates=replicates,
                      seed=seed, dest=dest, outvars=outvars, outfun=outfun) %>% postProcessScenarios(output=output)
  
  # Store results
  object@results <- results
  
  return(object)
})

#_______________________________________________________________________________
#----                               getForestPlot                           ----
#_______________________________________________________________________________

#' @rdname getForestPlot
setMethod("getForestPlot", signature=c("oat_analysis", "logical", "logical", "logical", "logical", "numeric", "numeric", "numeric", "numeric"),
          definition=function(object, relative, show_labels, show_ref, show_range, range, ci, limits, breaks, ...) {
            
  # Note when hjust not set, geom_labels are automatically aligned with data
  vjust <- 0
  nudge_x=0.15
  nudge_y=0
  size=3
  
  alpha <- (1-ci)/2
  # Recompute VALUE as relative value if relative is TRUE
  if (relative) {
    object@results$VALUE <- object@formula(object@results$VALUE, object@baseline)
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
