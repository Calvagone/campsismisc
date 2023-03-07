
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
    outputs="oat_analysis_outputs",
    replicates="integer",
    dest="character",
    settings="simulation_settings",
    results="oat_analysis_results"
  ),
  prototype=prototype(model=CampsisModel(), dataset=Dataset())
)

#_______________________________________________________________________________
#----                           computeBaseline                             ----
#_______________________________________________________________________________

#' Compute baseline.
#' 
#' @param object simulation results of the baseline, tibble
#' @param output OAT output, one or several
#' @return the baseline, double or list of double
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
  baseline <- object %>% dplyr::pull(outputName) %>% unique()
  assertthat::assert_that(length(baseline)==1,
                          msg=paste0("Baseline should be unique for parameter ", outputName))
  return(baseline)
})

#' @rdname computeBaseline
setMethod("computeBaseline", signature=c("tbl_df", "nca_metric_output"), definition=function(object, output) {
  baseline <- outfunNCA(metric=output@metric, x=output@filter(object)) %>% dplyr::pull(VALUE)
  return(baseline)
})

#' @rdname computeBaseline
setMethod("computeBaseline", signature=c("tbl_df", "oat_analysis_outputs"), definition=function(object, output) {
  baselines <- output@list %>% purrr::map(~computeBaseline(object=object, output=.x))
  return(baselines)
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
setMethod("postProcessScenarios", signature=c("tbl_df", "oat_analysis_output"), definition=function(object, output) {
  outputName <- output %>% getName()
  results <- object %>%
    dplyr::select(dplyr::any_of("replicate"), dplyr::all_of(c("SCENARIO", outputName))) %>%
    tidyr::unnest(cols=outputName) %>%
    dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev())) %>%
    dplyr::relocate(dplyr::any_of(c("replicate", "id", "VALUE", "SCENARIO")))
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
  # Fix IIV, VARCOV_OMEGA, VARCOV_SIGMA at the beginning!
  # This way, calling disable is not needed in the scenarios
  model <- object@model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA"))
  
  base_dataset <- object@dataset
  if (is(object, "forest_plot")) {
    base_dataset <- base_dataset %>% add(object@labeled_covariates@list)
  }
  items <- object@items
  outputs <- object@outputs
  replicates <- object@replicates
  dest <- object@dest
  seed <- 1
  
  # Compute and store baseline value of each output
  base_scenario <- simulate(model=model, dataset=base_dataset, seed=seed)
  baselines <- base_scenario %>% computeBaseline(output=outputs)
  
  # Generate scenarios
  scenarios <- object %>% createScenarios(dataset=base_dataset, model=model)
  
  # Getting all necessary 'outvars' across all outputs
  # This in order to make 1 call to the simulate method
  outvars <- outputs@list %>% purrr::map_chr(~getOutvars(.x)) %>% unique()
  
  # Preparing outfun function
  outfun <- function(x) {
    retValue <- outputs@list %>% purrr::map_dfc(.f=function(output) {
      outputName <- output %>% getName()
      if (is(output, "nca_metric_output")) {
        myFun <- function(x) {outfunNCA(metric=output@metric, x=output@filter(x))}
        innerRetValue <- myFun(x)
      } else if (is(output, "model_parameter_output")) {
        innerRetValue <- x[, outputName] %>% dplyr::distinct() %>%
          dplyr::rename(VALUE=!!outputName)
        assertthat::assert_that(nrow(innerRetValue)==1,
                                msg=paste0("Parameter ", outputName, " must not change over time"))
      } else {
        stop("Only NCA output or model parameter output")
      }
      return(tibble::tibble(!!outputName:=list(innerRetValue)))
    })
    return(retValue)
  }

  # Simulate
  allResults <- simulate(model=model, dataset=base_dataset, scenarios=scenarios, replicates=replicates,
                      seed=seed, dest=dest, outvars=outvars, outfun=outfun, settings=object@settings)
  
  # Post-processing
  tmpResults <- purrr::map2(.x=outputs@list, .y=baselines, .f=function(output, baseline) {
    res <- OATResult(output=output, baseline=baseline,
                     results=postProcessScenarios(object=allResults, output=output))
    return(res)
  })
  
  # Save
  object@results <- OATResults() %>%
    add(tmpResults)
  
  return(object)
})

#_______________________________________________________________________________
#----                               getForestPlot                           ----
#_______________________________________________________________________________

#' @rdname getForestPlot
setMethod("getForestPlot", signature=c("oat_analysis", "integer", "logical", "logical", "logical", "logical", "numeric", "numeric"),
          definition=function(object, index, relative, show_labels, show_ref, show_range, range, ci, 
                              geom_label_vjust=0, geom_label_nudge_x=0.15, geom_label_nudge_y=0, geom_label_size=3, label_nsig=3, geom_hline_color="darkblue", ...) {
            
  alpha <- (1-ci)/2
  formula <- preprocessFunction(fun=~.x/.y, name="forest_plot_fct")
  iResults <- object@results@list[[index]]
  output <- iResults@output
  results <- iResults@results
  baseline <- iResults@baseline
  
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
    plot <- plot + ggplot2::geom_label(ggplot2::aes_string(label="LABEL"),
                                       vjust=geom_label_vjust, nudge_x=geom_label_nudge_x, nudge_y=geom_label_nudge_y, size=geom_label_size, label.size=NA)
  }

  plot <- plot +
    ggplot2::coord_flip() +
    ggplot2::ylab(paste0(ifelse(relative, "Relative ", ""), output %>% getName())) +
    ggplot2::xlab(NULL)
  
  return(plot)
})

#_______________________________________________________________________________
#----                             getTornadoPlot                            ----
#_______________________________________________________________________________

#' @rdname getTornadoPlot
setMethod("getTornadoPlot", signature=c("oat_analysis", "integer", "logical", "logical", "logical"),
          definition=function(object, index, relative, show_labels, show_ref,
                              geom_bar_width=0.5, geom_text_nudge_y=1, label_nsig=3, geom_hline_color="grey", geom_text_size=3, ...) {
  isSensitivityAnalysis <- is(object, "sensitivity_analysis")
  iResults <- object@results@list[[index]]
  output <- iResults@output
  results <- iResults@results
  baseline <- iResults@baseline
  
  # Tornado plots are bidirectional, forest plots aren't
  if (isSensitivityAnalysis) {
    results <- results %>%
      tidyr::separate(col="SCENARIO", sep=", ", into=c("ITEM_NAME", "DIRECTION"))
  } else {
    results <- results %>%
      dplyr::mutate(ITEM_NAME=.data$SCENARIO, DIRECTION="up")
  }
  
  noOfScenarios <- object@items %>% length()
  formula <-  preprocessFunction(fun=~(.x - .y)*100/.y, name="tornado_plot_fct")
  
  if ("replicate" %in% colnames(results)) {
    warning("Multiple replicates detected, median value will be used in tornado plot")
  }
  
  # Always compute change from baseline because arrange will always called on the absolute change
  results$CHANGE <- formula(results$VALUE, baseline)
  
  summary <- results %>%
    dplyr::group_by(dplyr::across(c("ITEM_NAME", "DIRECTION"))) %>%
    dplyr::summarise(MED_CHANGE=median(.data$CHANGE), MED_VALUE=median(.data$VALUE)) %>% # If several replicates
    dplyr::group_by(dplyr::across("ITEM_NAME")) %>%
    dplyr::mutate(MAX_MED_CHANGE=max(abs(.data$MED_CHANGE))) %>%
    dplyr::arrange(abs(.data$MAX_MED_CHANGE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DIRECTION=factor(.data$DIRECTION, levels=c("up", "down"), labels=c("Up", "Down")))
  
  # Customize item name if sensitivity analysis object
  if (isSensitivityAnalysis) {
    itemNames <- object@items %>% getNames()
    itemLabels <- object@items@list %>% purrr::map_chr(~.x %>% getLabel(object@labeled_parameters))
    order <- order(match(itemNames, unique(summary$ITEM_NAME)))
    itemNames <- itemNames[order]
    itemLabels <- itemLabels[order]
    summary <- summary %>%
      dplyr::mutate(ITEM_NAME=factor(.data$ITEM_NAME, levels=itemNames, labels=itemLabels))
  } else {
    summary <- summary %>%
      dplyr::mutate(ITEM_NAME=factor(.data$ITEM_NAME, levels=unique(.data$ITEM_NAME)))
  }
  
  target <- ifelse(relative, "MED_CHANGE", "MED_VALUE")
  
  # Label
  summary$LABEL <- paste0(signif(summary %>% dplyr::pull(target), digits=label_nsig))
  
  # Nudge y-axis
  summary$NUDGE_Y <- ifelse(summary$MED_CHANGE > 0, geom_text_nudge_y, -geom_text_nudge_y)
  
  shift_trans = function(d=0) {
    scales::trans_new("shift", transform=function(x) x - d, inverse=function(x) x + d)
  }
  
  plot <- ggplot2::ggplot(summary, ggplot2::aes_string(x="ITEM_NAME", y=target, fill="DIRECTION", label="LABEL")) +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(stat="identity", position="identity", width=geom_bar_width) +
    ggrepel::geom_text_repel(nudge_y=summary$NUDGE_Y, size=geom_text_size) +
    ggplot2::xlab(NULL) +
    ggplot2::labs(fill="Direction")
  
  if (show_ref) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 0,  baseline), color=geom_hline_color)
  }
  
  if (!relative) {
    plot <- plot 
  }
  if (relative) {
    plot <- plot +
      ggplot2::ylab(paste0("Change in ", output %>% getName(), " (%)"))
  } else {
    plot <- plot +
      ggplot2::scale_y_continuous(trans=shift_trans(baseline)) +
      ggplot2::ylab(paste0(output %>% getName()))
  }
  
  return(plot)
})