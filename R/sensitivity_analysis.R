
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

#_______________________________________________________________________________
#----                             getTornadoPlot                            ----
#_______________________________________________________________________________

#' @rdname getTornadoPlot
setMethod("getTornadoPlot", signature=c("sensitivity_analysis", "logical", "logical", "logical"),
          definition=function(object, relative, show_labels, show_ref,
                              geom_bar_color="#94c0e3", geom_bar_width=0.5, geom_text_nudge_y=1, ...) {
  results <- object@results
  baseline <- object@baseline
  noOfScenarios <- object@items %>% length()
  formula <-  preprocessFunction(fun=~(.x - .y)*100/.y, name="tornado_plot_fct")
  
  if (results %>% nrow() > noOfScenarios) {
    warning("Multiple replicates detected, median value will be used in tornado plot")
  }
  
  # Always compute change from baseline because arrange will always called on the absolute change
  results$CHANGE <- formula(results$VALUE, baseline)

  summary <- results %>%
    dplyr::group_by(dplyr::across("SCENARIO")) %>%
    dplyr::summarise(MED_CHANGE=median(.data$CHANGE), MED_VALUE=median(.data$VALUE)) %>%
    dplyr::arrange(abs(.data$MED_CHANGE)) %>%
    dplyr::mutate(SCENARIO=factor(SCENARIO, levels=.data$SCENARIO %>% unique()))
  
  target <- ifelse(relative, "MED_CHANGE", "MED_VALUE")
  
  # Label
  summary$LABEL <- paste0(signif(summary %>% dplyr::pull(target), 3))
  
  shift_trans = function(d=0) {
    scales::trans_new("shift", transform=function(x) x - d, inverse = function(x) x + d)
  }
  
  plot <- ggplot2::ggplot(summary, ggplot2::aes_string(x="SCENARIO", y=target, label="LABEL")) +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(stat="identity", position="identity", width=geom_bar_width, color=geom_bar_color, fill=geom_bar_color) +
    ggplot2::geom_text(nudge_y=geom_text_nudge_y) +
    ggplot2::xlab(NULL)
  
  if (show_ref) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 0,  baseline), col="grey")
  }
  
  if (!relative) {
    plot <- plot 
  }
  if (relative) {
    plot <- plot +
      ggplot2::ylab(paste0("Change in ", object@output %>% getName(), " (%)"))
  } else {
    plot <- plot +
      ggplot2::scale_y_continuous(trans=shift_trans(baseline)) +
      ggplot2::ylab(paste0(object@output %>% getName()))
  }
    
  return(plot)
})
