
#_______________________________________________________________________________
#----                       sensitivity_analysis class                      ----
#_______________________________________________________________________________

#' Sensitivity analysis class.
#' 
#' @export
setClass(
  "sensitivity_analysis",
  representation(
    items="sensitivity_analysis_items",
    labeled_parameters="labeled_parameters"
  ),
  contains="oat_analysis",
  prototype=prototype(model=CampsisModel(), dataset=Dataset())
)

#' Create an OAT-based sensitivity analysis object.
#'
#' @param model CAMPSIS model
#' @param outputs outputs of interest, e.g. 'CL' or a PK metric from campsisnca or a list of them
#' @param dataset CAMPSIS dataset, if NULL, minimalist dataset with 1 subject 
#' and single observation at time 0 is created
#' @param replicates number of replicates, default is 1
#' @param seed simulation seed, default is 1
#' @param dest destination engine: rxode2, RxODE or mrgsolve (default)
#' @param settings simulation settings
#' @return an empty OAT-based sensibility analysis object
#' @export
SensitivityAnalysis <- function(model, outputs, dataset=NULL, replicates=1L, seed=1L, dest="mrgsolve", settings=Settings()) {
  if (is.null(dataset)) {
    dataset <- Dataset(1) %>%
      add(Observations(times=0))
  }
  if (is(outputs, "oat_analysis_output")) {
    # Add single output into multiple outputs object
    outputs <- OATOutputs() %>%
      add(outputs)
  }
  return(new("sensitivity_analysis", model=model, dataset=dataset, outputs=outputs,
             replicates=as.integer(replicates), seed=as.integer(seed), dest=dest, settings=settings))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________


setMethod("add", signature=c("sensitivity_analysis", "sensitivity_analysis_item"), definition=function(object, x) {
  object@items <- object@items %>% add(x)
  return(object)
})

setMethod("add", signature=c("sensitivity_analysis", "labeled_parameters"), definition=function(object, x) {
  object@labeled_parameters <- x
  return(object)
})

#_______________________________________________________________________________
#----                           createScenarios                             ----
#_______________________________________________________________________________

#' @rdname createScenarios
#' @param model Campsis model
setMethod("createScenarios", signature=c("sensitivity_analysis"), definition=function(object, model, ...) {
  # Note, IIV, VARCOV_OMEGA, VARCOV_SIGMA was already disabled in the given model, see oat_analysis.R, prepare()
  # Because simulations are not replicated, variance-covariance matrix is not used by default in CAMPSIS
  # If sensitivity analysis is replicated, then theta uncertainty is taken into account.
  scenarios <- Scenarios()
  items <- object@items
  
  for (item in items@list) {
    changes <-  item@changes
    modelUp <- model # First direction of tornado plot (direction up)
    modelDown <- model # Other direction of tornado plot (direction down)
    
    for (change in changes@list) {
      name <- change %>% getName()
      where <- change@where

      if (where == "equation") {
        equation <- model %>% find(Equation(name))
        if (is.null(equation)) {
          stop(paste0("Equation ", name, " cannot be found in model"))
        }
        equationUp <- equation
        equationDown <- equation
        if (change@up_down_as_factor) {
          equationUp@rhs <- paste0("(", equationUp@rhs, ") * ", change@up) # Multiplication
          equationDown@rhs <- paste0("(", equationDown@rhs, ") / ", change@down) # Division
        } else {
          # Use values as is
          equationUp@rhs <- change@up %>% as.character()
          equationDown@rhs <- change@down %>% as.character()
        }
        
        modelUp <- modelUp %>% campsismod::replace(equationUp)
        modelDown <- modelDown %>% campsismod::replace(equationDown)
        
      } else if (where == "theta") {
        theta <- model %>% find(Theta(name))
        if (is.null(theta)) {
          stop(paste0("Theta ", name, " cannot be found in model"))
        }
        thetaUp <- theta
        thetaDown <- theta
        if (change@up_down_as_factor) {
          if (change@log) {
            thetaUp@value <- log(exp(theta@value) * change@up) # Multiplication
            thetaDown@value <- log(exp(theta@value) / change@down) # Division
          } else {
            thetaUp@value <- theta@value * change@up # Multiplication
            thetaDown@value <- theta@value / change@down # Division
          }
        } else {
          # Use values as is
          if (change@log) {
            # Arbitrary but more convenient for the user to provide boundaries in the linear scale
            thetaUp@value <- log(change@up)
            thetaDown@value <- log(change@down)
          } else {
            thetaUp@value <- change@up
            thetaDown@value <- change@down
          }
        }
        
        modelUp <- modelUp %>% campsismod::replace(thetaUp)
        modelDown <- modelDown %>% campsismod::replace(thetaDown)
      } else {
        # Should not happen
        stop(paste0("Unknown where argument: ", where))
      }
    }
    scenarios <- scenarios %>%
      add(Scenario(name=paste0(item %>% getName(), ", up"), model=modelUp)) %>%
      add(Scenario(name=paste0(item %>% getName(), ", down"), model=modelDown))
  }
  return(scenarios)
})

#_______________________________________________________________________________
#----                             getTornadoPlot                            ----
#_______________________________________________________________________________

#' @rdname getTornadoPlot
#' @param geom_bar_width width argument of method geom_bar
#' @param geom_text_nudge_y nudge_y argument of method geom_text
#' @param label_nsig number of significant digits in label
#' @param geom_hline_color color argument of method geom_hline
#' @param geom_text_size size argument of method geom_text
setMethod("getTornadoPlot", signature=c("sensitivity_analysis", "integer", "logical", "logical", "logical", "numeric"),
          definition=function(object, index, relative, show_labels, show_ref, limits,
                              geom_bar_width=0.5, geom_text_nudge_y=1, label_nsig=3, geom_hline_color="grey", geom_text_size=3, ...) {
  isSensitivityAnalysis <- is(object, "sensitivity_analysis")
  iResults <- object@results@list[[index]]
  output <- iResults@output
  results <- iResults@results
  baseline <- iResults@baseline
  
  # Check limits argument
  if (any(is.na(limits))) {
    limits <- NULL
  }
  
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
  
  # Nudge y-axis
  summary$NUDGE_Y <- ifelse(summary$MED_CHANGE > 0, geom_text_nudge_y, -geom_text_nudge_y)
  
  shift_trans = function(d=0) {
    scales::trans_new("shift", transform=function(x) x - d, inverse=function(x) x + d)
  }
  
  # Relative or absolute
  summary <- summary %>%
    dplyr::mutate(TORNADO_VALUE=if(relative) {.data$MED_CHANGE} else {.data$MED_VALUE})
  
  # Label
  summary$LABEL <- paste0(signif(summary %>% dplyr::pull(TORNADO_VALUE), digits=label_nsig))
  
  plot <- ggplot2::ggplot(data=summary, mapping=ggplot2::aes(x=ITEM_NAME, y=TORNADO_VALUE, fill=DIRECTION, label=LABEL)) +
    ggplot2::coord_flip(ylim=limits) +
    ggplot2::geom_bar(stat="identity", position="identity", width=geom_bar_width)
  
  if (show_labels) {
    plot <- plot +
      ggrepel::geom_text_repel(nudge_y=summary$NUDGE_Y, size=geom_text_size)
  }
  
  plot <- plot +
    ggplot2::xlab(NULL) +
    ggplot2::labs(fill="Direction")
  
  if (show_ref) {
    plot <- plot + ggplot2::geom_hline(yintercept=ifelse(relative, 0,  baseline), color=geom_hline_color)
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
