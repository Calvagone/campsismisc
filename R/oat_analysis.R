
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
    results="data.frame",
    dest="character",
    formula="function" # 2 args: value, baseline
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
  formula <- object@formula
  seed <- 1
  
  # Compute baseline value
  base_scenario <- simulate(model=model %>% disable(c("IIV")), dataset=base_dataset, seed=seed)
  baseline <- base_scenario %>% computeBaseline(output=output)
  
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
  
  # Apply formula
  results$BASELINE <- baseline
  results$CHANGE <- formula(results$VALUE, results$BASELINE)
  object@results <- results
  return(object)
})

