
#_______________________________________________________________________________
#----                           oat_analysis class                          ----
#_______________________________________________________________________________

#' One-at-a-time analysis structure.
#' 
#' @importFrom campsis Dataset
#' @export
setClass(
  "oat_analysis",
  representation(
    model="campsis_model",
    dataset="dataset",
    outputs="oat_analysis_outputs",
    replicates="integer",
    seed="integer",
    dest="character",
    settings="simulation_settings",
    results="oat_analysis_results"
  ),
  prototype=prototype(model=CampsisModel(), dataset=campsis::Dataset(), seed=1L)
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
  outputName <- output %>% get_name()
  baseline <- object %>% dplyr::pull(outputName) %>% unique()
  assertthat::assert_that(length(baseline)==1,
                          msg=paste0("Baseline should be unique for parameter ", outputName))
  return(baseline)
})

#' @importFrom campsismod add export
#' @importFrom campsisnca calculate NCAAnalysis NCATable
#' @rdname computeBaseline
setMethod("computeBaseline", signature=c("tbl_df", "nca_metric_output"), definition=function(object, output) {
  nca <- campsisnca::NCAAnalysis() %>%
    add(output@metric)
  table <- campsisnca::NCATable() %>%
    add(nca)
  baseline <- table %>%
    campsisnca::calculate(x=object) %>%
    campsismod::export(dest="dataframe", type="individual")
  print("BASELINE OK")
  return(baseline %>% dplyr::pull(value))
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
  outputName <- output %>% get_name()
  results <- object %>%
    dplyr::filter(.data$metric==outputName) %>%
    dplyr::select(-dplyr::all_of(c("discrete_value", "metric"))) %>% # TODO
    dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev())) %>%
    dplyr::relocate(dplyr::any_of(c("replicate", "id", "value", "SCENARIO")))
  return(results)
})

#_______________________________________________________________________________
#----                               prepare                                 ----
#_______________________________________________________________________________


#' @rdname prepare
#' @importFrom campsis Outfun Outfuns NCATableOutfun simulate
#' @importFrom campsisnca NCAAnalysis NCATable
#' @importFrom progressr with_progress without_progress
setMethod("prepare", signature=c("oat_analysis"), definition=function(object) {
  # Fix IIV, VARCOV_OMEGA, VARCOV_SIGMA at the beginning!
  # This way, calling disable is not needed in the scenarios
  model <- object@model %>% disable(c("IIV", "VARCOV_OMEGA", "VARCOV_SIGMA"))
  base_dataset <- object@dataset
  if (is(object, "forest_plot")) {
    base_dataset <- base_dataset %>%
      add(object@labeled_covariates@list)
  }
  items <- object@items
  outputs <- object@outputs
  replicates <- object@replicates
  dest <- object@dest
  seed <- object@seed
  
  # Getting all necessary 'outvars' across all outputs
  # This in order to make 1 call to the simulate method
  outvars <- outputs@list %>% purrr::map_chr(~getOutvars(.x)) %>% unique()
  
  # Compute and store baseline value of each output
  # Progress from campsis is deactivated for the base scenario
  base_scenario <- progressr::without_progress(
    simulate(model=model, dataset=base_dataset, outvars=outvars, seed=seed, dest=dest)
  )
  baselines <- base_scenario %>% computeBaseline(output=outputs)
  
  # Generate scenarios
  scenarios <- object %>% createScenarios(dataset=base_dataset, model=model)

  # Prepare outfun function

  nca <- campsisnca::NCAAnalysis()
  for (output in outputs@list) {
    nca <- nca %>%
      add(output@metric)
  }
  outfun <- campsis::NCATableOutfun(table = campsisnca::NCATable() %>% add(nca), export_type = "individual")
  outfuns <- campsis::Outfuns() %>%
    add(outfun)

  # Simulate
  all_results <- campsis::simulate(model=model, dataset=base_dataset, scenarios=scenarios, replicates=replicates,
                      seed=seed, dest=dest, outvars=outvars, outfun=outfuns, settings=object@settings)

  # Post-processing
  tmp_results <- purrr::map2(.x=outputs@list, .y=baselines, .f=function(output, baseline) {
    res <- OATResult(output=output, baseline=baseline,
                     results=postProcessScenarios(object=all_results, output=output))
    return(res)
  })
  
  # Save
  object@results <- OATResults() %>%
    add(tmp_results)

  return(object)
})
