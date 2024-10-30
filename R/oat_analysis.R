
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
    seed="integer",
    dest="character",
    settings="simulation_settings",
    results="oat_analysis_results"
  ),
  prototype=prototype(model=CampsisModel(), dataset=Dataset(), seed=1L)
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
    tidyr::unnest(cols=dplyr::all_of(outputName)) %>%
    dplyr::mutate(SCENARIO=factor(SCENARIO, levels=unique(SCENARIO) %>% rev())) %>%
    dplyr::relocate(dplyr::any_of(c("replicate", "id", "VALUE", "SCENARIO")))
  return(results)
})

#_______________________________________________________________________________
#----                               prepare                                 ----
#_______________________________________________________________________________

#' Outfun NCA
#' 
#' @param metric NCA metric
#' @param x tibble
#' @return tibble
#' @importFrom campsisnca calculate
#' @importFrom dplyr rename_at
#' @export
outfunNCA <- function(metric, x) {
  metric@x <- x
  metric <- metric %>% campsisnca::calculate()
  return(metric@individual %>% dplyr::rename_at(.vars="value", .funs=~"VALUE"))
}

#' Campsismisc output function.
#' 
#' @param x tibble
#' @param outputs oat_analysis_outputs
#' @return tibble
#' @importFrom purrr map_dfc
#' @importFrom dplyr distinct rename
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
campsismiscOutfun <- function(x, outputs) {
  retValue <- outputs@list %>% purrr::map_dfc(.f=function(output) {
    outputName <- output %>% getName()
    if (is(output, "nca_metric_output")) {
      myFun <- function(x) {campsismisc::outfunNCA(metric=output@metric, x=output@filter(x))}
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

#' @rdname prepare
#' @importFrom progressr with_progress without_progress
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
  outfun <- Outfun(fun=campsismiscOutfun, args=list(outputs=outputs), level="scenario")

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
