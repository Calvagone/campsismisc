#_______________________________________________________________________________
#----                         forest_plot_item class                        ----
#_______________________________________________________________________________

#' Forest plot item: one or more labeled covariates.
#' 
#' @export
setClass(
  "sensitivity_analysis_item",
  representation(
    factors="sensitivity_analysis_factors"
  ),
  contains="pmx_element"
)

#' 
#' Create a sensitivity analysis item.
#' 
#' @param ... all factors
#' @return a sensitivity analysis item
#' @export
SensitivityAnalysisItem <- function(...) {
  args <- list(...)
  areFactors <- args %>% purrr::map_lgl(~is(.x, "sensitivity_analysis_factor"))
  assertthat::assert_that(all(areFactors), msg="Sensitivity analysis item can only accept one or multiple factors")
  factors <- SensitivityAnalysisFactors() %>% add(args)
  return(new("sensitivity_analysis_item", factors=factors))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("sensitivity_analysis_item"), definition=function(x) {
  names <- x@factors@list %>% purrr::map_chr(~.x@parameter)
  return(paste0(names, collapse="/"))
})
