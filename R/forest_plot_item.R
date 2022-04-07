#_______________________________________________________________________________
#----                         forest_plot_item class                        ----
#_______________________________________________________________________________

#' Forest plot item: one or more labeled covariates.
#' 
#' @export
setClass(
  "forest_plot_item",
  representation(
    covariates="labeled_covariates"
  ),
  contains="pmx_element"
)

#' 
#' Create a forest plot item.
#' 
#' @param ... all labeled covariates
#' @return a forest plot item
#' @export
ForestPlotItem <- function(...) {
  args <- list(...)
  areLabeledCovariates <- args %>% purrr::map_lgl(~is(.x, "labeled_covariate"))
  assertthat::assert_that(all(areLabeledCovariates), msg="Forest plot item can only accept labeled covariates")
  covariates <- LabeledCovariates() %>% add(args)
  return(new("forest_plot_item", covariates=covariates))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("forest_plot_item"), definition=function(x) {
  return(x@covariates@list[[1]]@name)
})
