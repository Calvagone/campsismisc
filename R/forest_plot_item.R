#_______________________________________________________________________________
#----                         forest_plot_item class                        ----
#_______________________________________________________________________________

#' Forest plot item: one or more labeled covariates.
#' 
#' @export
setClass(
  "forest_plot_item",
  representation(
    covariates="covariates"
  ),
  contains="pmx_element"
)

#' 
#' Create a forest plot item.
#' 
#' @param ... all constant covariates
#' @return a forest plot item
#' @export
ForestPlotItem <- function(...) {
  args <- list(...)
  areConstantCovariates <- args %>% purrr::map_lgl(~is(.x, "constant_covariate"))
  assertthat::assert_that(all(areConstantCovariates), msg="Forest plot item can only accept constant covariates")
  covariates <- new("covariates") %>% add(args)
  return(new("forest_plot_item", covariates=covariates))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("forest_plot_item"), definition=function(x) {
  names <- x@covariates %>% purrr::map_chr(~paste0(.x@name, ":", .x@distribution@value))
  return(paste0(names, collapse="/"))
})

#_______________________________________________________________________________
#----                           getCovariates                               ----
#_______________________________________________________________________________

setMethod("getCovariates", signature=c("forest_plot_item"), definition=function(object) {
  return(x@covariates)
})
