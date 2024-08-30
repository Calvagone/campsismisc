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
  areConstantCovariates <- args %>% purrr::map_lgl(~is(.x, "fixed_covariate"))
  assertthat::assert_that(all(areConstantCovariates), msg="Forest plot item can only accept fixed covariates")
  covariates <- new("covariates") %>% add(args)
  return(new("forest_plot_item", covariates=covariates))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("forest_plot_item"), definition=function(x) {
  names <- x@covariates@list %>% purrr::map_chr(~paste0(.x@name, ":", .x@distribution@value))
  return(paste0(names, collapse="/"))
})

#_______________________________________________________________________________
#----                              getLabel                                 ----
#_______________________________________________________________________________

#' @rdname getLabel
#' @param labeled_covariates labeled covariates object
setMethod("getLabel", signature=c("forest_plot_item"), definition=function(object, labeled_covariates) {
  if (!is(labeled_covariates, "labeled_covariates")) {
    stop("Please provide all labeled covariates")
  }
  individualLabels <- object@covariates@list %>% purrr::map_chr(.f=function(covariate) {
    labeledCovariate <- labeled_covariates %>% find(covariate)
    if (is(labeledCovariate, "categorical_labeled_covariate")) {
      return(paste0(labeledCovariate@label, ": ",
                    names(which(labeledCovariate@categories==covariate@distribution@value))))
    } else {
      if (!is.na(labeledCovariate@unit) && labeledCovariate@unit != "") {
        return(paste0(labeledCovariate@label, ": ", covariate@distribution@value, " ", labeledCovariate@unit))
      } else {
        return(paste0(labeledCovariate@label, ": ", covariate@distribution@value))
      }
    }
  })
  
  return(paste0(individualLabels, collapse=" / "))
})

#_______________________________________________________________________________
#----                           getCovariates                               ----
#_______________________________________________________________________________

setMethod("getCovariates", signature=c("forest_plot_item"), definition=function(object) {
  return(object@covariates)
})
