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
  are_constant_covariates <- args %>% purrr::map_lgl(~is(.x, "fixed_covariate"))
  assertthat::assert_that(all(are_constant_covariates), msg="Forest plot item can only accept fixed covariates")
  covariates <- new("covariates") %>% add(args)
  return(new("forest_plot_item", covariates=covariates))
}

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

setMethod("get_name", signature=c("forest_plot_item"), definition=function(x) {
  names <- x@covariates@list %>% purrr::map_chr(~paste0(.x@name, ":", .x@distribution@value))
  return(paste0(names, collapse="/"))
})

#_______________________________________________________________________________
#----                              get_label                                ----
#_______________________________________________________________________________

#' @rdname get_label
#' @param labeled_covariates labeled covariates object
setMethod("get_label", signature=c("forest_plot_item"), definition=function(object, labeled_covariates) {
  if (!is(labeled_covariates, "labeled_covariates")) {
    stop("Please provide all labeled covariates")
  }
  individual_labels <- object@covariates@list %>% purrr::map_chr(.f=function(covariate) {
    labeled_covariate <- labeled_covariates %>% find(covariate)
    if (is(labeled_covariate, "categorical_labeled_covariate")) {
      return(paste0(labeled_covariate@label, ": ",
                    names(which(labeled_covariate@categories==covariate@distribution@value))))
    } else {
      if (!is.na(labeled_covariate@unit) && labeled_covariate@unit != "") {
        return(paste0(labeled_covariate@label, ": ", covariate@distribution@value, " ", labeled_covariate@unit))
      } else {
        return(paste0(labeled_covariate@label, ": ", covariate@distribution@value))
      }
    }
  })
  
  return(paste0(individual_labels, collapse=" / "))
})
