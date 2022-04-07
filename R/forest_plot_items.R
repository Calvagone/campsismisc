#_______________________________________________________________________________
#----                        forest_plot_items class                        ----
#_______________________________________________________________________________

#' Forest plot items class.
#' 
#' @export
setClass(
  "forest_plot_items",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="forest_plot_item")
)

#' Create a list of forest plot items
#' 
#' @return a list of forest plot items
#' @export
ForestPlotItems <- function() {
  return(new("forest_plot_items"))
}
