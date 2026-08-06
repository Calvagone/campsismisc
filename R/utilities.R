#' Pre-process function argument.
#'
#' @param fun function or lambda formula
#' @param name function name
#' @return a function in any case
#' @importFrom assertthat assert_that
#' @importFrom rlang as_function
#' @keywords internal
#'
preprocess_function <- function(fun, name) {
  if (is.null(fun)) {
    return(identity)
  }

  is_formula <- inherits(fun, "formula")

  assertthat::assert_that(
    is.function(fun) || is_formula,
    msg = paste0(name, " must be a function or a lambda formula")
  )

  if (is_formula) {
    fun <- rlang::as_function(fun)
    # Class of fun is c("rlang_lambda_function","function")
    # Workaround for S4 dispatch issues expecting strictly "function"
    class(fun) <- "function"
  }

  return(fun)
}
