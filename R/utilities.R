#' Pre-process function argument.
#'
#' @param fun function or lambda formula
#' @param name function name
#' @return a function in any case
#' @importFrom assertthat assert_that
#' @importFrom plyr is.formula
#' @importFrom rlang as_function
#' @keywords internal
#' 
preprocessFunction <- function(fun, name) {
  if (is.null(fun)) {
    fun <- function(x){x}
    return(fun)
  } else {
    assertthat::assert_that(is.function(fun) || plyr::is.formula(fun),
                            msg=paste0(name, " must be a function or a lambda formula"))
    if (plyr::is.formula(fun)) {
      fun <- rlang::as_function(fun)
      # Class of fun is c("rlang_lambda_function","function")
      # However, not accepted as argument if method signature is "function"... Bug?
      # Workaround is to set a unique class
      class(fun) <- "function"
    }
    return(fun)
  }
}
