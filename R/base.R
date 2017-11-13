#' \%+\%
#'
#' Allows concatenation of character vectors. A short form of basic paste0
#' functionality.
#'
#' @param x The first vector to call the operator on.
#' @param y The second vector to call the operator on.
#'
#' @seealso \code{\link[base]{Arithmetic}}
#'
#' @export
'%+%' <- function(x, y) {
  paste0(x, y)
}
