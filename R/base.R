#' droplevels
#'
#' The function dropevels is used to drop unused levels from a factor or, more
#' commonly, from factors in a data frame. Unlike the base function, droplevels
#' will preserve labels.
#'
#' @param x an object from which to drop unsued factor levels.
#'
#' @seealso \code{\link[base]{base::droplevels}}
#'
#' @export
droplevels <- function(x, ...) {
  if (is.factor(x)) {
    if (Hmisc::label(x) != '') {
      tmp <- label(x)
      x <- base::droplevels(x, ...)
      label(x) <- tmp
      return(x)
    }
  } else if (is.data.frame(x)) {
    tmp <- lapply(names(Hmisc::label(x)), function(y) label(x)[y])
    x <- base::droplevels(x, ...)
    label(x) <- tmp
    return(x)
  }
  base::droplevels(x, ...)
}
