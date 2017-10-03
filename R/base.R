# Helper function which drops levels while preserving attributes.
.droplevels <- function(x, ...) {
  oldAttrs <- attributes(x)
  x <- droplevels(x, ...)
  newLevels <- attr(x, 'levels')
  attributes(x) <- oldAttrs
  attr(x, 'levels') <- newLevels
  x
}

#' drop_levels
#'
#' The function drop_levels is used to drop unused levels from a factor or, more
#' commonly, from factors in a data frame. Unlike the base function, drop_levels
#' will preserve labels.
#'
#' @param x an object from which to drop unsued factor levels.
#'
#' @param ... further arguments passed to base droplevels.
#'
#' @seealso \code{\link[base]{droplevels}}
#'
#' @export
#'
drop_levels <- function(x, ...) UseMethod('drop_levels')

drop_levels.default <- function(x, ...) droplevels(x, ...)
drop_levels.labelled <- function(x, ...) .droplevels(x, ...)
drop_levels.data.frame <- function(x, except = NULL, ...) {
  ix <- vapply(x, is.factor, NA)
  if (!is.null(except))
    ix[except] <- FALSE
  x[ix] <- lapply(x[ix], drop_levels)
  x
}

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

#' ifel
#'
#' ifel returns a value with the same shape as test which is filled with
#' elements selected from either yes or no depending on whether the element of
#' test is TRUE or FALSE. Unlike the base function, ifel optionally allows
#' preservation of attributes, either from the yes or no parameters.
#'
#' @param test an object which can be coerced to logical mode.
#' @param yes return values for true elements of test.
#' @param no return values for false elements of test.
#' @param attributes Whether to optionally preserve attributes, specifically
#'        levels and class. Preserved attributes can be "default", which mimics
#'        the default base ifelse behaviour (and is the default), or to preserve
#'        the attributes of either "test", "yes" or "no".
#'
#' @seealso \code{\link[base]{ifelse}}
#'
#' @export
ifel <- function(test, yes, no, attributes = 'default') {
  if (attributes == 'default') {
    ifelse(test, yes, no)
  } else {
    attrs <- switch(EXPR = attributes, 'test' = test, 'yes'  = yes, 'no' = no)
    keepClass <- class(attrs)
    keepLevels <- levels(attrs)
    keepClassIsFactor <- 'factor' %in% keepClass

    # First call normal ifelse.
    returned <- ifelse(test, yes, no)

    # Now keep class of the chosen parameter, and factor levels, if applicable.
    if (keepClassIsFactor) {
      returned <- as.factor(returned)
      levels(returned) <- keepLevels

      if (length(keepClass) > 1)
        class(returned) <- keepClass
    } else {
      class(returned) <- keepClass
    }

    returned
  }
}
