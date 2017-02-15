# Helper function which drops levels while preserving attributes.
.droplevels <- function(x, ...) {
  oldAttrs <- attributes(x)
  x <- base::droplevels(x, ...)
  newLevels <- attr(x, 'levels')
  attributes(x) <- oldAttrs
  attr(x, 'levels') <- newLevels
  x
}

#' droplevels
#'
#' The function dropevels is used to drop unused levels from a factor or, more
#' commonly, from factors in a data frame. Unlike the base function, droplevels
#' will preserve labels.
#'
#' @param x an object from which to drop unsued factor levels.
#'
#' @seealso \code{\link[base]{droplevels}}
#'
#' @export
#'
droplevels <- function(x, ...) UseMethod('droplevelsAttr')

#' @export
droplevelsAttr.default <- function(x, ...) base::droplevels(x, ...)

#' @export
droplevelsAttr.labelled <- function(x, ...) .droplevels(x, ...)

#' @export
droplevelsAttr.data.frame <- function(x, except = NULL, ...) {
  ix <- vapply(x, is.factor, NA)
  if (!is.null(except))
    ix[except] <- FALSE
  x[ix] <- lapply(x[ix], droplevels)
  x
}

#' +
#'
#' Same as the base '+' arithmetic operator, with the exception that it
#' overloads it, allowing concatenation of character vectors. Note that there is
#' no type coercion. The function will throw an error if character is mixed with
#' other types.
#'
#' @param x The first vector to call the '+' operator on.
#' @param y The second vector to call the '+' operator on.
#'
#' @seealso \code{\link[base]{Arithmetic}}
#'
#' @export
`+` <- function(x, ...) UseMethod('sumOperatorOver')

#' @export
sumOperatorOver.default <- function(x, y) .Primitive('+')(x, y)

#' @export
sumOperatorOver.character <- function(x, y) {
  if (!is.character(y))
    stop('Non-character argument to paste operator')
  if (length(x) == length(y))
    paste(x, y, sep = '')
  else
    stop('Character vectors are different lengths.')
}

#' ifelse
#'
#' ifelse returns a value with the same shape as test which is filled with
#' elements selected from either yes or no depending on whether the element of
#' test i or FALSE. Unlike the base function, ifelse optionally allows
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
ifelse <- function(test, yes, no, attributes = 'default') {
  if (attributes == 'default') {
    base::ifelse(test, yes, no)
  } else {
    attrs <- switch(EXPR = attributes, 'test' = test, 'yes'  = yes, 'no' = no)
    keepClass <- class(attrs)
    keepLevels <- levels(attrs)
    keepClassIsFactor <- 'factor' %in% keepClass

    # First call normal ifelse.
    returned <- base::ifelse(test, yes, no)

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
