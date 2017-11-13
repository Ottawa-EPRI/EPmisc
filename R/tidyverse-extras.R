#' mutateX
#'
#' mutate which replaces X with the name of the assigned column.
#'
#' @param .data A tbl.
#'
#' @param ... Name-value pairs of expressions.
#'
#' @param .varX What to use as a substitute.
#'
#' @return An object of the same class as .data.
#'
#' @examples
#' data <- data.frame(some_var = 1:100)
#' mutateX(data, some_var = ifelse(X > 10, 10, X), noChange = some_var + 1)
#' # Or in vanilla mutate:
#' library(dplyr)
#' data <- data.frame(some_var = 1:100)
#' mutate(data, some_var = ifelse(some_var > 10, 10, some_var),
#'        noChange = some_var + 1)
#'
#' @export
#'

mutateX <- function(.data, ..., .varX = 'X') {
  modQuos <- rlang::quos(...)

  for (qs in seq_along(modQuos)) {
    modQuos[[qs]][[2]] <- dplyr:::expr_substitute(
      modQuos[[qs]][[2]], as.name(.varX), as.name(names(modQuos)[[qs]])
    )
  }

  dplyr::mutate(.data, !!!modQuos)
}

#' Drop unused levels
#'
#' Wrapper around forcats drop_levels which will not modify or drop
#' non-level attributes. Compared to `base::droplevels()` does not drop `NA`
#' levels that have values.
#'
#' @param f A factor
#' @param only A character vector restricting the set of levels to be dropped.
#'   If supplied, only levels that have no entries and appear in this vector
#'   will be removed.
#' @seealso \code{\link[forcats]{fct_drop}}
#' @examples
#' f <- factor(c("a", "b"), levels = c("a", "b", "c"))
#' f
#' fct_drop_levels(f)
#'
#' # Set only to restrict which levels to drop
#' fct_drop_levels(f, only = "a")
#' fct_drop_levels(f, only = "c")
#' @export
fct_drop_levels <- function(f, only) {
  attr(f, 'levels') <- attr(forcats::fct_drop(f, only), 'levels')
  f
}

#' Display filtered data frame.
#'
#' \code{vf} displays a data frame using the View function but allows conditions
#' on the data by using \code{dplyr::filter} and displays the conditions in the
#' windows title.
#'
#' @param df A data frame.
#' @param ... Conditions on the data frame. Comma-separated conditions are
#' joined by \code{&}
#' @return Invisible \code{NULL}. Result is displayed in a window.
#'
#' @examples
#' \dontrun{
#' vf(iris, Sepal.Width >= 4)
#' vf(iris, Sepal.Length > 6, Sepal.Width < 4)
#' }
#'
#' @export

vf <- function(df, ...) {
  args <- eval(substitute(alist(...)))
  View(dplyr::filter(df, ...),
       title = paste0(
         deparse(substitute(df)), ": ",
         paste(unlist(lapply(args, deparse)), collapse = ' & '))
       )
}

#' Display selected elements from a data frame.
#'
#' \code{vs} displays a data frame using the View function but allows selecting
#' variables from the data using \code{dplyr::select} and displays the
#' number of selected variables in the windows title.
#'
#' @param df A data frame.
#' @param ... Names the variables to be selected.
#'
#' @return Invisible \code{NULL}. Result is displayed in a window.
#'
#' @examples
#' \dontrun{
#' vs(iris, Sepal.Width)
#' vs(iris, Sepal.Length, Sepal.Width)
#' }
#'
#' @export

vs <- function(df, ...) {
  args <- eval(substitute(alist(...)))
  nargs <- length(unique(unlist(lapply(args, deparse))))
  if (nargs == 0) {
     View(df, title = paste0(deparse(substitute(df)), ": ", 'all selected'))
  } else {
    View(dplyr::select(df, ...),
         title = paste0(deparse(substitute(df)), ": ", nargs, ' selected'))
  }
}
