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
#' mutateX(data, some_var = ifelse(X > 10, 10, X), noChange = some_var + 1)
#' # Or in vanilla mutate:
#' mutate(data, some_var = ifelse(some_var > 10, 10, some_var),
#'        noChange = some_var + 1)
#'
#' @export
#'

mutateX <- function(.data, ..., .varX = 'X') {
  require_package('dplyr')

  modQuos <- rlang::quos(...)

  for (qs in seq_along(modQuos)) {
    modQuos[[qs]][[2]] <- dplyr:::expr_substitute(
      modQuos[[qs]][[2]], as.name(.varX), as.name(names(modQuos)[[qs]])
    )
  }

  dplyr::mutate(.data, !!!modQuos)
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
#' vf(iris, Sepal.Width >= 4)
#' vf(iris, Sepal.Length > 6, Sepal.Width < 4)
#' @export

vf <- function(df, ...){
  require_package('dplyr')

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
#' vs(iris, Sepal.Width)
#' vs(iris, Sepal.Length, Sepal.Width)
#' @export

vs <- function(df, ...) {
  require_package('dplyr')

  args <- eval(substitute(alist(...)))
  nargs <- length(unique(unlist(lapply(args, deparse))))
  if (nargs == 0) {
     View(df, title = paste0(deparse(substitute(df)), ": ", 'all selected'))
  } else {
    View(dplyr::select(df, ...),
         title = paste0(deparse(substitute(df)), ": ", nargs, ' selected'))
  }
}
