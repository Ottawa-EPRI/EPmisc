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
  modQuos <- rlang::quos(...)
  browser()

  for (qs in seq_along(modQuos)) {
    modQuos[[qs]][[2]] <- dplyr:::expr_substitute(
      modQuos[[qs]][[2]], as.name(.varX), as.name(names(modQuos)[[qs]])
    )
  }

  dplyr::mutate(.data, !!!modQuos)
}
