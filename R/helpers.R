require_package <- function(package) {
   if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste(package, 'needed for this function to work. Please install it.'),
         call. = FALSE)
  }
}

#' key_value
#'
#' Return a list or vector with names becoming $key elements and value
#' elements becoming $value elements.
#'
#' @param lst A named list or vector.
#'
#' @return A list with name elements stored as $key and values stored as $value
#'         for each original element in the vector or list.
#'
#' @examples
#' vect <- list('A', 'B', 'C')
#' names(vect) <- c('X', 'Y', 'Z')
#' for (el in key_value(vect)) {
#'   print(paste(el$key, el$value))
#' }
#'
#' @export
#'

key_value <- function(lst) {
  if (!(is.vector(lst) || class(lst) == 'list')) {
    stop('Input to key_value must be either vector or list.')
  }
  if (is.null(names(lst)) || any(is.na(names(lst)))) {
    stop(
      paste(
        'All elements of a list or vector need to be named',
        'for key_value to work.'
      )
    )
  }
  kv <- Map(function(x, y) list(key = x, value = y), names(lst), lst)
  names(kv) <- NULL
  kv
}
