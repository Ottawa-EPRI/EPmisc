#' NA to factor level
#'
#' Change NA to a factor level in data frame factor variables.
#'
#' @param df Data frame to change the levels to.
#' @param missingName What to call the new NA level.
#' @param checkNA Add a level only if there are NA factors in a given factor.
#' @param except A regex of column names not to match.
#'
#' @return data.frame
#'
#' @examples
#' df <- data.frame(a = factor(c('a', 'b', NA)), b = factor(c('x', 'y', 'z')))
#'
#' levelNA(df)
#' levelNA(df, missingName = 'Not Available')
#' levelNA(df, checkNA = FALSE)
#' levelNA(df, except = '^(a|b)'
#'
#' @export
#'
levelNA <- function(df, missingName = 'Missing', checkNA = TRUE, except = NULL) {
  # Mandatory args.
  if (missing(df)) stop('The first argument (df) is missing')

  # Check appropriate types.
  if (!is.data.frame(df)) stop('The first argument (df) must be a data frame.')
  if (is.character(missingName) == FALSE || length(missingName) > 1)
    stop('missingName should be a character vector of length 1')
  if (is.logical(checkNA) == FALSE)
    stop('checkNA must be a logical (TRUE / FALSE)')
  if (
    is.null(except) == FALSE &&
    (is.character(except) == FALSE || length(except) > 1)
  )
    stop('except must be either NULL or a character vector of length 1')

  # Assign missing values.
  for (x in names(df)) {
    conds <- is.factor(df[[x]])
    if (checkNA) conds <- conds && anyNA(df[[x]])
    if (!is.null(except)) conds <- conds && !(grepl(except, x))

    if (conds) {
      df[[x]] <- factor(df[[x]], levels = c(levels(df[[x]]), missingName))
      df[[x]] <- replace(df[[x]], is.na(df[[x]]), missingName)
    }
  }
  df
}