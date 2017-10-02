require_package <- function(package) {
   if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste(package, 'needed for this function to work. Please install it.'),
         call. = FALSE)
  }
}
