#' load_remote_zip
#'
#' Load a remote zip file as a library. Nothing is permanently installed.
#' While technically this _should_ work for any package, its use is in
#' practice reserved for packages small enough to be very quickly loaded.
#'
#' @param remote_zip A url to a web address containing a zipped R package.
#'
#' @examples
#' \dontrun{
#' load_remote_zip('/path/to/some_zipped_package.zip')
#' }
#'
#' @export
#'

load_remote_zip <- function(remote_zip) {
  require_package('devtools')

  op <- options(warn = 2)
  on.exit(options(op))

  dir.create(tempdir(), showWarnings = FALSE)
  tmp_file <- tempfile(fileext = '.zip')
  tmp_dir <- tempfile()

  file.create(tmp_file)
  download.file(remote_zip, tmp_file)
  dir.create(tmp_dir)
  unzip(tmp_file, exdir = tmp_dir)
  lib <- list.files(tmp_dir, full.names = TRUE)
  devtools::load_all(lib)

  unlink(tmp_file)
  unlink(tmp_dir, recursive = TRUE)
}
