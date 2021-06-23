#' cbi
#'
#' @description Checks, builds, and installs the currently loaded project.
#'
#' @importFrom devtools check build install
#' @export
#'
cbi <- function() {
  devtools::check()
  devtools::build()
  devtools::install()
}
