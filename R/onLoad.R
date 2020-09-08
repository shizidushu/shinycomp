#' Adds static resources to shiny
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("shinycomp", system.file(package = "shinycomp"))
}
