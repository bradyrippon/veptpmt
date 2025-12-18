
#' Run the VEPT PMT Shiny App
#'
#' Launches the VEPT PMT quarterly report Shiny application shipped with the package.
#'
#' @param ... Passed to [shiny::runApp()] (e.g., `launch.browser = TRUE`).
#' @export
run_vept_app <- function(...) {
  app_dir <- system.file("app", package = "veptpmt")
  if (app_dir == "") {
    stop("Could not find app directory. Was the package installed correctly?", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
