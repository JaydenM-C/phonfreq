#' Run Phoneme Frequencies Viewer
#'
#' A Shiny app for viewing distributions of phoneme frequencies in Australian languages
#'
#' @description This is a simple wrapper function for opening the phonfreq
#' viewer app using Shiny. The app produces plots of phoneme frequencies in
#' Australian languages, plots the fit of different distributions to these
#' frequencies, and plots the range of uncertainty for various parameter
#' estimates for each distribution.
#'
#' @export


phonfreq_viewer <- function () {
  appDir <- system.file("shiny-examples", "phonfreq_viewer", package = "phonfreq")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
