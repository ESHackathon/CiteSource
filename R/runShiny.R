#' A wrapper function to run Shiny Apps from \code{CiteSource}.
#'
#' Running this function will launch the CiteSource shiny app
#'
#' @return CiteSource shiny app
#' @param app Defaults to CiteSource - possibly other apps will be included in the future
#' @param offer_install Should user be prompted to install required packages if they are missing?
#' @export
#' @examples 
#' if (interactive()) {
#'   # To run the CiteSource Shiny app:
#'   runShiny()
#' }
runShiny <- function(app = "CiteSource", offer_install = interactive()) {

  # Check that required packages are installed
  req_packages <- c("shiny", "shinyalert", "shinybusy", "shinyWidgets")
  if (!all(available <- purrr::map_lgl(req_packages, ~ requireNamespace(.x, quietly = TRUE)))) {
    if (offer_install == TRUE) { 
      message("Some packages required for the CiteSource shiny app are missing. The following need to be installed: ",
           glue::glue_collapse(req_packages[!available], sep = ", ", last = " and "))
      if (ui_yeah("Should these packages be installed?", n_no = 1) == TRUE) {
        utils::install.packages(req_packages[!available])
        runShiny(offer_install = FALSE)
      }
    } else {
      stop("Some packages required for the CiteSource shiny app are missing. Ensure you have all of the following installed: ",
           glue::glue_collapse(req_packages, sep = ", ", last = "and"))
      }
  }

  # find and launch the app
  appDir <- system.file("shiny-app", app, package = "CiteSource")

  shiny::runApp(appDir, display.mode = "normal")
}
