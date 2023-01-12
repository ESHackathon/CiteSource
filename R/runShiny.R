#' A wrapper function to run Shiny Apps from \code{CiteSource}.
#' 
#' Running this function will launch the CiteSource shiny app
#' 
#' @return CiteSource shiny app
#' @param app Defaults to CiteSource - possibly other apps will be included in the future
#' @importFrom magrittr %>%
#' @export

runShiny <- function(app = "CiteSource"){
  
  # find and launch the app
  appDir <- system.file("shiny-app", app, package = "CiteSource")
  
  shiny::runApp(appDir, display.mode = "normal")
}