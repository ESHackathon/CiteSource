#' A wrapper function to run Shiny Apps from \code{CiteSource}.
#' 
#' Running this function will launch the CiteSource shiny
#' @return CiteSource shiny app
#' @param app CiteSource 
#' @import shiny
#' @import synthesisr
#' @import dplyr
#' @import shinyWidgets
#' @import htmltools
#' @import markdown
#' @import CiteSource
#' @import plotly
#' @import shinyalert
#' @export

runShiny <- function(app="CiteSource"){
  
  # find and launch the app
  appDir <- system.file("shiny-examples", app, package = "CiteSource")
  
  shiny::runApp(appDir, display.mode = "normal")
}