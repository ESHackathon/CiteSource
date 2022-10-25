library(shiny)
library(synthesisr)
library(dplyr)
library(tidyr)
library(DT)
library(shinyWidgets)
library(htmltools)
library(markdown)
library(CiteSource)
library(plotly)
options(shiny.maxRequestSize=30*1024^2)
# Set background colour
tags$head(tags$style(
  HTML('
                     #sidebar {
                        background-color: #ffffff;
                    }
            
                    body, label, input, button, select { 
                      font-family: "Arial";
                    }')))


# Define UI for data upload app ----
ui <- navbarPage("CiteSource", id = "tabs",
                 
                 # Home tab
                 tabPanel('Home',
                          navlistPanel(
                            tabPanel(title = 'About',
                                     htmltools::includeMarkdown('www/about.md')
                            ),
                            tabPanel(title = 'Use Cases',
                                     column(9,
                                            'instructions text'),
                                     column(1,
                                            tags$img(height=150,src='https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png'))
                            )
                          )
                 ),
                 
                 # File upload tab
                 tabPanel("File upload & Deduplicate",
                          fluidRow(
                            column(12,
                                   # Sidebar layout with input and output definitions ----
                                   sidebarLayout(
                                       sidebarPanel(# Input: Select a file ----
                                                    fileInput("files", 
                                                              "Upload", 
                                                              multiple = TRUE, 
                                                              accept = c('.ris', '.txt', '.bib')),
                                                    textInput('source', 'Citesource', placeholder = 'e.g. Scopus'),
                                                    textInput('tag', 'Citestring', placeholder = 'e.g. search string 1.3'),
                                                    textInput('label', 'Citelabel', placeholder = 'e.g. post Ti/Ab screen'),
                                                    actionButton('upload', 'Upload file')
                                       ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       # Output: Data file ----
                                       dataTableOutput("tbl_out")
                                       
                                     )
                                   )
                            )
                          )
                 ),
                 
                 # Visualise tab
                 tabPanel("Visualise",
                          fluidRow(
                            column(12,
                                   fluidRow(
                                     column(12,
                                            # Sidebar layout with input and output definitions ----
                                            sidebarLayout(
                                              
                                              # Sidebar panel for inputs ----
                                              sidebarPanel(id="sidebar",
                                                           'Select sources',
                                                           uiOutput('checkbox1'),
                                                           hr(),
                                                           'Select tags',
                                                           uiOutput('checkbox2')),
                                              
                                              mainPanel(
                                                tabsetPanel(
                                                  tabPanel("Plot overlap as a heatmap matrix", plotly::plotlyOutput("plotgraph1")),
                                                  tabPanel("Plot overlap as an upset plot", downloadButton("downloadPlot"),
                                                           plotOutput("plotgraph2"))
                                                  ))))
                            )))),
                 
                 
                 # Export tab
                 tabPanel("Export",
                          fluidRow(
                            column(12,
                                   'Export your data in various formats here.')
                          )
                 )
                 
                 
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    
    
  )
  
  rv$df <- data.frame()
  rv$upload_df <- data.frame()
  rv$CiteSource<-data.frame()
  
  #### Upload files tab section ####
  #upload on click
  observeEvent(input$upload,{
    validate(need(input$files != "", "Select your bibliographic file to upload..."))
    
    if (is.null(input$files)) {
      return(NULL)
    } else {
      files=input$files$datapath
      cite_sources=paste0("c(",input$source, ")")
      cite_sources=eval(parse(text=cite_sources))
      rv$upload_df <- CiteSource::read_citations(files=files, 
                                              cite_sources = cite_sources)

    }
    
 
  # # display summary input table
   output$tbl_out <- renderDataTable({
     DT::datatable(rv$upload_df, 
                   options = list(paging = FALSE, 
                                  searching = FALSE),
                   rownames = FALSE)
   })
 
  
  dedup_results<-CiteSource::dedup_citations(rv$upload_df, merge_citations = TRUE)
    unique_citations <- dedup_results$unique
    
    output$dedup_results_table<-renderDataTable({
      DT::datatable(unique_citations,
                    options = list(paging = FALSE,
                                   searching = FALSE),
                    rownames = FALSE)


  
    })
  })

  # Add data for testing 
  dedup_results <- readRDS("www/dedup.RDS") #TODO remove when the upload and dedup is finished
  
  #### end section ####
  
  #### Visualise tab ####
  output$plotgraph1<-renderPlotly({
    unique_citations <- dedup_results$unique
  n_unique <- count_unique(unique_citations)
  # for each unique citation, which sources are present
  source_comparison <- compare_sources(unique_citations, comp_type = "sources")
  label_comparison <- compare_sources(unique_citations, comp_type = "labels")
  n_unique$cite_label <- factor(n_unique$cite_label, levels=c('search','screen','final'))
  plot_source_overlap_heatmap(source_comparison)
  })

output$plotgraph2<-renderPlot({
  plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
})


output$plotgraph2 <- renderPlot({
  myplot <- plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
  myplot
})

plotInput = function() {
  
  ##################################################
  ##################################################
  #This doesn't work - upset plots not supported
  unique_citations <- dedup_results$unique
  n_unique <- count_unique(unique_citations)
    source_comparison <- compare_sources(unique_citations, comp_type = "sources")
  plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
  ##################################################
  ##################################################
  
}

output$downloadPlot <- downloadHandler(
  filename = function() { paste("test", '.svg', sep='') },
  content = function(file) {
    ggsave(file, plot = plotInput(), device = "svg")
  }
)


}

# Create Shiny app ----
shinyApp(ui, server)