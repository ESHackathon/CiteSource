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
library(shinyalert)

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
                                                  fileInput("file", 
                                                            "Upload", 
                                                            multiple = FALSE, 
                                                            accept = c('.ris', '.txt', '.bib')),
                                                  textInput('source', 'Citesource', placeholder = 'e.g. Scopus'),
                                                  textInput('string', 'Citestring', placeholder = 'e.g. search string 1.3'),
                                                  textInput('label', 'Citelabel', placeholder = 'e.g. post Ti/Ab screen'),
                                                  actionButton('upload', 'Add file'),
                                                  actionButton('identify_dups', 'Identify duplicates')
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       
                                       # Output: Data file ----
                                       dataTableOutput("tbl_out"),
                                   
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
                                                           
                                                           # COME BACK TO IN V2?
                                                           # 'Select sources',
                                                           # uiOutput('checkbox1'),
                                                           # hr(),
                                                           # 'Select tags',
                                                           # uiOutput('checkbox2')),
                                              
                                              shinyWidgets::prettyRadioButtons(
                                                inputId = "comp_type",
                                                label = "Chose a comparison",
                                                inline = TRUE,
                                                choices = c("sources",
                                                            "labels", "strings"),
                                                status="primary")),
                                              
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
                            mainPanel(
                              dataTableOutput("exportTbl")
                            )
                          ))
                 )
                 
                 
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    
    
  )
  
  rv$df <- data.frame()
  rv$upload_df <- data.frame()
  rv$CiteSource<-data.frame()
  rv$unique<-data.frame()
  
  #### Upload files tab section ####
  #upload on click
  observeEvent(input$upload,{
    validate(need(input$file != "", "Select your bibliographic file to upload..."))
    
    if (is.null(input$file)) {
      return(NULL)
    } else {
      
      #upload files one-by-one
      path_list <- input$file$datapath
      rv$upload_number <- 0
      rv$upload_number <- rv$upload_number + 1
      upload_df <- CiteSource::read_citations(files=input$file$datapath, 
                                              cite_sources = input$source,
                                              cite_labels = input$label,
                                              cite_strings = input$strings)
      upload_length <- nrow(upload_df)
      
      #create a dataframe summarising inputs
      df <- data.frame('file' = input$file[1], 
                       'records' = upload_length,
                       'source' = input$source,
                       'label' = input$label,
                       'string' = input$string)
      
      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df) 
      
    }
  })
  
  
  # # display summary input table - summary of files added
  output$tbl_out <- renderDataTable({
    DT::datatable(rv$df, 
                  options = list(paging = FALSE, 
                                 searching = FALSE),
                  rownames = FALSE)
  })
  
  
  observeEvent(input$identify_dups,{
    
    dedup_results <- CiteSource::dedup_citations(rv$upload_df, merge_citations = TRUE)
    rv$unique <- dedup_results$unique
        
    n_citations <- nrow(rv$upload_df)
    n_unique <- nrow(rv$unique)
    n_duplicate <-n_citations - n_unique
    
        shinyalert("Deduplication complete", 
                   paste("From a total of", n_citations, "citations added, there are", n_unique, "unique citations. Compare citations across sources,
                   labels, and strings in the visualisation tab"), type = "success")
    
  })
  
  
  #### end section ####
  
  #### Visualise tab ####
  output$plotgraph1<-renderPlotly({
    n_unique <- count_unique(rv$unique)
    
    # for each unique citation, which sources/ strings/ labels are present
    source_comparison <- CiteSource::compare_sources(rv$unique, comp_type = input$comp_type)
    plot_source_overlap_heatmap(source_comparison)
  })
  
  plotInput <- reactive({
    source_comparison <- CiteSource::compare_sources(rv$unique, comp_type = input$comp_type)
    plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
    
  })
  
  output$plotgraph2<-renderPlot({
    print(plotInput())
    })
  
   output$downloadPlot <- downloadHandler(
     filename = function() { paste("upset", '.png', sep='') },
     content = function(file) {
       png(file)
       print(plotInput())
       dev.off()
        }
  )
   
  #### Export tab ####
   output$exportTbl<-DT::renderDataTable(
     DT::datatable(rv$unique,
                   extensions = 'Buttons',
                   options=list(
                     paging=TRUE,
                     searching=TRUE,
                     fixedColumns=TRUE,
                     autoWidth=TRUE,
                     ordering=TRUE,
                     dom = 'Bfrtip',
                     buttons=c("copy", "csv", "pdf", "excel"),
                     class="display"
                   ))
   )
  
}

# Create Shiny app ----
shinyApp(ui, server)