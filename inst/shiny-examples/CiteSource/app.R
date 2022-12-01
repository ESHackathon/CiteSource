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
                 tabPanel("File upload",
                          fluidRow(
                            column(12,
                                   # Sidebar layout with input and output definitions ----
                                   sidebarLayout(
                                     sidebarPanel(# Input: Select a file ----
                                                  h4("Step 1: Upload your citation files"),
                                                  fileInput("file",  "",
                                                            multiple = TRUE, 
                                                            accept = c('.ris', '.txt', '.bib')),
                                                  # textInput('source', 'Citesource', placeholder = 'e.g. Scopus'),
                                                  # textInput('string', 'Citestring', placeholder = 'e.g. search string 1.3'),
                                                  # textInput('label', 'Citelabel', placeholder = 'e.g. post Ti/Ab screen'),
                                                  # actionBttn(
                                                  #   'upload', 'Add citation file',
                                                  #   style = "pill",
                                                  #   color = "primary",
                                                  #   icon = icon("plus")
                                                  # )
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       
                                       h4("Step 2: Double click the row to edit sources, labels, and strings"),
                                       # Output: Data file ----
                                       dataTableOutput("tbl_out"),
                                   
                                     )
                                   )
                            )
                          )
                 ),
                 
                 tabPanel("Deduplicate",
                          
                            # Action button: identify duplicates in uploaded datset
                            actionBttn(
                              'identify_dups', 'Identify duplicate citations',
                              style = "pill",
                              color = "primary",
                              icon = icon("search")
                            ),               
                                     
                          # Output: datatable of deduplication results
                          dataTableOutput("dedup_results")
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
                              #,
                              #downloadButton("downloadData", "Download bibtex")
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
  observeEvent(input$file,{
    validate(need(input$file != "", "Select your bibliographic file to upload..."))
    
    if (is.null(input$file)) {
      return(NULL)
    } else {
      
      #upload files one-by-one
      path_list <- input$file$datapath
      rv$upload_number <- 0
      rv$upload_number <- rv$upload_number + 1
      suggested_source <- stringr::str_replace_all(input$file$name, ".ris", "")
      suggested_source <- stringr::str_replace_all(suggested_source, ".bib", "")
      suggested_source <- stringr::str_replace_all(suggested_source, ".txt", "")
      upload_df <- CiteSource::read_citations(files=input$file$datapath, 
                                              cite_sources = suggested_source,
                                              cite_labels = rep("", length(input$file$datapath)),
                                              cite_strings =rep("", length(input$file$datapath)))
      upload_length <- upload_df %>%
        group_by(cite_source) %>%
        count(name="records") %>%
        rename(source = cite_source)
      
      #create a dataframe summarising inputs
      df <- data.frame('file' = input$file, 
                       'source' = suggested_source,
                       'label' = rep("", length(input$file$datapath)),
                       'string' = rep("", length(input$file$datapath)))
      
      df <- left_join(upload_length, df, by="source") %>%
        select(file.name,records, source, label, string)
      
      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df) 
      
    }
  })
  
  
  # # display summary input table - summary of files added
  output$tbl_out <- renderDataTable({
    DT::datatable(rv$df, 
                  editable = TRUE,
                  options = list(paging = FALSE, 
                                 searching = FALSE),
                  rownames = FALSE)
  })
  
  
  ### Deduplication tab ####
  
  # when dedup button clicked, deduplicate
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
  
  # display results of deduplication
  dedup_results <- renderDataTable(
    datatable(rv$unique)
  )
  
  
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
   # # Downloadable bibtex ----
   # output$downloadData <- downloadHandler(
   #   filename = function() {
   #     paste("bib", ".bib", sep = "")
   #   },
   #   content = function(file) {
   #     rv$unique %>% 
   #       select(!c(duplicate_id, cite_source, cite_string, cite_label,
   #                 record_id, record_ids)) %>% 
   #       bib2df::df2bib(., file="bib.bib") 
   #     
   #   }
   # )
   
  
}

# Create Shiny app ----
shinyApp(ui, server)