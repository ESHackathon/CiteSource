
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
ui <- shiny::navbarPage("CiteSource", id = "tabs",
                 
                 # Home tab
                 shiny::tabPanel('Home',
                                 shiny::navlistPanel(
                                   shiny::tabPanel(title = 'About',
                                     htmltools::includeMarkdown('www/about.md')
                            ),
                            shiny::tabPanel(title = 'Use Cases',
                                     column(9,
                                            'instructions text'),
                                     column(1,
                                            tags$img(height=150,src='https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png'))
                            )
                          )
                 ),
                 
                 # File upload tab
                 shiny::tabPanel("File upload",
                                 shiny::fluidRow(
                            column(12,
                                   # Sidebar layout with input and output definitions ----
                                   shiny::sidebarLayout(
                                     shiny::sidebarPanel(# Input: Select a file ----
                                                  h4("Step 1: Upload your citation files"),
                                                  shiny::fileInput("file",  "",
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
                                     shiny::mainPanel(
                                       
                                       h4("Step 2: Double click the row to edit sources, labels, and strings"),
                                       # Output: Data file ----
                                       DT::dataTableOutput("tbl_out"),
                                   
                                     )
                                   )
                            )
                          )
                 ),
                 
                 shiny::tabPanel("Deduplicate",
                          
                            # Action button: identify duplicates in uploaded datset
                            shinyWidgets::actionBttn(
                              'identify_dups', 'Identify duplicate citations',
                              style = "pill",
                              color = "primary",
                              icon = icon("search")
                            ),               
                                     
                          # Output: datatable of deduplication results
                          DT::dataTableOutput("dedup_results")
                ),
                 
                 # Visualise tab
                shiny::tabPanel("Visualise",
                                shiny::fluidRow(
                            column(12,
                                   shiny::fluidRow(
                                     column(12,
                                            # Sidebar layout with input and output definitions ----
                                            shiny::sidebarLayout(
                                              
                                              # Sidebar panel for inputs ----
                                              shiny::sidebarPanel(id="sidebar",
                                                           
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
                                              
                                              shiny::mainPanel(
                                                shiny::tabsetPanel(
                                                  shiny::tabPanel("Plot overlap as a heatmap matrix", plotly::plotlyOutput("plotgraph1")),
                                                  shiny::tabPanel("Plot overlap as an upset plot", downloadButton("downloadPlot"),
                                                                  shiny::plotOutput("plotgraph2"))
                                                ))))
                                   )))),
                 
                 
                 # Export tab
                shiny::tabPanel("Export",
                                shiny::fluidRow(
                            column(12,
                            mainPanel(
                              DT::dataTableOutput("exportTbl")
                              #,
                              #downloadButton("downloadData", "Download bibtex")
                            )
                          ))
                 )
                 
                 
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  rv <- shiny::reactiveValues(
    
    
  )
  
  rv$df <- data.frame()
  rv$upload_df <- data.frame()
  rv$CiteSource<-data.frame()
  rv$unique<-data.frame()
  
  #### Upload files tab section ####
  #upload on click
  shiny::observeEvent(input$file,{
    shiny::validate(need(input$file != "", "Select your bibliographic file to upload..."))
    
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
      upload_df <- read_citations(files=input$file$datapath, 
                                              cite_sources = suggested_source,
                                              cite_labels = rep("", length(input$file$datapath)),
                                              cite_strings =rep("", length(input$file$datapath)))
      upload_length <- upload_df %>%
        dplyr::group_by(cite_source) %>%
        dplyr::count(name="records") %>%
        dplyr::rename(source = cite_source)
      
      #create a dataframe summarising inputs
      df <- data.frame('file' = input$file, 
                       'source' = suggested_source,
                       'label' = rep("", length(input$file$datapath)),
                       'string' = rep("", length(input$file$datapath)))
      
      upload_df <- dplyr::left_join(upload_df, df, by=c("cite_source"="source")) %>%
        dplyr::select(-source, -label, -string) %>%
        dplyr::select(cite_source, cite_label, cite_string, everything()) 
      
      df <- dplyr::left_join(upload_length, df, by="source") %>%
        dplyr::select(file.name,records, source, label, string)

      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df) 
      
    }
  })
  
  
  # # display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    DT::datatable(rv$df, 
                  editable = TRUE,
                  options = list(paging = FALSE, 
                                 searching = FALSE),
                  rownames = FALSE)
  })
  
  # when file upload table is edited, edit reactive value upload df
  shiny::observeEvent(input$tbl_out_cell_edit, {
    
    # make sure not blank to avoid blanks in output
  
    info <- input$tbl_out_cell_edit
    val <- info$value
    
    if(val == ""){
      
      val <- NA
    }
    
    print(val)
    
    rv$df[info$row, info$col+1] <- val

    # get rownames for file  
    row_indexes <- rv$upload_df %>%
      dplyr::mutate(rowname = dplyr::row_number()) %>%
      dplyr::group_by(file.name) %>%
      dplyr::summarise(min_row = dplyr::first(rowname), max_row=dplyr::last(rowname)) 
    
    rows <- row_indexes[info$row, 2:3]
    col <- paste0("cite_", names(rv$df[info$col+1]))
    file <- rv$df[info$row,1]
    
    rv$upload_df[c(rows$min_row:rows$max_row), col] <- val
    
  
    })
  
  ### Deduplication tab ####
  
  # when dedup button clicked, deduplicate
  shiny::observeEvent(input$identify_dups,{
    
    dedup_results <- dedup_citations(rv$upload_df, merge_citations = TRUE)
    rv$unique <- dedup_results$unique
        
    n_citations <- nrow(rv$upload_df)
    n_unique <- nrow(rv$unique)
    n_duplicate <-n_citations - n_unique
    
      shinyalert::shinyalert("Deduplication complete", 
                   paste("From a total of", n_citations, "citations added, there are", n_unique, "unique citations. Compare citations across sources,
                   labels, and strings in the visualisation tab"), type = "success")
    
  })
  
  # # display results of deduplication
  # output$dedup_results <- renderDataTable({
  # 
  #   gr
  # })

  
  #### end section ####
  
  #### Visualise tab ####
  output$plotgraph1<-plotly::renderPlotly({
    n_unique <- count_unique(rv$unique)
    
    # for each unique citation, which sources/ strings/ labels are present
    source_comparison <- compare_sources(rv$unique, comp_type = input$comp_type)
    plot_source_overlap_heatmap(source_comparison)
  })
  
  plotInput <- reactive({
    source_comparison <- CiteSource::compare_sources(rv$unique, comp_type = input$comp_type)
    plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
    
  })
  
  output$plotgraph2<-shiny::renderPlot({
    print(plotInput())
    })
  
   output$downloadPlot <- shiny::downloadHandler(
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