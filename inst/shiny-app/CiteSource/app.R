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
                       header = tagList(
                          shinybusy::add_busy_spinner(spin = "circle")
                          ),
                 # Home tab
                 shiny::tabPanel('Home',
                                 shiny::navlistPanel(
                                   shiny::tabPanel(title = 'About',
                                     htmltools::includeMarkdown('www/about.md')
                            ),
                            shiny::tabPanel(title = 'Use Cases',
                                    
                                            htmltools::includeMarkdown('www/use-cases.md')
                                    
                          ))
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
                            # Action button: identify duplicates in uploaded dataset
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
                                                           
                                              shinyWidgets::prettyRadioButtons(
                                                inputId = "comp_type",
                                                label = "Chose a comparison",
                                                inline = TRUE,
                                                choices = c("sources",
                                                            "labels", "strings"),
                                                status="primary"),
                                              selectInput(inputId = 'sources_visual', 
                                                          'Sources to include', 
                                                          list(), 
                                                          multiple = TRUE, 
                                                          selectize=TRUE),                                              
                                              selectInput(inputId = 'labels_visual', 
                                                          'Labels to include', 
                                                          list(), 
                                                          multiple = TRUE, 
                                                          selectize=TRUE),
                                              selectInput(inputId = 'strings_visual', 
                                                          'Strings to include', 
                                                          list(), 
                                                          multiple = TRUE, 
                                                          selectize=TRUE)                                            
                                              ),
                                                                                            
                                              shiny::mainPanel(
                                                shiny::tabsetPanel(
                                                  shiny::tabPanel("Plot overlap as a heatmap matrix", plotly::plotlyOutput("plotgraph1")),
                                                  shiny::tabPanel("Plot overlap as an upset plot", downloadButton("downloadPlot"),
                                                                  shiny::plotOutput("plotgraph2"))
                                                ))))
                                   )))),
                 
                # Tables tab
                shiny::tabPanel("Tables",
                                shiny::fluidRow(
                                  column(12,
                                         shiny::fluidRow(
                                           column(12,
                                                  # Sidebar layout with input and output definitions ----
                                                  shiny::sidebarLayout(
                                                    
                                                    # Sidebar panel for inputs ----
                                                    shiny::sidebarPanel(id="sidebar",
                                                                        
                                                                        selectInput(inputId = 'sources_tables', 
                                                                                    'Sources to include', 
                                                                                    list(), 
                                                                                    multiple = TRUE, 
                                                                                    selectize=TRUE),                                              
                                                                        selectInput(inputId = 'labels_tables', 
                                                                                    'Labels to include', 
                                                                                    list(), 
                                                                                    multiple = TRUE, 
                                                                                    selectize=TRUE),
                                                                        selectInput(inputId = 'strings_tables', 
                                                                                    'Strings to include', 
                                                                                    list(), 
                                                                                    multiple = TRUE, 
                                                                                    selectize=TRUE)                                            
                                                    ),
                                                    
                                                    shiny::mainPanel(
                                                      shiny::tabsetPanel(
                                                        shiny::tabPanel("Review individual records", DT::dataTableOutput("reviewTab")),
                                                        shiny::tabPanel("View summary table", gt::gt_output("summaryTab"))
                                                      ))))
                                         )))),
                
                 # Export tab
                shiny::tabPanel("Export",
                                shiny::fluidRow(
                            column(12,
                            mainPanel(
                              downloadButton("downloadData", "Download csv"),
                              downloadButton("downloadData2", "Download bibtex")
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
                       'suggested_source' = suggested_source,
                       'label' = rep("", length(input$file$datapath)),
                       'string' = rep("", length(input$file$datapath)))
      
      upload_df <- dplyr::left_join(upload_df, df, by=c("cite_source"="suggested_source")) %>%
        dplyr::select(-label, -string) %>%
        dplyr::select(cite_source, cite_label, cite_string, everything()) 
       
      # make sure required cols are present
      required_cols <- c("title", "doi", "label","isbn","source",
                     "year", "journal", "pages", "volume", "number",
                     "abstract")
      upload_df[required_cols[!(required_cols %in% colnames(upload_df))]] = NA
      
      df <- dplyr::left_join(upload_length, df, by=c("source" = "suggested_source")) %>%
        dplyr::select(file.name,records, source, label, string)

      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df) 
      
    }
  })
  
  
  ## display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    DT::datatable(rv$df, 
                  editable = TRUE,
                  options = list(paging = FALSE, 
                                 searching = FALSE),
                  rownames = FALSE)
  })

  ## Update filters
  observe({
    if (!is.null(rv$unique)) {
      shiny::updateSelectInput(inputId = "sources_visual", choices = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "labels_visual", choices = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "strings_visual", choices = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique())
    }
    if (!is.null(rv$unique)) {
      shiny::updateSelectInput(inputId = "sources_tables", choices = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "labels_tables", choices = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "strings_tables", choices = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique())
    }
  })
    
  # when file upload table is edited, edit reactive value upload df
  shiny::observeEvent(input$tbl_out_cell_edit, {
    
    # make sure not blank to avoid blanks in output
  
    info <- input$tbl_out_cell_edit
    val <- info$value
    
    if(val == ""){
      
      val <- NA
    }
    
    
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
    if (nrow(rv$upload_df) == 0) {
      shinyalert::shinyalert("Data needed", 
                             "Please import your citations first.", 
                             type = "error")
      # Stop plotting
      req(FALSE)  
    }
    
    last_message <- NULL
    dedup_results <- withCallingHandlers(
      dedup_citations(rv$upload_df),
      message = function(m) {
        if (!is.null(last_message)) removeNotification(last_message)
        last_message <<- showNotification(m$message, duration = NULL, type = "message")
      }
    )

    rv$unique <- dedup_results
        
    n_citations <- nrow(rv$upload_df)
    n_unique <- nrow(rv$unique)
    n_duplicate <-n_citations - n_unique
    if (!is.null(last_message)) removeNotification(last_message)
    
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
  
  unique_filtered_visual <- reactive({
    sources <- input$sources_visual %>% paste(collapse = "|")
    strings <- input$strings_visual %>% paste(collapse = "|")
    labels <- input$labels_visual %>% paste(collapse = "|")
    if (sources == "") sources <- ".*"
    if (strings == "") strings <- ".*"
    if (labels == "") labels <- ".*"
    out <- rv$unique %>% 
      dplyr::filter(.data$cite_source == "" | stringr::str_detect(.data$cite_source, sources),
                    .data$cite_string == "" | stringr::str_detect(.data$cite_string, strings),
                    .data$cite_label == "" | stringr::str_detect(.data$cite_label, labels)
      )
    
    out$cite_source <- stringr::str_extract_all(out$cite_source, sources) %>% 
      purrr::map_chr(~paste(.x, collapse = ", "))
    out$cite_label <- stringr::str_extract_all(out$cite_label, labels) %>% 
      purrr::map_chr(~paste(.x, collapse = ", "))
    out$cite_string <- stringr::str_extract_all(out$cite_string, strings) %>% 
      purrr::map_chr(~paste(.x, collapse = ", "))
    
    out
  })
  
  output$plotgraph1<-plotly::renderPlotly({
    if (nrow(rv$unique) == 0) {
      shinyalert::shinyalert("Data needed", 
                             "Please import and deduplicate your citations first.", 
                             type = "error")
      # Stop plotting
      req(FALSE)  
    } 
    # for each unique citation, which sources/ strings/ labels are present
    source_comparison <- compare_sources(unique_filtered_visual(), comp_type = input$comp_type)
    plot_source_overlap_heatmap(source_comparison, cells = stringr::str_sub(input$comp_type, end = -2))
  })
  
  plotInput <- reactive({
    source_comparison <- compare_sources(unique_filtered_visual(), comp_type = input$comp_type)
    plot_source_overlap_upset(source_comparison, groups = stringr::str_sub(input$comp_type, end = -2), decreasing = c(TRUE, TRUE))
    
  })
  
  output$plotgraph2<-shiny::renderPlot({
    if (nrow(rv$unique) == 0) {
      shinyalert::shinyalert("Data needed", 
                             "Please import and deduplicate your citations first.", 
                             type = "error")
      # Stop plotting
      req(FALSE)  
    } 
    print(plotInput())
    })
  
   output$downloadPlot <- shiny::downloadHandler(
     filename = function() { paste("upset", '.png', sep='') },
     content = function(file) {
       png(file)
       print(plotInput())
       dev.off()
        })
   
   #### Table tab ####
   
   unique_filtered_table <- reactive({
     sources <- input$sources_tables %>% paste(collapse = "|")
     strings <- input$strings_tables %>% paste(collapse = "|")
     labels <- input$labels_tables %>% paste(collapse = "|")
     if (sources == "") sources <- ".*"
     if (strings == "") strings <- ".*"
     if (labels == "") labels <- ".*"
     out <- rv$unique %>% 
       dplyr::filter(.data$cite_source == "" | stringr::str_detect(.data$cite_source, sources),
                     .data$cite_string == "" | stringr::str_detect(.data$cite_string, strings),
                     .data$cite_label == "" | stringr::str_detect(.data$cite_label, labels)
       )
     
     out$cite_source <- stringr::str_extract_all(out$cite_source, sources) %>% 
       purrr::map_chr(~paste(.x, collapse = ", "))
     out$cite_label <- stringr::str_extract_all(out$cite_label, labels) %>% 
       purrr::map_chr(~paste(.x, collapse = ", "))
     out$cite_string <- stringr::str_extract_all(out$cite_string, strings) %>% 
       purrr::map_chr(~paste(.x, collapse = ", "))
     
     out
   })
   
   output$reviewTab <- DT::renderDataTable({
     if (nrow(rv$unique) == 0) {
       shinyalert::shinyalert("Data needed", 
                              "Please import and deduplicate your citations first.", 
                              type = "error")
       # Stop plotting
       req(FALSE)  
     }
     citations<-unique_filtered_table()
     citations$source <- citations$cite_source
     record_level_table(citations=citations,return = "DT")
   })

   output$summaryTab <- gt::render_gt({
     if (nrow(rv$unique) == 0) {
       shinyalert::shinyalert("Data needed", 
                              "Please import and deduplicate your citations first.", 
                              type = "error")
       # Stop plotting
       req(FALSE)  
     }
     citation_summary_table(unique_filtered_table())
   })
   
      
  #### Export tab ####
   
   # # Downloadable bibtex ----
   output$downloadData <- downloadHandler(
     
     filename = function() {
       paste("data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(rv$unique, file)
     }
   )
   output$downloadData2 <- downloadHandler(
     
     filename = function() {
       paste("data-", Sys.Date(), ".bib", sep="")
     },
     content = function(file) {
       export_bib(rv$unique, file)
     }
   )
   
  
}

# Create Shiny app ----
shinyApp(ui, server)
