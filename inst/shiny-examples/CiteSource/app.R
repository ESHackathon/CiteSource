library(shiny)
library(synthesisr)
library(dplyr)
library(tidyr)
library(DT)
library(shinyWidgets)
library(htmltools)
library(markdown)
library(CiteSource)
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
                                     
                                     # Sidebar panel for inputs ----
                                     sidebarPanel(
                                       
                                       # Input: Select a file ----
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
                 
                 # Deduplication tab
                 tabPanel("Deduplicate",
                          
                          fluidRow(
                            
                            column(12,
                                   # Sidebar layout ----
                                   sidebarLayout(
                                     
                                     # Sidebar panel for inputs with fields required for dedup  ----
                                     sidebarPanel(
                                       
                                       # Source selection from user input sources (updated in server)
                                       radioGroupButtons(
                                         inputId = "source_selection",
                                         label = "",
                                         choices = ""
                                       ),
                                       
                                       # Matching fields in RIS to ASYSD required fields (updated in server)
                                       selectInput(
                                         "record_id_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "title_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "author_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "year_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "abstract_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "journal_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "doi_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "start_page_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "end_page_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "pages_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "volume_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "number_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       selectInput(
                                         "isbn_field_match",
                                         "Match fields",
                                         choices = ""
                                       ),
                                       
                                       # action button to deduplicate records after matching
                                       actionButton("deduplicate", "Deduplicate citations")
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       
                                       tabsetPanel(
                                         
                                         tabPanel("Raw citations",
                                                  
                                                  dataTableOutput("refs_df")
                                         ),
                                         
                                         tabPanel("Deduplication results",
                                                  
                                                  tableOutput("dedup_results_table")
                                                  
                                         ),
                                         tabPanel("Source comparison table",
                                                  
                                                  dataTableOutput("comparison_table")
                                                  
                                         )
                                         
                                       ))
                                     
                                   )))),
                 
                 
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
                                              
                                              mainPanel('Your visualisations will appear here')
                                            )))
                            ))),
                 
                 
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
      
      #upload files one-by-one
      path_list <- input$files$datapath
      if(rlang::is_empty(input$source)){
        source=rep(NA,length(path_list))
              }else{
                source=unlist(strsplit(input$source, split=" "))  
              }
                
      if(rlang::is_empty(input$tag)){
        tag=NULL
      }else{
              tag=unlist(strsplit(input$tag, split=" "))  
              }
      if(rlang::is_empty(input$label)){
        label=NULL   }else{
                label=unlist(strsplit(input$label, split=" "))  
                
      }
      
    
      files=input$files$datapath
      cite_sources=source
      cite_strings = tag
      cite_labels = label
      
      print(rlang::is_empty(cite_sources))
      print(files)
      print(cite_sources)
      print(cite_strings)
      print(cite_labels)
      
      rv$upload_df <- CiteSource::read_citations(files=files, 
                                                 cite_sources=cite_sources,
                                                 cite_strings = cite_strings,
                                                 cite_labels = cite_labels
                                                 )
      
      print(rv$upload_df)
      
      
      #test citesource::read_citations()
      
      
      
      #save the df to a reactive value and store the upload and its source-tag data in a list
      
    
      }
  })
  
  # # display summary input table
  # output$tbl_out <- renderDataTable({
  #   DT::datatable(rv$df, 
  #                 options = list(paging = FALSE, 
  #                                searching = FALSE),
  #                 rownames = FALSE)
  # })
  # 
  # # display summary of bound dfs
  # output$refs_df <- renderDataTable({
  #   
  #   data <- rv$upload_df %>%
  #     filter(cs_source == input$source_selection) 
  #   
  #   
  #   try(data$abstract <- paste(strtrim(data$abstract, 100), "..."), silent=TRUE)
  #   try(data$keywords <- paste(strtrim(data$keywords, 100), "..."), silent=TRUE)
  #   try(data$author <- paste(strtrim(data$author, 100), "..."), silent=TRUE)
  #   try(data$address <- paste(strtrim(data$address, 100), "..."), silent=TRUE)
  #   
  #   
  #   data <- data[,colSums(is.na(data))<nrow(data)]
  #   
  #   DT::datatable(head(data, 10), 
  #                 options = list(paging = FALSE, 
  #                                searching = FALSE),
  #                 rownames = FALSE)
  #   
  # })
  # 
  # # get rective object field_names with names from each search file
  # field_names <- reactive({
  #   
  #   data <-  rv$upload_df
  #   data_split <- split(data , f = data$cs_source)
  #   data_split <- lapply(data_split, function(x) x[,colSums(is.na(x))<nrow(x)])
  #   
  #   data_split_names <- lapply(data_split , function(x) names(x))
  #   
  # })
  # 
  # # when files uploaded, update buttons for each source
  # observeEvent(input$upload, {
  #   
  #   source_choices <- unique(rv$df$cs_source)
  #   source_choices <- as.character(source_choices)
  #   
  #   updateRadioGroupButtons(session, "source_selection",
  #                           label = "Select source and match the correct fields",
  #                           choices =  source_choices,
  #                           checkIcon = list(
  #                             yes = tags$i(class = "fa fa-circle", 
  #                                          style = "color: steelblue"),
  #                             no = tags$i(class = "fa fa-circle-o", 
  #                                         style = "color: steelblue")))
  #   
  # })
  # 
  # # when source selected, get all relevant field names
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('title',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   # update selectInputs for each ASYSD relevant field based on source selected 
  #   updateSelectInput(session, "title_field_match",
  #                     label = "Title",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('record_id',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "record_id_field_match",
  #                     label = "Record ID",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('author',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "author_field_match",
  #                     label = "Author",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('source',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "journal_field_match",
  #                     label = "Journal",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('year',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "year_field_match",
  #                     label = "Year",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('doi',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "doi_field_match",
  #                     label = "DOI",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('start_page',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "start_page_field_match",
  #                     label = "Start page",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # 
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('end_page',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "end_page_field_match",
  #                     label = "End page",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('pages',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "pages_field_match",
  #                     label = "Pages",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('volume',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "volume_field_match",
  #                     label = "Volume",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('issue',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "number_field_match",
  #                     label = "Issue number",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('abstract',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "abstract_field_match",
  #                     label = "Abstract",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # observeEvent(input$source_selection, {
  #   
  #   relevant_choices <- field_names() 
  #   relevant_choices <- relevant_choices[[input$source_selection]]
  #   
  #   index <- match('isbn',relevant_choices)
  #   
  #   if(is.na(index)){
  #     
  #     index <- 1
  #   }
  #   
  #   updateSelectInput(session, "isbn_field_match",
  #                     label = "ISBN",
  #                     choices =  c("None", relevant_choices),
  #                     selected = relevant_choices[index])
  #   
  # })
  # 
  # # display summary input table1
  # output$db_name <- renderDataTable({
  #   DT::datatable(rv$df, 
  #                 options = list(paging = FALSE, 
  #                                searching = FALSE),
  #                 rownames = FALSE)
  # })
  # 
  # 
  # # when deduplication button clicked, deduplicate citations using ASYSD and merge citations together
  # unique_citations <- eventReactive(input$deduplicate, {
  #   
  #   all_citations <- rv$upload_df
  #   
  #   names(all_citations)[which(names(all_citations) == input$title_field_match)] <- "title"
  #   names(all_citations)[which(names(all_citations) == input$author_field_match)] <- "author"
  #   names(all_citations)[which(names(all_citations) == input$year_field_match)] <- "year"
  #   names(all_citations)[which(names(all_citations) == input$pages_field_match)] <- "pages"
  #   names(all_citations)[which(names(all_citations) == input$volume_field_match)] <- "volume"
  #   names(all_citations)[which(names(all_citations) == input$number_field_match)] <- "number"
  #   names(all_citations)[which(names(all_citations) == input$journal_field_match)] <- "journal"
  #   names(all_citations)[which(names(all_citations) == input$doi_field_match)] <- "doi"
  #   names(all_citations)[which(names(all_citations) == input$start_page_field_match)] <- "start_page"
  #   names(all_citations)[which(names(all_citations) == input$end_page_field_match)] <- "end_page"
  #   names(all_citations)[which(names(all_citations) == input$isbn_field_match)] <- "isbn"
  #   names(all_citations)[which(names(all_citations) == input$abstract)] <- "abstract"
  #   names(all_citations)[which(names(all_citations) == input$record_id)] <- "record_id"
  #   names(all_citations)[which(names(all_citations) ==  "cs_source")] <- "source"
  #   names(all_citations)[which(names(all_citations) ==  "tag")] <- "label"
  #   
  #   cols_needed <- c("title","author","year","pages", "volume", "number", "journal", "doi", "isbn", "abstract", "record_id", "label", "source")
  #   all_citations[cols_needed[!(cols_needed %in% colnames(all_citations))]] = NA
  #   
  #   all_citations <- all_citations %>%
  #     mutate(pages = ifelse(is.na(pages), paste0(start_page, "-", end_page), paste(pages))) %>%
  #     mutate(pages = ifelse(pages == "NA-NA", NA, paste(pages)))
  #   
  #   result <- dedup_citations(all_citations, merge_citations = TRUE)
  #   
  #   unique_citations <- result$unique
  #   
  # })
  # 
  # 
  # # output for comparison table across souces
  # output$comparison_table <- renderDataTable({
  #   
  #   source_comparison <- function(unique_data){
  #     
  #     db_comparison <- unique_data %>%
  #       select(duplicate_id, label, source, record_id) %>%
  #       separate_rows(c(source, label), convert = TRUE, sep=", ") %>%
  #       unique() %>%
  #       pivot_wider(id_col = duplicate_id, names_from=c(source), values_from=c(record_id),
  #                   values_fn =  function(x) paste("x"),
  #                   values_fill = "")
  #     
  #     unique_data$author <- gsub(",.*", "", unique_data$author)
  #     
  #     pub_id <- unique_data %>%
  #       select(author, year, duplicate_id) %>%
  #       mutate(pub_id = paste0(author, "-", year)) %>%
  #       select(-author, -year) %>%
  #       select(pub_id, everything())
  #     
  #     db_comparison <- left_join(db_comparison, pub_id, by="duplicate_id")
  #   }
  #   
  #   
  #   comparison <- source_comparison(unique_citations())
  #   
  #   DT::datatable(comparison,
  #                 options = list(paging = FALSE, 
  #                                searching = FALSE),
  #                 rownames = FALSE)
  # })
  # 
  # # output results table from deduplication pre/post numbers per source and overall
  # output$dedup_results_table <- function() {
  #   
  #   
  #   req(unique_citations())
  #   
  #   rem_dup_word <- function(x){
  #     x <- tolower(x)
  #     paste(unique(trimws(unlist(strsplit(x,split=", ",fixed=F,perl=T)))),collapse = 
  #             ", ")
  #   }
  #   
  #   # pre-dedup table
  #   pre_dedup <- rv$upload_df %>%
  #     group_by(cs_source) %>%
  #     count()  %>%
  #     pivot_wider(names_from=c(cs_source), values_from=n) 
  #   
  #   #post-dedup table
  #   post_dedup <- unique_citations() %>%
  #     select(duplicate_id, label, source, record_id) %>%
  #     separate_rows(c(source, label), convert = TRUE, sep=", ") %>%
  #     unique() %>%
  #     pivot_wider(id_col = duplicate_id, names_from=c(source), values_from=c(record_id),
  #                 values_fn = list(record_id = length)) %>%
  #     select_if(is.numeric) %>%
  #     summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 
  #   
  #   combined_dedup_table <- rbind(pre_dedup, post_dedup)
  #   combined_dedup_table <- cbind(combined_dedup_table, 
  #                                 Total = rowSums(combined_dedup_table)) 
  #   
  #   combined_dedup_table$Total[2] <- paste0(nrow(unique_citations()), " merged unique citations")
  #   
  #   combined_dedup_table %>%
  #     knitr::kable("html",  caption = "De-duplication results") %>%
  #     kable_styling("striped", full_width = T)  %>%
  #     pack_rows(
  #       index = c("Before deduplication" = 1, "After deduplication" = 1))
  #   
  #   
  # }
  # 
  # 
  # #### end section #### year,
  # 
  # 
  # 
  # 
  # #### Visualise section ####
  # #render check box ui based on uploaded sources and tags
  # output$checkbox1 <- renderUI({
  #   checkboxGroupInput('source_check', 'Sources', unique(rv$df$source))
  # })
  # output$checkbox2 <- renderUI({
  #   checkboxGroupInput('tag_check', 'Tags', unique(rv$df$tag))
  # })
  # 
  
  #### end section ####
  
  
  #### Export section ####
  
  #### end section ####
  
}

# Create Shiny app ----
shinyApp(ui, server)