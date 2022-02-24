library(shiny)
library(synthesisr)
library(dplyr)
library(DT)
library(shinyWidgets)
library(htmltools)
library(markdown)

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
                 tabPanel('Home/About',
                          navlistPanel(
                              tabPanel(title = 'Use cases',
                                       htmltools::includeMarkdown('www/useCases.md')
                                       ),
                              tabPanel(title = 'Instructions',
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
                                             fileInput("file", 
                                                       "Upload", 
                                                       multiple = FALSE, 
                                                       accept = c('.ris', '.txt')),
                                             textInput('source', 'Where did you file come from?', placeholder = 'e.g. Scopus'),
                                             textInput('tag', 'Tag your file with a name/label', placeholder = 'e.g. search string 1.3'),
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
    
    rv <- reactiveValues()
    
    rv$df <- data.frame()
    rv$upload_df <- data.frame()
    
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
            upload <- synthesisr::read_refs(input$file$datapath)
            source <- input$source
            tag <- input$tag
            upload_df <- data.frame(upload,
                           'source' = source,
                           'tag' = tag) #CAN DELETE - this is just a placeholder for visualisation, we can read in the dfs using our own import based on the filepaths, sources and tags
            upload_length <- nrow(upload_df)
            #create a dataframe summarising inputs
            df <- data.frame('file' = input$file[1], 
                             'records' = upload_length,
                             'source' = source,
                             'tag' = tag)
            #save the df to a reactive value and store the upload and its source-tag data in a list
            rv$df <- dplyr::bind_rows(rv$df, df)
            rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df) #CAN DELETE - this is just a placeholder for visualisation, we can read in the dfs using our own import based on the filepaths, sources and tags

        }
    })
    
    # display summary input table
    output$tbl_out <- renderDataTable({
        DT::datatable(rv$df, 
                      options = list(paging = FALSE, 
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    # display summary of bound dfs
    output$refs_df <- renderDataTable({
        DT::datatable(rv$upload_df, 
                      options = list(paging = FALSE, 
                                     searching = FALSE),
                      rownames = FALSE)
    })
    #### end section ####
    
    
    
    #### Visualise section ####
    #render check box ui based on uploaded sources and tags
    output$checkbox1 <- renderUI({
        checkboxGroupInput('source_check', 'Sources', unique(rv$df$source))
    })
    output$checkbox2 <- renderUI({
        checkboxGroupInput('tag_check', 'Tags', unique(rv$df$tag))
    })
    
    
    #### end section ####
    
    
    #### Export section ####
    
    #### end section ####
    
}

# Create Shiny app ----
shinyApp(ui, server)