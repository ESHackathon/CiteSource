library(shiny)
library(synthesisr)
library(dplyr)
library(DT)

# Define UI for data upload app ----
ui <- navbarPage("CiteSource", id = "tabs",
                 
                 # Home tab
                 tabPanel('Home/About',
                          navlistPanel(
                              tabPanel(title = 'Use cases',
                                       'Use case text'
                                       ),
                              tabPanel(title = 'Instructions',
                                       'instructions text'
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
    ),
    fluidRow(
        column(12,
               dataTableOutput('refs_df')
               )
        )
                              )
                          )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    rv <- reactiveValues()
    
    observeEvent(input$upload,{
        validate(need(input$file != "", "Select your bibliographic file to upload..."))
        
        if (is.null(input$file)) {
            return(NULL)
        } else {
            
            #upload files one-by-one
            path_list <- input$file$datapath
            upload <- synthesisr::read_refs(input$file$datapath)
            source <- input$source
            tag <- input$tag
            upload_df <- data.frame(upload,
                           'source' = source,
                           'tag' = tag)
            upload_length <- nrow(upload_df)
            #create a dataframe summarising inputs
            df <- data.frame('file' = input$file[1], 
                             'records' = upload_length,
                             'source' = source,
                             'tag' = tag)
            #save the df to a reactive value and store the upload and its source-tag data in a list
            if(is.null(rv$df) == TRUE){ 
                rv$df <- df
                rvupload_df <- upload_df
            } else {
                rv$df <- dplyr::bind_rows(rv$df, df)
                rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)
            }

        }
    })
    
    output$tbl_out <- renderDataTable({
        DT::datatable(rv$df, 
                      options = list(paging = FALSE, 
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    output$refs_df <- renderDataTable({
        DT::datatable(rv$upload_df, 
                      options = list(paging = FALSE, 
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)