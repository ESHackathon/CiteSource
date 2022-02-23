library(shiny)
library(synthesisr)
library(dplyr)

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
                      multiple = TRUE, 
                      accept = c('.ris', '.txt'))
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Data file ----
            dataTableOutput("tbl_out")
            
        )
    )
                              )
                          )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    lst1 <- reactive({
        validate(need(input$file != "", "Select your bibliographic file to upload..."))
        
        if (is.null(input$file)) {
            return(NULL)
        } else {
            
            path_list <- as.list(input$file$datapath)
            tbl_list <- lapply(input$file$datapath, synthesisr::read_refs)
            tbl_length <- unlist(lapply(tbl_list, nrow))
            
            df <- data.frame(input$file[1], tbl_length, row.names = NULL)
            colnames(df) <- c('file', 'records')
            #df <- do.call(dplyr::bind_rows, tbl_list)
            return(df)
        }
    })
    
    output$tbl_out <- renderDataTable({
        lst1()
    }, options = list(paging = FALSE, searching = FALSE))
    
}

# Create Shiny app ----
shinyApp(ui, server)