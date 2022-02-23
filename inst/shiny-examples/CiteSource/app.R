library(shiny)
library(synthesisr)
library(dplyr)

# Define UI for data upload app ----
ui <- navbarPage("CiteSource", id = "tabs",
                 
                 # Home tab
                 tabPanel("Home",
                          fluidRow(
                              column(12,
                                     h1('Welcome to CiteSource'),
                                     'Explanatory text'
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
            fileInput("files", "Upload", multiple = TRUE, accept = c(".xls"))
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
        validate(need(input$files != "", "Select your bibliographic files to upload..."))
        
        if (is.null(input$files)) {
            return(NULL)
        } else {
            
            path_list <- as.list(input$files$datapath)
            tbl_list <- lapply(input$files$datapath, synthesisr::read_refs)
            tbl_length <- unlist(lapply(tbl_list, nrow))
            
            df <- data.frame(input$files[1], tbl_length, row.names = NULL)
            colnames(df) <- c('file', 'records')
            #df <- do.call(dplyr::bind_rows, tbl_list)
            return(df)
        }
    })
    
    output$tbl_out <- renderDataTable({
        lst1()
    }, rownames= FALSE, options = list(paging = FALSE, searching = FALSE))
    
}

# Create Shiny app ----
shinyApp(ui, server)