library(DT)
library(CiteSource)
library(dplyr)

app_environment <- Sys.getenv("APP_ENV")
# Determine which Google Analytics file to include based on the environment
ga_include_file <- NULL
if (app_environment == "production") {
  ga_include_file <- "google_analytics_main.html"
} else if (app_environment == "development") {
  ga_include_file <- "google_analytics_dev.html"
}

shiny::tags$head(
  # Include the appropriate Google Analytics file if determined
  if (!is.null(ga_include_file) && file.exists(ga_include_file)) {
    includeHTML(ga_include_file)
  },
  # style
  shiny::tags$style(shiny::HTML('
                     #sidebar {
                        background-color: #ffffff;
                    }

                    body, label, input, button, select {
                      font-family: "Arial";
                    }')
))

columns2hide <- c("title", "author", "doi", "volume",
                  "pages", "number", "year", "abstract", "journal", "isbn")


# ---- Define UI ----
ui <- shiny::navbarPage("CiteSource",
                        id = "tabs",
                        header = shiny::tagList(
                          shinybusy::add_busy_spinner(spin = "circle"),
                          shinyjs::useShinyjs(),
                          tags$head(
                            tags$link(rel = "icon", type = "image/png", href = "www/favicon.png"),  # Add favicon
                            tags$style(HTML("
                              h6, .h6, h5, .h5, h4, .h4, h3, .h3, h2, .h2, h1, .h1 {
                                margin-top: 0;
                                margin-bottom: .5rem;
                                font-weight: 500;
                                line-height: 1.2;
                                color: #23395B;
                              }
                            "))
                          )
                        ),
                        theme = bslib::bs_theme(
                          bg = "rgb(251, 251, 251)",
                          primary = "#008080",
                          secondary = "#CBF7ED",
                          success = "#23395B",
                          info = "#82D173",
                          warning = "#FFC07F",
                          danger = "#008080",
                          font_scale = NULL,
                          bootswatch = "cerulean",
                          fg = "#000",
                          input_bg = "#E0E0E0",  # Set the background color for input boxes
                          input_border_color = "#23395B"  # Set the border color for input boxes
                        ),
                        # Home tab ----
                        shiny::tabPanel(
                          "Home",
                          shiny::navlistPanel(
                            shiny::tabPanel(
                              title = "About",
                              htmltools::includeMarkdown("www/about.md")
                            ),
                            shiny::tabPanel(
                              title = "Use Cases",
                              htmltools::includeMarkdown("www/use-cases.md")
                            ),
                            # User Guide
                            shiny::tabPanel(
                              title = "User Guide",
                              # Load the external Markdown file
                              htmltools::includeMarkdown("www/user_guide.md")
                            ),
                            widths = c(2, 10)
                          )
                        ),
                        shiny::tabPanel(
                          "File upload",
                          shiny::fluidRow(
                            shiny::column(
                              12,
                              # Sidebar layout ----
                              shiny::sidebarLayout(
                                shiny::sidebarPanel( # Input: Select a file ----
                                                     shiny::h5("Step 1: Upload your citation files"),
                                                     shiny::fileInput("file", "",
                                                                      multiple = TRUE,
                                                                      accept = c(".ris", ".txt", ".bib")
                                                     ),
                                                     shiny::hr(),
                                                     shiny::h5("OR: Re-upload an .ris or .csv exported from CiteSource"),
                                                     shiny::fileInput("file_reimport", "",
                                                                      multiple = TRUE,
                                                                      accept = c(".ris", ".csv")
                                                     )
                                ),
                                # Main panel for displaying outputs ----
                                shiny::mainPanel(
                                  shiny::h5("Step 2: Double click on a column to edit sources, labels, and strings. Use *Ctrl+Enter* to save edits, one column at a time"),
                                  # Output: Data file ----
                                  DT::dataTableOutput("tbl_out")
                                )
                              )
                            )
                          )
                        ),
                        shiny::tabPanel(
                          "Deduplicate",
                          shiny::tabsetPanel(
                            shiny::tabPanel(
                              "Automated deduplication",
                              br(),
                              shiny::h5("Step 3: Deduplicate"),
                              shiny::p("Click the button below to detect and remove duplicates automatically"),
                              
                              # Action button: identify duplicates in uploaded dataset
                              shinyWidgets::actionBttn(
                                "identify_dups", "Find duplicates",
                                style = "jelly",
                                color = "primary",
                                icon = shiny::icon("search")
                              ) %>% htmltools::tagAppendAttributes(style = "background-color: #008080; margin-right: 20px"),
                            ),
                            shiny::tabPanel(
                              "Manual deduplication",
                              br(),
                              shiny::h5("Step 4: Review potential duplicates manually"),
                              shiny::p("The following records were identified as potential duplicates. Potential duplicates are combined into a single row with metadata fields for each record represented (ex. Title 1 & Title 2). Click any row to indicate that the records in that row ARE duplicates. Once all duplicates are identified you can click the button 'Remove additional duplicates' and then proceed to the visualizations."),
                              shiny::textOutput("Manual_pretext"),
                              shiny::br(),
                              
                              # Button
                              shinyWidgets::actionBttn(
                                inputId = "nomanualdedup",
                                label = "Go to visualisations",
                                style = "jelly",
                                icon = shiny::icon("arrow-right"),
                                color = "primary"
                              ) %>% htmltools::tagAppendAttributes(style = "background-color: #82D173"),
                              br(),
                              shinyWidgets::actionBttn(
                                inputId = "manualdedupsubmit",
                                label = "Remove additional duplicates",
                                style = "jelly",
                                icon = shiny::icon("reply"),
                                color = "primary" # Hide the button initially
                              ) %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
                              
                              shinyWidgets::dropdown(

                                tags$h3("Select columns to display"),
                                
                                shinyWidgets::pickerInput(
                                  inputId = "manual_dedup_cols",
                                  label = "Choose columns",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    `live-search` = TRUE,
                                    `actions-box` = TRUE,
                                    style = "btn-primary")
                                ), 
                                icon = icon("filter"),
                                inline =TRUE,
                                status = "danger", width = "600px",
                                tooltip = shinyWidgets::tooltipOptions(title = "Select columns to display")),
                              
                              DT::DTOutput("manual_dedup_dt"),
                              tags$style(HTML(".table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: #CBF7ED!important; color: black!important}")),
                              
                            )
                          )
                        ),
                        shiny::tabPanel(
                          "Visualise",
                          
                          # Sidebar layout ----
                          shiny::sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            shiny::sidebarPanel(
                              width = 3,
                              id = "sidebar",
                              shiny::h5("Step 5: Visualise overlap"),
                              shinyWidgets::prettyRadioButtons(
                                inputId = "comp_type",
                                label = "Choose a comparison",
                                inline = TRUE,
                                choices = c(
                                  "sources",
                                  "labels", 
                                  "strings"
                                ),
                                status = "primary"
                              ),
                              selectInput(
                                inputId = "sources_visual",
                                "Sources to include",
                                list(),
                                multiple = TRUE,
                                selectize = TRUE
                              ),
                              selectInput(
                                inputId = "labels_visual",
                                "Labels to include",
                                list(),
                                multiple = TRUE,
                                selectize = TRUE
                              ),
                              selectInput(
                                inputId = "strings_visual",
                                "Strings to include",
                                list(),
                                multiple = TRUE,
                                selectize = TRUE
                              )
                            ),
                            
                            # Main panel for displaying outputs ----
                            shiny::mainPanel(
                              shiny::tabsetPanel(
                                shiny::tabPanel(
                                  "Plot overlap as a heatmap matrix",
                                  shiny::downloadButton("downloadHeatPlot"),
                                  plotly::plotlyOutput("plotgraph1")
                                ),
                                shiny::tabPanel(
                                  "Plot overlap as an upset plot",
                                  shiny::downloadButton("downloadUpsetPlot"),
                                  shiny::plotOutput("plotgraph2")
                                ),
                                shiny::tabPanel(
                                  "Phase Analysis",  # New Tab for Phase Analysis
                                  shiny::downloadButton("downloadPhasePlot"),
                                  shiny::plotOutput("phasePlot")
                                )
                              )
                            )
                          )
                        ),
                        
                        shiny::tabPanel(
                          "Tables",
                          
                          shiny::sidebarLayout(
                            
                            shiny::sidebarPanel(
                              id = "sidebar",
                              width = 3,
                              shiny::h5("Step 6: Summary tables"),
                              selectInput(
                                inputId = "sources_tables",
                                "Sources to include",
                                list(),
                                multiple = TRUE,
                                selectize = TRUE
                              ),
                              selectInput(
                                inputId = "labels_tables",
                                "Labels to include",
                                list(),
                                multiple = TRUE,
                                selectize = TRUE
                              ),
                              selectInput(
                                inputId = "strings_tables",
                                "Strings to include",
                                list(),
                                multiple = TRUE,
                                selectize = TRUE
                              )
                            ),
                            
                            shiny::mainPanel(
                              shiny::tabsetPanel(
                                shiny::tabPanel(
                                  "Detailed Record Table",
                                  shiny::div("Summary of unique and non-unique records by source."),
                                  shinyWidgets::actionBttn(
                                    "generateDetailedRecordTable", "Generate Detailed Record Table",
                                    style = "jelly",
                                    icon = shiny::icon("table"),
                                    color = "primary") %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
                                  shiny::br(),
                                  shiny::br(),
                                  gt::gt_output("detailedRecordTab")
                                ),
                                
                                shiny::tabPanel(
                                  "Precision/Sensitivity Table",
                                  shiny::div("Precision and Sensitivity of records across screening phases."),
                                  shinyWidgets::actionBttn(
                                    "generatePrecisionTable", "Generate Precision/Sensitivity Table",
                                    style = "jelly",
                                    icon = shiny::icon("table"),
                                    color = "primary") %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
                                  shiny::br(),
                                  shiny::br(),
                                  gt::gt_output("summaryPrecTab")
                                ),
                                
                                shiny::tabPanel(
                                  "Review individual records",
                                  shiny::br(),
                                  shinyWidgets::actionBttn(
                                    "generateRecordTable", "Generate the table",
                                    style = "jelly",
                                    icon = shiny::icon("table"),
                                    color = "primary") %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
                                  
                                  shiny::br(),
                                  
                                  shiny::wellPanel(
                                    style = "background-color: #f0f8ff; border-color: #bce8f1; margin-top: 15px; margin-bottom: 15px; padding: 15px;", # Style
                                    shiny::tags$h5(" Using the Interactive Record Table", style = "margin-top: 0; color: #31708f;"), # Title
                                    shiny::tags$p("After clicking 'Generate the table', you can explore the records using these features:"), # Introduction
                                    
                                    # Performance Note
                                    shiny::tags$p(
                                      style = "margin-bottom: 12px;",
                                      shiny::tags$strong(" Performance Note:"),
                                      " The record table may take a long time to generate if you include more than a few hundred references. Consider filtering your data first using the sidebar selectors before generating."
                                    ),
                                    
                                    # Instruction 1: Expand/Collapse
                                    shiny::tags$p(
                                      style = "margin-bottom: 12px;",
                                      shiny::tags$strong(" Expand/Collapse Row:"),
                                      " Click the ", shiny::tags$code(HTML("&oplus;")), " symbol in a row to view the full APA reference. Click ", shiny::tags$code(HTML("&CircleMinus;")), " to hide it again."
                                    ),
                                    
                                    # Instruction 2: Single Sort
                                    shiny::tags$p(
                                      style = "margin-bottom: 12px;",
                                      shiny::tags$strong(" Sort by Single Column:"),
                                      " Click any column header (like 'Citation' or a source name) to sort the table by that column's values. Click the header again to reverse the sort order."
                                    ),
                                    
                                    # Instruction 3: Multi Sort
                                    shiny::tags$p(
                                      style = "margin-bottom: 12px;",
                                      shiny::tags$strong(" Sort by Multiple Columns:"),
                                      " Click the primary column header you want to sort by. Then, hold down the ", shiny::tags$strong("Shift"), " key on your keyboard and click a second column header. You can repeat this for more sorting levels."
                                    ),
                                    
                                    # Instruction 4: Filter/Search
                                    shiny::tags$p(
                                      style = "margin-bottom: 12px;",
                                      shiny::tags$strong(" Filter/Search:"),
                                      " Type into the search box located at the top-right of the table to dynamically filter records based on any information displayed."
                                    ),
                                    
                                    # Instruction 5: Download
                                    shiny::tags$p(
                                      shiny::tags$strong(" Download Data:"),
                                      " Click the 'Download CSV' button (located above the table, next to 'Print') to save the data currently shown in the table (including applied filters) as a CSV file."
                                    )
                                  ),
                                  DT::dataTableOutput("reviewTab")
                                )
                              )
                            )
                          )
                        ),
                        
                        shiny::tabPanel(
                          "Export",
                          shiny::fluidRow(
                            shiny::column(
                              12,
                              shiny::mainPanel(
                                shiny::h5("Step 7: Export citations"),
                                shiny::h6("Note that you can only download the data after you have run the deduplication. Also, you are only able to re-upload CSV and RIS files to continue with CiteSource, so please use these formats if you want that option."),
                                shiny::downloadButton("downloadCsv", "Download csv"),
                                shiny::downloadButton("downloadRis", "Download RIS"),
                                shiny::downloadButton("downloadBib", "Download BibTex")
                              )
                            )
                          )
                        )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {
  rv <- shiny::reactiveValues()
  rv$df <- data.frame()
  rv$upload_df <- data.frame()#for original uploads
  rv$latest_unique <- data.frame()#for reimported data
  rv$pairs_to_check <- data.frame()#for potential duplicates/manual dedup
  rv$pairs_removed <- data.frame()#for removed records
  
  #### Upload files tab section ------
  # upload on click
  shiny::observeEvent(input$file, {
    shiny::validate(need(input$file != "", "Select your bibliographic file to upload..."))
    if (is.null(input$file)) {
      return(NULL)
    } 
    else {
      # upload files one-by-one
      path_list <- input$file$datapath
      suggested_source <- stringr::str_replace_all(input$file$name, "\\.(ris|bib|txt)$", "")
      suggested_label <- rep("search", length(input$file$datapath))
      empty_strings <- rep("", length(input$file$datapath))
      
      # Read the uploaded citations
      upload_df <- CiteSource::read_citations(
        files = path_list,
        cite_sources = suggested_source,
        cite_labels = suggested_label,
        cite_strings = empty_strings,
        only_key_fields = FALSE

      )
      
      # Summarize the number of records by citation source
      upload_length <- upload_df %>%
        dplyr::group_by(cite_source) %>%
        dplyr::count(name = "records") %>%
        dplyr::rename(source = cite_source)
      
      # Create a data frame summarizing the uploaded files
      df <- data.frame(
        "file" = input$file,
        "suggested_source" = suggested_source,
        "label" = suggested_label,
        "string" = empty_strings
      )
      
      # Join upload_df with df to match on cite_source
      upload_df <- dplyr::left_join(upload_df, df, by = c("cite_source" = "suggested_source")) %>%
        dplyr::select(-label, -string) %>%
        dplyr::select(cite_source, cite_label, cite_string, dplyr::everything())
      
      # Ensure required columns are present in upload_df
      required_cols <- c("title", "doi", "label", "isbn", "source", "year", "journal", "pages", "volume", "number", "abstract")
      upload_df[required_cols[!(required_cols %in% colnames(upload_df))]] <- NA
      
      # Update the summary data frame df with record counts
      df <- dplyr::left_join(upload_length, df, by = c("source" = "suggested_source")) %>%
        dplyr::select(file.datapath, file.name, records, source, label, string)
      
      # Append the results to the reactive values
      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)
    }
  })
  
  
  ## display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    if (is.null(input$file_reimport)) {
      DT::datatable(
        rv$df,
        options = list(
          paging = FALSE,
          searching = FALSE,
          columnDefs = list(list(visible = FALSE, targets = c(0))) 
        ),
        editable = list(
          target = 'column',
          disable = list(columns = c(1, 2)) 
        ),
        rownames = FALSE
      )
    }
  })
  
  shiny::observeEvent(input$file_reimport, {
    file_extension <- tolower(tools::file_ext(input$file_reimport$datapath))
    
    if (file_extension == "csv") {
      rv$latest_unique <- reimport_csv(input$file_reimport$datapath)
    } else if (file_extension == "ris") {
      rv$latest_unique <- reimport_ris(input$file_reimport$datapath)
    } else {
      warning("Invalid file extension, needs to be .ris or .csv")
    }
    
    rv$n_unique <- count_unique(rv$latest_unique)
    
    shinyalert::shinyalert("Re-import successful",
                           paste("Imported", nrow(rv$latest_unique), "citations. You can now proceed to visualisation and tables."),
                           type = "success"
    )
    
  })
  
  ## Update filters
  shiny::observe({
    # Make sure rv$latest_unique is populated and is a data frame
    if (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0) {
      
      # --- Handle cite_source ---
      sources_raw <- rv$latest_unique$cite_source
      sources_choices <- NULL # Initialize choices list
      
      if (!all(is.na(sources_raw) | sources_raw == "")) {
        sources_choices <- sources_raw[!is.na(sources_raw) & sources_raw != ""] %>%
          stringr::str_split(",\\s*") %>% # Use regex for robustness
          unlist() %>%
          unique() %>%
          sort()
      }
      
      # *** Calculate the default selection ONLY for sources_visual (exclude "unknown") ***
      sources_visual_selected_default <- NULL
      if (!is.null(sources_choices)) {
        sources_visual_selected_default <- sources_choices[sources_choices != "unknown"]
        # Handle edge case where only "unknown" was present
        if (length(sources_visual_selected_default) == 0 && "unknown" %in% sources_choices) {
          sources_visual_selected_default <- NULL
        }
      }
      # For sources_tables, the default remains all available choices (sources_choices)
      
      
      # --- Handle cite_label ---
      labels_raw <- rv$latest_unique$cite_label
      labels_choices <- NULL
      if (!all(is.na(labels_raw) | labels_raw == "")) {
        labels_choices <- unique(labels_raw[!is.na(labels_raw) & labels_raw != ""]) %>%
          stringr::str_split(",\\s*") %>%
          unlist() %>%
          unique() %>%
          sort()
      }
      labels_selected_default <- labels_choices # Default: select all valid labels
      
      # --- Handle cite_string ---
      strings_raw <- rv$latest_unique$cite_string
      strings_choices <- NULL
      if (!all(is.na(strings_raw) | strings_raw == "")) {
        strings_choices <- unique(strings_raw[!is.na(strings_raw) & strings_raw != ""]) %>%
          stringr::str_split(",\\s*") %>%
          unlist() %>%
          unique() %>%
          sort()
      }
      strings_selected_default <- strings_choices # Default: select all valid strings
      
      
      # --- Update select inputs ---
      # Use the specific default (excluding "unknown") for sources_visual
      shiny::updateSelectInput(session, inputId = "sources_visual", choices = sources_choices, selected = sources_visual_selected_default)
      shiny::updateSelectInput(session, inputId = "labels_visual", choices = labels_choices, selected = labels_selected_default)
      shiny::updateSelectInput(session, inputId = "strings_visual", choices = strings_choices, selected = strings_selected_default)
      
      # Use the original default (all choices) for sources_tables
      shiny::updateSelectInput(session, inputId = "sources_tables", choices = sources_choices, selected = sources_choices) # Reverted to selecting all
      shiny::updateSelectInput(session, inputId = "labels_tables", choices = labels_choices, selected = labels_selected_default)
      shiny::updateSelectInput(session, inputId = "strings_tables", choices = strings_choices, selected = strings_selected_default)
      
    } else {
      # Optional: Clear the inputs if rv$latest_unique is empty or not a data frame
      shiny::updateSelectInput(session, inputId = "sources_visual", choices = character(0), selected = character(0))
      shiny::updateSelectInput(session, inputId = "labels_visual", choices = character(0), selected = character(0))
      shiny::updateSelectInput(session, inputId = "strings_visual", choices = character(0), selected = character(0))
      shiny::updateSelectInput(session, inputId = "sources_tables", choices = character(0), selected = character(0))
      shiny::updateSelectInput(session, inputId = "labels_tables", choices = character(0), selected = character(0))
      shiny::updateSelectInput(session, inputId = "strings_tables", choices = character(0), selected = character(0))
    }
  }) # End update filters observe
  
  # Robust Observer for Cell Edits in tbl_out
  shiny::observeEvent(input$tbl_out_cell_edit, {
    # This observer handles edits made to the summary table (rv$df)
    # and propagates relevant changes (source, label, string)
    # to the corresponding records in the detailed table (rv$upload_df).
    
    info <- input$tbl_out_cell_edit
    
    # Ensure rv$df and rv$upload_df are valid data frames before proceeding
    if (!is.data.frame(rv$df) || nrow(rv$df) == 0) {
      # Silently return if summary data isn't ready (e.g., during initial load)
      return()
    }
    if (!is.data.frame(rv$upload_df)) {
      # Log warning if detailed data structure is missing, but allow proceeding
      # if only rv$df needs update (though propagation will fail later)
      warning("rv$upload_df is not a valid data frame. Edits cannot be propagated.")
      # Depending on desired behavior, could 'return()' here too.
    }
    
    # Get column names from the summary data frame
    df_col_names <- names(rv$df)
    
    # Determine the number of edits reported in this event
    n_edits <- length(info$row)
    
    # Process each reported edit individually
    for (i in 1:n_edits) {
      # Extract information for the current (i-th) edit
      target_row_df <- as.integer(info$row[i]) # 1-based row index for rv$df
      target_col_dt <- as.integer(info$col[i]) # 0-based column index from DT
      target_col_df <- target_col_dt + 1       # Convert to 1-based R index for rv$df
      val <- if (is.list(info$value)) info$value[[i]] else info$value[i] # Handle list/vector values
      
      # Convert blank input ("") to logical NA
      if (length(val) == 1 && !is.na(val) && val == "") {
        val <- NA
      }
      
      # --- Validate indices against rv$df ---
      if (target_row_df <= 0 || target_row_df > nrow(rv$df) ||
          target_col_df <= 0 || target_col_df > ncol(rv$df)) {
        warning(paste("Invalid row/column index received from DT edit. Row:",
                      target_row_df, "Col:", target_col_df, ". Skipping this edit."))
        next # Skip to the next edit
      }
      
      # --- 1. Update rv$df (the summary table data) ---
      # Use tryCatch to handle potential errors during assignment (e.g., type mismatch)
      tryCatch({
        rv$df[target_row_df, target_col_df] <- val
      }, error = function(e) {
        warning(paste("Error updating rv$df[", target_row_df, ",", target_col_df, "]:", e$message))
        # Continue to the next edit even if this one failed
        next
      })
      
      # --- 2. Propagate change to rv$upload_df (if applicable) ---
      
      # Get the file.datapath associated with the edited row in rv$df
      # This assumes column 1 of rv$df is 'file.datapath'
      if (df_col_names[1] != "file.datapath") {
        warning("Column 1 of rv$df is not 'file.datapath'. Cannot link edits to rv$upload_df.")
        next # Skip propagation for this edit
      }
      edited_datapath <- rv$df[[target_row_df, 1]]
      
      # Check if the datapath is valid for lookup
      if (is.na(edited_datapath) || edited_datapath == "") {
        # Don't warn every time, might be expected if datapath is missing
        next # Cannot link without a valid datapath
      }
      
      # Check if rv$upload_df is ready for update
      if (nrow(rv$upload_df) == 0 || !"file.datapath" %in% names(rv$upload_df)) {
        # Silently skip if detailed data isn't ready or lacks the key column
        next
      }
      
      # Find rows in rv$upload_df matching the datapath
      target_rows_upload_idx <- which(rv$upload_df$file.datapath == edited_datapath)
      
      if (length(target_rows_upload_idx) == 0) {
        # No matching rows found in detailed data, nothing to propagate
        next
      }
      
      # Determine the target column name in rv$upload_df based on the edited column in rv$df
      col_name_df <- df_col_names[target_col_df] # Name of edited column in summary table
      col_name_upload <- NULL # Target column name in detailed table
      
      # Define the mapping for propagation
      if (col_name_df == "source") {
        col_name_upload <- "cite_source"
      } else if (col_name_df == "label") {
        col_name_upload <- "cite_label"
      } else if (col_name_df == "string") {
        col_name_upload <- "cite_string"
      } else {
        # If the edited column (e.g., 'records') shouldn't be propagated, skip
        next
      }
      
      # Check if the target column exists in rv$upload_df
      if (!col_name_upload %in% names(rv$upload_df)) {
        warning(paste("Target column '", col_name_upload, "' not found in rv$upload_df. Cannot propagate edit."))
        next
      }
      
      # Perform the update on all matching rows in the detailed data frame
      tryCatch({
        rv$upload_df[target_rows_upload_idx, col_name_upload] <- val
      }, error = function(e) {
        warning(paste("Error updating rv$upload_df rows for datapath", edited_datapath,
                      "Column:", col_name_upload, ":", e$message))
        # Continue to the next edit even if propagation failed
      })
      
    } # End FOR loop iterating through edits reported by DT
    
  })
  
  # Deduplication tab -----------------
  
  # when dedup button clicked, deduplicate
  shiny::observeEvent(input$identify_dups, {
    if (nrow(rv$upload_df) == 0) {
      if (nrow(rv$latest_unique) > 0) {
        shinyalert::shinyalert("Deduplications already complete",
                               "You have reimported a dataset that has already been deduplicated. In that case, further deduplication is not possible here, but would need to take place outside the app.",
                               type = "error"
        )
      } else {
        shinyalert::shinyalert("Data needed",
                               "Please import your citations first.",
                               type = "error"
        )
      }
      return()  # Early return to stop further execution
    }
    
    # Assign unique IDs to avoid issues with manual deduplication
    rv$upload_df <- rv$upload_df %>% dplyr::mutate(record_id = as.character(1000 + dplyr::row_number()))
    
    # Perform deduplication
    dedup_results <- CiteSource::dedup_citations(rv$upload_df, manual = TRUE, show_unknown_tags = TRUE)
    rv$pairs_to_check <- dedup_results$manual_dedup
    rv$latest_unique <- dedup_results$unique
    rv$n_unique <- count_unique(rv$latest_unique)  # Generate the n_unique data
    
    # Generate a summary message based on deduplication results
    n_citations <- nrow(rv$upload_df)
    n_unique_records <- nrow(rv$n_unique)  # Changed variable name to avoid conflict
    n_pairs_manual <- nrow(rv$pairs_to_check)
    
    message <- if (n_pairs_manual > 0) {
      paste(
        "From a total of", n_citations, "citations added, there are", n_unique_records, 
        "unique citations. Head to the manual deduplication tab to check", n_pairs_manual, "potential duplicates."
      )
    } else {
      paste(
        "From a total of", n_citations, "citations added, there are", n_unique_records, 
        "unique citations. There are no potential duplicates for manual review. You can proceed to the visualization tab."
      )
    }
    
    shinyalert::shinyalert("Auto-deduplication complete", message, type = "success")
  })
  
  ## Manual deduplication -----
  
  # Action button: remove manually selected duplicates [merged two segments into one]
  # remove manually selected duplicates 
  observeEvent(input$manualdedupsubmit,{
    
    rv$pairs_removed <- rv$pairs_to_check[input$manual_dedup_dt_rows_selected,]
    rv$pairs_to_check <- rv$pairs_to_check[-input$manual_dedup_dt_rows_selected,]
    
    if(nrow(rv$pairs_removed) < 1){
      shinyalert("Oops!", "You haven't selected any duplicate pairs to remove.", type = "error")
      return()
    }
    
    after <- dedup_citations_add_manual(rv$latest_unique,
                                        additional_pairs = rv$pairs_removed)
    
    # update latest unique df reactive value
    rv$latest_unique <- after
    
  })
  observe({
    all_cols <- names(rv$pairs_to_check)
    base_cols <- unique(gsub("(1|2)$", "", all_cols)) # Remove "1" or "2" suffix
    
    # Initial selection
    initial_base_selection <- c("year", "author", "title", "journal", "abstract",
                                "doi", "pages", "source", "label")
    
    shinyWidgets::updatePickerInput(session = session, "manual_dedup_cols",
                                    choices = base_cols,
                                    selected = initial_base_selection)
  })
  
  
  # if no manual dedup, proceed to visualisations
  shiny::observeEvent(input$nomanualdedup, {
    
    shiny::updateNavbarPage(
      session = session,
      inputId = "tabs",
      selected = "Visualise"
    )
  })
  
  # if rows selected in manual dedup, make buttom appear
  observe({
    selected_rows <- input$manual_dedup_dt_rows_selected
    if (length(selected_rows) > 0) {
      shinyjs::show("manualdedupsubmit")  # Show the button when rows are selected
    } else {
      shinyjs::hide("manualdedupsubmit")  # Hide the button when no rows are selected
    }
  })
  
  # Output: manual dedup datatable
  manual_dedup_data <- reactive({
    
    data <- rv$pairs_to_check[,1:36]
    selected_cols <- input$manual_dedup_cols
    
    # Define the desired base order
    core_col_order <- c("author", "title", "year", "journal", "abstract","doi", "pages","volume","number","source","label","string")
    # Create the desired interleaved order of columns to select
    
    desired_table_order <- character(0)
    for (base_col in core_col_order) {
      col1 <- paste0(base_col, "1")
      col2 <- paste0(base_col, "2")
      desired_table_order <- c(desired_table_order, col1, col2)
    }
    
    # Intersect with the actual and selected columns to maintain order and presence
    cols_to_show <- intersect(desired_table_order, colnames(data))
    cols_to_show <- intersect(cols_to_show, paste0(selected_cols, rep(c("1", "2"), each = length(selected_cols))))
    
    ordered_data <- data %>%
      dplyr::select(any_of(cols_to_show))
    
    # Define match_cols INSIDE the reactive expression
    match_cols <- c("title", "author", "doi", "volume",
                    "pages", "number", "year", "abstract", "journal", "isbn")
    
    
    # Add the match_number_cols at the end (if they exist)
    
    match_number_cols_to_add <- intersect(paste0(match_cols), colnames(data))
    if (length(match_number_cols_to_add) > 0) {
      ordered_data <- cbind(ordered_data, data[, match_number_cols_to_add])
    }
    
    ordered_data
    
  })
  output$manual_dedup_dt <- DT::renderDataTable({
    
    data <- manual_dedup_data()
    
    format_cols <- c(
      "title1", "author1", "doi1", "volume1",
      "pages1", "number1", "year1", "abstract1", "journal1", "isbn1",
      "title2", "author2", "doi2", "volume2",
      "pages2", "number2", "year2", "abstract2", "journal2", "isbn2"
    )
    
    format_cols <- intersect(format_cols, colnames(data))
    shinyjs::useShinyjs()
    
    datatable(data,
              options = list(
                pageLength = 100, 
                info = FALSE,
                lengthMenu = list(c(100, -1), c("100", "All")),
                columnDefs =
                  list(
                    list(visible = FALSE, 
                         targets = columns2hide),
                    list(
                      targets = "_all",
                      render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data != null && data.length > 25 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                        "}"
                      )
                    )
                  )
              ))
  })
  
  
  # ASySD manual dedup pre text 
  output$Manual_pretext <- shiny::renderText({
    
    paste(nrow(rv$pairs_to_check), "pairs of citations require manual deduplication. Review the pairs in the table
        below.")
    
  })
  
  
  #### Visualise tab ####
  
  # Reactive expression to filter the data for visualization (used for Heatmap and Upset)
  unique_filtered_visual <- shiny::reactive({
    shiny::req(rv$latest_unique, is.data.frame(rv$latest_unique), nrow(rv$latest_unique) > 0)
    
    data_in <- rv$latest_unique
    
    sources_selected_in_input <- input$sources_visual
    sources_selected_in_input <- sources_selected_in_input[!is.na(sources_selected_in_input) & sources_selected_in_input != ""]
    
    labels_selected_in_input <- input$labels_visual
    labels_selected_in_input <- labels_selected_in_input[!is.na(labels_selected_in_input) & labels_selected_in_input != ""]
    
    strings_selected_in_input <- input$strings_visual
    strings_selected_in_input <- strings_selected_in_input[!is.na(strings_selected_in_input) & strings_selected_in_input != ""]
    
    # Step 1: Filter rows to ensure they contain at least one selected item from ANY category
    # This pre-filters, but the main goal is to correctly process the strings in Step 2
    data_candidate_rows <- data_in
    
    # Apply row-level filtering if selections are made.
    # If a category (e.g. sources) has selections, row must match one of them.
    # If a category has NO selections, that category doesn't restrict rows.
    if (length(sources_selected_in_input) > 0) {
      pattern <- paste0("\\b(", paste(sources_selected_in_input, collapse = "|"), ")\\b")
      if ("cite_source" %in% names(data_candidate_rows) && is.character(data_candidate_rows$cite_source)) {
        data_candidate_rows <- data_candidate_rows %>%
          dplyr::filter(stringr::str_detect(as.character(cite_source), pattern))
      } else { data_candidate_rows <- data_candidate_rows %>% dplyr::slice(0) } # No relevant column, empty
    }
    # Only proceed if rows remain
    if(nrow(data_candidate_rows) == 0) return(rv$latest_unique %>% dplyr::slice(0))
    
    
    if (length(labels_selected_in_input) > 0) {
      pattern <- paste0("\\b(", paste(labels_selected_in_input, collapse = "|"), ")\\b")
      if ("cite_label" %in% names(data_candidate_rows) && is.character(data_candidate_rows$cite_label)) {
        data_candidate_rows <- data_candidate_rows %>%
          dplyr::filter(stringr::str_detect(as.character(cite_label), pattern))
      } else { data_candidate_rows <- data_candidate_rows %>% dplyr::slice(0) }
    }
    if(nrow(data_candidate_rows) == 0) return(rv$latest_unique %>% dplyr::slice(0))
    
    
    if (length(strings_selected_in_input) > 0) {
      pattern <- paste0("\\b(", paste(strings_selected_in_input, collapse = "|"), ")\\b")
      if ("cite_string" %in% names(data_candidate_rows) && is.character(data_candidate_rows$cite_string)) {
        data_candidate_rows <- data_candidate_rows %>%
          dplyr::filter(stringr::str_detect(as.character(cite_string), pattern))
      } else { data_candidate_rows <- data_candidate_rows %>% dplyr::slice(0) }
    }
    if(nrow(data_candidate_rows) == 0) return(rv$latest_unique %>% dplyr::slice(0))
    
    
    # Step 2: Mutate the columns to only contain selected items
    data_processed_cols <- data_candidate_rows %>%
      dplyr::mutate(
        cite_source = if ("cite_source" %in% names(.)) {
          sapply(as.character(cite_source), function(cs_val) {
            if (is.na(cs_val) || cs_val == "") return("") 
            items <- stringr::str_split(cs_val, ",\\s*")[[1]]
            items <- items[!is.na(items) & items != ""]
            
            items_to_keep <- if (length(sources_selected_in_input) > 0) {
              items[items %in% sources_selected_in_input]
            } else { # If no specific sources were selected by user, keep all original (valid) items for this row
              items 
            }
            paste(unique(items_to_keep), collapse = ", ") # Ensure unique items are pasted
          }, USE.NAMES = FALSE)
        } else { NA_character_ },
        
        cite_label = if ("cite_label" %in% names(.)) {
          sapply(as.character(cite_label), function(cl_val) {
            if (is.na(cl_val) || cl_val == "") return("")
            items <- stringr::str_split(cl_val, ",\\s*")[[1]]
            items <- items[!is.na(items) & items != ""]
            items_to_keep <- if (length(labels_selected_in_input) > 0) {
              items[items %in% labels_selected_in_input]
            } else {
              items
            }
            paste(unique(items_to_keep), collapse = ", ")
          }, USE.NAMES = FALSE)
        } else { NA_character_ },
        
        cite_string = if ("cite_string" %in% names(.)) {
          sapply(as.character(cite_string), function(cstr_val) {
            if (is.na(cstr_val) || cstr_val == "") return("")
            items <- stringr::str_split(cstr_val, ",\\s*")[[1]]
            items <- items[!is.na(items) & items != ""]
            items_to_keep <- if (length(strings_selected_in_input) > 0) {
              items[items %in% strings_selected_in_input]
            } else {
              items
            }
            paste(unique(items_to_keep), collapse = ", ")
          }, USE.NAMES = FALSE)
        } else { NA_character_ }
      )
    
    # Step 3: Filter out rows where the column relevant to comp_type became empty
    # This ensures that if comp_type is "sources", only rows with non-empty cite_source are passed.
    data_final <- data_processed_cols
    current_comp_type <- input$comp_type # Get the comparison type
    
    if (current_comp_type == "sources") {
      if ("cite_source" %in% names(data_final)) {
        data_final <- data_final %>% dplyr::filter(!is.na(cite_source) & cite_source != "")
      } else { # If cite_source column doesn't exist, return empty
        return(rv$latest_unique %>% dplyr::slice(0))
      }
    } else if (current_comp_type == "labels") {
      if ("cite_label" %in% names(data_final)) {
        data_final <- data_final %>% dplyr::filter(!is.na(cite_label) & cite_label != "")
      } else {
        return(rv$latest_unique %>% dplyr::slice(0))
      }
    } else if (current_comp_type == "strings") {
      if ("cite_string" %in% names(data_final)) {
        data_final <- data_final %>% dplyr::filter(!is.na(cite_string) & cite_string != "")
      } else {
        return(rv$latest_unique %>% dplyr::slice(0))
      }
    }
    
    return(data_final)
  })
  
  # REACTIVE for Phase Plot Data
  unique_separated_phase <- shiny::reactive({
    # Require rv$latest_unique to have data
    shiny::req(nrow(rv$latest_unique) > 0)
    
    # Get filter inputs
    sources_filt <- input$sources_visual
    sources_filt <- ifelse(sources_filt == "_blank_", "unknown", sources_filt)
    labels_filt <- input$labels_visual
    labels_filt <- ifelse(labels_filt == "_blank_", "unknown", labels_filt)
    
    # Start with the base unique data, select columns needed
    df <- rv$latest_unique %>%
      dplyr::select(duplicate_id, cite_source, cite_label) # Add record_ids if needed
    
    # Separate rows for source and label
    df_long <- df %>%
      tidyr::separate_rows(cite_source, sep = ",\\s*") %>%
      tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
      dplyr::filter(!is.na(cite_source) & cite_source != "", # Ensure no blank/NA sources/labels after separation
                    !is.na(cite_label) & cite_label != "")
    
    # Apply filtering based on selected sources and labels for the plot
    df_filtered <- df_long %>%
      dplyr::filter(length(sources_filt) == 0 | cite_source %in% sources_filt) %>%
      dplyr::filter(length(labels_filt) == 0 | cite_label %in% labels_filt)
    
    # Check if filtering resulted in empty data
    if (nrow(df_filtered) == 0) {
      # Return an empty tibble with correct columns/types if no data matches
      return(tibble::tibble(
        duplicate_id = character(),
        cite_source = character(),
        cite_label = character(),
        type = factor(levels = c("unique", "duplicated"))
      ))
    }
    
    # Calculate 'type' based on uniqueness *within* each phase (label) for the *filtered* set
    # A record is 'unique' in a phase if it's found in only 1 source within that phase in the filtered data
    df_typed <- df_filtered %>%
      dplyr::group_by(duplicate_id, cite_label) %>%
      dplyr::mutate(n_sources_in_phase = dplyr::n_distinct(cite_source)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        type = factor(
          ifelse(n_sources_in_phase == 1, "unique", "duplicated"),
          levels = c("unique", "duplicated")
        )
      ) %>%
      # Select relevant columns needed by plot_contributions
      dplyr::select(duplicate_id, cite_source, cite_label, type)
    
    # Return the long data frame, ensuring distinct rows
    dplyr::distinct(df_typed)
  })# End Phase Plot Reactive
  
  
  # Heatmap plot (uses unique_filtered_visual)
  plotHeat <- shiny::reactive({
    # Add check if data is available
    data_vis <- unique_filtered_visual()
    shiny::req(nrow(data_vis) > 0)
    source_comparison <- compare_sources(data_vis, comp_type = input$comp_type)
    plot_source_overlap_heatmap(source_comparison, cells = stringr::str_sub(input$comp_type, end = -2))
  })
  
  output$plotgraph1 <- plotly::renderPlotly({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      shiny::req(FALSE)
    }
    # Add check specific to filtered data for this plot
    shiny::req(nrow(unique_filtered_visual()) > 0, cancelOutput = TRUE)
    print(plotHeat())
  })
  
  output$downloadHeatPlot <- shiny::downloadHandler(
    filename = function() {
      paste("heatmap-overlap", Sys.Date(), ".png", sep = "") # Added date for uniqueness
    },
    content = function(file) {
      # Ensure data is available
      shiny::req(nrow(unique_filtered_visual()) > 0)
      
      # Generate the plot object
      heat_plot_obj <- plotHeat()
      
      # Check if plot object was created successfully
      if (!is.null(heat_plot_obj)) {
        # Save the ggplot object directly using ggsave
        ggplot2::ggsave(filename = file, plot = heat_plot_obj, device = "png", width = 10, height = 8, dpi = 300) # Adjust size/dpi as needed
      } else {
        stop("Failed to generate heatmap plot for download.")
      }
    }
  )
  
  # Upset plot (uses unique_filtered_visual)
  plotUpset <- shiny::reactive({
    # Add check if data is available
    data_vis <- unique_filtered_visual()
    shiny::req(nrow(data_vis) > 0)
    source_comparison <- compare_sources(data_vis, comp_type = input$comp_type)
    plot_source_overlap_upset(source_comparison, groups = stringr::str_sub(input$comp_type, end = -2), decreasing = c(TRUE, TRUE))
  })
  
  output$plotgraph2 <- shiny::renderPlot({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      shiny::req(FALSE)
    }
    # Add check specific to filtered data for this plot
    shiny::req(nrow(unique_filtered_visual()) > 0, cancelOutput = TRUE)
    print(plotUpset())
  })
  
  output$downloadUpsetPlot <- shiny::downloadHandler(
    filename = function() {
      paste("upset-overlap", Sys.Date(), ".png", sep = "") # Added date
    },
    content = function(file) {
      # Ensure data is available
      shiny::req(nrow(unique_filtered_visual()) > 0)
      
      # Generate the plot object (we need it to print)
      upset_plot_obj <- plotUpset() # plotUpset() returns the plot object
      
      # Check if plot object was created successfully
      if (!is.null(upset_plot_obj)) {
        # Use png device for static plot
        grDevices::png(file, width = 1200, height = 800, res = 100) # Adjust size/resolution
        # Print the plot object to the device
        print(upset_plot_obj)
        # The device is automatically closed by downloadHandler
        grDevices::dev.off() # Explicitly close device for clarity/safety
      } else {
        stop("Failed to generate upset plot for download.")
        
      }
    }
  )
  
  # Phase Analysis plot (uses unique_separated_phase reactive)
  # Keep rv$n_unique definition as is for potential use in table functions,
  # but note it relies on the potentially problematic unique_filtered_visual().
  # If table functions are updated or don't rely on this specific structure,
  # this reactive could be revisited or removed.
  rv$n_unique <- shiny::reactive({
    # Original logic based on unique_filtered_visual and count_unique
    # Ensure unique_filtered_visual() is not empty
    ufv <- unique_filtered_visual()
    shiny::req(nrow(ufv) > 0)
    count_unique(ufv) # Pass the wide, summarized, filtered data
  })
  
  # Phase plot output
  output$phasePlot <- shiny::renderPlot({
    # Initial check if any data has been processed
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error")
      shiny::req(FALSE, cancelOutput = TRUE)
    }
    
    # Use the reactive data specifically prepared for this plot
    plot_data <- unique_separated_phase()
    
    # Check if the prepared data is empty after filtering
    if (nrow(plot_data) == 0) {
      # Optionally, display a message instead of just cancelling
      plot.new()
      mtext("No data matches the selected filters for Phase Analysis.")
      shiny::req(FALSE, cancelOutput = TRUE)
    }
    
    # Call plot_contributions with the prepared data
    CiteSource::plot_contributions( # Explicitly call from CiteSource if needed
      data = plot_data, # Use the new reactive data
      center = TRUE,
      bar_order = c("search", "screened", "final"), # Keep or make dynamic?
      color_order = c("unique", "duplicated"),
      totals_in_legend = FALSE #legend total needs an update
    )
  })
  
  # Phase plot download
  output$downloadPhasePlot <- shiny::downloadHandler(
    filename = function() {
      paste("phase-analysis", Sys.Date(), ".png", sep = "") # Added date
    },
    content = function(file) {
      # Use the NEW reactive data
      plot_data <- unique_separated_phase()
      
      # Add check if data is empty
      if (nrow(plot_data) == 0) {
        # Stop execution or create a plot with a message
        stop("No data available to plot based on current filters.")
      }
      
      # Generate the plot object itself
      phase_plot_obj <- CiteSource::plot_contributions( # Explicitly call from CiteSource if needed
        data = plot_data, # Use the prepared reactive data
        center = TRUE,
        bar_order = c("search", "screened", "final"), # Make dynamic if needed
        color_order = c("unique", "duplicated"),
        totals_in_legend = FALSE
      )
      
      # Check if plot object was created successfully
      if (!is.null(phase_plot_obj)) {
        grDevices::png(file, width = 1000, height = 700, res=100) # Adjust size/resolution
        print(phase_plot_obj) # Print the generated plot object
        grDevices::dev.off() # Explicitly close device
      } else {
        stop("Failed to generate phase plot for download.")
      }
    }
  )
  
  
  #### Table tab ####
  
  # Event reactive for filtering the data used in the record table and summary table
  unique_filtered_table <- shiny::eventReactive(
    c(input$generateRecordTable,
      input$sources_tables, 
      input$strings_tables, 
      input$labels_tables,
      input$generateDetailedRecordTable,
      input$generatePrecisionTable),
    {
      shiny::req(rv$latest_unique, is.data.frame(rv$latest_unique), nrow(rv$latest_unique) > 0)
      
      data_in <- rv$latest_unique
      
      # Get current selections from table filters
      sources_sel_tbl <- input$sources_tables
      sources_sel_tbl <- sources_sel_tbl[!is.na(sources_sel_tbl) & sources_sel_tbl != ""] # Clean selections
      
      labels_sel_tbl <- input$labels_tables
      labels_sel_tbl <- labels_sel_tbl[!is.na(labels_sel_tbl) & labels_sel_tbl != ""]
      
      strings_sel_tbl <- input$strings_tables
      strings_sel_tbl <- strings_sel_tbl[!is.na(strings_sel_tbl) & strings_sel_tbl != ""]
      
      # Step 1: Filter rows based on selections.
      # A row must match selections in each category that HAS selections.
      data_candidate_rows <- data_in
      
      if (length(sources_sel_tbl) > 0) { # Only filter by source if sources are selected in the input
        pattern <- paste0("\\b(", paste(sources_sel_tbl, collapse = "|"), ")\\b")
        if ("cite_source" %in% names(data_candidate_rows) && is.character(data_candidate_rows$cite_source)) {
          data_candidate_rows <- data_candidate_rows %>%
            dplyr::filter(stringr::str_detect(as.character(cite_source), pattern))
        } else { # If cite_source column is missing, no rows can match this criteria
          data_candidate_rows <- data_candidate_rows %>% dplyr::slice(0) 
        }
      }
      # If no rows remain after source filtering (and sources were selected), return empty
      if(nrow(data_candidate_rows) == 0 && length(sources_sel_tbl) > 0) return(rv$latest_unique %>% dplyr::slice(0))
      
      
      if (length(labels_sel_tbl) > 0) { # Only filter by label if labels are selected
        pattern <- paste0("\\b(", paste(labels_sel_tbl, collapse = "|"), ")\\b")
        if ("cite_label" %in% names(data_candidate_rows) && is.character(data_candidate_rows$cite_label)) {
          data_candidate_rows <- data_candidate_rows %>%
            dplyr::filter(stringr::str_detect(as.character(cite_label), pattern))
        } else {
          data_candidate_rows <- data_candidate_rows %>% dplyr::slice(0)
        }
      }
      if(nrow(data_candidate_rows) == 0 && length(labels_sel_tbl) > 0) return(rv$latest_unique %>% dplyr::slice(0))
      
      
      if (length(strings_sel_tbl) > 0) { # Only filter by string if strings are selected
        pattern <- paste0("\\b(", paste(strings_sel_tbl, collapse = "|"), ")\\b")
        if ("cite_string" %in% names(data_candidate_rows) && is.character(data_candidate_rows$cite_string)) {
          data_candidate_rows <- data_candidate_rows %>%
            dplyr::filter(stringr::str_detect(as.character(cite_string), pattern))
        } else {
          data_candidate_rows <- data_candidate_rows %>% dplyr::slice(0)
        }
      }
      # If, after all row filtering, no candidates remain, return an empty frame
      if(nrow(data_candidate_rows) == 0) return(rv$latest_unique %>% dplyr::slice(0))
      
      
      # Step 2: For the remaining rows, mutate their cite_source, cite_label, cite_string
      # to only contain the items that were actually selected in the input filters.
      data_processed_cols <- data_candidate_rows %>%
        dplyr::mutate(
          cite_source = if ("cite_source" %in% names(.)) {
            sapply(as.character(cite_source), function(cs_val) {
              if (is.na(cs_val) || cs_val == "") return("") 
              items <- stringr::str_split(cs_val, ",\\s*")[[1]]
              items <- items[!is.na(items) & items != ""] # Clean items
              
              # If user selected specific sources for tables, filter by that selection
              # Otherwise (if input$sources_tables was empty), keep all original items for this record
              items_to_keep <- if (length(sources_sel_tbl) > 0) {
                items[items %in% sources_sel_tbl]
              } else {
                items 
              }
              paste(unique(items_to_keep), collapse = ", ") # Ensure unique items are pasted
            }, USE.NAMES = FALSE) # Prevent sapply from naming the vector
          } else { NA_character_ }, # Column didn't exist
          
          cite_label = if ("cite_label" %in% names(.)) {
            sapply(as.character(cite_label), function(cl_val) {
              if (is.na(cl_val) || cl_val == "") return("")
              items <- stringr::str_split(cl_val, ",\\s*")[[1]]
              items <- items[!is.na(items) & items != ""]
              items_to_keep <- if (length(labels_sel_tbl) > 0) {
                items[items %in% labels_sel_tbl]
              } else {
                items
              }
              paste(unique(items_to_keep), collapse = ", ")
            }, USE.NAMES = FALSE)
          } else { NA_character_ },
          
          cite_string = if ("cite_string" %in% names(.)) {
            sapply(as.character(cite_string), function(cstr_val) {
              if (is.na(cstr_val) || cstr_val == "") return("")
              items <- stringr::str_split(cstr_val, ",\\s*")[[1]]
              items <- items[!is.na(items) & items != ""]
              items_to_keep <- if (length(strings_sel_tbl) > 0) {
                items[items %in% strings_sel_tbl]
              } else {
                items
              }
              paste(unique(items_to_keep), collapse = ", ")
            }, USE.NAMES = FALSE)
          } else { NA_character_ }
        ) # End mutate
      
      # Step 3: Filter out rows that might have become "empty" in all key identifier fields
      # (cite_source, cite_label, cite_string) after the transformation.
      # This prevents passing rows that are no longer meaningful based on these common categorizations.
      data_final <- data_processed_cols %>%
        dplyr::filter(
          !( (is.na(cite_source) | cite_source == "") &
               (is.na(cite_label)  | cite_label  == "") &
               (is.na(cite_string) | cite_string == "") )
        )
      
      # If, after all processing, the dataframe is empty, return an empty version
      # of the original structure to avoid downstream errors with missing columns.
      if(nrow(data_final) == 0) {
        return(rv$latest_unique %>% dplyr::slice(0))
      }
      
      return(data_final)
    } # End eventReactive logic
  ) # End unique_filtered_table
  
  detailed_table_data <- reactive({
    # Require base data to proceed
    shiny::req(is.data.frame(rv$latest_unique), nrow(rv$latest_unique) > 0)
    
    # Get current filter selections from the UI
    sources_filt <- input$sources_tables
    sources_filt <- ifelse(sources_filt == "_blank_", "unknown", sources_filt)
    labels_filt <- input$labels_tables
    labels_filt <- ifelse(labels_filt == "_blank_", "unknown", labels_filt)
    strings_filt <- input$strings_tables
    strings_filt <- ifelse(strings_filt == "_blank_", "unknown", strings_filt)
    
    # Create filter patterns (ensure robust handling of empty/NA filter values)
    labels_filt_cleaned <- labels_filt[!is.na(labels_filt) & labels_filt != ""]
    labels_pattern <- if (length(labels_filt_cleaned) > 0) paste0("\\b(", paste(labels_filt_cleaned, collapse = "|"), ")\\b") else NULL
    
    strings_filt_cleaned <- strings_filt[!is.na(strings_filt) & strings_filt != ""]
    strings_pattern <- if (length(strings_filt_cleaned) > 0) paste0("\\b(", paste(strings_filt_cleaned, collapse = "|"), ")\\b") else NULL
    
    # Apply initial filters for labels and strings only
    df_filtered_wide <- rv$latest_unique %>%
      dplyr::filter(
        (is.null(labels_pattern) | stringr::str_detect(as.character(cite_label), labels_pattern)),
        (is.null(strings_pattern) | stringr::str_detect(as.character(cite_string), strings_pattern))
      )
    
    empty_result_df <- tibble::tibble( # Define structure for empty returns
      Source = character(), `Records Imported` = integer(), `Distinct Records` = integer(),
      `Unique Records` = integer(), `Non-unique Records` = integer(),
      `Source Contribution %` = character(), `Source Unique Contribution %` = character(),
      `Source Unique %` = character() )
    
    if (nrow(df_filtered_wide) == 0) { return(empty_result_df) }
    
    # Separate cite_source column
    df_long_source <- df_filtered_wide %>%
      dplyr::select(duplicate_id, cite_source, cite_label, cite_string) %>% 
      tidyr::separate_rows(cite_source, sep = ",\\s*") %>%
      dplyr::mutate(cite_source = trimws(cite_source)) %>%
      dplyr::filter(!is.na(cite_source) & cite_source != "")
    
    # Apply source filter
    sources_filt_cleaned <- sources_filt[!is.na(sources_filt) & sources_filt != ""]
    df_long_source_filtered <- df_long_source %>%
      dplyr::filter(length(sources_filt_cleaned) == 0 | cite_source %in% sources_filt_cleaned)
    
    if (nrow(df_long_source_filtered) == 0) { return(empty_result_df) }
    
    # Calculate 'Records Imported' and 'Distinct Records' per source
    source_base_counts <- df_long_source_filtered %>%
      dplyr::group_by(cite_source) %>%
      dplyr::summarise(
        `Records Imported` = dplyr::n(),
        `Distinct Records` = dplyr::n_distinct(duplicate_id), # 'Distinct Records' for this source
        .groups = 'drop'
      )
    
    if (nrow(source_base_counts) == 0) { return(empty_result_df) } # Ensure source_base_counts has rows for sum below
    
    # Calculate 'Unique Records' per source
    record_source_counts <- df_long_source_filtered %>%
      dplyr::group_by(duplicate_id) %>%
      dplyr::summarise(n_sources_for_id = dplyr::n_distinct(cite_source), .groups = 'drop')
    
    unique_record_sources <- df_long_source_filtered %>% # Records found in only one of the *currently filtered* sources
      dplyr::inner_join(record_source_counts, by = "duplicate_id") %>%
      dplyr::filter(n_sources_for_id == 1) %>%
      dplyr::distinct(duplicate_id, cite_source) # Get the source for these unique records
    
    source_unique_counts <- unique_record_sources %>%
      dplyr::group_by(cite_source) %>%
      dplyr::summarise(`Unique Records` = dplyr::n_distinct(duplicate_id), .groups = 'drop')
    
    # Combine counts
    detailed_counts_per_source <- source_base_counts %>%
      dplyr::left_join(source_unique_counts, by = "cite_source") %>%
      dplyr::mutate(
        `Unique Records` = tidyr::replace_na(`Unique Records`, 0),
        `Non-unique Records` = `Distinct Records` - `Unique Records`
      )
    
    # Denominator for 'Source Contribution %' should be the sum of 'Distinct Records' from each source.
    denominator_source_contribution <- sum(detailed_counts_per_source$`Distinct Records`, na.rm = TRUE)
    denominator_source_contribution_safe <- ifelse(denominator_source_contribution == 0, 1, denominator_source_contribution)
    
    # Denominator for 'Source Unique Contribution %' (total unique records overall from *these filtered sources*)
    total_overall_unique_records = dplyr::n_distinct(unique_record_sources$duplicate_id) # Count unique IDs from records that are unique to some source
    denominator_source_unique_contribution_safe <- ifelse(total_overall_unique_records == 0, 1, total_overall_unique_records)
    
    
    # Calculate percentages
    detailed_counts_final <- detailed_counts_per_source %>%
      dplyr::mutate(
        perc_contr = `Distinct Records` / denominator_source_contribution_safe, # MODIFIED DENOMINATOR
        perc_unique_contr = `Unique Records` / denominator_source_unique_contribution_safe,
        perc_source_unique = ifelse(`Distinct Records` == 0, 0, `Unique Records` / `Distinct Records`)
      ) %>%
      dplyr::mutate(
        `Source Contribution %` = scales::percent(perc_contr, accuracy = 0.1),
        `Source Unique Contribution %` = scales::percent(perc_unique_contr, accuracy = 0.1),
        `Source Unique %` = scales::percent(perc_source_unique, accuracy = 0.1)
      ) %>%
      dplyr::select( 
        cite_source, `Records Imported`, `Distinct Records`, `Unique Records`,
        `Non-unique Records`, `Source Contribution %`,
        `Source Unique Contribution %`, `Source Unique %`
      )
    
    # --- TOTAL ROW ---
    # 'Total Distinct Records' in the total row should be the overall number of unique duplicate_ids found.
    overall_total_distinct_records <- dplyr::n_distinct(df_long_source_filtered$duplicate_id)
    
    total_row <- tibble::tibble(
      cite_source = "Total",
      `Records Imported` = sum(detailed_counts_final$`Records Imported`, na.rm = TRUE),
      `Distinct Records` = overall_total_distinct_records, # Overall unique items
      `Unique Records` = sum(detailed_counts_final$`Unique Records`, na.rm = TRUE), 
      `Non-unique Records` = sum(detailed_counts_final$`Non-unique Records`, na.rm = TRUE),
      `Source Contribution %` = scales::percent(1.0, accuracy = 0.1), # Sum of these per-source % should now be 100%
      `Source Unique Contribution %` = if(total_overall_unique_records > 0) scales::percent(1.0, accuracy = 0.1) else scales::percent(0.0, accuracy = 0.1), # Sum of these should be 100% if any uniques
      `Source Unique %` = scales::percent(sum(detailed_counts_final$`Unique Records`, na.rm = TRUE) / ifelse(overall_total_distinct_records == 0, 1, overall_total_distinct_records), accuracy = 0.1)
    )
    
    detailed_counts_final <- dplyr::bind_rows(detailed_counts_final, total_row) %>%
      dplyr::rename(Source = cite_source)
    
    return(detailed_counts_final)
  })
 
  # Rendering the detailed record table
  output$detailedRecordTab <- gt::render_gt({
    # Check if base data is loaded
    if (!is.data.frame(rv$latest_unique) || nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed", "Please import and deduplicate your citations first.", type = "error")
      shiny::req(FALSE) # Stop execution
    }
    # Get the data from the new reactive
    table_data <- detailed_table_data()
    # Check if the reactive returned any data (e.g., after filtering)
    shiny::validate(
      shiny::need(is.data.frame(table_data) && nrow(table_data) > 0,
                  "No records match the current filter selections for the Detailed Record Table.")
    )
    # Pass the prepared data frame to the formatting function
    create_detailed_record_table(table_data)
    # Bind to the same button trigger
  }) %>% shiny::bindEvent(input$generateDetailedRecordTable)
  
  # Rendering the precision and sensitivity table ----
  output$summaryPrecTab <- gt::render_gt({
    
    unique_citations <- unique_filtered_table()
    
    # The table is only for phase comparison, include "final" in labels for comparison
    if (!any(stringr::str_detect(tolower(unique_citations$cite_label), "final"))) {
      shiny::req(FALSE)
    }
    
    unique_citations <- unique_filtered_table()
    phase_counts <- calculate_phase_records(unique_citations, n_unique, "cite_source")
    create_precision_sensitivity_table(phase_counts)
  }) %>% shiny::bindEvent(input$generatePrecisionTable)
  
  
  # Rendering the record-level table ----
  output$reviewTab <- DT::renderDataTable({
    
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      shiny::req(FALSE)
    }
    
    citations <- unique_filtered_table()
    citations$source <- citations$cite_source
    record_level_table(citations = citations, return = "DT")
  }) %>% shiny::bindEvent(input$generateRecordTable)
  
  
  
  #### Export tab ####
  
  # Downloadable bibtex ----
  output$downloadCsv <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (nrow(rv$latest_unique) > 0) {
        write.csv(rv$latest_unique, file)
      } else {
        stop("No data to download!")
        shiny::req(FALSE)
      }
    }
  )
  output$downloadBib <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".bib", sep = "")
    },
    content = function(file) {
      export_bib(rv$latest_unique, file)
    }
  )
  
  output$downloadRis <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".ris", sep = "")
    },
    content = function(file) {
      export_ris(rv$latest_unique, file)
    }
  )
}

# Create Shiny app ----
shiny::shinyApp(ui, server)
