library(DT)
library(CiteSource)
library(dplyr)

# Set background colour
shiny::tags$head(shiny::tags$style(
  shiny::HTML('
                     #sidebar {
                        background-color: #ffffff;
                    }

                    body, label, input, button, select {
                      font-family: "Arial";
                    }')
))

columns2hide <- c("title", "author", "doi", "volume",
                "pages", "number", "year", "abstract", "journal", "isbn")


# Define UI for data upload app ----
ui <- shiny::navbarPage("CiteSource",
  id = "tabs",
  header = shiny::tagList(
    shinybusy::add_busy_spinner(spin = "circle"),
    shinyjs::useShinyjs(),
    #header text colour
    tags$head(
      tags$style(HTML("
      h6, .h6, h5, .h5, h4, .h4, h3, .h3, h2, .h2, h1, .h1 {
        margin-top: 0;
        margin-bottom: .5rem;
        font-weight: 500;
        line-height: 1.2;
        color: #23395B;
        }
    blockquote {
          border-left: 5px solid #008080; /* Use theme primary color for border */
          background-color: #f8f9fa;    /* A very light grey background */
          padding: 10px 20px;           /* Add padding */
          margin-bottom: 1rem;          /* Add space below */
          color: #212529;              /* Ensure text color contrasts well */
        }
        summary strong {
          font-size: 1.15rem; /* Adjust size similar to h5, tweak as needed */
          /* font-weight: bold; /* Handled by strong, but can be explicit */
          /* Optional: Adjust vertical alignment slightly if needed */
           vertical-align: middle;
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
  # Home tab
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
      ), # End tabPanel,
      
      widths = c(2, 10)
    ) # End navlistPanel
  ), # End Home tabPanel
  
  shiny::tabPanel(
    "File upload",
    shiny::fluidRow(
      shiny::column(
        12,
        # Sidebar layout with input and output definitions ----
        shiny::sidebarLayout(
          shiny::sidebarPanel( 
            shiny::h5("Step 1: Upload your citation files"),
            
            shiny::fileInput("file", "",
                             multiple = TRUE,
                             accept = c(".ris", ".txt", ".bib")
            ),
            
            shiny::selectInput("upload_label_select",
                               label = "Set Label for Uploaded File(s):",
                               choices = c("search", "screened", "final"), 
                               selected = "search" 
            ),

            shiny::hr(),
            shiny::br(),
            shiny::h5("Re-upload an .ris or .csv exported from CiteSource"),
            shiny::fileInput("file_reimport", "",
                             multiple = TRUE,
                             accept = c(".ris", ".csv")
            ),
            shiny::helpText("Re-uploading skips deduplication (Steps 3 & 4) and uses the metadata already present in the exported file. Proceed directly to 'Visualise' or 'Tables'.", style="font-size: 85%;"),
          ),
          # Main panel for displaying outputs ----
          shiny::mainPanel(
            shiny::h5("Step 2: Double click the row to edit sources, labels, and strings"),
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
        shiny::p(shiny::p("CiteSource uses ASySD to identify duplicates."),
        shiny::p(shiny::p("Dedplication takes place *within* each file (Internal Deduplication) and *across* all uploaded files (External Deduplication).")),
        ),
        # Action button: identify duplicates in uploaded dataset
        shinyWidgets::actionBttn(
          "identify_dups", "Find duplicates",
          style = "jelly",
          color = "primary",
          icon = shiny::icon("search")
        ) %>% htmltools::tagAppendAttributes(style = "background-color: #008080; margin-right: 20px"),

        # Output: datatable of deduplication results
        DT::dataTableOutput("dedup_results")
      ),
      shiny::tabPanel(
        "Manual deduplication",
        br(),
        shiny::h5("Step 4: Review potential duplicates manually"),
        shiny::p("The following records were identified as potential duplicate pairs. Pairs are combined into a single row with metadata fields for each record represented (ex. Title 1 & Title 2). Click any row to indicate that the records in that row ARE duplicates. Once all duplicates are identified you can click the button 'Remove additional duplicates' and then proceed to the visualizations."),
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
    
    # Sidebar layout with input and output definitions ----
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
            "labels", "strings"
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
            "Initial Records Table",
            shiny::div("View the initial record counts and deduplication results."),
            shinyWidgets::actionBttn(
              "generateInitialRecordTable", "Generate Initial Records Table",
              style = "jelly",
              icon = shiny::icon("table"),
              color = "primary") %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
            shiny::br(),
            shiny::br(),
            gt::gt_output("initialRecordTab")
          ),
          
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
            gt::gt_output("summaryRecordTab")
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
              shiny::tags$h5(shiny::icon("info-circle", lib = "font-awesome"), " Using the Interactive Record Table", style = "margin-top: 0; color: #31708f;"), # Title
              shiny::tags$p("After clicking 'Generate the table', you can explore the records using these features:"), # Introduction
              
              # Performance Note
              shiny::tags$p(
                style = "margin-bottom: 12px;", # Add space below this paragraph
                # Using exclamation-triangle icon for warning
                shiny::icon("exclamation-triangle", lib = "font-awesome"), shiny::tags$strong(" Performance Note:"),
                " The record table may take a long time to generate if you include more than a few hundred references. Consider filtering your data first using the sidebar selectors before generating."
              ),
              
              # Instruction 1: Expand/Collapse
              shiny::tags$p(
                style = "margin-bottom: 12px;",
                shiny::icon("expand", lib = "font-awesome"), shiny::tags$strong(" Expand/Collapse Row:"),
                " Click the ", shiny::tags$code(HTML("&oplus;")), " symbol in a row to view the full APA reference. Click ", shiny::tags$code(HTML("&CircleMinus;")), " to hide it again."
              ),
              
              # Instruction 2: Single Sort
              shiny::tags$p(
                style = "margin-bottom: 12px;",
                shiny::icon("sort", lib = "font-awesome"), shiny::tags$strong(" Sort by Single Column:"),
                " Click any column header (like 'Citation' or a source name) to sort the table by that column's values. Click the header again to reverse the sort order."
              ),
              
              # Instruction 3: Multi Sort
              shiny::tags$p(
                style = "margin-bottom: 12px;",
                shiny::icon("bars", lib = "font-awesome"), shiny::tags$strong(" Sort by Multiple Columns:"),
                " Click the primary column header you want to sort by. Then, hold down the ", shiny::tags$strong("Shift"), " key on your keyboard and click a second column header. You can repeat this for more sorting levels."
              ),
              
              # Instruction 4: Filter/Search
              shiny::tags$p(
                style = "margin-bottom: 12px;",
                shiny::icon("filter", lib = "font-awesome"), shiny::tags$strong(" Filter/Search:"),
                " Type into the search box located at the top-right of the table to dynamically filter records based on any information displayed."
              ),
              
              # Instruction 5: Download (No bottom margin needed on the last item)
              shiny::tags$p(
                shiny::icon("download", lib = "font-awesome"), shiny::tags$strong(" Download Data:"),
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
          shiny::h6("Data is only availabel after deduplication. CiteSource currently only supports re-upload of CSV and RIS files"),
          shiny::h6("CiteSource currently only supports re-upload of CSV and RIS files"),
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
  rv$latest_unique <- data.frame() #for reimported data
  rv$pairs_to_check <- data.frame()#for potential duplicates/manual dedup
  rv$pairs_removed <- data.frame()#for removed records
  
  #### Upload files tab section ------
  # upload on click
  shiny::observeEvent(input$file, {
    shiny::validate(need(input$file != "", "Select your bibliographic file to upload..."))
    if (is.null(input$file)) {
      return(NULL)
    } else {
      # --- NEW: Read the selected label from the dropdown ---
      selected_label <- input$upload_label_select
      # Ensure it's not NULL or empty if that's possible
      if (is.null(selected_label) || selected_label == "") {
        warning("No label selected for upload. Defaulting to 'search'.")
        selected_label <- "search" # Use the default from the selectInput if needed
      }
      
      # Get file paths and names (as before)
      path_list <- input$file$datapath
      file_names <- input$file$name
      
      # Generate suggested source names (as before)
      suggested_source <- stringr::str_replace_all(file_names, "\\.(ris|bib|txt)$", "")
      
      # --- MODIFIED: Create label vector using the selected label ---
      # Repeat the single selected label for each file being uploaded in this batch
      labels_for_upload <- rep(selected_label, length(path_list))
      # Keep strings empty unless you add a similar input for them
      empty_strings <- rep("", length(path_list))
      
      # Increment upload number (as before)
      if (is.null(rv$upload_number)) {
        rv$upload_number <- 1
      } else {
        rv$upload_number <- rv$upload_number + 1
      }
      
      # --- MODIFIED: Call read_citations with the selected labels ---
      # Use tryCatch for better error handling during file reading
      upload_df <- tryCatch({
        CiteSource::read_citations(
          files = path_list,
          cite_sources = suggested_source,
          cite_labels = labels_for_upload, # Use the label from the dropdown
          cite_strings = empty_strings,    # Keep strings empty
          only_key_fields = FALSE
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file(s):", e$message), type = "error", duration = 10)
        return(NULL) # Return NULL on error
      })
      
      # Stop processing if read_citations failed
      if(is.null(upload_df) || nrow(upload_df) == 0) {
        # Optionally clear the file input if reading failed completely for all files
        # resetFileInput("file") # You would need a function like this using shinyjs
        return(NULL)
      }
      
      # --- MODIFIED: Create the summary df with the selected label ---
      # Ensure df has columns corresponding to the uploaded files in this batch
      df <- data.frame(
        file.datapath = path_list[1:length(suggested_source)], # Match length just in case
        file.name = file_names[1:length(suggested_source)],
        suggested_source = suggested_source,
        label = labels_for_upload[1:length(suggested_source)], # Assign the label chosen in the dropdown
        string = empty_strings[1:length(suggested_source)],    # Keep strings empty
        stringsAsFactors = FALSE # Good practice
      )
      
      # Summarize record counts from the successfully read data (as before)
      upload_length <- upload_df %>%
        dplyr::group_by(cite_source) %>%
        dplyr::count(name = "records") %>%
        dplyr::ungroup() %>% # Ungroup after counting
        dplyr::rename(source = cite_source)
      
      # Update the summary data frame df with record counts (as before)
      # Join counts back to the summary df for display
      df <- dplyr::left_join(df, upload_length, by = c("suggested_source" = "source")) %>%
        dplyr::select(file.datapath, file.name, records, source = suggested_source, label, string) # Reorder/rename for consistency
      
      # Ensure required columns are present in upload_df (label is now cite_label)
      # No need to join the label/string from df to upload_df anymore, it came from read_citations
      required_cols <- c("cite_source", "cite_label", "cite_string", "title", "doi", "isbn", "year", "journal", "pages", "volume", "number", "abstract")
      # Add missing required columns as NA
      missing_cols <- setdiff(required_cols, colnames(upload_df))
      if(length(missing_cols) > 0) {
        upload_df[missing_cols] <- NA
      }
      # Select a reasonable default order including new cite_* columns
      upload_df <- upload_df %>%
        dplyr::select(dplyr::any_of(c("cite_source", "cite_label", "cite_string")), dplyr::everything())
      
      
      # Append the results to the reactive values (as before)
      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)
      
    }
  })
  
  
  ## display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    if (is.null(input$file_reimport)) {
      DT::datatable(rv$df,
                    editable = TRUE,
                    options = list(paging = FALSE,
                                   searching = FALSE,
                                   columnDefs = list(list(visible = FALSE, targets = c(0)))),
                    rownames = FALSE)
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
    if (nrow(rv$latest_unique) > 0) {
      
      # Handle cite_source
      sources <- rv$latest_unique$cite_source
      if (all(is.na(sources) | sources == "")) {
        sources <- NULL  # Leave it as NULL or NA
      } else {
        sources <- unique(sources[!is.na(sources) & sources != ""]) %>%
          stringr::str_split(", ") %>%
          unlist() %>%
          unique() %>%
          sort()
      }
      
      # Handle cite_label
      labels <- rv$latest_unique$cite_label
      if (all(is.na(labels) | labels == "")) {
        labels <- NULL  # Leave it as NULL or NA
      } else {
        labels <- unique(labels[!is.na(labels) & labels != ""]) %>%
          stringr::str_split(", ") %>%
          unlist() %>%
          unique() %>%
          sort()
        
      }
      
      # Handle cite_string
      strings <- rv$latest_unique$cite_string
      if (all(is.na(strings) | strings == "")) {
        strings <- NULL  # Leave it as NULL or NA
      } else {
        strings <- unique(strings[!is.na(strings) & strings != ""]) %>%
          stringr::str_split(", ") %>%
          unlist() %>%
          unique() %>%
          sort()
      }
      
      # Update select inputs
      shiny::updateSelectInput(inputId = "sources_visual", choices = sources, selected = sources)
      shiny::updateSelectInput(inputId = "labels_visual", choices = labels, selected = labels)
      shiny::updateSelectInput(inputId = "strings_visual", choices = strings, selected = strings)

      shiny::updateSelectInput(inputId = "sources_tables", choices = sources, selected = sources)
      shiny::updateSelectInput(inputId = "labels_tables", choices = labels, selected = labels)
      shiny::updateSelectInput(inputId = "strings_tables", choices = strings, selected = strings)
    }
  })

  # when file upload table is edited, edit reactive value upload df
  shiny::observeEvent(input$tbl_out_cell_edit, {
    # make sure not blank to avoid blanks in output

    info <- input$tbl_out_cell_edit
    val <- info$value

    if (val == "") {
      val <- NA
    }

    rv$df[info$row, info$col + 1] <- val
    
    # get rownames for file
    row_indexes <- rv$upload_df %>%
      dplyr::mutate(rowname = dplyr::row_number()) %>%
      dplyr::group_by(file.datapath) %>%
      dplyr::summarise(min_row = dplyr::first(rowname), max_row = dplyr::last(rowname))

    rows <- row_indexes[row_indexes$file.datapath == rv$df[info$row, 1], 2:3]
    col <- paste0("cite_", names(rv$df[info$col + 1]))
    file <- rv$df[info$row, 1]

    rv$upload_df[c(rows$min_row:rows$max_row), col] <- val
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
    n_distinct_records <- nrow(rv$n_unique)
    n_unique_records <- nrow(dedup_results$unique)
    n_pairs_manual <- nrow(rv$pairs_to_check)
    
    # Title for the alert
    alert_title <- "Automated Deduplication Complete"
    
    # Text for the alert body
    if (n_pairs_manual > 0) {
      alert_text <- sprintf(
        "Process Summary:\n - Started with %d uploaded records.\n - Found %d distinct records (after removing duplicates within each source file).\n - Found %d unique records (after removing duplicates across source files).\n\nAction Required:\n%d potential duplicate pairs require your review. Please go to the 'Manual deduplication' sub-tab.",
        n_citations, n_distinct_records, n_unique_records, n_pairs_manual
      )
    } else {
      alert_text <- sprintf(
        "Process Summary:\n - Started with %d uploaded records.\n - Found %d distinct records (after removing duplicates withi* each source file).\n - Found %d unique records (after removing duplicates across source files).\n\nNo potential duplicates were flagged for manual review.\n\nNext Step:\nYou can now proceed to the 'Visualise' tab.",
        n_citations, n_distinct_records, n_unique_records
      )
    }
    
    # Show the alert
    shinyalert::shinyalert(title = alert_title, 
                           text = alert_text, 
                           type = "success",
                           size = "l")
    
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
  
    observeEvent(input$completed_manual_dedup,{
      
    # provide shiny alert
    shinyalert::shinyalert("Manual deduplication complete",
                           paste(
                             "From a total of", nrow(rv$upload_df), "citations uploaded, there are", nrow(rv$latest_unique),
                             "unique citations after automated and manual deduplication.
                           Compare citations across sources, labels, and strings in the visualisation tab"
                           ),
                           type = "success"
    )

      })

    observe({
      all_cols <- names(rv$pairs_to_check)
      base_cols <- unique(gsub("(1|2)$", "", all_cols)) # Remove "1" or "2" suffix
      
      # Initial selection
      initial_base_selection <- c("author", "title", "year", "journal", "abstract", "doi", "pages")
      
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
    
    data <- rv$pairs_to_check[, 1:36]
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
    
    ordered_data <- ordered_data %>% dplyr::select(-any_of(match_cols))
    
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
              extensions = c('FixedHeader', 'FixedColumns'),
              options = list(
                fixedHeader = TRUE,                   
                fixedColumns = list(leftColumns = 2), 
                scrollX = TRUE,
                pageLength = 100,
                info = FALSE,
                lengthMenu = list(c(100, -1), c("100", "All")),
                columnDefs = list(
                  list(
                    targets = "_all",
                    render = JS(
                      "function(data, type, row, meta) {",
                      "  if (type === 'display' && data != null && typeof data === 'string' && data.length > 25) {",
                      "    var rawData = data;",
                      "    return '<span title=\"' + rawData + '\">' + rawData.substr(0, 25) + '...</span>';",
                      "  } else {",
                      "    return data;",
                      "  }",
                      "}"
                    )
                  ),
                  list(
                    visible = FALSE,
                    targets = columns2hide
                  )
                ) 
              ))
    # ) %>%
    #   DT::formatStyle(
    #     columns = format_cols,
    #     backgroundColor = DT::styleInterval(c(0.95, 1), c("white", "#82d173", "#82d173")),
    #     target = "row"
    #   ) %>%
    #   DT::formatStyle(
    #     columns = format_cols,
    #     backgroundColor = DT::styleInterval(c(0.95, 1), c("white", "#82d173", "#82d173")),
    #     target = "cell",
    #     color = JS("value >= 0.95 ? 'white' : null")
    #   )
  })


  # ASySD manual dedup pre text 
  output$Manual_pretext <- shiny::renderText({

    paste(nrow(rv$pairs_to_check), "pairs of citations require manual deduplication. Review the pairs in the table
        below.")

  })


  #### Visualise tab ####
  
  # Reactive expression to filter the data for visualization 
  unique_filtered_visual <- shiny::reactive({
    sources <- input$sources_visual 
    sources <- ifelse(sources == "_blank_", "unknown", sources)
    strings <- input$strings_visual
    strings <- ifelse(strings == "_blank_", "unknown", strings)
    labels <- input$labels_visual 
    labels <- ifelse(labels == "_blank_", "unknown", labels)
    
    out <- rv$latest_unique %>%
      # Group by the ID representing unique/duplicate groups
      dplyr::group_by(duplicate_id) %>%
      # Separate comma-separated values into individual rows
      tidyr::separate_rows(c(record_ids, cite_label, cite_source, cite_string), sep=", ") %>%
      dplyr::filter(length(sources) == 0 | cite_source %in% sources) %>%
      dplyr::filter(length(strings) == 0 | cite_string %in% strings) %>%
      dplyr::filter(length(labels) == 0 | cite_label %in% labels) %>%
      dplyr::summarise(across(c(record_ids, cite_label, cite_source, cite_string), ~ trimws(paste(na.omit(.), collapse = ', ')))) %>%
      dplyr::ungroup()
  })
  
  # Heatmap plot
  plotHeat <- shiny::reactive({
    source_comparison <- compare_sources(unique_filtered_visual(), comp_type = input$comp_type)
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
    
    print(plotHeat())
  })
  
  output$downloadHeatPlot <- shiny::downloadHandler(
    filename = function() {
      paste("heatmap", ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(plotHeat())
      dev.off()
    }
  )
  
  # Upset plot
  plotUpset <- shiny::reactive({
    source_comparison <- compare_sources(unique_filtered_visual(), comp_type = input$comp_type)
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
    print(plotUpset())
  })
  
  output$downloadUpsetPlot <- shiny::downloadHandler(
    filename = function() {
      paste("upset", ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(plotUpset())
      dev.off()
    }
  )
  
  # Phase Analysis plot
  # Update n_unique based on the filtered data
  rv$n_unique <- shiny::reactive({
    filtered_data <- unique_filtered_visual()
    count_unique(filtered_data) 
  })
  
  output$phasePlot <- shiny::renderPlot({
    unique_citations <- unique_filtered_visual()
    rv$n_unique<- count_unique(unique_citations)
    plot_contributions(
      data = rv$n_unique,
      center = TRUE,
      bar_order = c("search", "screened", "final"),
      color_order = c("unique", "duplicated")
    )
  })
  
  output$downloadPhasePlot <- shiny::downloadHandler(
    filename = function() {
      paste("phase_analysis", ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(
        plot_contributions(
          data = rv$n_unique,
          center = TRUE,
          bar_order = c("search", "screened", "final"),
          color_order = c("unique", "duplicated")
        )
      )
      dev.off()
    }
  )
  
  #### Table tab ####
  
  # Event reactive for filtering the data used in the record table and summary table
  unique_filtered_table <- shiny::eventReactive(
    c(input$generateRecordTable, input$generateSummaryTable, 
      input$sources_tables, input$strings_tables, input$labels_tables,
      input$generateInitialRecordTable, input$generateDetailedRecordTable, 
      input$generatePrecisionTable),
    {
      sources <- input$sources_tables 
      sources <- ifelse(sources == "_blank_", "unknown", sources)
      strings <- input$strings_tables
      strings <- ifelse(strings == "_blank_", "unknown", strings)
      labels <- input$labels_tables
      labels <- ifelse(labels == "_blank_", "unknown", labels)
      
      rv$latest_unique %>%
        dplyr::group_by(duplicate_id) %>%
        tidyr::separate_rows(c(record_ids, cite_label, cite_source, cite_string), sep=", ") %>%
        dplyr::filter(length(sources) == 0 | cite_source %in% sources) %>%
        dplyr::filter(length(strings) == 0 | cite_string %in% strings) %>%
        dplyr::filter(length(labels) == 0 | cite_label %in% labels) %>%
        dplyr::mutate(across(c(record_ids, cite_label, cite_source, cite_string), ~ trimws(paste(na.omit(.), collapse = ', ')))) %>%
        unique() %>%
        dplyr::ungroup()
    }
  )
  
  # Rendering the initial record table
  output$initialRecordTab <- gt::render_gt({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      shiny::req(FALSE)
    }
    
    unique_citations <- unique_filtered_table()
    initial_records <- calculate_initial_records(unique_citations, "search")
    create_initial_record_table(initial_records)
  }) %>% shiny::bindEvent(input$generateInitialRecordTable)
  
  # Rendering the detailed record table
  output$summaryRecordTab <- gt::render_gt({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      shiny::req(FALSE)
    }
    
    unique_citations <- unique_filtered_table()
    detailed_count <- calculate_detailed_records(unique_citations, rv$n_unique, "search")
    create_detailed_record_table(detailed_count)
  }) %>% shiny::bindEvent(input$generateDetailedRecordTable)
  
  # Rendering the precision and sensitivity table
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
  
  
  # Rendering the record-level table
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

  # # Downloadable bibtex ----
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
