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
      widths = c(2, 10)
    )
  ),
  shiny::tabPanel(
    "File upload",
    shiny::fluidRow(
      shiny::column(
        12,
        # Sidebar layout with input and output definitions ----
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
        shiny::p("Click the button below to detect and remove duplicates automatically"),

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
            shiny::div("Note that the record table will take a long time to create if you include more than a few hundred references ... so you might want to filter your data first."),
            shiny::br(),
            shinyWidgets::actionBttn(
              "generateRecordTable", "Generate the table",
              style = "jelly",
              icon = shiny::icon("table"),
              color = "primary") %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
            
            shiny::br(),
            shiny::br(" "),
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
  #for original uploads
  rv$upload_df <- data.frame()
  #for reimported data
  rv$latest_unique <- data.frame()
  #for potential duplicates/manual dedup
  rv$pairs_to_check <- data.frame()
  #for removed records
  rv$pairs_removed <- data.frame()
  
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
      
      # Increment the upload number
      if (is.null(rv$upload_number)) {
        rv$upload_number <- 1
      } else {
        rv$upload_number <- rv$upload_number + 1
      }
      
      suggested_source <- stringr::str_replace_all(input$file$name, "\\.(ris|bib|txt)$", "")
      
      empty_strings <- rep("", length(input$file$datapath))
      
      # Read the uploaded citations
      upload_df <- CiteSource::read_citations(
        files = path_list,
        cite_sources = suggested_source,
        cite_labels = empty_strings,
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
        "label" = empty_strings,
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
      
      sources <- unique(rv$latest_unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort() 
      labels <- unique(rv$latest_unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort()
      strings <- unique(rv$latest_unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort()
      
      sources <- sources[sources != "unknown"]
      labels  <- labels[labels != "unknown"]
      strings <- strings[strings != "unknown"]
      
      if (any(rv$latest_unique$cite_source == "unknown")) sources <- c(sources, "_blank_")
      if (any(rv$latest_unique$cite_label == "unknown")) labels <- c(labels, "_blank_")
      if (any(rv$latest_unique$cite_string == "unknown")) strings <- c(strings, "_blank_")
      
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
   shinyWidgets::updatePickerInput(session = session, "manual_dedup_cols",
                      choices = names(rv$pairs_to_check)[c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,26,27,28,29,31,32,33,34,35,36)],
                      selected = names(rv$pairs_to_check)[c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,26,27,28,29,31,32,33,34,35,36)])
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
    data <- data[,c(paste0(input$manual_dedup_cols))]
    
    match_cols <- c("title", "author", "doi", "volume",
                    "pages", "number", "year", "abstract", "journal", "isbn")
    
    
    data <- data %>% dplyr::select(-any_of(match_cols))
   match_number_cols <- rv$pairs_to_check[,c(paste0(match_cols))]
   
   data <- cbind(data,match_number_cols)
   
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
                pageLength = 100, info = FALSE,
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
      dplyr::group_by(duplicate_id) %>%
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
