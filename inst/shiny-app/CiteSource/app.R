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
    shinyjs::useShinyjs()
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
            shiny::h3("Step 1:"),
            shiny::h5("Option 1: Upload your citation files"),
            shiny::fileInput("file", "Select .ris, .bib, or .txt files",
              multiple = TRUE,
              accept = c(".ris", ".txt", ".bib")
            ),
            shiny::hr(),
            shiny::h5("Option 2: Re-upload a CiteSource file"),
            shiny::fileInput("file_reimport", "Select a file exported from CiteSource",
              multiple = TRUE,
              accept = c(".ris", ".csv", ".bib"),
            shiny::h6("Note: reimported files do not require deduplication")
            )
          ),

          # Main panel for displaying outputs ----
          shiny::mainPanel(
            shiny::h3("Step 2:"),
            shiny::h5("Add custom metadata"),
            shiny::h5("Double click the cells to add data to Source, String, or Label fields"),
            # Output: Data file ----
            DT::dataTableOutput("tbl_out")
          ),
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
                  label = "Chose a comparison",
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
              shiny::mainPanel(
                shiny::tabsetPanel(
                  shiny::tabPanel(
                    "Plot overlap as a heatmap matrix",
                    plotly::plotlyOutput("plotgraph1")
                  ),
                  shiny::tabPanel(
                    "Plot overlap as an upset plot",
                    shiny::downloadButton("downloadPlot"),
                    shiny::plotOutput("plotgraph2")
                  )
                )
              )
            )),
  
  shiny::tabPanel(
    "Tables",
    
    # Sidebar layout with input and output definitions ----
          shiny::sidebarLayout(

              # Sidebar panel for inputs ----
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
                    "View summary table",
                    shiny::div("The first table will summarise the results by source. If your labels include 'search', 'screened' and 'final', a second table will summarise the contribution of each source across these stages."),
                    shiny::selectInput("summary_type", "Select grouping for summary table:",
                               choices = c("source", "label", "string")),
                    shinyWidgets::actionBttn(
                      "generateSummaryTable", "Generate the table(s)",
                      style = "jelly",
                      icon = shiny::icon("table"),
                      color = "primary") %>% htmltools::tagAppendAttributes(style = "background-color: #23395B"),
              
                    shiny::br(),
                    shiny::br(" "),
                    gt::gt_output("summaryRecordTab"),
                    shiny::br(),
                    shiny::br(" "),
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
              ))
    ),
    
  shiny::tabPanel(
    "Export",
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::mainPanel(
          shiny::h5("Step 7: Export citations"),
          shiny::h6("Note that you can only download the data after you have run the deduplication."),
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
  rv$upload_df <- data.frame()
  rv$latest_unique <- data.frame()
  rv$pairs_to_check <- data.frame()
  rv$pairs_removed <- data.frame()
  
  #### Upload files tab section ------
  # upload on click
  shiny::observeEvent(input$file, {
    shiny::validate(need(input$file != "", "Select your bibliographic file to upload..."))

    if (is.null(input$file)) {
      return(NULL)
    } else {
      # upload files one-by-one
      path_list <- input$file$datapath
      rv$upload_number <- 0
      rv$upload_number <- rv$upload_number + 1
      suggested_source <- stringr::str_replace_all(input$file$name, ".ris", "")
      suggested_source <- stringr::str_replace_all(suggested_source, ".bib", "")
      suggested_source <- stringr::str_replace_all(suggested_source, ".txt", "")
      upload_df <- CiteSource::read_citations(
        files = input$file$datapath,
        cite_sources = suggested_source,
        cite_labels = rep("", length(input$file$datapath)),
        cite_strings = rep("", length(input$file$datapath))
      )
      upload_length <- upload_df %>%
        dplyr::group_by(cite_source) %>%
        dplyr::count(name = "records") %>%
        dplyr::rename(source = cite_source)

      # create a dataframe summarising inputs
      df <- data.frame(
        "file" = input$file,
        "suggested_source" = suggested_source,
        "label" = rep("", length(input$file$datapath)),
        "string" = rep("", length(input$file$datapath))
      )

      upload_df <- dplyr::left_join(upload_df, df, by = c("cite_source" = "suggested_source")) %>%
        dplyr::select(-label, -string) %>%
        dplyr::select(cite_source, cite_label, cite_string, dplyr::everything())

      # make sure required cols are present
      required_cols <- c(
        "title", "doi", "label", "isbn", "source",
        "year", "journal", "pages", "volume", "number",
        "abstract"
      )
      upload_df[required_cols[!(required_cols %in% colnames(upload_df))]] <- NA

      df <- dplyr::left_join(upload_length, df, by = c("source" = "suggested_source")) %>%
        dplyr::select(file.datapath, file.name, records, source, label, string)

      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)
    }
  })


  ## display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    if (!is.null(input$file_reimport)) {
      shiny::req(FALSE)
    }
    DT::datatable(rv$df,
      editable = TRUE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        columnDefs = list(list(visible=FALSE, targets=c(0)))
      ),
      rownames = FALSE
    )
  })

  shiny::observeEvent(input$file_reimport, {
    file_extension <- tolower(tools::file_ext(input$file_reimport$datapath))

    if (file_extension == "csv") {
      rv$latest_unique <- reimport_csv(input$file_reimport$datapath)
    } else if (file_extension == "ris") {
      rv$latest_unique <- reimport_ris(input$file_reimport$datapath)
    } else if (file_extension == "bib") {
      rv$latest_unique <- reimport_bib(input$file_reimport$datapath)
    } else {
      warning("Invalid file extension, needs to be .ris, .csv, or .bib")
    }
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
      shinyalert::shinyalert("Data needed",
        "Please import your citations first.",
        type = "error"
      )
      # Stop plotting
      shiny::req(FALSE)
    }
    
    #Set here, otherwise manual deduplication fails if there are duplicate IDs
    rv$upload_df <- rv$upload_df %>% dplyr::mutate(record_id = as.character(1000 + dplyr::row_number()))
    
    # results of auto dedup
    dedup_results <- CiteSource::dedup_citations(rv$upload_df, manual = TRUE, show_unknown_tags=TRUE)

    # Possible way of PRIORITISING manual dedup 
    # priority_df <- rv$upload_df %>%
    #   filter(cite_label %in% c("final", "screened")) %>%
    #   select(record_id)
    
    # manual_to_review <- dedup_results$manual_dedup %>%
    #   mutate(match_score_ls = RecordLinkage::levenshteinSim(title1, title2)) %>%
    #   arrange(desc(match_score_ls)) %>%
    #   mutate(priority = ifelse(record_id1 %in% priority_df$record_id, "Yes", "No")) %>%
    #   mutate(priority = ifelse(record_id2 %in% priority_df$record_id, "Yes", priority)) %>%
    #   arrange(desc(priority)) %>%
    #   filter(priority == "Yes")
    
    rv$pairs_to_check <- dedup_results$manual_dedup
    rv$latest_unique <- dedup_results$unique

    # generate shiny alert with dedup results
    n_citations <- nrow(rv$upload_df)
    n_unique <- nrow(rv$latest_unique)
    n_pairs_manual <- nrow(rv$pairs_to_check)
    
    shinyalert::shinyalert("Auto-deduplication complete",
      paste("From a total of", n_citations, "citations added, there are", n_unique, "unique citations. Head to the manual deduplication
                   tab to check ", n_pairs_manual, " potential duplicates"),
      type = "success"
    )
  })

  # Action button: remove manually selected duplicates
  observeEvent(input$manualdedupsubmit, {
    
    rv$pairs_removed <- rv$pairs_to_check[input$manual_dedup_dt_rows_selected,]
    rv$pairs_to_check <- rv$pairs_to_check[-input$manual_dedup_dt_rows_selected,]
    
    if(nrow(rv$pairs_removed) < 1){
      shinyalert("Oops!", "You haven't selected any duplicate pairs to remove.", type = "error")
      return()
    }
    
  })
  

  ## Manual deduplication -----
  
  # remove manually selected duplicates 
  observeEvent(input$manualdedupsubmit,{
    
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
      dplyr::filter(cite_source %in% sources) %>%
      dplyr::filter(cite_string %in% strings) %>%
      dplyr::filter(cite_label %in% labels) %>%
      dplyr::summarise(across(c(record_ids, cite_label, cite_source, cite_string), ~ trimws(paste(na.omit(.), collapse = ', ')))) %>%
      dplyr::ungroup()
    
  })

  output$plotgraph1 <- plotly::renderPlotly({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      shiny::req(FALSE)
    }
    # for each unique citation, which sources/ strings/ labels are present
    source_comparison <- compare_sources(unique_filtered_visual(), comp_type = input$comp_type)
    plot_source_overlap_heatmap(source_comparison, cells = stringr::str_sub(input$comp_type, end = -2))
  })

  plotInput <- shiny::reactive({
    source_comparison <- compare_sources(unique_filtered_visual(), comp_type = input$comp_type)
    plot_source_overlap_upset(source_comparison, groups = stringr::str_sub(input$comp_type, end = -2), decreasing = c(TRUE, TRUE))
  })

  output$plotgraph2 <- shiny::renderPlot({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      shiny::req(FALSE)
    }
    print(plotInput())
  })

  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste("upset", ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    }
  )

  #### Table tab ####
  unique_filtered_table <- shiny::eventReactive(
      c(input$generateRecordTable,
      input$generateSummaryTable),
    {
      sources <- input$sources_tables 
      sources <- ifelse(sources == "_blank_", "unknown", sources)
      strings <- input$strings_tables
      strings <- ifelse(strings == "_blank_", "unknown", strings)
      labels <- input$labels_tables
      labels <- ifelse(labels == "_blank_", "unknown", labels)
      
      out <- rv$latest_unique %>%
        dplyr::group_by(duplicate_id) %>%
        tidyr::separate_rows(c(record_ids, cite_label, cite_source, cite_string), sep=", ") %>%
        dplyr::filter(cite_source %in% sources) %>%
        dplyr::filter(cite_string %in% strings) %>%
        dplyr::filter(cite_label %in% labels) %>%
        dplyr::mutate(across(c(record_ids, cite_label, cite_source, cite_string), ~ trimws(paste(na.omit(.), collapse = ', ')))) %>%
        unique() %>%
        dplyr::ungroup()
    }
  )

  output$reviewTab <- DT::renderDataTable({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      shiny::req(FALSE)
    }

    citations <- unique_filtered_table()
    citations$source <- citations$cite_source
    record_level_table(citations = citations, return = "DT")
  }) %>% shiny::bindEvent(input$generateRecordTable)

  full_filtered_table <- reactive({
    sources <- input$sources_tables 
    sources <- ifelse(sources == "_blank_", "", sources)
    strings <- input$strings_tables
    strings <- ifelse(strings == "_blank_", "", strings)
    labels <- input$labels_tables 
    labels <- ifelse(labels == "_blank_", "", labels)
    
    out <- rv$upload_df  %>%
      dplyr::mutate(cite_source = ifelse(is.na(cite_source), "", cite_source)) %>%
      dplyr::mutate(cite_string = ifelse(is.na(cite_string), "", cite_string)) %>%
      dplyr::mutate(cite_label = ifelse(is.na(cite_label), "", cite_label)) %>%
      dplyr::filter(cite_source %in% sources) %>%
      dplyr::filter(cite_string %in% strings) %>%
      dplyr::filter(cite_label %in% labels) %>%
      unique() %>%
      dplyr::ungroup()
    }
  ) %>%  shiny::bindEvent(input$generateSummaryTable)
  
  output$summaryRecordTab <- gt::render_gt({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      shiny::req(FALSE)
    }
    unique_citations <- unique_filtered_table()
    full_citations <- full_filtered_table()
    
    # browser()
    
    calculated_counts <- calculate_record_counts(unique_citations, full_citations, count_unique(unique_citations), paste0("cite_", input$summary_type))
    record_summary_table(calculated_counts)
  }) %>% shiny::bindEvent(input$generateSummaryTable)

  output$summaryPrecTab <- gt::render_gt({
    if (nrow(rv$latest_unique) == 0) {
      shinyalert::shinyalert("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      # Stop plotting
      shiny::req(FALSE)
    }
    unique_citations <- unique_filtered_table()
    full_citations <- full_filtered_table()
    
    # Table only makes sense if screening stages are provided and sources or strings compared
    if (!any(stringr::str_detect(tolower(unique_citations$cite_label), "final")) ||
        !input$summary_type %in% c("source", "string")) shiny::req(FALSE)
    
    phase_counts <- calculate_phase_count(unique_citations, full_citations, paste0("cite_", input$summary_type))
    precision_sensitivity_table(phase_counts)
  }) %>% shiny::bindEvent(input$generateSummaryTable)
  

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
