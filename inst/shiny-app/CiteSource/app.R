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


# Define UI for data upload app ----
ui <- shiny::navbarPage("CiteSource",
  id = "tabs",
  header = shiny::tagList(
    shinybusy::add_busy_spinner(spin = "circle"),
    shinyjs::useShinyjs()
  ),
  # theme = bslib::bs_theme(bg = "rgb(251, 251, 251)",
  #                  primary = "#008080",
  #                  secondary = "#00CED1",
  #                  success = "#48D1CC",
  #                  info = "#82D173",
  #                  warning = "#FFC07F",
  #                  danger = "#C1666B",
  #                  font_scale = NULL,
  #                  bootswatch = "cerulean",
  #                  fg = "#000"),


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
      )
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
            shiny::h4("Step 1: Upload your citation files"),
            shiny::fileInput("file", "",
              multiple = TRUE,
              accept = c(".ris", ".txt", ".bib")
            ),
            shiny::hr(),
            shiny::h4("OR: Re-upload a file exported from CiteSource"),
            shiny::fileInput("file_reimport", "",
              multiple = TRUE,
              accept = c(".ris", ".csv")
            )
          ),

          # Main panel for displaying outputs ----
          shiny::mainPanel(
            shiny::h4("Step 2: Double click the row to edit sources, labels, and strings"),
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
        shiny::h4("Step 3: Deduplicate"),
        shiny::p("Click the button below to detect and remove duplicates automatically"),

        # Action button: identify duplicates in uploaded dataset
        shinyWidgets::actionBttn(
          "identify_dups", "Find duplicates",
          style = "jelly",
          color = "primary",
          icon = shiny::icon("search")
        ),

        # Output: datatable of deduplication results
        DT::dataTableOutput("dedup_results")
      ),
      shiny::tabPanel(
        "Manual deduplication",
        shiny::h4("Step 4: Review potential duplicates manually"),
        shiny::textOutput("Manual_pretext"),
        shiny::br(),

        # Button
        shinyWidgets::actionBttn(
          inputId = "manualdedupsubmit",
          label = "Remove duplicates",
          style = "jelly",
          icon = shiny::icon("reply"),
          color = "primary"
        ) %>% htmltools::tagAppendAttributes(style = "background-color: #754E9B; margin-right: 20px"),
        shinyWidgets::actionBttn(
          inputId = "completed_manual_dedup",
          label = "Finished removing extra duplicates",
          style = "jelly",
          icon = shiny::icon("arrow-right"),
          color = "primary"
        ) %>% htmltools::tagAppendAttributes(style = "background-color: #754E9B"),
        shinyWidgets::actionBttn(
          inputId = "nomanualdedup",
          label = "Proceed without manual dedup",
          style = "jelly",
          icon = shiny::icon("arrow-right"),
          color = "primary"
        ) %>% htmltools::tagAppendAttributes(style = "background-color: #754E9B"),
        shiny::br(),
        DT::DTOutput("manual_dedup_dt")
      )
    )
  ),
  shiny::tabPanel(
    "Visualise",
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::fluidRow(
          shiny::column(
            12,
            # Sidebar layout with input and output definitions ----
            shiny::sidebarLayout(

              # Sidebar panel for inputs ----
              shiny::sidebarPanel(
                id = "sidebar",
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
            )
          )
        )
      )
    )
  ),
  shiny::tabPanel(
    "Tables",
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::fluidRow(
          shiny::column(
            12,
            # Sidebar layout with input and output definitions ----
            shiny::sidebarLayout(

              # Sidebar panel for inputs ----
              shiny::sidebarPanel(
                id = "sidebar",
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
                    "Review individual records",
                    shiny::div("Note that the record table will take a long time to create if you include more than a few hundred references ... so you might want to filter your data first."),
                    shiny::br(),
                    shinyWidgets::actionBttn(
                      "generateRecordTable", "Generate the table",
                      style = "pill",
                      color = "primary",
                      icon = shiny::icon("table")
                    ),
                    shiny::br(),
                    shiny::br(" "),
                    DT::dataTableOutput("reviewTab")
                  ),
                  shiny::tabPanel(
                    "View summary table",
                    shiny::div("The first table will summarise the results by source. If your labels include 'search', 'screened' and 'final', a second table will summarise the contribution of each source across these stages."),
                    #shiny::selectInput("summary_type", "Select grouping for summary table:",
                    #            choices = c("source", "label", "string")),
                    shinyWidgets::actionBttn(
                      "generateSummaryTable", "Generate the table(s)",
                      style = "pill",
                      color = "primary",
                      icon = shiny::icon("table")
                    ),
                    shiny::br(),
                    shiny::br(" "),
                    gt::gt_output("summaryRecordTab"),
                    shiny::br(),
                    shiny::br(" "),
                    gt::gt_output("summaryPrecTab")
                  )
                )
              )
            )
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
          shiny::h5("Note that you can only download the data after you have run the deduplication."),
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
      upload_df <- read_citations(
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
    } else {
      warning("Invalid file extension, needs to be .ris or .csv")
    }
  })

  ## Update filters
  shiny::observe({
    if (nrow(rv$latest_unique) > 0) {
      
      sources <- unique(rv$latest_unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort() 
      labels <- unique(rv$latest_unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort()
      strings <- unique(rv$latest_unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort()
      
      sources[sources != "unknown"]
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
    dedup_results <- dedup(rv$upload_df, manual = TRUE, shiny_progress = TRUE, show_unknown_tags=TRUE)

    # unique and pairs to check sent to reactive values
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
    
    after <- remove_extra_pairs(rv$latest_unique,
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

   
  # if no manual dedup, proceed to visualisations
  shiny::observeEvent(input$nomanualdedup, {
    
    shiny::updateNavbarPage(
      session = session,
      inputId = "tabs",
      selected = "Visualise"
    )
  })


  # Output: manual dedup datatable
  output$manual_dedup_dt <- DT::renderDT(
    DT::datatable(rv$pairs_to_check,
      options = list(
        dom = "tp",
        pageLength = 10,
        fixedColumns = TRUE,
        scrollX = TRUE,
        columnDefs =
          list(
            list(visible = FALSE, targets = c(3, 6, 9, 12, 15, 18, 21, 24, 25, 30, 31, 32, 33, 34, 35, 36)),
            list(
              targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
              render = htmlwidgets::JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data != null && data.length > 25 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                "}"
              )
            )
          )
      )
    ) %>%
      DT::formatStyle(
        c(
          "title1", "author1", "doi1", "volume1",
          "pages1", "number1", "year1", "abstract1", "journal1", "isbn1"
        ),
        c(
          "title", "author", "doi", "volume",
          "pages", "number", "year", "abstract", "journal", "isbn"
        ),
        backgroundColor = DT::styleInterval(c(0.95, 1), c("white", "#82d173", "#82d173"))
      ) %>%
      DT::formatStyle(
        c(
          "title2", "author2", "doi2", "volume2",
          "pages2", "number2", "year2", "abstract2", "journal2", "isbn2"
        ),
        c(
          "title", "author", "doi", "volume",
          "pages", "number", "year", "abstract", "journal", "isbn"
        ),
        backgroundColor = DT::styleInterval(c(0.95, 1), c("white", "#82d173", "#82d173"))
      )
  )


  # ASySD manual dedup pre text 
  output$Manual_pretext <- shiny::renderText({

    paste(nrow(rv$pairs_to_check), "pairs of citations require manual deduplication. Review the pairs in the table
        below. You can scroll right to see all citation metadata and hover over any cell to see truncated text. Identical and near-identical fields are highlighted in green.
        Select all rows which contain duplicate pairs and click the button below to remove extra
        duplicates.")
  })


  #### Visualise tab ####

  unique_filtered_visual <- shiny::reactive({
    out <- rv$latest_unique %>%
      group_by(duplicate_id) %>%
      tidyr::separate_rows(c(record_ids, cite_label, cite_source, cite_string), sep=", ")
    
    # 
    # sources <- input$sources_visual %>% paste(collapse = "|")
    # strings <- input$strings_visual %>% paste(collapse = "|")
    # labels <- input$labels_visual %>% paste(collapse = "|")
    # if (sources == "") sources <- ".*"
    # if (strings == "") strings <- ".*"
    # if (labels == "") labels <- ".*"
    # out <- rv$latest_unique %>%
    #   dplyr::filter(
    #     (stringr::str_detect(sources, "unknown"))  | stringr::str_detect(.data$cite_source, sources),
    #     (stringr::str_detect(strings, "unknown")) | stringr::str_detect(.data$cite_string, strings),
    #     (stringr::str_detect(labels, "unknown")) | stringr::str_detect(.data$cite_label, labels))
    # 
    # out$cite_source <- stringr::str_extract_all(out$cite_source, sources) %>%
    #   purrr::map_chr(~ paste(.x, collapse = ", "))
    # out$cite_label <- stringr::str_extract_all(out$cite_label, labels) %>%
    #   purrr::map_chr(~ paste(.x, collapse = ", "))
    # out$cite_string <- stringr::str_extract_all(out$cite_string, strings) %>%
    #   purrr::map_chr(~ paste(.x, collapse = ", "))
    # 
    # out
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
      sources <- input$sources_tables %>% paste(collapse = "|")
      strings <- input$strings_tables %>% paste(collapse = "|")
      labels <- input$labels_tables %>% paste(collapse = "|")
      if (sources == "") sources <- ".*"
      if (strings == "") strings <- ".*"
      if (labels == "") labels <- ".*"
      
      out <- rv$latest_unique %>%
        dplyr::filter(
          (.data$cite_source == "" & stringr::str_detect(sources, "_blank_"))  | stringr::str_detect(.data$cite_source, sources),
          (.data$cite_string == "" & stringr::str_detect(strings, "_blank_")) | stringr::str_detect(.data$cite_string, strings),
          (.data$cite_label == "" & stringr::str_detect(labels, "_blank_")) | stringr::str_detect(.data$cite_label, labels)
        )
      
      out$cite_source <- stringr::str_extract_all(out$cite_source, sources) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
      out$cite_label <- stringr::str_extract_all(out$cite_label, labels) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
      out$cite_string <- stringr::str_extract_all(out$cite_string, strings) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
      out
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
      sources <- input$sources_tables %>% paste(collapse = "|")
      strings <- input$strings_tables %>% paste(collapse = "|")
      labels <- input$labels_tables %>% paste(collapse = "|")
      if (sources == "") sources <- ".*"
      if (strings == "") strings <- ".*"
      if (labels == "") labels <- ".*"
      
      out <- rv$upload_df %>%
        dplyr::filter(
          (.data$cite_source == "" & stringr::str_detect(sources, "_blank_"))  | stringr::str_detect(.data$cite_source, sources),
          (.data$cite_string == "" & stringr::str_detect(strings, "_blank_")) | stringr::str_detect(.data$cite_string, strings),
          (.data$cite_label == "" & stringr::str_detect(labels, "_blank_")) | stringr::str_detect(.data$cite_label, labels)
        )
      
      out$cite_source <- stringr::str_extract_all(out$cite_source, sources) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
      out$cite_label <- stringr::str_extract_all(out$cite_label, labels) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
      out$cite_string <- stringr::str_extract_all(out$cite_string, strings) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
      out
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
      paste("data-", Sys.Date(), ".bib", sep = "")
    },
    content = function(file) {
      export_ris(rv$latest_unique, file)
    }
  )
}

# Create Shiny app ----
shiny::shinyApp(ui, server)
