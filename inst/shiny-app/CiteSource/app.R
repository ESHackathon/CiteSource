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
    shinybusy::add_busy_spinner(spin = "circle")
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

  shinyjs::useShinyjs(),

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
                    DT::dataTableOutput("reviewTab")
                  ),
                  shiny::tabPanel(
                    "View summary table",
                    selectInput(
                      inputId = "summary_search_labels",
                      "Labels indicating search stage",
                      list(),
                      multiple = TRUE,
                      selectize = TRUE
                    ),
                    selectInput(
                      inputId = "summary_screening_labels",
                      "Labels indicating screening stages",
                      list(),
                      multiple = TRUE,
                      selectize = TRUE
                    ),
                    shinyWidgets::actionBttn(
                      "generateSummaryTable", "Generate the table",
                      style = "pill",
                      color = "primary",
                      icon = shiny::icon("table")
                    ),
                    gt::gt_output("summaryTab")
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
  rv$unique <- data.frame()
  rv$post_manual_dedup <- data.frame()

  #### Upload files tab section ####
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
        dplyr::select(file.name, records, source, label, string)

      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)
    }
  })


  ## display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    if (!is.null(input$file_reimport)) {
      req(FALSE)
    }
    DT::datatable(rv$df,
      editable = TRUE,
      options = list(
        paging = FALSE,
        searching = FALSE
      ),
      rownames = FALSE
    )
  })

  shiny::observeEvent(input$file_reimport, {
    file_extension <- tolower(tools::file_ext(input$file_reimport$datapath))

    if (file_extension == "csv") {
      rv$unique <- reimport_csv(input$file_reimport$datapath)
    } else if (file_extension == "ris") {
      rv$unique <- reimport_ris(input$file_reimport$datapath)
    } else {
      warning("Invalid file extension, needs to be .ris or .csv")
    }
  })

  ## Update filters
  shiny::observe({
    if (!is.null(rv$unique)) {
      shiny::updateSelectInput(inputId = "sources_visual", choices = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "labels_visual", choices = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "strings_visual", choices = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique())

      shiny::updateSelectInput(inputId = "sources_tables", choices = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_source) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "labels_tables", choices = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "strings_tables", choices = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_string) %>% stringr::str_split(", ") %>% unlist() %>% unique())

      shiny::updateSelectInput(inputId = "summary_search_labels", choices = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique())
      shiny::updateSelectInput(inputId = "summary_screening_labels", choices = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique() %>% sort(), selected = unique(rv$unique$cite_label) %>% stringr::str_split(", ") %>% unlist() %>% unique())
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
      dplyr::group_by(file.name) %>%
      dplyr::summarise(min_row = dplyr::first(rowname), max_row = dplyr::last(rowname))

    rows <- row_indexes[info$row, 2:3]
    col <- paste0("cite_", names(rv$df[info$col + 1]))
    file <- rv$df[info$row, 1]

    rv$upload_df[c(rows$min_row:rows$max_row), col] <- val
  })

  # Deduplication tab ----

  # when dedup button clicked, deduplicate
  shiny::observeEvent(input$identify_dups, {
    if (nrow(rv$upload_df) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import your citations first.",
        type = "error"
      )
      # Stop plotting
      req(FALSE)
    }

    dedup_results <- dedup_citations(rv$upload_df, manual = TRUE, shiny_progress = TRUE)


    rv$unique <- dedup_results$unique
    rv$manual <- dedup_results$manual_dedup

    n_citations <- nrow(rv$upload_df)
    n_unique <- nrow(rv$unique)
    n_pairs_manual <- nrow(rv$manual)
    shinyalert::shinyalert("Auto-deduplication complete",
      paste("From a total of", n_citations, "citations added, there are", n_unique, "unique citations. Head to the manual deduplication
                   tab to check ", n_pairs_manual, " potential duplicates"),
      type = "success"
    )
  })

  # Action button: remove manually selected duplicates
  shiny::observeEvent(c(input$manualdedupsubmit, input$nomanualdedup), {
    if (input$nomanualdedup > 0) {
      rv$post_manual_dedup <- rv$unique
    } else if (input$manualdedupsubmit > 0) {
      removeManual <- rv$manual %>%
        dplyr::select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2, record_id1, record_id2)

      duplicates <- removeManual[input$manual_dedup_dt_rows_selected, ]

      if (nrow(duplicates) < 1) {
        shinyalert::shinyalert("Oops!", "You haven't selected any duplicate pairs to remove.", type = "error")
        return()
      }

      unique_citations <- rv$unique
      after <- dedup_citations_add_manual(rv$upload_df,
        additional_pairs = duplicates
      )

      rv$post_manual_dedup <- after

      shinyalert::shinyalert("Manual deduplication complete",
        paste(
          "From a total of", as.numeric(length(rv$upload_df)), "citations uploaded, there are", as.numeric(length(after$duplicate_id)),
          "unique citations after automated and manual deduplication.
                           Compare citations across sources, labels, and strings in the visualisation tab"
        ),
        type = "success"
      )
    }
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
    DT::datatable(rv$manual,
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


  # Action: ASySD manual dedup pre text ----
  output$Manual_pretext <- shiny::renderText({
    manualrefs <- rv$manual
    manualrefs <- as.numeric(length(manualrefs$record_id1))

    paste(manualrefs, "pairs of citations require manual deduplication. Review the pairs in the table
        below. You can scroll right to see all citation metadata and hover over any cell to see truncated text. Identical and near-identical fields are highlighted in green.
        Select all rows which contain duplicate pairs and click the button below to remove extra
        duplicates.")
  })


  # # display results of deduplication
  # output$dedup_results <- renderDataTable({
  #
  #   gr
  # })


  #### Visualise tab ####

  unique_filtered_visual <- shiny::reactive({
    sources <- input$sources_visual %>% paste(collapse = "|")
    strings <- input$strings_visual %>% paste(collapse = "|")
    labels <- input$labels_visual %>% paste(collapse = "|")
    if (sources == "") sources <- ".*"
    if (strings == "") strings <- ".*"
    if (labels == "") labels <- ".*"
    out <- rv$post_manual_dedup %>%
      dplyr::filter(
        .data$cite_source == "" | stringr::str_detect(.data$cite_source, sources),
        .data$cite_string == "" | stringr::str_detect(.data$cite_string, strings),
        .data$cite_label == "" | stringr::str_detect(.data$cite_label, labels)
      )

    out$cite_source <- stringr::str_extract_all(out$cite_source, sources) %>%
      purrr::map_chr(~ paste(.x, collapse = ", "))
    out$cite_label <- stringr::str_extract_all(out$cite_label, labels) %>%
      purrr::map_chr(~ paste(.x, collapse = ", "))
    out$cite_string <- stringr::str_extract_all(out$cite_string, strings) %>%
      purrr::map_chr(~ paste(.x, collapse = ", "))

    out
  })

  output$plotgraph1 <- plotly::renderPlotly({
    if (nrow(rv$post_manual_dedup) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      req(FALSE)
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
    if (nrow(rv$post_manual_dedup) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      req(FALSE)
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
    {
      input$generateRecordTable
      input$generateSummaryTable
    },
    {
      sources <- input$sources_tables %>% paste(collapse = "|")
      strings <- input$strings_tables %>% paste(collapse = "|")
      labels <- input$labels_tables %>% paste(collapse = "|")
      if (sources == "") sources <- ".*"
      if (strings == "") strings <- ".*"
      if (labels == "") labels <- ".*"
      out <- rv$post_manual_dedup %>%
        dplyr::filter(
          .data$cite_source == "" | stringr::str_detect(.data$cite_source, sources),
          .data$cite_string == "" | stringr::str_detect(.data$cite_string, strings),
          .data$cite_label == "" | stringr::str_detect(.data$cite_label, labels)
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
    if (nrow(rv$post_manual_dedup) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      req(FALSE)
    }
    citations <- unique_filtered_table()
    citations$source <- citations$cite_source
    record_level_table(citations = citations, return = "DT")
  })

  summary_filtered_table <- shiny::eventReactive(input$generateSummaryTable, {
    citations <- unique_filtered_table()
    citation_summary_table(citations,
      search_label = input$summary_search_labels,
      screening_label = input$summary_screening_labels
    )
  })

  output$summaryTab <- gt::render_gt({
    if (nrow(rv$post_manual_dedup) == 0) {
      shinyalert::shinyalert("Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      # Stop plotting
      req(FALSE)
    }
    summary_filtered_table()
  })


  #### Export tab ####

  # # Downloadable bibtex ----
  # Downloadable bibtex ----
  output$downloadCsv <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (nrow(rv$post_manual_dedup) > 0) {
        write.csv(manual_dedup_result, file)
      } else {
        stop("No data to download!")
        req(FALSE)
      }
    }
  )
  output$downloadBib <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".bib", sep = "")
    },
    content = function(file) {
      export_bib(rv$post_manual_dedup, file)
    }
  )

  output$downloadRis <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".bib", sep = "")
    },
    content = function(file) {
      export_ris(rv$post_manual_dedup, file)
    }
  )
}

# Create Shiny app ----
shiny::shinyApp(ui, server)
