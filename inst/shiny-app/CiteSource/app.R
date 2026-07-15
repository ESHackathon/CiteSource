options(shiny.maxRequestSize=10000*1024^2, timeout = 40000000) 

library(DT)
library(CiteSource)
library(dplyr)

# Source local plots.R so the app picks up changes without a full package rebuild
local({
  f <- normalizePath("../../../R/plots.R", mustWork = FALSE)
  if (file.exists(f)) source(f, local = FALSE)
})

columns2hide <- c("title", "author", "doi", "volume",
                  "pages", "number", "year", "abstract", "journal", "isbn")

# Google Analytics: set CITESOURCE_ENV=production or CITESOURCE_ENV=dev at deployment
.ga_file <- local({
  switch(Sys.getenv("CITESOURCE_ENV", "local"),
    production = "google_analytics_main.html",
    dev        = "google_analytics_dev.html",
    NULL
  )
})

# Vectorized helper: split a multi-value column and keep only items in keep_set
.filter_multivalue_col <- function(vals, keep_set) {
  items_list <- strsplit(as.character(vals), ",\\s*")
  vapply(items_list, function(items) {
    items <- items[!is.na(items) & items != ""]
    if (length(keep_set) > 0) items <- items[items %in% keep_set]
    paste(unique(items), collapse = ", ")
  }, character(1))
}


# ---- Define UI ----
ui <- shiny::navbarPage("CiteSource",
                        id = "tabs",
                        fluid = TRUE,
                        header = shiny::tagList(
                          shinybusy::add_busy_spinner(spin = "circle"),
                          shinyjs::useShinyjs(),
                          tags$head(
                            tags$link(rel = "icon", type = "image/png", href = "www/favicon.png"),
                            # Toastr CSS and JS
                            tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.css"),
                            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.js"),
                            tags$script(HTML("
                              toastr.options = {
                                'closeButton': true,
                                'progressBar': true,
                                'positionClass': 'toast-top-right',
                                'timeOut': '5000',
                                'extendedTimeOut': '2000',
                                'showMethod': 'fadeIn',
                                'hideMethod': 'fadeOut',
                                'escapeHtml': false
                              };
                            ")),
                            # Field preference button JS handler
                            tags$script(HTML("
                            $(document).on('click', '.btn-field-preference', function(e) {
                              var clickedBtn = $(this);
                              var buttonId = clickedBtn.attr('id');
                              var parts = buttonId.split('_');
                              if (parts.length >= 4) {
                                var recordType = parts[parts.length - 1];
                                var field = parts[parts.length - 2];
                                var idxParts = parts.slice(2, parts.length - 2);
                                var pairIdx = idxParts.join('_');
                                var partnerType = (recordType === 'A') ? 'B' : 'A';
                                var baseId = parts.slice(0, parts.length - 1).join('_');
                                var partnerId = baseId + '_' + partnerType;
                                var partnerBtn = $('#' + partnerId);
                                var isAlreadySelected = clickedBtn.hasClass('selected');
                                var action = '';
                                if (isAlreadySelected) {
                                  clickedBtn.removeClass('selected');
                                  clickedBtn.css({'background-color':'white','color':'#333','border':'1px solid #ddd','font-weight':'normal'});
                                  clickedBtn.html('<i class=\"fa fa-check\"></i> Use This');
                                  action = 'clear';
                                } else {
                                  clickedBtn.addClass('selected');
                                  clickedBtn.css({'background-color':'white','color':'#2d8659','border':'2px solid #2d8659','font-weight':'bold'});
                                  clickedBtn.html('<i class=\"fa fa-check\"></i> Selected');
                                  partnerBtn.removeClass('selected');
                                  partnerBtn.css({'background-color':'white','color':'#333','border':'1px solid #ddd','font-weight':'normal'});
                                  partnerBtn.html('<i class=\"fa fa-check\"></i> Use This');
                                  action = 'select';
                                }
                                Shiny.setInputValue('field_preference_click', {
                                  pair_idx: pairIdx,
                                  field: field,
                                  record: (action === 'select') ? recordType : 'clear',
                                  nonce: Math.random()
                                });
                              }
                            });
                            ")),
                            tags$style(HTML("
                              h6, .h6, h5, .h5, h4, .h4, h3, .h3, h2, .h2, h1, .h1 {
                                margin-top: 0;
                                margin-bottom: .5rem;
                                font-weight: 500;
                                line-height: 1.2;
                                color: #23395B;
                              }
                              .dedup-card { border: 2px solid #dee2e6; border-radius: 8px; padding: 8px; margin-bottom: 10px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); font-size: 0.9em; }
                              .dedup-card.record-a { border-left: 4px solid #008080; }
                              .dedup-card.record-b { border-left: 4px solid #23395B; }
                              .dedup-field { margin-bottom: 6px; padding: 4px 6px; border-radius: 4px; min-height: 32px; }
                              .dedup-field.match { background-color: #d4edda; border-left: 3px solid #82D173; }
                              .dedup-field.different { background-color: #fff3cd; border-left: 3px solid #ffc107; }
                              .dedup-field.missing { background-color: #f8d7da; border-left: 3px solid #dc3545; }
                              .dedup-field-label { font-weight: bold; color: #23395B; margin-bottom: 2px; font-size: 0.85em; display: flex; justify-content: space-between; align-items: center; }
                              .dedup-field-value { min-height: 20px; word-wrap: break-word; overflow-wrap: break-word; }
                              .dedup-similarity-badge { display: inline-block; padding: 5px 15px; border-radius: 20px; font-weight: bold; margin-bottom: 15px; }
                              .dedup-similarity-high { background-color: #d4edda; color: #155724; }
                              .dedup-similarity-medium { background-color: #fff3cd; color: #856404; }
                              .dedup-similarity-low { background-color: #f8d7da; color: #721c24; }
                              .btn-field-preference { padding: 4px 8px; font-size: 0.8em; border-radius: 3px; cursor: pointer; transition: all 0.2s ease; margin-left: 8px; }
                              .btn-field-preference.selected { background-color: white !important; color: #2d8659 !important; border: 2px solid #2d8659 !important; font-weight: bold; }
                              .default-indicator { display: inline-block; margin-right: 4px; font-size: 0.75em; padding: 2px 6px; background-color: #e8f4f8; color: #0066cc; border-radius: 3px; border: 1px solid #0066cc; font-weight: 600; }
                              .field-preferred { box-shadow: 0 0 8px rgba(130, 209, 115, 0.3); }
                              /* Workflow stepper */
                              .workflow-stepper {
                                display: flex; align-items: center;
                                padding: 10px 24px; background: #ffffff;
                                border-bottom: 1px solid #dee2e6; margin: 0;
                              }
                              .ws-step {
                                display: flex; flex-direction: column; align-items: center;
                                cursor: pointer; min-width: 64px;
                              }
                              .ws-step:hover .ws-circle { filter: brightness(0.88); }
                              .ws-circle {
                                width: 28px; height: 28px; border-radius: 50%;
                                display: flex; align-items: center; justify-content: center;
                                font-weight: 700; font-size: 0.82em;
                                transition: filter 0.15s;
                              }
                              .ws-label {
                                font-size: 0.72em; margin-top: 4px;
                                font-weight: 500; white-space: nowrap;
                              }
                              .ws-step.ws-completed .ws-circle { background-color: #23395B; color: #ffffff; }
                              .ws-step.ws-completed .ws-label  { color: #23395B; }
                              .ws-step.ws-active .ws-circle    {
                                background-color: #008080; color: #ffffff;
                                box-shadow: 0 0 0 3px rgba(0,128,128,0.2);
                              }
                              .ws-step.ws-active .ws-label     { color: #008080; font-weight: 700; }
                              .ws-step.ws-pending .ws-circle   { background-color: #e9ecef; color: #adb5bd; cursor: default; }
                              .ws-step.ws-pending .ws-label    { color: #adb5bd; }
                              .ws-step.ws-pending:hover .ws-circle { filter: none; }
                              .ws-line {
                                flex: 1; height: 2px; min-width: 24px; margin-bottom: 22px;
                              }
                              .ws-line.ws-done    { background-color: #23395B; }
                              .ws-line.ws-pending { background-color: #e9ecef; }

                              /* ── Global typography ─────────────────────────────── */
                              body, .form-control, .selectize-input, label, button, select {
                                font-family: 'Inter', -apple-system, BlinkMacSystemFont,
                                             'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
                              }

                              /* ── Navbar ────────────────────────────────────────── */
                              .navbar { box-shadow: 0 2px 10px rgba(0,0,0,0.12); }
                              .navbar-brand { font-weight: 700; letter-spacing: 0.2px; }

                              /* ── Sidebar / .well ───────────────────────────────── */
                              #sidebar, #sidebar_tables { background-color: #ffffff; }
                              .well {
                                background-color: #ffffff !important;
                                border: 1px solid #dde3ed !important;
                                border-radius: 10px !important;
                                box-shadow: 0 2px 8px rgba(0,0,0,0.05) !important;
                              }

                              /* ── bslib cards ───────────────────────────────────── */
                              .card {
                                border-radius: 10px !important;
                                border: 1px solid #dde3ed !important;
                                box-shadow: 0 2px 10px rgba(0,0,0,0.06) !important;
                              }
                              .card-header {
                                font-weight: 600;
                                font-size: 0.92em;
                                letter-spacing: 0.15px;
                                border-bottom: 1px solid #dde3ed !important;
                              }

                              /* ── Form controls ─────────────────────────────────── */
                              .form-control {
                                border-radius: 6px;
                                transition: border-color 0.15s, box-shadow 0.15s;
                              }
                              .form-control:focus {
                                box-shadow: 0 0 0 0.2rem rgba(0,128,128,0.18);
                              }
                              .selectize-input { border-radius: 6px !important; }

                              /* ── Buttons ───────────────────────────────────────── */
                              .btn { border-radius: 6px; font-weight: 500; }
                              a.shiny-download-link.btn { font-size: 0.88em; }

                              /* ── Home tab pills ────────────────────────────────── */
                              .nav-pills .nav-link { border-radius: 20px; font-weight: 500; }

                              /* ── Tab content ───────────────────────────────────── */
                              .tab-pane { padding-top: 4px; }

                              /* ── DataTables ────────────────────────────────────── */
                              .dataTables_wrapper { font-size: 0.92em; }

                              /* ── Links ─────────────────────────────────────────── */
                              a { color: #008080; }
                              a:hover { color: #006666; }

                              /* ── Accordion ─────────────────────────────────────── */
                              .accordion-button:not(.collapsed) {
                                color: #008080;
                                background-color: #f0fafa;
                              }
                              .accordion-button:focus {
                                box-shadow: 0 0 0 0.2rem rgba(0,128,128,0.18);
                              }

                              /* Home tab docs: full-width reading area (avoid narrow 900px column) */
                              html { scroll-behavior: smooth; }
                              #shiny-tab-home .home-tab-wrap {
                                width: 100%;
                                max-width: 100%;
                                box-sizing: border-box;
                                padding: 0.5rem 0 1.25rem;
                              }
                              #shiny-tab-home .home-doc-pane {
                                width: 100%;
                                max-width: 100%;
                                margin: 0 auto;
                                box-sizing: border-box;
                                padding: 0.75rem clamp(0.25rem, 2vw, 2rem) 1.25rem;
                                line-height: 1.65;
                                font-size: 1.02rem;
                              }
                              #shiny-tab-home .home-doc-pane img {
                                max-width: min(280px, 100%);
                                height: auto;
                              }
                              #shiny-tab-home .guide-toc {
                                font-size: 0.9rem;
                              }
                              #shiny-tab-home .guide-toc a {
                                color: #3d4f63;
                                text-decoration: none;
                                display: block;
                                padding: 0.28rem 0;
                                border-radius: 4px;
                              }
                              #shiny-tab-home .guide-toc a:hover {
                                color: #008080;
                                background-color: rgba(0, 128, 128, 0.06);
                                padding-left: 6px;
                              }
                            ")),
                            if (!is.null(.ga_file) && file.exists(.ga_file)) includeHTML(.ga_file)
                          ),
                          # Workflow progress stepper (hidden on Home tab)
                          shiny::conditionalPanel(
                            condition = "input.tabs !== 'Home'",
                            shiny::uiOutput("workflow_stepper")
                          )
                        ),
                        theme = bslib::bs_theme(
                          bg           = "rgb(248, 249, 251)",
                          primary      = "#008080",
                          secondary    = "#CBF7ED",
                          success      = "#23395B",
                          info         = "#82D173",
                          warning      = "#FFC07F",
                          danger       = "#008080",
                          font_scale   = NULL,
                          bootswatch   = "cerulean",
                          fg           = "#1a1a2e",
                          input_bg     = "#ffffff",
                          input_border_color = "#ced4da",
                          base_font    = bslib::font_google("Inter", wght = "300..700")
                        ),
                        # Home tab ----
                        shiny::tabPanel(
                          "Home",
                          shiny::div(
                            class = "home-tab-wrap",
                            shiny::tabsetPanel(
                              type = "pills",
                              shiny::tabPanel(
                                title = "About",
                                shiny::div(
                                  class = "home-doc-pane",
                                  htmltools::includeMarkdown("www/about.md")
                                )
                              ),
                              shiny::tabPanel(
                                title = "Use Cases",
                                shiny::div(
                                  class = "home-doc-pane",
                                  htmltools::includeMarkdown("www/use-cases.md")
                                )
                              ),
                              shiny::tabPanel(
                                title = "User Guide",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shiny::div(
                                      class = "guide-toc d-none d-md-block",
                                      style = "position: sticky; top: 1rem; align-self: flex-start;",
                                      shiny::tags$p(
                                        shiny::tags$strong("Jump to a step"),
                                        class = "small text-muted mb-2"
                                      ),
                                      shiny::tags$ul(
                                        class = "list-unstyled mb-0 ps-0",
                                        shiny::tags$li(shiny::tags$a("Step 1 — Upload files", href = "#step-1")),
                                        shiny::tags$li(shiny::tags$a("Step 2 — Auto deduplication", href = "#step-2")),
                                        shiny::tags$li(shiny::tags$a("Step 3 — Manual deduplication", href = "#step-3")),
                                        shiny::tags$li(shiny::tags$a("Step 4 — Visualise overlap", href = "#step-4")),
                                        shiny::tags$li(shiny::tags$a("Step 5 — Summary tables", href = "#step-5")),
                                        shiny::tags$li(shiny::tags$a("Step 6 — Export results", href = "#step-6"))
                                      )
                                    )
                                  ),
                                  shiny::column(
                                    width = 9,
                                    shiny::div(
                                      class = "home-doc-pane",
                                      htmltools::includeMarkdown("www/user_guide.md")
                                    )
                                  )
                                )
                              )
                            )
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
                                                     shiny::h5("Step 1: Upload citation files"),
                                                     shiny::p("New search/database exports to deduplicate.",
                                                              style = "font-size:0.8em;color:#6c757d;margin-top:-4px;margin-bottom:6px;"),
                                                     shiny::fileInput("file", "Add new files (.ris, .bib, .txt)",
                                                                      multiple = TRUE,
                                                                      accept = c(".ris", ".txt", ".bib")
                                                     ),
                                                     shiny::hr(),
                                                     shiny::h5("Re-upload a CiteSource export"),
                                                     shiny::p(
                                                       "A previously deduplicated set (.csv or .ris) to keep working with — view its sources below, add new files above to merge in, or finish manual review by also re-uploading a candidate-pairs .csv.",
                                                       style = "font-size:0.8em;color:#6c757d;margin-top:-4px;margin-bottom:6px;"),
                                                     shiny::fileInput("file_reimport", "Re-upload exported file(s)",
                                                                      multiple = TRUE,
                                                                      accept = c(".ris", ".csv")
                                                     )
                                ),
                                # Main panel for displaying outputs ----
                                shiny::mainPanel(
                                  shiny::uiOutput("reimport_summary"),
                                  shiny::uiOutput("metadata_form"),
                                  shiny::uiOutput("post_upload_guide")
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
                              shiny::p("Already re-uploaded a deduplicated set? Add new citation files on the File upload tab, then click Find duplicates to merge them into the existing set.",
                                       style = "font-size:0.82em;color:#6c757d;"),
                              
                              # Action button: identify duplicates in uploaded dataset
                              shinyWidgets::actionBttn(
                                "identify_dups", "Find duplicates",
                                style = "jelly",
                                color = "primary",
                                icon = shiny::icon("search")
                              ) %>% htmltools::tagAppendAttributes(style = "background-color: #008080; margin-right: 20px"),
                              shiny::br(),
                              shiny::uiOutput("dedup_summary_card")
                            ),
                            shiny::tabPanel(
                              "Manual deduplication",
                              br(),
                              shiny::h5("Step 4: Review potential duplicates manually"),
                              shiny::textOutput("Manual_pretext"),
                              shiny::br(),

                              # Action buttons (always visible)
                              shiny::div(
                                style = "text-align: right; margin-bottom: 15px;",
                                shinyWidgets::actionBttn(
                                  inputId = "manualdedupsubmit",
                                  label = "Remove Selected Duplicates",
                                  style = "jelly",
                                  icon = shiny::icon("trash"),
                                  color = "primary",
                                  size = "sm"
                                ) %>% htmltools::tagAppendAttributes(style = "background-color: #23395B; margin-right: 10px;"),
                                shinyWidgets::actionBttn(
                                  inputId = "nomanualdedup",
                                  label = "Go to Visualisations",
                                  style = "jelly",
                                  icon = shiny::icon("arrow-right"),
                                  color = "primary",
                                  size = "sm"
                                ) %>% htmltools::tagAppendAttributes(style = "background-color: #82D173;")
                              ),

                              # Options & Filters accordion
                              bslib::accordion(
                                open = FALSE,
                                bslib::accordion_panel(
                                  title = "Options & Filters",
                                  icon = shiny::icon("sliders-h"),
                                  shiny::fluidRow(
                                    shiny::column(
                                      12,
                                      shinyWidgets::prettyRadioButtons(
                                        inputId = "dedup_view_mode",
                                        label = shiny::tags$strong("View Mode:"),
                                        choices = c("Card View" = "card", "Table View" = "table"),
                                        selected = "card",
                                        inline = TRUE,
                                        status = "primary"
                                      )
                                    )
                                  ),
                                  shiny::hr(),
                                  # Card View filters (conditional)
                                  shiny::conditionalPanel(
                                    condition = "input.dedup_view_mode == 'card'",
                                    shiny::tags$h6(shiny::icon("filter"), " Card Navigation"),
                                    shiny::fluidRow(
                                      shiny::column(
                                        4,
                                        shiny::sliderInput(
                                          inputId = "similarity_filter",
                                          label = "Min Similarity",
                                          min = 0, max = 100, value = 0, step = 5, post = "%", width = "100%"
                                        )
                                      ),
                                      shiny::column(
                                        4,
                                        shiny::selectInput(
                                          inputId = "similarity_sort",
                                          label = "Sort by Similarity",
                                          choices = list("Highest First" = "desc", "Lowest First" = "asc"),
                                          selected = "desc", width = "100%"
                                        )
                                      ),
                                      shiny::column(4, shiny::uiOutput("dedup_progress"))
                                    )
                                  ),
                                  # Table View options (conditional)
                                  shiny::conditionalPanel(
                                    condition = "input.dedup_view_mode == 'table'",
                                    shiny::tags$h6(shiny::icon("columns"), " Table Columns"),
                                    shinyWidgets::pickerInput(
                                      inputId = "manual_dedup_cols",
                                      label = "Choose columns",
                                      choices = NULL, selected = NULL, multiple = TRUE,
                                      options = list(`live-search` = TRUE, `actions-box` = TRUE, style = "btn-primary"),
                                      width = "100%"
                                    )
                                  )
                                )
                              ),
                              shiny::br(),

                              # Card View output
                              shiny::conditionalPanel(
                                condition = "input.dedup_view_mode == 'card'",
                                shiny::uiOutput("dedup_card_view")
                              ),

                              # Table View output
                              shiny::conditionalPanel(
                                condition = "input.dedup_view_mode == 'table'",
                                DT::DTOutput("manual_dedup_dt"),
                                tags$style(HTML(".table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: #CBF7ED!important; color: black!important}"))
                              )
                            ),
                            shiny::tabPanel(
                              "How deduplication works",
                              shiny::fluidRow(
                                shiny::column(
                                  12,
                                  shiny::uiOutput("dedup_logic_guide")
                                )
                              )
                            )
                          )
                        ),
                        shiny::tabPanel(
                          "Visualise",
                          
                          # Sidebar layout ----
                          shiny::sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            shiny::sidebarPanel(
                              width = 2,
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
                              ),
                              shiny::tags$small(
                                style = "color: #6c757d; display: block; margin-top: 6px;",
                                shiny::tags$i(class = "fa fa-link", style = "margin-right: 4px;"),
                                "Filters sync with the Tables tab"
                              )
                            ),

                            # Main panel for displaying outputs ----
                            shiny::mainPanel(
                              shiny::uiOutput("visualise_empty_state"),
                              shiny::tags$div(
                                id = "visualise_tabs_wrapper",
                                # Per-tab controls: shown conditionally based on active tab
                                shiny::conditionalPanel(
                                  "input.vis_tabs === 'Plot overlap as a heatmap matrix'",
                                  shiny::div(
                                    style = "padding: 6px 0 10px 0;",
                                    shiny::checkboxInput(
                                      "heatmap_log_scale",
                                      shiny::tagList(
                                        "Log scale colors ",
                                        shiny::tags$i(
                                          class = "fa fa-question-circle",
                                          title = "Applies a log transform to the color gradient so small overlaps remain visible when one source has far more records than others.",
                                          style = "color:#6c757d; cursor:help;"
                                        )
                                      ),
                                      value = FALSE
                                    )
                                  )
                                ),
                                shiny::tabsetPanel(
                                  id = "vis_tabs",
                                  shiny::tabPanel(
                                    "Plot overlap as a heatmap matrix",
                                    shiny::br(),
                                    shiny::downloadButton("downloadHeatPlot"),
                                    bslib::card(
                                      bslib::card_body(shiny::uiOutput("heatmapUI"))
                                    )
                                  ),
                                  shiny::tabPanel(
                                    "Plot overlap as an upset plot",
                                    shiny::br(),
                                    shiny::fluidRow(
                                      shiny::column(3,
                                        shiny::sliderInput("upset_nsets", "Max sources shown",
                                                           min = 2, max = 20, value = 10, step = 1),
                                        shiny::tags$small(
                                          style = "color:#6c757d; display:block; margin-top:-8px; margin-bottom:8px;",
                                          "Display only — to change which sources are analyzed, use the filter on the left."
                                        )
                                      ),
                                      shiny::column(3,
                                        shiny::sliderInput("upset_nintersects", "Max intersections shown",
                                                           min = 5, max = 100, value = 40, step = 5)
                                      ),
                                      shiny::column(2,
                                        shiny::div(style = "margin-top:24px;",
                                          shiny::actionButton("apply_upset", "Apply",
                                            icon = shiny::icon("rotate-right"),
                                            class = "btn-primary btn-sm")
                                        )
                                      ),
                                      shiny::column(4,
                                        shiny::div(style = "margin-top:24px; text-align:right;",
                                          shiny::downloadButton("downloadUpsetPlot")
                                        )
                                      )
                                    ),
                                    bslib::card(
                                      bslib::card_body(shiny::plotOutput("plotgraph2"))
                                    )
                                  ),
                                  shiny::tabPanel(
                                    "Phase Analysis",
                                    shiny::br(),
                                    shiny::downloadButton("downloadPhasePlot"),
                                    bslib::card(
                                      bslib::card_body(shiny::plotOutput("phasePlot"))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),

                        shiny::tabPanel(
                          "Tables",

                          shiny::sidebarLayout(

                            shiny::sidebarPanel(
                              id = "sidebar_tables",
                              width = 2,
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
                              ),
                              shiny::tags$small(
                                style = "color: #6c757d; display: block; margin-top: 6px;",
                                shiny::tags$i(class = "fa fa-link", style = "margin-right: 4px;"),
                                "Filters sync with the Visualise tab"
                              )
                            ),

                            shiny::mainPanel(
                              shiny::uiOutput("tables_empty_state"),
                              shiny::tags$div(
                                id = "tables_tabs_wrapper",
                                shiny::tabsetPanel(
                                  shiny::tabPanel(
                                    "Detailed Record Table",
                                    shiny::div("Summary of unique and non-unique records by source."),
                                    shiny::br(),
                                    gt::gt_output("detailedRecordTab")
                                  ),

                                  shiny::tabPanel(
                                    "Precision/Sensitivity Table",
                                    shiny::div("Precision and Sensitivity of records across screening phases. Only available when data includes a 'final' screening label."),
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
                          )
                        ),

                        shiny::tabPanel(
                          "Export",
                          shiny::div(
                            style = "padding: 20px;",
                            shiny::uiOutput("export_empty_state"),
                            shiny::tags$div(
                              id = "export_content_wrapper",
                              shiny::h5("Step 7: Export your results"),
                              shiny::br(),
                              shiny::fluidRow(
                                shiny::column(
                                  8,
                                  shiny::wellPanel(
                                    style="background:#f8f9fa;border:1px solid #dee2e6;",
                                    shiny::h5("Cite CiteSource", style="margin-top:0;"),
                                    shiny::p("If you use these results in a publication, please cite the software:"),
                                    shiny::tags$code("Riley, T., Young, S., Paxton, A., Wallrich, L., Hair, K., & Grainger, M. (2026). CiteSource: An R package for data-driven search strategy development and enhanced evidence synthesis reporting. Research Synthesis Methods. https://doi.org/10.1017/rsm.2026.10084"),
                                    shiny::br(), shiny::br(),
                                    shiny::a("View Publication",
                                      href="https://doi.org/10.1017/rsm.2026.10084", target="_blank")
                                  ),
                                  shiny::br(),
                                  bslib::card(
                                    bslib::card_header(
                                      shiny::tags$i(class="fa fa-file-alt", style="margin-right:7px;"),
                                      "Citations"
                                    ),
                                    bslib::card_body(
                                      shiny::p("Download the deduplicated citation set.",
                                        style="color:#6c757d;font-size:0.88em;margin-bottom:10px;"),
                                      shiny::div(
                                        style="font-size:0.82em;color:#856404;background:#fff8e1;padding:8px 12px;border-radius:4px;margin-bottom:14px;",
                                        shiny::tags$i(class="fa fa-info-circle", style="margin-right:5px;"),
                                        "Only .ris and .csv can be re-imported into CiteSource."
                                      ),
                                      shiny::tags$strong("CSV field selection", style="font-size:0.88em;"),
                                      shiny::radioButtons(
                                        "csv_fields_preset",
                                        label = NULL,
                                        choices = c(
                                          "Full — all fields (reimportable into CiteSource)" = "full",
                                          "Standard — title, author, year, journal, volume, issue, pages, doi, url, abstract, keywords, type, isbn, issn, cite_source, cite_label, cite_string" = "standard",
                                          "Custom — choose columns" = "custom"
                                        ),
                                        selected = "full"
                                      ),
                                      shiny::uiOutput("csv_custom_cols_ui"),
                                      shiny::uiOutput("csv_reimport_warning"),
                                      shiny::downloadButton("downloadCsv", "CSV",
                                        style="margin-right:6px;margin-bottom:4px;"),
                                      shiny::downloadButton("downloadRis", "RIS",
                                        style="margin-right:6px;margin-bottom:4px;"),
                                      shiny::downloadButton("downloadBib", "BibTeX",
                                        style="margin-bottom:4px;"),
                                      shiny::tags$hr(style="margin:14px 0 10px 0;"),
                                      shiny::tags$strong("Dedup provenance log",
                                        style="font-size:0.88em;display:block;margin-bottom:4px;"),
                                      shiny::p(
                                        "CSV of every merged duplicate pair, flagged as automated or manual.",
                                        style="color:#6c757d;font-size:0.82em;margin-bottom:8px;"),
                                      shiny::downloadButton("downloadDedupLog", "Dedup Log (CSV)",
                                        style="margin-bottom:4px;"),
                                      shiny::tags$hr(style="margin:14px 0 10px 0;"),
                                      shiny::tags$strong("Manual review candidates",
                                        style="font-size:0.88em;display:block;margin-bottom:4px;"),
                                      shiny::p(
                                        "CSV of unresolved candidate pairs. Re-upload it together with the citations CSV to finish manual deduplication later.",
                                        style="color:#6c757d;font-size:0.82em;margin-bottom:8px;"),
                                      shiny::downloadButton("downloadCandidates", "Candidate Pairs (CSV)",
                                        style="margin-bottom:4px;")
                                    )
                                  ),
                                  shiny::br(),
                                  bslib::card(
                                    bslib::card_header(
                                      shiny::tags$i(class="fa fa-chart-bar", style="margin-right:7px;"),
                                      "Plots"
                                    ),
                                    bslib::card_body(
                                      shiny::p("Download visualisations as PNG. Content reflects current filter selections on the Visualise tab.",
                                        style="color:#6c757d;font-size:0.88em;margin-bottom:12px;"),
                                      shiny::downloadButton("export_heatplot",  "Overlap Heatmap",
                                        style="margin-right:6px;margin-bottom:4px;"),
                                      shiny::downloadButton("export_upsetplot", "Upset Plot",
                                        style="margin-right:6px;margin-bottom:4px;"),
                                      shiny::downloadButton("export_phaseplot", "Phase Analysis",
                                        style="margin-bottom:4px;")
                                    )
                                  ),
                                  shiny::br(),
                                  bslib::card(
                                    bslib::card_header(
                                      shiny::tags$i(class="fa fa-table", style="margin-right:7px;"),
                                      "Tables"
                                    ),
                                    bslib::card_body(
                                      shiny::p("Download the detailed record table as CSV (reflects current filter selections on the Tables tab).",
                                        style="color:#6c757d;font-size:0.88em;margin-bottom:12px;"),
                                      shiny::downloadButton("exportDetailedTable", "Detailed Record Table",
                                        style="margin-bottom:4px;")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {

  # --- Toastr notification helper ---
  show_toastr <- function(title, message, type = "info") {
    toastr_type <- switch(type, success = "success", error = "error", warning = "warning", "info")
    timeout_ms  <- if (type == "error") 10000 else 5000
    ext_timeout <- if (type == "error") 4000  else 2000
    escape_js <- function(x) {
      x <- as.character(x)
      x <- gsub("\n", "<br>", x, fixed = TRUE)
      x <- gsub("\\\\", "\\\\\\\\", x)
      x <- gsub("'",  "\\'",  x, fixed = TRUE)
      x <- gsub('"', '\\"', x, fixed = TRUE)
      x
    }
    js <- paste0("toastr.", toastr_type, "('", escape_js(message), "','", escape_js(title),
                 "',{'timeOut':", timeout_ms, ",'extendedTimeOut':", ext_timeout,
                 ",'closeButton':true,'progressBar':true,'escapeHtml':false});")
    shinyjs::runjs(js)
  }

  # --- Reactive Values ---
  # Used to store data that changes during the session
  rv <- shiny::reactiveValues()
  rv$df <- data.frame()
  rv$upload_df <- data.frame()#for original uploads
  rv$latest_unique <- data.frame()#for reimported data
  rv$pairs_to_check <- data.frame()#for potential duplicates/manual dedup
  rv$pairs_removed <- data.frame()#for removed records
  rv$auto_pairs    <- data.frame()#auto-merged pairs, for the dedup log
  rv$existing_dedup_present <- FALSE # TRUE when latest_unique is a reimported deduped set that new uploads should be merged INTO (Goal 2)
  rv$file_meta           <- list()  # Per-file metadata (source/label/string); keyed by file.datapath
  # Card view state
  rv$selected_pairs_card <- integer(0)
  rv$current_pair_index  <- 1L
  rv$field_preferences   <- list()

  # ---- Workflow stepper ----
  output$workflow_stepper <- shiny::renderUI({
    shiny::req(input$tabs)

    has_data  <- (is.data.frame(rv$upload_df)    && nrow(rv$upload_df)    > 0) ||
                 (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    has_dedup <-  is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0

    steps <- list(
      list(n = 1L, label = "Upload",      tab = "File upload",  done = has_data),
      list(n = 2L, label = "Deduplicate", tab = "Deduplicate",  done = has_dedup),
      list(n = 3L, label = "Visualise",   tab = "Visualise",    done = FALSE),
      list(n = 4L, label = "Tables",      tab = "Tables",       done = FALSE),
      list(n = 5L, label = "Export",      tab = "Export",       done = FALSE)
    )

    tab_to_idx <- c("File upload" = 1L, "Deduplicate" = 2L,
                    "Visualise"   = 3L, "Tables"       = 4L, "Export" = 5L)
    cur_idx <- tab_to_idx[input$tabs]
    cur_idx <- if (is.na(cur_idx)) 0L else as.integer(cur_idx)

    items <- vector("list", length(steps) * 2L - 1L)
    for (i in seq_along(steps)) {
      s      <- steps[[i]]
      status <- if (i == cur_idx) "ws-active"
                else if (s$done || i < cur_idx) "ws-completed"
                else "ws-pending"

      circle_inner <- if (status == "ws-completed")
        shiny::tags$i(class = "fa fa-check", `aria-hidden` = "true")
      else
        as.character(s$n)

      # Pending steps are not clickable
      onclick_attr <- if (status != "ws-pending")
        sprintf("Shiny.setInputValue('stepper_nav','%s',{priority:'event'})", s$tab)
      else
        NULL

      items[[i * 2L - 1L]] <- shiny::tags$div(
        class   = paste("ws-step", status),
        onclick = onclick_attr,
        shiny::tags$div(class = "ws-circle", circle_inner),
        shiny::tags$div(class = "ws-label",  s$label)
      )

      if (i < length(steps)) {
        lc <- if (s$done || i < cur_idx) "ws-done" else "ws-pending"
        items[[i * 2L]] <- shiny::tags$div(class = paste("ws-line", lc))
      }
    }

    shiny::tags$div(class = "workflow-stepper", items)
  })

  shiny::observeEvent(input$stepper_nav, {
    shiny::updateNavbarPage(session, "tabs", selected = input$stepper_nav)
  }, ignoreInit = TRUE)
  # ---- End workflow stepper ----

  # 1. The Container (Decides WHEN to show it)
  output$post_upload_guide <- shiny::renderUI({
    # Only show this if the upload dataframe exists and has rows
    shiny::req(is.data.frame(rv$df) && nrow(rv$df) > 0)
    
    shiny::tagList(
      shiny::br(),
      shiny::hr(),
      shiny::h5("Tagging Overview"),
      # We refer to the table output created below
      shiny::tableOutput("guide_table_content") 
    )
  })
  
  # 2. The Table Content (Generates the table itself)
  output$guide_table_content <- shiny::renderTable({
    # We don't need the req() here because the UI above handles the hiding
    
    data.frame(
      "Column Name" = c("Source", "Label", "String"),
      "Description" = c(
        "'Source' is used to citations in files according to where the came from. This can include database names (e.g. Web of Science, Scopus) or a method used to find the citations (e.g. citation searching, numbered search string).",
        
        "'Label' is used to tag citations in files with information related to their associated screening phase. The label field requires one of three terms: ‘search’, ‘screened’, or ‘final’. All plots/tables require at least one file to be labeled as ‘search’, no other terms in the label field are permitted. <strong>NOTE: files that are tagged as 'screened' or 'final' should not have a 'source' tag.</strong>",
        
        "'String' is used to further differentiate sets of records. While the source/label fields alone can handle most use cases, the string field can be used to record other supplementary information a user may want to retain for analysis, or to further differentiate string variations (e.g. String1-narrow, String1-broad, String2-narrow, etc.)"
      ),
      check.names = FALSE
    )
  },
  striped = TRUE,
  hover = TRUE,
  width = "100%",
  align = "l",
  sanitize.text.function = function(x) x # Allows the <strong> tags to work
  )

  # --- Re-imported deduplicated set: read-only source overview ---------------
  # When a previously deduplicated/exported set is re-uploaded, show what is
  # already in it (sources, and any labels/strings) so the user can see the
  # existing content before adding more references. View only — this set is kept
  # separate from the newly uploaded files and its tags cannot be edited here.
  reimport_summary_data <- shiny::reactive({
    shiny::req(isTRUE(rv$existing_dedup_present),
               is.data.frame(rv$latest_unique), nrow(rv$latest_unique) > 0)

    # Count, per field value, the number of unique records that include it.
    # Tokens are de-duplicated within each record so a record merged from
    # several sources counts once per distinct source/label/string.
    tally_field <- function(col, field_name) {
      if (!col %in% names(rv$latest_unique)) return(NULL)
      vals <- rv$latest_unique[[col]]
      vals <- vals[!is.na(vals) & !vals %in% c("", "NA")]
      if (length(vals) == 0) return(NULL)
      toks <- unlist(lapply(strsplit(vals, ",\\s*"), function(t) unique(trimws(t))))
      toks <- toks[toks != "" & toks != "NA"]
      if (length(toks) == 0) return(NULL)
      tbl <- sort(table(toks), decreasing = TRUE)
      data.frame(Field = field_name, Value = names(tbl),
                 Records = as.integer(tbl), check.names = FALSE,
                 row.names = NULL, stringsAsFactors = FALSE)
    }

    out <- dplyr::bind_rows(
      tally_field("cite_source", "Source"),
      tally_field("cite_label",  "Label"),
      tally_field("cite_string", "String")
    )
    if (is.null(out) || nrow(out) == 0) return(NULL)
    out
  })

  output$reimport_summary <- shiny::renderUI({
    summ <- reimport_summary_data()
    if (is.null(summ)) return(NULL)

    flag <- rv$latest_unique$manual_dedup_complete
    reviewed <- length(flag) > 0 && as.character(flag[1]) %in% c("TRUE", "T", "1")

    bslib::card(
      bslib::card_header(
        shiny::tags$i(class = "fa fa-database", style = "margin-right:7px;"),
        "Re-imported deduplicated set"
      ),
      bslib::card_body(
        shiny::p(
          paste0(format(nrow(rv$latest_unique), big.mark = ","),
                 " unique records re-imported",
                 if (reviewed) " (manual deduplication marked complete)." else "."),
          style = "color:#6c757d;font-size:0.88em;margin-bottom:4px;"
        ),
        shiny::p(
          "View only — this set is kept separate from any new files you upload. Add new citation files on the left, then click Find duplicates to merge them in.",
          style = "color:#6c757d;font-size:0.8em;margin-bottom:10px;"
        ),
        shiny::tableOutput("reimport_summary_tbl")
      )
    )
  })

  output$reimport_summary_tbl <- shiny::renderTable(
    {
      summ <- reimport_summary_data()
      shiny::req(summ)
      summ
    },
    striped = TRUE, hover = TRUE, width = "100%", align = "l"
  )

  # --- Google Analytics Integration ---
  # Flag to ensure GA script is inserted only once per session
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

      # Seed file_meta for each new file (only if not already present)
      for (fi in seq_along(input$file$datapath)) {
        dp <- input$file$datapath[fi]
        if (is.null(rv$file_meta[[dp]])) {
          rv$file_meta[[dp]] <- list(
            source = suggested_source[fi],
            label  = "search",
            string = ""
          )
        }
      }
    }
  })
  
  
  ## Per-file metadata assignment form
  output$metadata_form <- shiny::renderUI({
    df_snap <- rv$df

    if (!is.data.frame(df_snap) || nrow(df_snap) == 0) {
      return(shiny::div(
        style = paste(
          "margin-top: 20px; padding: 30px 20px; background: #f8f9fa;",
          "border: 2px dashed #dee2e6; border-radius: 8px;",
          "text-align: center; color: #6c757d;"
        ),
        shiny::tags$i(class = "fa fa-upload",
                      style = "font-size: 2em; margin-bottom: 10px; display: block;"),
        shiny::p("Upload citation files on the left to get started.",
                 style = "margin-bottom: 0;")
      ))
    }

    # Read file_meta via isolate so edits don't re-trigger this renderUI
    fm <- shiny::isolate(rv$file_meta)

    header <- shiny::fluidRow(
      style = paste(
        "font-weight: 600; padding: 4px 8px; border-bottom: 2px solid #dee2e6;",
        "margin-bottom: 6px; color: #23395B; font-size: 0.85em;"
      ),
      shiny::column(3, "File"),
      shiny::column(1, shiny::div(style = "text-align:center;", "Records")),
      shiny::column(3, "Source"),
      shiny::column(2, "Label"),
      shiny::column(3, "String")
    )

    rows <- lapply(seq_len(nrow(df_snap)), function(i) {
      dp      <- df_snap$file.datapath[i]
      meta    <- fm[[dp]]
      cur_src <- if (!is.null(meta$source)) meta$source else df_snap$source[i]
      cur_lbl <- if (!is.null(meta$label))  meta$label  else "search"
      cur_str <- if (!is.null(meta$string)) meta$string else ""

      shiny::fluidRow(
        style = "border-bottom: 1px solid #f0f0f0; padding: 4px 0;",
        shiny::column(3,
          shiny::div(
            style = "font-size: 0.82em; word-break: break-all; padding-top: 8px; color: #23395B;",
            shiny::tags$i(class = "fa fa-file-alt",
                          style = "color: #aaa; margin-right: 4px;"),
            df_snap$file.name[i]
          )
        ),
        shiny::column(1,
          shiny::div(
            style = "text-align: center; padding-top: 8px; font-weight: 600; color: #23395B;",
            as.character(df_snap$records[i])
          )
        ),
        shiny::column(3,
          shiny::textInput(
            inputId     = paste0("file_source_", i),
            label       = NULL,
            value       = cur_src,
            placeholder = "e.g. Web of Science",
            width       = "100%"
          )
        ),
        shiny::column(2,
          shiny::selectInput(
            inputId  = paste0("file_label_", i),
            label    = NULL,
            choices  = c("search", "screened", "final"),
            selected = cur_lbl,
            width    = "100%"
          )
        ),
        shiny::column(3,
          shiny::textInput(
            inputId     = paste0("file_string_", i),
            label       = NULL,
            value       = cur_str,
            placeholder = "e.g. string1",
            width       = "100%"
          )
        )
      )
    })

    shiny::tagList(
      shiny::h5("Step 2: Assign metadata for each uploaded file"),
      shiny::div(
        style = paste(
          "background: white; border: 1px solid #dee2e6;",
          "border-radius: 6px; padding: 10px 12px;"
        ),
        header,
        do.call(shiny::tagList, rows),
        shiny::div(
          style = "margin-top: 10px; font-size: 0.82em; color: #6c757d;",
          shiny::tags$i(class = "fa fa-info-circle"),
          " Label at least one file as ",
          shiny::strong("'search'"),
          " to proceed with deduplication.",
          shiny::tags$br(),
          shiny::tags$i(class = "fa fa-info-circle"),
          " Files labeled ",
          shiny::strong("'screened'"),
          " or ",
          shiny::strong("'final'"),
          " should have their Source field left blank."
        )
      ),
      shiny::div(
        style = "margin-top: 14px;",
        shinyWidgets::actionBttn(
          inputId = "go_to_dedup",
          label   = "Continue to deduplication",
          style   = "jelly",
          icon    = shiny::icon("arrow-right"),
          color   = "primary",
          size    = "sm"
        ) %>% htmltools::tagAppendAttributes(style = "background-color: #008080;")
      )
    )
  })
  
  shiny::observeEvent(input$go_to_dedup, {
    shiny::updateNavbarPage(session, "tabs", selected = "Deduplicate")
  })

  shiny::observeEvent(input$file_reimport, {
    files <- input$file_reimport   # data frame: one row per file (multiple = TRUE)

    n_unique_imported     <- 0L
    n_candidates_imported <- 0L
    errors <- character(0)

    for (i in seq_len(nrow(files))) {
      path <- files$datapath[i]
      nm   <- files$name[i]
      ext  <- tolower(tools::file_ext(nm))

      tryCatch({
        if (ext == "ris") {
          rv$latest_unique  <- reimport_ris(path)
          rv$existing_dedup_present <- TRUE
          n_unique_imported <- nrow(rv$latest_unique)
        } else if (ext == "csv") {
          # Route by content: a candidate-pairs file has duplicate_id.x / .y;
          # a deduplicated citation set has the cite_* / duplicate_id columns.
          hdr <- names(utils::read.csv(path, nrows = 1, stringsAsFactors = FALSE))
          if (all(c("duplicate_id.x", "duplicate_id.y") %in% hdr)) {
            cand <- reimport_dedup_candidates(path)
            # Drop the R-workflow `result` column: in the app the merge decision
            # is the user's row selection, not result == "match".
            cand$result <- NULL
            rv$pairs_to_check     <- cand
            n_candidates_imported <- nrow(cand)
          } else {
            rv$latest_unique  <- reimport_csv(path)
            rv$existing_dedup_present <- TRUE
            n_unique_imported <- nrow(rv$latest_unique)
          }
        } else {
          errors <- c(errors, paste0(nm, " (unsupported type)"))
        }
      }, error = function(e) {
        errors <<- c(errors, paste0(nm, ": ", conditionMessage(e)))
      })
    }

    if (n_unique_imported > 0) rv$n_unique <- count_unique(rv$latest_unique)

    if (length(errors) > 0) {
      show_toastr("Some files could not be re-imported",
                  paste(errors, collapse = "; "), type = "error")
    }
    if (n_unique_imported > 0 || n_candidates_imported > 0) {
      msg <- character(0)
      if (n_unique_imported > 0)
        msg <- c(msg, paste("Imported", n_unique_imported, "deduplicated citations."))
      if (n_candidates_imported > 0)
        msg <- c(msg, paste("Restored", n_candidates_imported,
                            "candidate pair(s) — finish review on the Deduplicate tab."))
      show_toastr("Re-import successful", paste(msg, collapse = " "), type = "success")
    }
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

  # ---- Filter sync: keep Visualise ↔ Tables in lock-step ----
  # setequal() guard prevents ping-pong; ignoreInit stops firing at load time.
  shiny::observeEvent(input$sources_visual, {
    if (!setequal(input$sources_visual, input$sources_tables))
      shiny::updateSelectInput(session, "sources_tables", selected = input$sources_visual)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$sources_tables, {
    if (!setequal(input$sources_tables, input$sources_visual))
      shiny::updateSelectInput(session, "sources_visual", selected = input$sources_tables)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$labels_visual, {
    if (!setequal(input$labels_visual, input$labels_tables))
      shiny::updateSelectInput(session, "labels_tables", selected = input$labels_visual)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$labels_tables, {
    if (!setequal(input$labels_tables, input$labels_visual))
      shiny::updateSelectInput(session, "labels_visual", selected = input$labels_tables)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$strings_visual, {
    if (!setequal(input$strings_visual, input$strings_tables))
      shiny::updateSelectInput(session, "strings_tables", selected = input$strings_visual)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$strings_tables, {
    if (!setequal(input$strings_tables, input$strings_visual))
      shiny::updateSelectInput(session, "strings_visual", selected = input$strings_tables)
  }, ignoreInit = TRUE)
  # ---- End filter sync ----

  # Propagate file metadata form inputs to rv$upload_df.
  # rv$df is NOT isolated here so the observer re-runs whenever files are added
  # (registering the new inputs as reactive dependencies on the next pass).
  # rv$upload_df writes stay isolated to avoid a feedback loop.
  shiny::observe({
    n_files <- if (is.data.frame(rv$df)) nrow(rv$df) else 0L
    if (n_files == 0L) return()

    for (i in seq_len(n_files)) {
      src_val <- input[[paste0("file_source_", i)]]
      lbl_val <- input[[paste0("file_label_",  i)]]
      str_val <- input[[paste0("file_string_", i)]]

      # Inputs don't exist yet if renderUI hasn't rendered them
      if (is.null(src_val) || is.null(lbl_val) || is.null(str_val)) next

      # Auto-clear source when label is switched to screened or final
      if (lbl_val %in% c("screened", "final") && src_val != "") {
        shiny::updateTextInput(session, paste0("file_source_", i), value = "")
        src_val <- ""
      }

      dp <- rv$df$file.datapath[i]

      # Persist to file_meta so the form can restore values on re-render
      rv$file_meta[[dp]] <- list(source = src_val, label = lbl_val, string = str_val)

      # Propagate to upload_df (isolated to avoid making upload_df a dep of this observer)
      shiny::isolate({
        if (is.data.frame(rv$upload_df) && nrow(rv$upload_df) > 0 &&
            "file.datapath" %in% names(rv$upload_df)) {
          idx <- which(rv$upload_df$file.datapath == dp)
          if (length(idx) > 0) {
            rv$upload_df$cite_source[idx] <- src_val
            rv$upload_df$cite_label[idx]  <- lbl_val
            rv$upload_df$cite_string[idx] <- str_val
          }
        }
      })
    }
  })
  
  # Deduplication tab -----------------
  
  # when dedup button clicked, deduplicate
  shiny::observeEvent(input$identify_dups, {
    has_new      <- is.data.frame(rv$upload_df) && nrow(rv$upload_df) > 0
    has_existing <- isTRUE(rv$existing_dedup_present) &&
                    is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0

    if (!has_new) {
      if (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0) {
        show_toastr("Already deduplicated",
                    "This set is already deduplicated. To add more sources, upload new citation files above, then click Find duplicates to merge them in.",
                    type = "info")
      } else {
        show_toastr("Data needed", "Please import your citations first.", type = "error")
      }
      return()  # Early return to stop further execution
    }

    # Sync file metadata inputs into upload_df right before dedup runs.
    # This is a guaranteed flush regardless of whether the reactive observer
    # above has already propagated the latest changes.
    if (is.data.frame(rv$df) && nrow(rv$df) > 0 && "file.datapath" %in% names(rv$upload_df)) {
      for (.i in seq_len(nrow(rv$df))) {
        .src <- input[[paste0("file_source_", .i)]]
        .lbl <- input[[paste0("file_label_",  .i)]]
        .str <- input[[paste0("file_string_", .i)]]
        if (is.null(.src) || is.null(.lbl) || is.null(.str)) next
        if (.lbl %in% c("screened", "final")) .src <- ""
        .dp  <- rv$df$file.datapath[.i]
        .idx <- which(rv$upload_df$file.datapath == .dp)
        if (length(.idx) > 0) {
          rv$upload_df$cite_source[.idx] <- .src
          rv$upload_df$cite_label[.idx]  <- .lbl
          rv$upload_df$cite_string[.idx] <- .str
        }
      }
    }

    # Assign unique IDs to avoid issues with manual deduplication
    rv$upload_df <- rv$upload_df %>% dplyr::mutate(record_id = as.character(1000 + dplyr::row_number()))

    n_new <- nrow(rv$upload_df)  # capture before any clearing

    # Perform deduplication. With a reimported deduplicated set present, merge
    # the new uploads INTO it (Goal 2); otherwise deduplicate the uploads.
    if (has_existing) {
      dedup_results <- CiteSource::dedup_citations_add_sources(
        rv$latest_unique, rv$upload_df, manual = TRUE, show_unknown_tags = FALSE)
    } else {
      dedup_results <- CiteSource::dedup_citations(
        rv$upload_df, manual = TRUE, show_unknown_tags = FALSE)
    }
    rv$pairs_to_check <- dedup_results$manual_dedup
    rv$latest_unique  <- dedup_results$unique
    rv$auto_pairs     <- if (is.null(dedup_results$auto_pairs)) data.frame() else dedup_results$auto_pairs
    rv$pairs_removed  <- data.frame()  # reset manual log on a fresh dedup run
    rv$n_unique <- count_unique(rv$latest_unique)  # Generate the n_unique data

    n_unique_records <- nrow(rv$latest_unique)
    n_pairs_manual   <- nrow(rv$pairs_to_check)
    fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)

    if (has_existing) {
      # The combined set is now a standalone deduplicated set. Clear the uploads
      # (and the upload form) so the same new records can't be added twice, and
      # keep the merged set flagged as an existing set for further additions.
      rv$df <- data.frame(); rv$upload_df <- data.frame(); rv$file_meta <- list()
      rv$existing_dedup_present <- TRUE

      review_msg <- if (n_pairs_manual > 0)
        paste0(n_pairs_manual, " potential duplicate pair(s) flagged for manual review.")
      else "No potential duplicates for manual review."
      message <- paste0("Added ", fmt(n_new), " new citation(s) to the existing set.\n",
                        "Unique citations after merge: ", fmt(n_unique_records), "\n\n", review_msg)
      show_toastr("Sources added", message, type = "success")
    } else {
      rv$existing_dedup_present <- FALSE
      n_duplicates_removed <- n_new - n_unique_records
      message <- if (n_pairs_manual > 0) {
        paste0("Total citations uploaded: ", fmt(n_new), "\n",
               "Unique citations after deduplication: ", fmt(n_unique_records), "\n",
               "Duplicates removed: ", fmt(n_duplicates_removed), "\n\n",
               n_pairs_manual, " potential duplicate pair(s) flagged for manual review.")
      } else {
        paste0("Total citations uploaded: ", fmt(n_new), "\n",
               "Unique citations after deduplication: ", fmt(n_unique_records), "\n",
               "Duplicates removed: ", fmt(n_duplicates_removed), "\n\n",
               "No potential duplicates for manual review. You can proceed to the visualization tab.")
      }
      show_toastr("Auto-deduplication complete", message, type = "success")
    }
  })

  # ---- Post-dedup summary card ----
  output$dedup_summary_card <- shiny::renderUI({
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    shiny::req(is.data.frame(rv$upload_df)     && nrow(rv$upload_df)     > 0)

    n_total  <- nrow(rv$upload_df)
    n_unique <- nrow(rv$latest_unique)
    n_dupes  <- n_total - n_unique
    n_manual <- if (is.data.frame(rv$pairs_to_check)) nrow(rv$pairs_to_check) else 0L
    fmt      <- function(x) format(x, big.mark = ",", scientific = FALSE)

    # Per-source record counts (from pre-dedup data)
    src_counts <- rv$upload_df %>%
      dplyr::group_by(cite_source) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(n))
    max_n <- max(src_counts$n, 1L)

    src_bars <- lapply(seq_len(nrow(src_counts)), function(i) {
      src <- src_counts$cite_source[i]
      cnt <- src_counts$n[i]
      pct <- round(cnt / max_n * 100)
      label <- if (nchar(src) > 28) paste0(substr(src, 1, 26), "…") else src
      shiny::tags$div(
        style = "margin-bottom: 8px;",
        shiny::tags$div(
          style = "display:flex; justify-content:space-between; margin-bottom:2px;",
          shiny::tags$span(label,
            style = "font-size:0.82em; color:#23395B; font-weight:500;"),
          shiny::tags$span(fmt(cnt),
            style = "font-size:0.82em; color:#6c757d;")
        ),
        shiny::tags$div(
          style = "background:#e9ecef; border-radius:4px; height:8px;",
          shiny::tags$div(
            style = paste0("width:", pct, "%; height:8px;",
                           "background:#008080; border-radius:4px;")
          )
        )
      )
    })

    bottom_note <- if (n_manual > 0) {
      shiny::div(
        style = paste("margin-top:14px; padding:10px 14px;",
                      "background:#fff8e1; border-left:3px solid #ffc107;",
                      "border-radius:4px; font-size:0.85em; color:#856404;"),
        shiny::tags$i(class = "fa fa-exclamation-triangle",
                      style = "margin-right:6px;"),
        sprintf("%s potential duplicate pair%s flagged — review on the Manual deduplication tab.",
                fmt(n_manual), if (n_manual == 1L) "" else "s")
      )
    } else {
      shiny::div(
        style = paste("margin-top:14px; padding:10px 14px;",
                      "background:#d4edda; border-left:3px solid #82D173;",
                      "border-radius:4px; font-size:0.85em; color:#155724;"),
        shiny::tags$i(class = "fa fa-check-circle", style = "margin-right:6px;"),
        "No pairs flagged for manual review — you can proceed to Visualise."
      )
    }

    shiny::div(
      style = "margin-top:20px;",
      shiny::h5("Deduplication Summary"),
      # Metric tiles
      shiny::fluidRow(
        shiny::column(4,
          shiny::div(
            style = paste("background:#f8f9fa; border:1px solid #dee2e6;",
                          "border-radius:8px; padding:14px; text-align:center;"),
            shiny::div(style = "font-size:1.8em; font-weight:700; color:#495057;",
                       fmt(n_total)),
            shiny::div(style = "font-size:0.78em; color:#6c757d; margin-top:3px;",
                       "Total uploaded")
          )
        ),
        shiny::column(4,
          shiny::div(
            style = paste("background:#e8f5f5; border:1px solid #b2dfdb;",
                          "border-radius:8px; padding:14px; text-align:center;"),
            shiny::div(style = "font-size:1.8em; font-weight:700; color:#008080;",
                       fmt(n_dupes)),
            shiny::div(style = "font-size:0.78em; color:#6c757d; margin-top:3px;",
                       "Duplicates removed")
          )
        ),
        shiny::column(4,
          shiny::div(
            style = paste("background:#eef1f6; border:1px solid #c5cfe0;",
                          "border-radius:8px; padding:14px; text-align:center;"),
            shiny::div(style = "font-size:1.8em; font-weight:700; color:#23395B;",
                       fmt(n_unique)),
            shiny::div(style = "font-size:0.78em; color:#6c757d; margin-top:3px;",
                       "Unique citations")
          )
        )
      ),
      shiny::br(),
      # Per-source bars
      shiny::div(
        style = paste("background:white; border:1px solid #dee2e6;",
                      "border-radius:8px; padding:14px;"),
        shiny::tags$p(
          style = "font-size:0.85em; font-weight:600; color:#23395B; margin-bottom:10px;",
          shiny::tags$i(class = "fa fa-database", style = "margin-right:6px;"),
          "Records per source"
        ),
        do.call(shiny::tagList, src_bars)
      ),
      bottom_note
    )
  })
  # ---- End post-dedup summary card ----

  ## Manual deduplication -----
  
  # --- Card view helpers ---

  get_pair_preferences <- function(pair_row_idx) {
    key <- as.character(pair_row_idx)
    if (!key %in% names(rv$field_preferences)) rv$field_preferences[[key]] <- list()
    rv$field_preferences[[key]]
  }

  set_field_preference <- function(pair_row_idx, field, preference) {
    key <- as.character(pair_row_idx)
    if (!key %in% names(rv$field_preferences)) rv$field_preferences[[key]] <- list()
    if (preference == "clear") {
      rv$field_preferences[[key]][[field]] <- NULL
      if (length(rv$field_preferences[[key]]) == 0) rv$field_preferences[[key]] <- NULL
    } else {
      rv$field_preferences[[key]][[field]] <- preference
    }
  }

  apply_field_preferences <- function(merged_data, pairs_removed, original_unique) {
    if (nrow(pairs_removed) == 0) return(merged_data)
    id_col <- if ("record_id" %in% names(merged_data)) "record_id" else
              if ("duplicate_id" %in% names(merged_data)) "duplicate_id" else {
                warning("apply_field_preferences: no id column found"); return(merged_data)
              }
    for (i in seq_len(nrow(pairs_removed))) {
      pair <- pairs_removed[i, ]
      prefs_json <- if ("field_preferences" %in% names(pair)) pair$field_preferences else "{}"
      if (is.na(prefs_json) || prefs_json == "") prefs_json <- "{}"
      prefs <- tryCatch(jsonlite::fromJSON(prefs_json), error = function(e) list())
      if (length(prefs) > 0) {
        id1 <- if ("record_id1" %in% names(pair)) pair[["record_id1"]] else pair[["record_id.x"]]
        id2 <- if ("record_id2" %in% names(pair)) pair[["record_id2"]] else pair[["record_id.y"]]
        if (is.null(id1)) id1 <- NA; if (is.null(id2)) id2 <- NA
        target_row <- which(merged_data[[id_col]] == id1)
        if (length(target_row) == 0) target_row <- which(merged_data[[id_col]] == id2)
        if (length(target_row) > 0) {
          for (field in names(prefs)) {
            source_col <- paste0(field, if (prefs[[field]] == "A") "1" else "2")
            if (source_col %in% names(pair) && field %in% names(merged_data)) {
              new_val <- pair[[source_col]]
              if (is.null(new_val) || length(new_val) == 0) new_val <- NA
              merged_data[target_row, field] <- new_val
            }
          }
        }
      }
    }
    merged_data
  }

  # --- manualdedupsubmit: handles both card and table selections ---
  observeEvent(input$manualdedupsubmit, {
    selected_indices <- unique(c(input$manual_dedup_dt_rows_selected, rv$selected_pairs_card))
    if (length(selected_indices) == 0) {
      show_toastr("Oops!", "You haven't selected any duplicate pairs to remove.", type = "error")
      return()
    }
    new_removed <- rv$pairs_to_check[selected_indices, ]
    rv$pairs_to_check <- rv$pairs_to_check[-selected_indices, ]
    new_removed$field_preferences <- NA_character_
    for (i in seq_len(nrow(new_removed))) {
      pair_idx <- if ("original_row_index" %in% names(new_removed)) new_removed$original_row_index[i] else selected_indices[i]
      prefs <- get_pair_preferences(pair_idx)
      if (length(prefs) > 0)
        new_removed$field_preferences[i] <- jsonlite::toJSON(prefs, auto_unbox = TRUE)
    }
    rv$pairs_removed    <- dplyr::bind_rows(rv$pairs_removed, new_removed)
    rv$selected_pairs_card <- integer(0)
    after <- CiteSource::dedup_citations_add_manual(rv$latest_unique, additional_pairs = new_removed)
    after <- apply_field_preferences(after, new_removed, rv$latest_unique)
    rv$latest_unique <- after
    show_toastr("Manual deduplication complete",
                paste("Removed", nrow(new_removed), "duplicate pair(s)."), type = "success")
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
  
  # Show/hide Remove button based on any selection (table or card view)
  observe({
    if (length(input$manual_dedup_dt_rows_selected) > 0 || length(rv$selected_pairs_card) > 0) {
      shinyjs::show("manualdedupsubmit")
    } else {
      shinyjs::hide("manualdedupsubmit")
    }
  })

  # --- Card view similarity helpers ---
  calculate_similarity <- function(pair_row) {
    fields <- list(
      list(name="title", weight=0.30), list(name="author", weight=0.20),
      list(name="doi",   weight=0.20), list(name="year",   weight=0.10),
      list(name="journal", weight=0.10), list(name="abstract", weight=0.05),
      list(name="pages", weight=0.025), list(name="volume", weight=0.025)
    )
    total_score <- 0; total_weight <- 0
    for (f in fields) {
      col1 <- paste0(f$name, "1"); col2 <- paste0(f$name, "2")
      if (!col1 %in% names(pair_row) || !col2 %in% names(pair_row)) next
      v1 <- as.character(pair_row[[col1]]); v2 <- as.character(pair_row[[col2]])
      if (is.na(v1) || v1 == "NA") v1 <- ""
      if (is.na(v2) || v2 == "NA") v2 <- ""
      if (v1 == "" && v2 == "") next
      score <- if (v1 == "" || v2 == "") { 0.2
      } else if (tolower(trimws(v1)) == tolower(trimws(v2))) { 1.0
      } else if (grepl(tolower(v1), tolower(v2), fixed=TRUE) || grepl(tolower(v2), tolower(v1), fixed=TRUE)) { 0.8
      } else {
        ch1 <- strsplit(tolower(v1), "")[[1]]; ch2 <- strsplit(tolower(v2), "")[[1]]
        tot <- length(union(ch1, ch2))
        if (tot > 0) length(intersect(ch1, ch2)) / tot else 0
      }
      total_score  <- total_score  + score * f$weight
      total_weight <- total_weight + f$weight
    }
    if (total_weight > 0) round((total_score / total_weight) * 100) else 0L
  }

  compare_field <- function(v1, v2) {
    if (is.na(v1) || v1 %in% c("","NA")) v1 <- ""
    if (is.na(v2) || v2 %in% c("","NA")) v2 <- ""
    if (v1 == "" && v2 == "") return(list(status="missing", val1="N/A", val2="N/A"))
    if (v1 == "") return(list(status="missing", val1="N/A", val2=v2))
    if (v2 == "") return(list(status="missing", val1=v1,    val2="N/A"))
    if (tolower(trimws(v1)) == tolower(trimws(v2))) list(status="match", val1=v1, val2=v2)
    else list(status="different", val1=v1, val2=v2)
  }

  build_field_with_preference <- function(field, val1, val2, comparison, is_record_a, pair_row_idx) {
    selectable <- c("author","abstract","title","journal",
                    "year","pages","volume","doi","number")
    is_sel     <- field %in% selectable && comparison$status %in% c("different","missing")
    prefs      <- get_pair_preferences(pair_row_idx)
    cur_pref   <- prefs[[field]]
    is_preferred <- !is.null(cur_pref) && length(cur_pref) > 0 &&
                    isTRUE(if (is_record_a) cur_pref == "A" else cur_pref == "B")

    # Determine which side is the ASySD default (longer non-missing value)
    show_badge <- is_sel && comparison$status %in% c("different","missing")
    is_default <- FALSE
    if (show_badge) {
      if (comparison$status == "missing") {
        is_default <- if (is_record_a) comparison$val1 != "N/A" else comparison$val2 != "N/A"
      } else {
        l1 <- nchar(as.character(comparison$val1)); l2 <- nchar(as.character(comparison$val2))
        is_default <- if (l1 > l2) is_record_a else if (l2 > l1) !is_record_a else is_record_a
      }
    }

    field_val <- if (is_record_a) comparison$val1 else comparison$val2
    btn_id    <- paste0("field_pref_", pair_row_idx, "_", field, "_", if (is_record_a) "A" else "B")
    btn_style <- if (is_preferred)
      "background-color:white;color:#2d8659;border:2px solid #2d8659;font-weight:bold;"
      else "background-color:white;color:#333;border:1px solid #ddd;"

    shiny::tags$div(
      class = paste0("dedup-field ", comparison$status, if (is_preferred) " field-preferred" else ""),
      style = if (is_preferred) "background-color:#f0f8ff;border-left:3px solid #008080;" else "",
      shiny::tags$div(
        class = "dedup-field-label",
        shiny::tags$span(stringr::str_to_title(field), ":"),
        if (is_sel) shiny::tagList(
          shiny::actionButton(btn_id,
            label = shiny::tagList(shiny::icon("check"), if (is_preferred) "Selected" else "Use This"),
            class = paste0("btn-field-preference", if (is_preferred) " selected" else ""),
            style = btn_style, size = "sm"),
          if (show_badge && is_default)
            shiny::tags$span(class="default-indicator", style="margin-left:6px;",
                             shiny::icon("star"), "Default")
        )
      ),
      shiny::tags$div(class = "dedup-field-value", field_val)
    )
  }

  # Filtered & sorted pairs for card view
  filtered_pairs <- shiny::reactive({
    if (nrow(rv$pairs_to_check) == 0) return(data.frame())
    pairs <- rv$pairs_to_check
    min_sim   <- if (is.null(input$similarity_filter)) 0 else input$similarity_filter
    sort_ord  <- if (is.null(input$similarity_sort))   "desc" else input$similarity_sort
    scores <- sapply(seq_len(nrow(pairs)), function(i) calculate_similarity(pairs[i,]))
    pairs$original_row_index <- seq_len(nrow(pairs))
    pairs$similarity_score   <- scores
    pairs <- pairs[scores >= min_sim, ]
    if (nrow(pairs) > 0)
      pairs <- pairs[order(if (sort_ord == "desc") -pairs$similarity_score else pairs$similarity_score), ]
    pairs
  })

  # Navigation observers
  shiny::observeEvent(input$dedup_prev_pair, {
    if (rv$current_pair_index > 1) rv$current_pair_index <- rv$current_pair_index - 1L
  })
  shiny::observeEvent(input$dedup_next_pair, {
    fp <- filtered_pairs()
    if (nrow(fp) > 0 && rv$current_pair_index < nrow(fp))
      rv$current_pair_index <- rv$current_pair_index + 1L
  })
  shiny::observeEvent(input$dedup_mark_duplicate, {
    fp <- filtered_pairs()
    if (nrow(fp) > 0 && rv$current_pair_index >= 1 && rv$current_pair_index <= nrow(fp)) {
      pair_row <- fp[rv$current_pair_index, ]
      row_idx  <- if ("original_row_index" %in% names(pair_row)) pair_row$original_row_index else rv$current_pair_index
      rv$selected_pairs_card <- unique(c(rv$selected_pairs_card, row_idx))
      if (rv$current_pair_index < nrow(fp)) rv$current_pair_index <- rv$current_pair_index + 1L
    }
  })
  shiny::observeEvent(input$dedup_mark_not_duplicate, {
    fp <- filtered_pairs()
    if (nrow(fp) > 0 && rv$current_pair_index < nrow(fp))
      rv$current_pair_index <- rv$current_pair_index + 1L
  })
  shiny::observeEvent(input$dedup_skip, {
    fp <- filtered_pairs()
    if (nrow(fp) > 0 && rv$current_pair_index < nrow(fp))
      rv$current_pair_index <- rv$current_pair_index + 1L
  })

  # Field preference button handler
  shiny::observeEvent(input$field_preference_click, {
    click_data <- input$field_preference_click
    shiny::req(click_data)
    set_field_preference(click_data$pair_idx, click_data$field, click_data$record)
    if (click_data$record != "clear") {
      row_idx <- suppressWarnings(as.numeric(click_data$pair_idx))
      if (!is.na(row_idx) && !row_idx %in% rv$selected_pairs_card) {
        rv$selected_pairs_card <- unique(c(rv$selected_pairs_card, row_idx))
        show_toastr("Pair Marked", "Pair automatically marked as duplicate.", type = "info")
      }
    }
  })

  # Card view progress indicator
  output$dedup_progress <- shiny::renderUI({
    fp <- filtered_pairs()
    if (nrow(fp) == 0) return(shiny::div())
    cur   <- rv$current_pair_index
    total <- nrow(fp)
    pct   <- round((cur / total) * 100)
    shiny::div(
      style = "padding-top:5px;",
      shiny::tags$p(style="margin:0 0 5px;font-size:.95em;font-weight:bold;color:#23395B;",
                    paste("Pair", cur, "of", total)),
      shiny::tags$p(style="margin:0 0 8px;font-size:.85em;color:#666;",
                    paste(length(rv$selected_pairs_card), "selected")),
      shiny::tags$div(
        style="width:100%;height:6px;background:#e0e0e0;border-radius:3px;margin-bottom:8px;",
        shiny::tags$div(style=paste0("width:",pct,"%;height:100%;background:#008080;border-radius:3px;"))
      ),
      shiny::div(style="text-align:center;",
        shinyWidgets::actionBttn("dedup_prev_pair", label="", icon=shiny::icon("chevron-left"),
          style="jelly", color="primary", size="xs") %>% htmltools::tagAppendAttributes(style="margin-right:5px;"),
        shinyWidgets::actionBttn("dedup_next_pair", label="", icon=shiny::icon("chevron-right"),
          style="jelly", color="primary", size="xs")
      )
    )
  })

  # Card view main render
  output$dedup_card_view <- shiny::renderUI({
    fp <- filtered_pairs()
    if (nrow(fp) == 0) {
      return(shiny::wellPanel(style="text-align:center;padding:40px;",
        shiny::tags$p(style="font-size:1.2em;color:#666;",
          "No pairs match the current filter. Try lowering the minimum similarity score.")))
    }
    if (rv$current_pair_index < 1 || rv$current_pair_index > nrow(fp)) return(shiny::div())

    pair         <- fp[rv$current_pair_index, ]
    similarity   <- pair$similarity_score
    sim_class    <- if (similarity >= 80) "dedup-similarity-high" else
                    if (similarity >= 50) "dedup-similarity-medium" else "dedup-similarity-low"
    fields_show  <- c("title","author","year","journal","doi","pages","volume","abstract","source","label","type")
    pair_row_idx <- if ("original_row_index" %in% names(pair)) pair$original_row_index else rv$current_pair_index
    is_selected  <- isTRUE(pair_row_idx %in% rv$selected_pairs_card)

    make_card_fields <- function(is_a) {
      lapply(fields_show, function(field) {
        col1 <- paste0(field,"1"); col2 <- paste0(field,"2")
        v1 <- if (col1 %in% names(pair)) pair[[col1]] else ""
        v2 <- if (col2 %in% names(pair)) pair[[col2]] else ""
        cmp <- compare_field(v1, v2)
        build_field_with_preference(field, v1, v2, cmp, is_a, pair_row_idx)
      })
    }

    shiny::fluidRow(shiny::column(12,
      # Toolbar
      shiny::wellPanel(
        style="background:#f8f9fa;padding:12px;margin-bottom:15px;border:1px solid #dee2e6;",
        shiny::fluidRow(
          shiny::column(6,
            shiny::tags$p(style="margin:0;font-size:.9em;",
              shiny::tags$span(style="background:#d4edda;padding:3px 10px;border-radius:3px;margin-right:5px;","Green = Match"),
              shiny::tags$span(style="background:#fff3cd;padding:3px 10px;border-radius:3px;margin-right:5px;","Yellow = Different"),
              shiny::tags$span(style="background:#f8d7da;padding:3px 10px;border-radius:3px;","Red = Missing")
            ),
            shiny::tags$div(style="margin-top:8px;font-size:.85em;color:#555;font-style:italic;",
              shiny::icon("info-circle"),
              " Tip: clicking 'Use This' on a field automatically marks the pair as a duplicate.")
          ),
          shiny::column(6, shiny::div(style="text-align:right;",
            shiny::tags$strong(style="margin-right:10px;color:#23395B;font-size:.9em;","Quick Actions:"),
            shinyWidgets::actionBttn("dedup_mark_duplicate", "Duplicate",
              icon=shiny::icon("check"), style="jelly", color="success", size="sm") %>%
              htmltools::tagAppendAttributes(style="background:#82D173;margin-right:6px;"),
            shinyWidgets::actionBttn("dedup_mark_not_duplicate", "Not Duplicate",
              icon=shiny::icon("times"), style="jelly", color="danger", size="sm") %>%
              htmltools::tagAppendAttributes(style="background:#dc3545;margin-right:6px;"),
            shinyWidgets::actionBttn("dedup_skip", "Skip",
              icon=shiny::icon("forward"), style="jelly", color="warning", size="sm")
          ))
        )
      ),
      # Similarity badge
      shiny::div(class=paste("dedup-similarity-badge", sim_class),
                 paste("Similarity:", similarity, "%")),
      shiny::br(),
      # Side-by-side cards
      shiny::fluidRow(
        shiny::column(6,
          shiny::div(class="dedup-card record-a",
            shiny::tags$h6(style="margin-top:0;margin-bottom:8px;color:#008080;",
              shiny::icon("file-alt"), " Record A",
              if (is_selected) shiny::tags$span(style="float:right;color:#82D173;",
                shiny::icon("check-circle"), " Selected")),
            make_card_fields(TRUE)
          )
        ),
        shiny::column(6,
          shiny::div(class="dedup-card record-b",
            shiny::tags$h6(style="margin-top:0;margin-bottom:8px;color:#23395B;",
              shiny::icon("file-alt"), " Record B",
              if (is_selected) shiny::tags$span(style="float:right;color:#82D173;",
                shiny::icon("check-circle"), " Selected")),
            make_card_fields(FALSE)
          )
        )
      )
    ))
  })
  
  # Output: manual dedup datatable
  manual_dedup_data <- reactive({
    
    data <- rv$pairs_to_check
    selected_cols <- input$manual_dedup_cols
    
    # Define the desired base order
    core_col_order <- c("author", "title", "year", "journal", "abstract","doi", "pages","volume","number","source","label","string","type")
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

    # DT targets require 0-based integer indices, not column names
    hide_targets <- which(names(data) %in% columns2hide) - 1L

    datatable(data,
              options = list(
                pageLength = 100,
                info = FALSE,
                lengthMenu = list(c(100, -1), c("100", "All")),
                columnDefs =
                  list(
                    list(visible = FALSE,
                         targets = hide_targets),
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
  
  
  # Manual dedup pre-text
  output$Manual_pretext <- shiny::renderText({
    n <- nrow(rv$pairs_to_check)
    if (n == 0) "No pairs require manual deduplication."
    else paste(n, "pair(s) flagged for manual review. Use Card View (recommended) or Table View.")
  })
  
  
  ## How Deduplication works tab
  
  # 1. The Container (With Padding added)
  output$dedup_logic_guide <- shiny::renderUI({
    
    shiny::tagList(
      shiny::br(),
      # WRAPPER DIV: Adds 15px vertical and 30px horizontal padding
      shiny::div(style = "padding: 15px 30px;", 
                 
                 shiny::h4("Deduplication Logic: ASySD Criteria"),
                 shiny::p("ASySD identifies duplicates in two phases. First, it blocks records into potential groups. Second, it scores text similarity. Finally, 'close calls' are flagged for manual review."),
                 
                 shiny::hr(),
                 
                 # The table output
                 shiny::tableOutput("dedup_guide_table_content") 
      )
    )
    
    # 1. The Container (UI with Accordions)
    output$dedup_logic_guide <- shiny::renderUI({
      
      shiny::tagList(
        shiny::br(),
        # Wrapper div with padding
        shiny::div(style = "padding: 0px 15px; max-width: 1050px;", 
                   
                   shiny::h4("Deduplication Criteria"),
                   shiny::p("ASySD identifies duplicates in two automated phases, followed by a manual review safety net. Click a phase below to view the full logic."),
                   
                   shiny::hr(),
                   
                   # --- ACCORDION 1: BLOCKING ---
                   shiny::tags$details(
                     style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
                     shiny::tags$summary(style = "cursor: pointer; font-weight: bold; font-size: 15px;", 
                                         "Phase 1: Blocking (The Wide Net)"),
                     shiny::br(),
                     shiny::p(style = "font-style: italic; font-size: 13px;", 
                              "Records are grouped into potential duplicate sets if they match EXACTLY on any of these combinations."),
                     shiny::tableOutput("tbl_phase1")
                   ),
                   
                   # --- ACCORDION 2: VALIDATION ---
                   shiny::tags$details(
                     style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
                     shiny::tags$summary(style = "cursor: pointer; font-weight: bold; font-size: 15px;", 
                                         "Phase 2: Validation (The Strict Check)"),
                     shiny::br(),
                     shiny::p(style = "font-style: italic; font-size: 13px;", 
                              "Candidate pairs are text-scored (0-100%). A pair is confirmed as a duplicate ONLY if it meets one of these threshold sets."),
                     shiny::tableOutput("tbl_phase2")
                   ),
                   
                   # --- ACCORDION 3: MANUAL REVIEW ---
                   shiny::tags$details(
                     style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
                     shiny::tags$summary(style = "cursor: pointer; font-weight: bold; font-size: 15px;", 
                                         "Phase 3: Manual Review (The Safety Net)"),
                     shiny::br(),
                     shiny::p(style = "font-style: italic; font-size: 13px;", 
                              "Pairs that fall into the 'Grey Area' or have conflicting metadata are flagged for human review."),
                     shiny::tableOutput("tbl_phase3")
                   )
        ) # End div
      ) # End tagList
    }) # End renderUI
    
    
    # 2. Table Content - Phase 1 (Blocking)
    output$tbl_phase1 <- shiny::renderTable({
      data.frame(
        "Category" = c("Round 1 (Broad)", "Round 2 (Bibliographic)", "Round 3 (Numeric)", "Round 4 (Loose)"),
        "Criteria" = c(
          "<ul><li>Title & Pages</li><li>Title & Author</li><li>Title & Abstract</li><li>DOI (Exact)</li></ul>",
          "<ul><li>Author & Year & Pages</li><li>Journal & Volume & Pages</li><li>ISBN & Volume & Pages</li><li>Title & ISBN</li></ul>",
          "<ul><li>Year & Pages & Volume</li><li>Year & Number & Volume</li><li>Year & Pages & Number</li></ul>",
          "<ul><li>Author & Year</li><li>Year & Title</li><li>Title & Volume</li><li>Title & Journal</li></ul>"
        ),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%", sanitize.text.function = function(x) x)
  })
  
  
  # 3. Table Content - Phase 2 (Validation - FULL DETAIL)
  output$tbl_phase2 <- shiny::renderTable({
    data.frame(
      "Category" = c(
        "Strict Bibliographic", 
        "Abstract Heavy", 
        "DOI Specific", 
        "Complex Metadata", 
        "Strict Journal + Abstract", 
        "High Confidence Metadata", 
        "High Numeric Confidence", 
        "Title & Journal/ISBN"
      ),
      "Criteria" = c(
        # Strict Bibliographic
        "<ul>
         <li><b>Pages</b>(>80%) + <b>Vol</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>50%) + <b>ISBN</b>(>99%)</li>
         <li><b>Pages</b>(>80%) + <b>Vol</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>50%) + <b>Journal</b>(>60%)</li>
         <li><b>Pages</b>(>80%) + <b>No.</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>50%) + <b>Journal</b>(>60%)</li>
         <li><b>Vol</b>(>80%) + <b>No.</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>50%) + <b>Journal</b>(>60%)</li>
       </ul>",
        
        # Abstract Heavy
        "<ul>
         <li><b>Vol</b>(>80%) + <b>No.</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>80%)</li>
         <li><b>Vol</b>(>80%) + <b>Pages</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>80%)</li>
         <li><b>Pages</b>(>80%) + <b>No.</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>90%) + <b>Author</b>(>80%)</li>
       </ul>",
        
        # DOI Specific
        "<ul><li><b>DOI</b>(>95%) + <b>Author</b>(>75%) + <b>Title</b>(>90%)</li></ul>",
        
        # Complex Metadata
        "<ul>
         <li><b>Title</b>(>80%) + <b>Abstract</b>(>90%) + <b>Vol</b>(>85%) + <b>Journal</b>(>65%) + <b>Author</b>(>90%)</li>
         <li><b>Title</b>(>90%) + <b>Abstract</b>(>80%) + <b>Vol</b>(>85%) + <b>Journal</b>(>65%) + <b>Author</b>(>90%)</li>
       </ul>",
        
        # Strict Journal & Abstract
        "<ul>
         <li><b>Pages</b>(>80%) + <b>Vol</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>80%) + <b>Author</b>(>90%) + <b>Journal</b>(>75%)</li>
         <li><b>Pages</b>(>80%) + <b>No.</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>80%) + <b>Author</b>(>90%) + <b>Journal</b>(>75%)</li>
         <li><b>Vol</b>(>80%) + <b>No.</b>(>80%) + <b>Title</b>(>90%) + <b>Abstract</b>(>80%) + <b>Author</b>(>90%) + <b>Journal</b>(>75%)</li>
       </ul>",
        
        # High Confidence Metadata
        "<ul>
         <li><b>Title</b>(>90%) + <b>Author</b>(>90%) + <b>Abstract</b>(>90%) + <b>Journal</b>(>70%)</li>
         <li><b>Title</b>(>90%) + <b>Author</b>(>90%) + <b>Abstract</b>(>90%) + <b>ISBN</b>(>99%)</li>
       </ul>",
        
        # High Numeric Confidence
        "<ul>
         <li><b>Pages</b>(>90%) + <b>No.</b>(>90%) + <b>Title</b>(>90%) + <b>Author</b>(>80%) + <b>Journal</b>(>60%)</li>
         <li><b>No.</b>(>90%) + <b>Vol</b>(>90%) + <b>Title</b>(>90%) + <b>Author</b>(>90%) + <b>ISBN</b>(>99%)</li>
         <li><b>Pages</b>(>90%) + <b>Vol</b>(>90%) + <b>Title</b>(>90%) + <b>Author</b>(>80%) + <b>Journal</b>(>60%)</li>
         <li><b>Pages</b>(>90%) + <b>No.</b>(>90%) + <b>Title</b>(>90%) + <b>Author</b>(>80%) + <b>ISBN</b>(>99%)</li>
       </ul>",
        
        # Title & Journal/ISBN Specific
        "<ul>
         <li><b>Pages</b>(>80%) + <b>Vol</b>(>80%) + <b>Title</b>(>95%) + <b>Author</b>(>80%) + <b>Journal</b>(>90%)</li>
         <li><b>No.</b>(>80%) + <b>Vol</b>(>80%) + <b>Title</b>(>95%) + <b>Author</b>(>80%) + <b>Journal</b>(>90%)</li>
         <li><b>No.</b>(>80%) + <b>Pages</b>(>80%) + <b>Title</b>(>95%) + <b>Author</b>(>80%) + <b>Journal</b>(>90%)</li>
         <li><b>Pages</b>(>80%) + <b>Vol</b>(>80%) + <b>Title</b>(>95%) + <b>Author</b>(>80%) + <b>ISBN</b>(>99%)</li>
       </ul>"
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", sanitize.text.function = function(x) x)
  
  
  # 4. Table Content - Phase 3 (Manual)
  output$tbl_phase3 <- shiny::renderTable({
    data.frame(
      "Category" = c("The 'Grey Area'", "Conflicting DOI", "Year Mismatch"),
      "Criteria" = c(
        "<ul>
        <li><b>Title</b>(>85%) + <b>Author</b>(>75%)</li>
        <li><b>Title</b>(>80%) + <b>Abstract</b>(>80%)</li>
        <li><b>Title</b>(>80%) + <b>ISBN</b>(>99%)</li>
        <li><b>Title</b>(>80%) + <b>Journal</b>(>80%)</li>
       </ul>",
        "Pairs that match perfectly but have <b>different</b> DOIs.",
        "Pairs that match perfectly but are published <b>>1 year apart</b>."
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", sanitize.text.function = function(x) x)
  
  # ---- Empty state helpers ----

  # Renders a centered card explaining what's missing and how to fix it.
  # btn_tab must match a navbarPage tab name (used by the stepper_nav observer).
  .empty_state <- function(icon_name, heading, detail,
                           btn_label = NULL, btn_tab = NULL) {
    btn <- if (!is.null(btn_label) && !is.null(btn_tab)) {
      shiny::tags$button(
        class   = "btn btn-primary",
        style   = "margin-top: 16px;",
        onclick = sprintf(
          "Shiny.setInputValue('stepper_nav','%s',{priority:'event'})", btn_tab
        ),
        shiny::tags$i(class = "fa fa-arrow-right",
                      style = "margin-right: 6px;"),
        btn_label
      )
    } else NULL

    shiny::div(
      style = "padding: 56px 24px; text-align: center; color: #6c757d;",
      shiny::tags$i(
        class = paste0("fa fa-", icon_name),
        style = "font-size: 3em; color: #008080; margin-bottom: 16px; display: block;"
      ),
      shiny::h4(heading, style = "color: #23395B; margin-bottom: 8px;"),
      shiny::p(detail,
               style = "max-width: 380px; margin: 0 auto; font-size: 0.9em;"),
      btn
    )
  }

  output$visualise_empty_state <- shiny::renderUI({
    has_dedup <- is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0
    if (has_dedup) return(NULL)
    has_data  <- (is.data.frame(rv$upload_df)    && nrow(rv$upload_df)    > 0) ||
                 (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    if (!has_data)
      .empty_state("upload", "No citations uploaded yet",
                   "Upload your citation files first, then run deduplication to generate visualisations.",
                   "Go to Upload", "File upload")
    else
      .empty_state("copy", "Deduplication not yet run",
                   "Run automated deduplication to generate overlap visualisations.",
                   "Go to Deduplicate", "Deduplicate")
  })

  output$tables_empty_state <- shiny::renderUI({
    has_dedup <- is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0
    if (has_dedup) return(NULL)
    has_data  <- (is.data.frame(rv$upload_df)    && nrow(rv$upload_df)    > 0) ||
                 (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    if (!has_data)
      .empty_state("upload", "No citations uploaded yet",
                   "Upload your citation files first, then run deduplication to generate summary tables.",
                   "Go to Upload", "File upload")
    else
      .empty_state("copy", "Deduplication not yet run",
                   "Run automated deduplication to generate summary tables.",
                   "Go to Deduplicate", "Deduplicate")
  })

  output$export_empty_state <- shiny::renderUI({
    has_dedup <- is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0
    if (has_dedup) return(NULL)
    has_data  <- (is.data.frame(rv$upload_df)    && nrow(rv$upload_df)    > 0) ||
                 (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    if (!has_data)
      .empty_state("upload", "No citations uploaded yet",
                   "Upload your citation files first, then run deduplication before exporting.",
                   "Go to Upload", "File upload")
    else
      .empty_state("copy", "Deduplication not yet run",
                   "Run automated deduplication to make exports available.",
                   "Go to Deduplicate", "Deduplicate")
  })

  # Show/hide the plot, table, and export panels based on whether dedup data exists
  shiny::observe({
    has_dedup <- is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0
    if (has_dedup) {
      shinyjs::show("visualise_tabs_wrapper")
      shinyjs::show("tables_tabs_wrapper")
      shinyjs::show("export_content_wrapper")
    } else {
      shinyjs::hide("visualise_tabs_wrapper")
      shinyjs::hide("tables_tabs_wrapper")
      shinyjs::hide("export_content_wrapper")
    }
  })
  # ---- End empty state helpers ----

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
          .filter_multivalue_col(cite_source, sources_selected_in_input)
        } else { NA_character_ },
        cite_label = if ("cite_label" %in% names(.)) {
          .filter_multivalue_col(cite_label, labels_selected_in_input)
        } else { NA_character_ },
        cite_string = if ("cite_string" %in% names(.)) {
          .filter_multivalue_col(cite_string, strings_selected_in_input)
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

    # Get filter inputs (sources and labels from the Visualise sidebar)
    sources_filt <- input$sources_visual
    sources_filt <- ifelse(sources_filt == "_blank_", "unknown", sources_filt)
    labels_filt <- input$labels_visual
    labels_filt <- ifelse(labels_filt == "_blank_", "unknown", labels_filt)

    # Start with the base unique data, select columns needed
    df <- rv$latest_unique %>%
      dplyr::select(duplicate_id, cite_source, cite_label)

    # Separate rows for source and label
    df_long <- df %>%
      tidyr::separate_rows(cite_source, sep = ",\\s*") %>%
      tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
      dplyr::filter(!is.na(cite_source) & cite_source != "",
                    !is.na(cite_label) & cite_label != "")

    # Apply source and label filters
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
  
  
  # Shared reactive: compute compare_sources once for both heatmap and upset
  source_comparison_data <- shiny::reactive({
    data_vis <- unique_filtered_visual()
    shiny::req(nrow(data_vis) > 0)
    compare_sources(data_vis, comp_type = input$comp_type)
  })

  # Heatmap plot (uses shared source_comparison_data)
  plotHeat <- shiny::reactive({
    source_comparison <- source_comparison_data()
    shiny::req(!is.null(source_comparison))
    plot_source_overlap_heatmap(
      source_comparison,
      cells     = stringr::str_sub(input$comp_type, end = -2),
      log_scale = isTRUE(input$heatmap_log_scale)
    )
  })

  # Dynamic height: allocate ~55px per source, min 450px
  output$heatmapUI <- shiny::renderUI({
    shiny::req(nrow(unique_filtered_visual()) > 0, cancelOutput = TRUE)
    data <- source_comparison_data()
    shiny::req(!is.null(data), cancelOutput = TRUE)
    n <- data |>
      dplyr::select(tidyselect::matches(paste0(stringr::str_sub(input$comp_type, end = -2), "__"))) |>
      ncol()
    plotly::plotlyOutput("plotgraph1", height = paste0(max(450, n * 55), "px"))
  })

  output$plotgraph1 <- plotly::renderPlotly({
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
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
  
  # Keep both nsets sliders' max in sync with the actual number of groups in the data
  shiny::observe({
    data <- source_comparison_data()
    shiny::req(!is.null(data))
    n_groups <- data |>
      dplyr::select(tidyselect::matches(paste0(stringr::str_sub(input$comp_type, end = -2), "__"))) |>
      ncol()
    shiny::updateSliderInput(session, "upset_nsets",
      max = n_groups, value = min(input$upset_nsets, n_groups))
  })

  # Upset plot — only re-renders on Apply or when the underlying data changes
  plotUpset <- shiny::reactive({
    source_comparison <- source_comparison_data()
    shiny::req(!is.null(source_comparison))
    plot_source_overlap_upset(
      source_comparison,
      groups      = stringr::str_sub(input$comp_type, end = -2),
      decreasing  = c(TRUE, TRUE),
      nsets       = input$upset_nsets,
      nintersects = input$upset_nintersects
    )
  }) |> shiny::bindEvent(input$apply_upset, source_comparison_data(), ignoreNULL = FALSE)
  
  output$plotgraph2 <- shiny::renderPlot({
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
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
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    shiny::validate(
      shiny::need(
        any(stringr::str_detect(tolower(rv$latest_unique$cite_label), "screened|final")),
        paste0(
          "No screened or final labels found.\n\n",
          "To show phase analysis: in the Import tab, set files to 'screened' or ",
          "'final' labels, then re-run deduplication."
        )
      )
    )

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
  
  # Reactive for filtering the data used in the record table and summary table
  unique_filtered_table <- shiny::reactive({
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
            .filter_multivalue_col(cite_source, sources_sel_tbl)
          } else { NA_character_ },
          cite_label = if ("cite_label" %in% names(.)) {
            .filter_multivalue_col(cite_label, labels_sel_tbl)
          } else { NA_character_ },
          cite_string = if ("cite_string" %in% names(.)) {
            .filter_multivalue_col(cite_string, strings_sel_tbl)
          } else { NA_character_ }
        )
      
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
  }) # End unique_filtered_table
  
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
    
    # Apply initial filters for labels and strings only.
    # Filters are applied conditionally to avoid passing NULL to str_detect
    # (R's | does not short-circuit, so is.null(p) | str_detect(..., p) errors when p is NULL).
    df_filtered_wide <- rv$latest_unique
    if (!is.null(labels_pattern)) {
      df_filtered_wide <- df_filtered_wide %>%
        dplyr::filter(stringr::str_detect(as.character(cite_label), labels_pattern))
    }
    if (!is.null(strings_pattern)) {
      df_filtered_wide <- df_filtered_wide %>%
        dplyr::filter(stringr::str_detect(as.character(cite_string), strings_pattern))
    }
    
    empty_result_df <- tibble::tibble( # Define structure for empty returns
      Source = character(), `Records Imported` = integer(), `Distinct Records` = integer(),
      `Unique Records` = integer(), `Non-unique Records` = integer(),
      `Source Contribution %` = character(), `Source Unique Contribution %` = character(),
      `Source Unique %` = character() )
    
    if (nrow(df_filtered_wide) == 0) { return(empty_result_df) }
    
    # Separate cite_source column; exclude "unknown" (records from screened/final phases)
    df_long_source <- df_filtered_wide %>%
      dplyr::select(duplicate_id, cite_source, cite_label, cite_string) %>%
      tidyr::separate_rows(cite_source, sep = ",\\s*") %>%
      dplyr::mutate(cite_source = trimws(cite_source)) %>%
      dplyr::filter(!is.na(cite_source) & cite_source != "" & cite_source != "unknown")
    
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
      `Source Unique Contribution %` = (if(total_overall_unique_records > 0) scales::percent(1.0, accuracy = 0.1) else scales::percent(0.0, accuracy = 0.1)), # Sum of these should be 100% if any uniques
      `Source Unique %` = scales::percent(sum(detailed_counts_final$`Unique Records`, na.rm = TRUE) / ifelse(overall_total_distinct_records == 0, 1, overall_total_distinct_records), accuracy = 0.1)
    )
    
    detailed_counts_final <- dplyr::bind_rows(detailed_counts_final, total_row) %>%
      dplyr::rename(Source = cite_source)
    
    return(detailed_counts_final)
  })
  
  # Rendering the detailed record table
  output$detailedRecordTab <- gt::render_gt({
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    table_data <- detailed_table_data()
    shiny::validate(
      shiny::need(is.data.frame(table_data) && nrow(table_data) > 0,
                  "No records match the current filter selections for the Detailed Record Table.")
    )
    create_detailed_record_table(table_data)
  })
  
  # Rendering the precision and sensitivity table ----
  output$summaryPrecTab <- gt::render_gt({
    shiny::validate(
      shiny::need(
        is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0 &&
          any(stringr::str_detect(tolower(rv$latest_unique$cite_label), "final")),
        paste0(
          "No records labeled 'final' found.\n\n",
          "To use this table: in the Import tab, set at least one file's label ",
          "to 'final', then re-run deduplication."
        )
      )
    )
    unique_citations <- unique_filtered_table()
    phase_counts <- calculate_phase_records(unique_citations, rv$n_unique, "cite_source")
    create_precision_sensitivity_table(phase_counts)
  })
  
  
  # Rendering the record-level table ----
  output$reviewTab <- DT::renderDataTable({
    
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    citations <- unique_filtered_table()
    citations$source <- citations$cite_source
    record_level_table(citations = citations, include = c("sources", "labels"), return = "DT")
  }) %>% shiny::bindEvent(input$generateRecordTable)
  
  
  
  #### Export tab ####
  
  # Downloadable bibtex ----
  output$csv_custom_cols_ui <- shiny::renderUI({
    shiny::req(input$csv_fields_preset == "custom")
    shiny::req(is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0)
    required_cs <- c("cite_source", "cite_label", "cite_string", "duplicate_id", "record_ids")
    all_cols <- names(rv$latest_unique)
    col_labels <- setNames(
      ifelse(all_cols %in% required_cs, paste0(all_cols, " *"), all_cols),
      all_cols
    )
    shiny::tagList(
      shiny::tags$p("Select columns to include:", style = "font-size:0.85em;margin-bottom:4px;"),
      shiny::tags$p("* required for CiteSource reimport", style = "font-size:0.78em;color:#6c757d;margin-bottom:6px;"),
      shiny::checkboxGroupInput(
        "csv_custom_cols",
        label = NULL,
        choiceNames  = unname(as.list(col_labels)),
        choiceValues = unname(as.list(all_cols)),
        selected = all_cols,
        inline = FALSE
      )
    )
  })

  output$csv_reimport_warning <- shiny::renderUI({
    preset <- input$csv_fields_preset
    if (is.null(preset) || preset == "full") return(NULL)
    shiny::div(
      style = "font-size:0.82em;color:#856404;background:#fff8e1;padding:8px 12px;border-radius:4px;margin-bottom:10px;",
      shiny::tags$i(class = "fa fa-exclamation-triangle", style = "margin-right:5px;"),
      "This export cannot be reimported into CiteSource via reimport_csv()."
    )
  })

  output$downloadCsv <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (nrow(rv$latest_unique) > 0) {
        preset <- input$csv_fields_preset
        fields <- if (is.null(preset) || preset == "full") {
          "full"
        } else if (preset == "standard") {
          "standard"
        } else {
          cols <- input$csv_custom_cols
          if (is.null(cols) || length(cols) == 0) "full" else cols
        }
        # Flag the set as manually reviewed when no candidate pairs remain
        # pending (UX guard, read back by reimport_csv()). Only written on full
        # exports by export_csv().
        export_csv(rv$latest_unique, file, fields = fields,
                   manual_dedup_complete = (nrow(rv$pairs_to_check) == 0))
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

  # ---- Dedup log: combined auto + manual pair provenance ----
  dedup_log_data <- shiny::reactive({
    auto   <- if (is.data.frame(rv$auto_pairs))   rv$auto_pairs   else data.frame()
    manual <- if (is.data.frame(rv$pairs_removed)) rv$pairs_removed else data.frame()
    CiteSource::dedup_log(
      dedup_result           = list(auto_pairs = auto),
      confirmed_manual_pairs = if (nrow(manual) > 0) manual else NULL
    )
  })

  output$downloadDedupLog <- shiny::downloadHandler(
    filename = function() paste0("dedup-log-", Sys.Date(), ".csv"),
    content  = function(file) {
      log_df <- dedup_log_data()
      if (nrow(log_df) == 0) {
        utils::write.csv(
          data.frame(note = "No duplicate pairs were merged."),
          file, row.names = FALSE
        )
      } else {
        utils::write.csv(log_df, file, row.names = FALSE)
      }
    }
  )

  # ---- Manual-review candidate pairs: export to resume review later ----
  output$downloadCandidates <- shiny::downloadHandler(
    filename = function() paste0("candidate-pairs-", Sys.Date(), ".csv"),
    content  = function(file) {
      pairs <- rv$pairs_to_check
      if (!is.data.frame(pairs) || nrow(pairs) == 0) {
        utils::write.csv(
          data.frame(note = "No unresolved candidate pairs."),
          file, row.names = FALSE
        )
      } else {
        export_dedup_candidates(pairs, file)
      }
    }
  )

  # Export hub — plot downloads (mirror the Visualise tab download handlers)
  output$export_heatplot <- shiny::downloadHandler(
    filename = function() {
      paste("heatmap-overlap-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      shiny::req(nrow(unique_filtered_visual()) > 0)
      heat_plot_obj <- plotHeat()
      if (!is.null(heat_plot_obj)) {
        ggplot2::ggsave(filename = file, plot = heat_plot_obj,
                        device = "png", width = 10, height = 8, dpi = 300)
      } else {
        stop("Failed to generate heatmap plot for download.")
      }
    }
  )

  output$export_upsetplot <- shiny::downloadHandler(
    filename = function() {
      paste("upset-overlap-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      shiny::req(nrow(unique_filtered_visual()) > 0)
      upset_plot_obj <- plotUpset()
      if (!is.null(upset_plot_obj)) {
        grDevices::png(file, width = 1200, height = 800, res = 100)
        print(upset_plot_obj)
        grDevices::dev.off()
      } else {
        stop("Failed to generate upset plot for download.")
      }
    }
  )

  output$export_phaseplot <- shiny::downloadHandler(
    filename = function() {
      paste("phase-analysis-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot_data <- unique_separated_phase()
      if (nrow(plot_data) == 0) stop("No data available to plot based on current filters.")
      phase_plot_obj <- CiteSource::plot_contributions(
        data            = plot_data,
        center          = TRUE,
        bar_order       = c("search", "screened", "final"),
        color_order     = c("unique", "duplicated"),
        totals_in_legend = FALSE
      )
      if (!is.null(phase_plot_obj)) {
        grDevices::png(file, width = 1000, height = 700, res = 100)
        print(phase_plot_obj)
        grDevices::dev.off()
      } else {
        stop("Failed to generate phase analysis plot for download.")
      }
    }
  )

  # Export hub — detailed table download (mirrors Tables tab filter state)
  output$exportDetailedTable <- shiny::downloadHandler(
    filename = function() {
      paste("detailed-records-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tbl <- detailed_table_data()
      shiny::req(!is.null(tbl) && nrow(tbl) > 0)
      write.csv(tbl, file, row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shiny::shinyApp(ui, server)
