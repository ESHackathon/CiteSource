options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000)

library(DT)
library(CiteSource)
library(dplyr)

shiny::tags$head(
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
                            # Toastr CSS and JS
                            tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.css"),
                            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.js"),
                            tags$script(HTML("
                              // Configure toastr defaults
                              toastr.options = {
                                'closeButton': true,
                                'debug': false,
                                'newestOnTop': true,
                                'progressBar': true,
                                'positionClass': 'toast-top-right',
                                'preventDuplicates': false,
                                'onclick': null,
                                'showDuration': '300',
                                'hideDuration': '1000',
                                'timeOut': '5000',
                                'extendedTimeOut': '2000',
                                'showEasing': 'swing',
                                'hideEasing': 'linear',
                                'showMethod': 'fadeIn',
                                'hideMethod': 'fadeOut',
                                'escapeHtml': false
                              };
                            ")),
                            tags$script(HTML("
                            // Handle field preference button selection
                            $(document).on('click', '.btn-field-preference', function(e) {
                              var clickedBtn = $(this);
                              var buttonId = clickedBtn.attr('id');
                              
                              // Parse the ID: field_pref_[pair_idx]_[field]_[A or B]
                              var parts = buttonId.split('_');
                              
                              if (parts.length >= 4) {
                                var recordType = parts[parts.length - 1]; // 'A' or 'B'
                                var field = parts[parts.length - 2];
                                var idxParts = parts.slice(2, parts.length - 2);
                                var pairIdx = idxParts.join('_');
                                
                                var partnerType = (recordType === 'A') ? 'B' : 'A';
                                var baseId = parts.slice(0, parts.length - 1).join('_'); 
                                var partnerId = baseId + '_' + partnerType;
                                var partnerBtn = $('#' + partnerId);
                                
                                var isAlreadySelected = clickedBtn.hasClass('selected');
                                var action = ''; // 'select' or 'clear'
                                
                                if (isAlreadySelected) {
                                  // CASE 1: Deselect (Toggle off)
                                  clickedBtn.removeClass('selected');
                                  clickedBtn.css({
                                    'background-color': 'white',
                                    'color': '#333',
                                    'border': '1px solid #ddd',
                                    'font-weight': 'normal'
                                  });
                                  clickedBtn.html('<i class=\"fa fa-check\"></i> Use This');
                                  action = 'clear';
                                  
                                } else {
                                  // CASE 2: Select (and deselect partner)
                                  
                                  // Update clicked button
                                  clickedBtn.addClass('selected');
                                  clickedBtn.css({
                                    'background-color': 'white',
                                    'color': '#2d8659',
                                    'border': '2px solid #2d8659',
                                    'font-weight': 'bold'
                                  });
                                  clickedBtn.html('<i class=\"fa fa-check\"></i> Selected');
                                  
                                  // Reset partner button
                                  partnerBtn.removeClass('selected');
                                  partnerBtn.css({
                                    'background-color': 'white',
                                    'color': '#333',
                                    'border': '1px solid #ddd',
                                    'font-weight': 'normal'
                                  });
                                  partnerBtn.html('<i class=\"fa fa-check\"></i> Use This');
                                  action = 'select';
                                }
                                
                                // Send data to R
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
                              .dedup-card {
                                border: 2px solid #dee2e6;
                                border-radius: 8px;
                                padding: 8px;
                                margin-bottom: 10px;
                                background-color: white;
                                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                                font-size: 0.9em;
                                display: flex;
                                flex-direction: column;
                                height: 100%;
                              }
                              .dedup-card.record-a {
                                border-left: 4px solid #008080;
                              }
                              .dedup-card.record-b {
                                border-left: 4px solid #23395B;
                              }
                              .dedup-card-body {
                                display: flex;
                                flex-direction: column;
                                flex: 1;
                              }
                              .dedup-field {
                                margin-bottom: 6px;
                                padding: 4px 6px;
                                border-radius: 4px;
                                min-height: 32px;
                                display: flex;
                                flex-direction: column;
                                flex-shrink: 0;
                              }
                              .dedup-field-value {
                                min-height: 20px;
                                word-wrap: break-word;
                                overflow-wrap: break-word;
                              }
                              .dedup-cards-container {
                                display: flex;
                                align-items: stretch;
                              }
                              .dedup-cards-container .column {
                                display: flex;
                                flex-direction: column;
                              }
                              .dedup-cards-container .column > div {
                                flex: 1;
                                display: flex;
                                flex-direction: column;
                              }
                              .dedup-field.match {
                                background-color: #d4edda;
                                border-left: 3px solid #82D173;
                              }
                              .dedup-field.different {
                                background-color: #fff3cd;
                                border-left: 3px solid #ffc107;
                              }
                              .dedup-field.missing {
                                background-color: #f8d7da;
                                border-left: 3px solid #dc3545;
                              }
                              .dedup-field-label {
                                font-weight: bold;
                                color: #23395B;
                                margin-bottom: 2px;
                                font-size: 0.85em;
                              }
                              .dedup-similarity-badge {
                                display: inline-block;
                                padding: 5px 15px;
                                border-radius: 20px;
                                font-weight: bold;
                                margin-bottom: 15px;
                              }
                              .dedup-similarity-high {
                                background-color: #d4edda;
                                color: #155724;
                              }
                              .dedup-similarity-medium {
                                background-color: #fff3cd;
                                color: #856404;
                              }
                              .dedup-similarity-low {
                                background-color: #f8d7da;
                                color: #721c24;
                              }
                              .btn-field-preference {
                                padding: 4px 8px;
                                font-size: 0.8em;
                                border-radius: 3px;
                                cursor: pointer;
                                transition: all 0.2s ease;
                                margin-left: 8px;
                              }
                              .btn-field-preference:hover {
                                opacity: 0.9;
                                transform: scale(1.05);
                              }
                              .btn-field-preference.selected {
                                background-color: white !important;
                                color: #2d8659 !important;
                                border: 2px solid #2d8659 !important;
                                font-weight: bold;
                                box-shadow: 0 2px 6px rgba(45, 134, 89, 0.3);
                              }
                              .btn-field-preference.selected:hover {
                                background-color: #f0f8f5 !important;
                                box-shadow: 0 3px 8px rgba(45, 134, 89, 0.5);
                              }
                              .field-preferred {
                                box-shadow: 0 0 8px rgba(130, 209, 115, 0.3);
                              }
                              .default-indicator {
                                display: inline-block;
                                margin-right: 4px;
                                font-size: 0.75em;
                                padding: 2px 6px;
                                background-color: #e8f4f8;
                                color: #0066cc;
                                border-radius: 3px;
                                border: 1px solid #0066cc;
                                font-weight: 600;
                              }
                              .dedup-field-label {
                                display: flex;
                                justify-content: space-between;
                                align-items: center;
                                margin-bottom: 4px;
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
                                  DT::dataTableOutput("tbl_out"),
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
                              
                              # Toggle between default and custom thresholds
                              shiny::div(
                                style = "margin-bottom: 20px;",
                                shinyWidgets::prettySwitch(
                                  inputId = "use_custom_thresholds",
                                  label = "Use Custom Thresholds",
                                  value = FALSE,
                                  status = "primary",
                                  fill = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.9em; color: #666; margin-top: 5px;",
                                  "Toggle to customize blocking rounds and validation criteria thresholds"
                                )
                              ),
                              
                              # Custom thresholds UI (conditional)
                              shiny::conditionalPanel(
                                condition = "input.use_custom_thresholds == true",
                                bslib::accordion(
                                  open = FALSE,
                                  bslib::accordion_panel(
                                    title = "Custom Blocking Rounds",
                                    icon = shiny::icon("filter"),
                                    shiny::uiOutput("custom_blocking_ui")
                                  ),
                                  bslib::accordion_panel(
                                    title = "Custom Validation Criteria",
                                    icon = shiny::icon("check-circle"),
                                    shiny::uiOutput("custom_validation_ui")
                                  )
                                ),
                                shiny::br()
                              ),
                              
                              # Action button: identify duplicates in uploaded dataset
                              shinyWidgets::actionBttn(
                                "identify_dups", "Find duplicates",
                                style = "jelly",
                                color = "primary",
                                icon = shiny::icon("search")
                              ) %>% htmltools::tagAppendAttributes(style = "background-color: #008080; margin-right: 20px"),
                              
                              shiny::br(),
                              shiny::br(),
                              
                              # Statistics table (appears after deduplication)
                              shiny::conditionalPanel(
                                condition = "output.show_dedup_stats",
                                shiny::h5("Deduplication Statistics"),
                                shiny::uiOutput("dedup_statistics_table")
                              )
                            ),
                            shiny::tabPanel(
                              "Manual deduplication",
                              br(),
                              shiny::h5("Step 4: Review potential duplicates manually"),
                              shiny::textOutput("Manual_pretext"),
                              shiny::br(),
                              
                              # 1. Action Buttons (Always Visible)
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
                              
                              # 2. Collapsible Options Box
                              bslib::accordion(
                                open = "Options & Filters", # Default state: Open
                                bslib::accordion_panel(
                                  title = "Options & Filters",
                                  icon = shiny::icon("sliders-h"),
                                  
                                  # View Mode Selection
                                  shiny::fluidRow(
                                    shiny::column(
                                      12,
                                      shinyWidgets::prettyRadioButtons(
                                        inputId = "dedup_view_mode",
                                        label = shiny::tags$strong("View Mode:"),
                                        choices = c("Card View" = "card", "Table View" = "table"),
                                        selected = "table",
                                        inline = TRUE,
                                        status = "primary"
                                      )
                                    )
                                  ),
                                  shiny::hr(),
                                  # Card View Filters (Conditional)
                                  shiny::conditionalPanel(
                                    condition = "input.dedup_view_mode == 'card'",
                                    shiny::tags$h6(shiny::icon("filter"), " Card Navigation"),
                                    shiny::fluidRow(
                                      shiny::column(
                                        4,
                                        shiny::sliderInput(
                                          inputId = "similarity_filter",
                                          label = "Min Similarity",
                                          min = 0,
                                          max = 100,
                                          value = 0,
                                          step = 5,
                                          post = "%",
                                          width = "100%"
                                        )
                                      ),
                                      shiny::column(
                                        4,
                                        shiny::selectInput(
                                          inputId = "similarity_sort",
                                          label = "Sort by Similarity",
                                          choices = list(
                                            "Highest First" = "desc",
                                            "Lowest First" = "asc"
                                          ),
                                          selected = "desc",
                                          width = "100%"
                                        )
                                      ),
                                      shiny::column(
                                        4,
                                        shiny::uiOutput("dedup_progress")
                                      )
                                    )
                                  ),
                                  # Table View Options (Conditional)
                                  shiny::conditionalPanel(
                                    condition = "input.dedup_view_mode == 'table'",
                                    shiny::tags$h6(shiny::icon("columns"), " Table Columns"),
                                    shinyWidgets::pickerInput(
                                      inputId = "manual_dedup_cols",
                                      label = "Choose columns",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        `live-search` = TRUE,
                                        `actions-box` = TRUE,
                                        style = "btn-primary"),
                                      width = "100%"
                                    )
                                  )
                                )
                              ),
                              
                              shiny::br(),
                              
                              # 3. Main Content Area
                              
                              # Card View Output
                              shiny::conditionalPanel(
                                condition = "input.dedup_view_mode == 'card'",
                                shiny::uiOutput("dedup_card_view")
                              ),
                              
                              # Table View Output
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
                              width = 3,
                              id = "sidebar",
                              shiny::h5("Step 5: Visualise overlap"),
                              
                              # Help text explaining comparison types
                              shiny::wellPanel(
                                style = "background-color: #f0f8ff; border-color: #bce8f1; padding: 10px; margin-bottom: 15px;",
                                shiny::tags$p(
                                  style = "margin: 0; font-size: 0.9em;",
                                  shiny::tags$strong("Tip: "),
                                  "Choose what to compare: ",
                                  shiny::tags$code("sources"), " (databases/methods), ",
                                  shiny::tags$code("labels"), " (workflow phases), or ",
                                  shiny::tags$code("strings"), " (custom categories)."
                                )
                              ),
                              
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
                              
                              # Sources filter with help
                              shiny::div(
                                shiny::selectInput(
                                  inputId = "sources_visual",
                                  label = "Sources to include",
                                  choices = list(),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.85em; color: #666; margin-top: -10px; margin-bottom: 15px;",
                                  "Select citation sources (e.g., PubMed, Web of Science). ",
                                  shiny::tags$em("Leave empty to include all sources.")
                                )
                              ),
                              
                              # Labels filter with help
                              shiny::div(
                                shiny::selectInput(
                                  inputId = "labels_visual",
                                  label = "Labels to include",
                                  choices = list(),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.85em; color: #666; margin-top: -10px; margin-bottom: 15px;",
                                  "Select workflow phases (e.g., search, screened, final). ",
                                  shiny::tags$em("Leave empty to include all labels.")
                                )
                              ),
                              
                              # Strings filter with help
                              shiny::div(
                                shiny::selectInput(
                                  inputId = "strings_visual",
                                  label = "Strings to include",
                                  choices = list(),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.85em; color: #666; margin-top: -10px; margin-bottom: 15px;",
                                  "Select custom search strings or categories. ",
                                  shiny::tags$em("Leave empty to include all strings.")
                                )
                              ),
                              
                              # Filter status indicator
                              shiny::uiOutput("filter_status_visual")
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
                              
                              # Help text explaining filtering
                              shiny::wellPanel(
                                style = "background-color: #f0f8ff; border-color: #bce8f1; padding: 10px; margin-bottom: 15px;",
                                shiny::tags$p(
                                  style = "margin: 0; font-size: 0.9em;",
                                  shiny::tags$strong("Filtering: "),
                                  "Select items to include. Records must match ",
                                  shiny::tags$em("all selected filters"), 
                                  " (AND logic). Leave a filter empty to include all items in that category."
                                )
                              ),
                              
                              shiny::div(
                                shiny::selectInput(
                                  inputId = "sources_tables",
                                  label = "Sources to include",
                                  choices = list(),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.85em; color: #666; margin-top: -10px; margin-bottom: 15px;",
                                  "Citation sources (databases, search methods). ",
                                  shiny::tags$em("Empty = all sources.")
                                )
                              ),
                              
                              shiny::div(
                                shiny::selectInput(
                                  inputId = "labels_tables",
                                  label = "Labels to include",
                                  choices = list(),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.85em; color: #666; margin-top: -10px; margin-bottom: 15px;",
                                  "Workflow phases (search, screened, final). ",
                                  shiny::tags$em("Empty = all labels.")
                                )
                              ),
                              
                              shiny::div(
                                shiny::selectInput(
                                  inputId = "strings_tables",
                                  label = "Strings to include",
                                  choices = list(),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                shiny::tags$p(
                                  style = "font-size: 0.85em; color: #666; margin-top: -10px; margin-bottom: 15px;",
                                  "Custom search strings or categories. ",
                                  shiny::tags$em("Empty = all strings.")
                                )
                              ),
                              
                              # Filter status indicator
                              shiny::uiOutput("filter_status_tables")
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
  
  # --- Reactive Values ---
  # Used to store data that changes during the session
  rv <- shiny::reactiveValues()
  rv$df <- data.frame()
  rv$upload_df <- data.frame()#for original uploads
  rv$latest_unique <- data.frame()#for reimported data
  rv$pairs_to_check <- data.frame()#for potential duplicates/manual dedup
  rv$pairs_removed <- data.frame()#for removed records
  rv$dedup_stats <- NULL # Statistics from deduplication
  rv$custom_blocking_rounds <- NULL # Custom blocking rounds
  rv$custom_validation_criteria <- list() # Custom validation criteria (will be initialized with defaults)
  rv$defaults_loaded <- FALSE # Flag to prevent reloading defaults in a loop
  rv$criteria_manually_edited <- FALSE # Flag to track if user has manually edited criteria (prevents auto-reload)
  rv$sync_locked <- FALSE # Flag to temporarily disable sync after deletion to prevent reading stale inputs
  rv$unlock_sync_at <- NULL # Timestamp when sync should be unlocked
  
  # Initialize custom validation criteria with defaults (convert from 0-1 to 0-100)
  load_default_validation_criteria <- function() {
    tryCatch({
      message("load_default_validation_criteria: Calling get_default_validation_criteria()")
      defaults <- CiteSource:::get_default_validation_criteria()
      message("load_default_validation_criteria: Got ", length(defaults), " default criteria from function")
      if (is.null(defaults) || length(defaults) == 0) {
        message("load_default_validation_criteria: No defaults found, returning empty list")
        return(list())
      }
      if (length(defaults) > 0) {
        message("load_default_validation_criteria: First default name: ", defaults[[1]]$name)
        message("load_default_validation_criteria: First default criteria fields: ", paste(names(defaults[[1]]$criteria), collapse=", "))
        if ("title" %in% names(defaults[[1]]$criteria)) {
          message("load_default_validation_criteria: First default title value: ", defaults[[1]]$criteria$title)
        }
      }
      # Convert thresholds from decimals (0-1) to percentages (0-100)
      criteria_list <- lapply(defaults, function(criterion) {
        if (is.null(criterion) || is.null(criterion$criteria)) {
          return(NULL)
        }
        # Convert all criteria values from 0-1 to 0-100
        converted_criteria <- lapply(criterion$criteria, function(val) {
          if (is.null(val) || is.na(val)) {
            return(NA_real_)
          }
          num_val <- suppressWarnings(as.numeric(val))
          if (is.na(num_val)) {
            return(NA_real_)
          }
          round(num_val * 100, 0)
        })
        
        # Build the criterion list with all fields (use NA_real_ for missing fields)
        criterion_result <- list(
          name = ifelse(is.null(criterion$name) || criterion$name == "", "", as.character(criterion$name))
        )
        # Add all fields, using NA_real_ if not present
        for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
          if (field %in% names(converted_criteria)) {
            field_val <- converted_criteria[[field]]
            if (!is.null(field_val) && !is.na(field_val)) {
              # Ensure it's numeric
              num_val <- suppressWarnings(as.numeric(field_val))
              criterion_result[[field]] <- if (is.na(num_val)) NA_real_ else num_val
            } else {
              criterion_result[[field]] <- NA_real_
            }
          } else {
            criterion_result[[field]] <- NA_real_
          }
        }
        # Ensure all values are explicitly numeric (defensive programming)
        for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
          if (is.logical(criterion_result[[field]])) {
            criterion_result[[field]] <- NA_real_
          } else if (!is.numeric(criterion_result[[field]])) {
            num_val <- suppressWarnings(as.numeric(criterion_result[[field]]))
            criterion_result[[field]] <- if (is.na(num_val)) NA_real_ else num_val
          }
        }
        criterion_result
      })
      # Remove NULL entries
      criteria_list <- criteria_list[!sapply(criteria_list, is.null)]
      message("load_default_validation_criteria: Converted to ", length(criteria_list), " criteria")
      if (length(criteria_list) > 0) {
        message("load_default_validation_criteria: First converted name: ", criteria_list[[1]]$name)
        message("load_default_validation_criteria: First converted title type: ", class(criteria_list[[1]]$title), ", value: ", criteria_list[[1]]$title)
      }
      return(criteria_list)
    }, error = function(e) {
      # Fallback: return empty list if function not available
      warning("Could not load default validation criteria: ", e$message)
      message("load_default_validation_criteria: Error occurred: ", e$message)
      return(list())
    })
  }
  
  # Initialize on app start
  initial_criteria <- load_default_validation_criteria()
  if (length(initial_criteria) > 0) {
    # Defensive: ensure all values are numeric (convert any logical NAs to numeric NAs)
    initial_criteria <- lapply(initial_criteria, function(crit) {
      for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
        if (field %in% names(crit)) {
          if (is.logical(crit[[field]]) && is.na(crit[[field]])) {
            crit[[field]] <- NA_real_
          } else if (!is.numeric(crit[[field]])) {
            num_val <- suppressWarnings(as.numeric(crit[[field]]))
            crit[[field]] <- if (is.na(num_val)) NA_real_ else num_val
          }
        }
      }
      crit
    })
    rv$custom_validation_criteria <- initial_criteria
    message("Initialized custom_validation_criteria with ", length(initial_criteria), " default criteria")
    if (length(initial_criteria) > 0) {
      message("First criterion name: ", initial_criteria[[1]]$name)
      message("First criterion title type: ", class(initial_criteria[[1]]$title), ", value: ", initial_criteria[[1]]$title)
    }
  } else {
    message("Warning: Could not load default validation criteria on app start")
    rv$custom_validation_criteria <- list()
  }
  
  # Helper function to show toastr notifications
  show_toastr <- function(title, message, type = "info") {
    # Map shinyalert types to toastr types
    toastr_type <- switch(type,
      "success" = "success",
      "error" = "error",
      "warning" = "warning",
      "info" = "info",
      "info"  # default
    )
    
    # Escape special characters for JavaScript and handle newlines
    escape_js <- function(x) {
      if (is.null(x) || length(x) == 0) return("")
      x <- as.character(x)
      # Replace newlines with HTML line breaks for better display
      x <- gsub("\n", "<br>", x, fixed = TRUE)
      x <- gsub("\r", "", x, fixed = TRUE)
      # Escape backslashes first
      x <- gsub("\\\\", "\\\\\\\\", x)
      # Escape single quotes
      x <- gsub("'", "\\'", x, fixed = TRUE)
      # Escape double quotes
      x <- gsub('"', '\\"', x, fixed = TRUE)
      return(x)
    }
    
    # Create JavaScript to show toastr with HTML support
    # Note: toastr function signature is toastr.success(message, title, options)
    # Error messages get double the timeout (10 seconds) for better visibility
    timeout_ms <- if (type == "error") 10000 else 5000
    extended_timeout_ms <- if (type == "error") 4000 else 2000
    
    js_code <- paste0("
      toastr.", toastr_type, "('", 
      escape_js(message), 
      "', '", 
      escape_js(title), 
      "', { 'timeOut': ", timeout_ms, ", 'extendedTimeOut': ", extended_timeout_ms, ", 'closeButton': true, 'progressBar': true, 'escapeHtml': false });
    ")
    
    shinyjs::runjs(js_code)
  }
  
  # --- Google Analytics Integration ---
  # Flag to ensure GA script is inserted only once per session
  ga_script_inserted <- reactiveVal(FALSE)
  
  # Use observeEvent on session$clientData which becomes available early
  observeEvent(session$clientData, {
    # Only proceed if the script hasn't been inserted yet for this session
    if (!ga_script_inserted()) {
      # Get the application's path from the URL (e.g., /CiteSource_latest/)
      app_path <- session$clientData$url_pathname
      ga_include_file <- NULL # Variable to hold the GA HTML filename
      
      # --- Determine GA HTML filename based on the application path ---
      # Check if the path ends with '_latest' or '_latest/' (case-insensitive)
      if (grepl("_latest/?$", app_path, ignore.case = TRUE)) {
        # Development version
        message("GA: Detected DEV environment based on URL path: ", app_path) # Logging
        # *** SET the DEV Google Analytics HTML filename ***
        ga_include_file <- "google_analytics_dev.html" # file is in same directory as app.R
        
      }
      # Check if the path corresponds to the production app name (e.g., /CiteSource/ or /CiteSource)
      # Adjust '/CiteSource/?$' if your production app name is different
      else if (grepl("/CiteSource/?$", app_path, ignore.case = TRUE)) {
        # Production version
        message("GA: Detected PROD environment based on URL path: ", app_path) # Logging
        # *** SET the PROD Google Analytics HTML filename ***
        ga_include_file <- "google_analytics_main.html" # file is in same directory as app.R
        
      } else {
        # Path didn't match known patterns
        message("GA: Could not determine environment from URL path: ", app_path) # Logging
      }
      
      # --- Insert the GA HTML file content if a filename was determined and file exists ---
      if (!is.null(ga_include_file) && nzchar(ga_include_file)) {
        # Check if the determined file actually exists in the app directory
        if (file.exists(ga_include_file)) {
          # Insert the content of the HTML file into the document's <head>
          insertUI(
            selector = "head",     # Target the <head> tag
            where = "beforeEnd", # Add the script at the end of the head's content
            # Use includeHTML to read and insert the file content
            ui = includeHTML(ga_include_file),
            immediate = TRUE      # Attempt to insert as soon as possible
          )
          # Set the flag to TRUE to prevent this code running again for this session
          ga_script_inserted(TRUE)
          message("GA: Inserted script from file: ", ga_include_file) # Logging
        } else {
          # Log an error if the file is missing
          message("GA Error: HTML file not found: ", ga_include_file)
          # Optionally set the flag anyway to prevent repeated checks for missing file
          ga_script_inserted(TRUE)
        }
      } else {
        # If no file was determined (e.g., path didn't match), set flag to prevent re-check
        ga_script_inserted(TRUE)
      }
    }
  }, ignoreNULL = TRUE, once = FALSE) # Trigger when clientData is available, but flag prevents re-run
  # --- End Google Analytics Integration ---
  
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
      
      # Read the uploaded citations with error handling
      upload_df <- tryCatch({
        CiteSource::read_citations(
          files = path_list,
          cite_sources = suggested_source,
          cite_labels = suggested_label,
          cite_strings = empty_strings,
          only_key_fields = FALSE
        )
      }, error = function(e) {
        # Show user-friendly error message
        show_toastr(
          "File upload failed",
          paste("An error occurred while reading your citation file(s):", e$message, 
                "\n\nPlease check that your files are in the correct format (.ris, .bib, or .txt) and try again."),
          type = "error"
        )
        return(NULL)
      })
      
      # If upload failed, stop processing
      if (is.null(upload_df)) {
        return()
      }
      
      # Check if any records were read
      if (nrow(upload_df) == 0) {
        show_toastr(
          "No records found",
          "The file(s) were read successfully, but no citation records were found. Please check that your files contain valid citation data.",
          type = "warning"
        )
        return()
      }
      
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
      # Note: Don't create "journal" column if it doesn't exist - let it be missing so we can detect it properly
      required_cols <- c("title", "doi", "label", "isbn", "source", "year", "pages", "volume", "number", "abstract")
      upload_df[required_cols[!(required_cols %in% colnames(upload_df))]] <- NA
      # Only add journal column if it truly doesn't exist (not in the imported data)
      if (!"journal" %in% colnames(upload_df)) {
        upload_df$journal <- NA
      }
      
      # Update the summary data frame df with record counts
      df <- dplyr::left_join(upload_length, df, by = c("source" = "suggested_source")) %>%
        dplyr::select(file.datapath, file.name, records, source, label, string)
      
      # Append the results to the reactive values
      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)
      
      # Show success message
      total_records <- sum(df$records, na.rm = TRUE)
      show_toastr(
        "Upload successful",
        paste("Successfully uploaded", length(path_list), "file(s) containing", total_records, "citation record(s)."),
        type = "success"
      )
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
    
    # Validate file extension
    if (!file_extension %in% c("csv", "ris")) {
      show_toastr(
        "Invalid file type",
        "Please upload a .ris or .csv file that was previously exported from CiteSource.",
        type = "error"
      )
      return()
    }
    
    # Attempt to reimport with error handling
    reimport_result <- tryCatch({
      if (file_extension == "csv") {
        reimport_csv(input$file_reimport$datapath)
      } else if (file_extension == "ris") {
        reimport_ris(input$file_reimport$datapath)
      }
    }, error = function(e) {
      show_toastr(
        "Re-import failed",
        paste("An error occurred while reading the file:", e$message,
              "\n\nPlease ensure the file was exported from CiteSource and is not corrupted."),
        type = "error"
      )
      return(NULL)
    })
    
    # Check if reimport was successful
    if (is.null(reimport_result)) {
      return()
    }
    
    rv$latest_unique <- reimport_result
    
    # Check if any records were imported
    if (nrow(rv$latest_unique) == 0) {
      show_toastr(
        "No records found",
        "The file was read successfully, but no citation records were found. Please check that the file contains valid CiteSource data.",
        type = "warning"
      )
      return()
    }
    
    rv$n_unique <- count_unique(rv$latest_unique)
    
    show_toastr(
      "Re-import successful",
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
  
  # Add filter status indicators
  output$filter_status_visual <- shiny::renderUI({
    if (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0) {
      sources_count <- length(input$sources_visual)
      labels_count <- length(input$labels_visual)
      strings_count <- length(input$strings_visual)
      
      total_selected <- sum(sources_count > 0, labels_count > 0, strings_count > 0)
      
      if (total_selected > 0) {
        filter_parts <- character(0)
        if (sources_count > 0) filter_parts <- c(filter_parts, paste0(sources_count, " source(s)"))
        if (labels_count > 0) filter_parts <- c(filter_parts, paste0(labels_count, " label(s)"))
        if (strings_count > 0) filter_parts <- c(filter_parts, paste0(strings_count, " string(s)"))
        
        shiny::wellPanel(
          style = "background-color: #fff3cd; border-color: #ffc107; padding: 8px; margin-top: 10px;",
          shiny::tags$p(
            style = "margin: 0; font-size: 0.85em;",
            shiny::tags$strong("Active filters: "),
            paste(filter_parts, collapse = ", ")
          )
        )
      }
    }
  })
  
  output$filter_status_tables <- shiny::renderUI({
    if (is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0) {
      sources_count <- length(input$sources_tables)
      labels_count <- length(input$labels_tables)
      strings_count <- length(input$strings_tables)
      
      total_selected <- sum(sources_count > 0, labels_count > 0, strings_count > 0)
      
      if (total_selected > 0) {
        filter_parts <- character(0)
        if (sources_count > 0) filter_parts <- c(filter_parts, paste0(sources_count, " source(s)"))
        if (labels_count > 0) filter_parts <- c(filter_parts, paste0(labels_count, " label(s)"))
        if (strings_count > 0) filter_parts <- c(filter_parts, paste0(strings_count, " string(s)"))
        
        shiny::wellPanel(
          style = "background-color: #fff3cd; border-color: #ffc107; padding: 8px; margin-top: 10px;",
          shiny::tags$p(
            style = "margin: 0; font-size: 0.85em;",
            shiny::tags$strong("Active filters: "),
            paste(filter_parts, collapse = ", ")
          )
        )
      }
    }
  })
  
  # Robust Observer for Cell Edits in tbl_out
  shiny::observeEvent(input$tbl_out_cell_edit, {
    # This observer handles edits made to the summary table (rv$df)
    # and propagates relevant changes (source, label, string)
    # to the corresponding records in the detailed table (rv$upload_df).
    
    info <- input$tbl_out_cell_edit
    
    # Track errors for user feedback
    edit_errors <- character(0)
    
    # Ensure rv$df and rv$upload_df are valid data frames before proceeding
    if (!is.data.frame(rv$df) || nrow(rv$df) == 0) {
      # Silently return if summary data isn't ready (e.g., during initial load)
      return()
    }
    if (!is.data.frame(rv$upload_df)) {
      # Show user-visible error if detailed data structure is missing
      show_toastr(
        "Edit Warning",
        "The data structure is not ready. Your edit was saved to the summary table, but could not be applied to the detailed records. Please try again after the data has finished loading.",
        type = "warning"
      )
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
        edit_errors <- c(edit_errors, paste("Invalid row/column index. The edit could not be applied."))
        next # Skip to the next edit
      }
      
      # --- 1. Update rv$df (the summary table data) ---
      # Use tryCatch to handle potential errors during assignment (e.g., type mismatch)
      update_success <- tryCatch({
        rv$df[target_row_df, target_col_df] <- val
        TRUE
      }, error = function(e) {
        edit_errors <<- c(edit_errors, paste("Error saving edit:", e$message))
        FALSE
      })
      
      if (!update_success) {
        next # Continue to the next edit even if this one failed
      }
      
      # --- 2. Propagate change to rv$upload_df (if applicable) ---
      
      # Get the file.datapath associated with the edited row in rv$df
      # This assumes column 1 of rv$df is 'file.datapath'
      if (df_col_names[1] != "file.datapath") {
        # This is a structural issue, but don't show error to user as it's internal
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
        edit_errors <- c(edit_errors, paste("Could not update detailed records: column '", col_name_upload, "' not found."))
        next
      }
      
      # Perform the update on all matching rows in the detailed data frame
      propagation_success <- tryCatch({
        rv$upload_df[target_rows_upload_idx, col_name_upload] <- val
        TRUE
      }, error = function(e) {
        edit_errors <<- c(edit_errors, paste("Error updating detailed records:", e$message))
        FALSE
      })
      
    } # End FOR loop iterating through edits reported by DT
    
    # Show user-visible feedback if there were errors
    if (length(edit_errors) > 0) {
      error_message <- paste("Some edits could not be completed:", paste(edit_errors, collapse = " "))
      show_toastr(
        "Edit Error",
        error_message,
        type = "error"
      )
    }
    
  })
  
  # Deduplication tab -----------------
  
  # when dedup button clicked, deduplicate
  shiny::observeEvent(input$identify_dups, {
    if (nrow(rv$upload_df) == 0) {
      if (nrow(rv$latest_unique) > 0) {
        show_toastr("Deduplications already complete",
                    "You have reimported a dataset that has already been deduplicated. In that case, further deduplication is not possible here, but would need to take place outside the app.",
                    type = "error"
        )
      } else {
        show_toastr("Data needed",
                    "Please import your citations first.",
                    type = "error"
        )
      }
      return()  # Early return to stop further execution
    }
    
    # Assign unique IDs to avoid issues with manual deduplication
    rv$upload_df <- rv$upload_df %>% dplyr::mutate(record_id = as.character(1000 + dplyr::row_number()))
    
    # Check if using custom thresholds
    use_custom <- isTRUE(input$use_custom_thresholds)
    blocking_rounds <- NULL
    validation_criteria <- NULL
    
    # Always use custom path to get statistics, but with defaults if not customizing
    # This ensures statistics are always available as per requirements
    if (use_custom) {
      # Build custom validation criteria from reactive values
      criteria_list <- rv$custom_validation_criteria
      
      if (is.null(criteria_list) || length(criteria_list) == 0) {
        show_toastr(
          "No Criteria Defined",
          "Please define at least one validation criterion. Using default ASySD criteria instead.",
          type = "warning"
        )
        validation_criteria <- NULL
      } else {
        # Convert criteria from UI format (percentages 0-100) to deduplication format (decimals 0-1)
        # Debug: log what we're reading from reactive values
        message("Building validation criteria from ", length(criteria_list), " criteria in reactive values")
        if (length(criteria_list) > 0) {
          # Show first few criteria to verify custom ones are included
          for (i in 1:min(3, length(criteria_list))) {
            crit <- criteria_list[[i]]
            message("Criterion ", i, " from reactive values: name='", crit$name, 
                    "', title=", crit$title, ", author=", crit$author,
                    ", abstract=", crit$abstract, ", journal=", crit$journal)
          }
        }
        
        validation_criteria <- lapply(criteria_list, function(criterion) {
          # Build criteria list, only including fields with valid values (> 0, not NA)
          criteria_fields <- list()
          
          if (!is.null(criterion$title) && !is.na(criterion$title) && criterion$title > 0) {
            criteria_fields$title <- criterion$title / 100
          }
          if (!is.null(criterion$author) && !is.na(criterion$author) && criterion$author > 0) {
            criteria_fields$author <- criterion$author / 100
          }
          if (!is.null(criterion$abstract) && !is.na(criterion$abstract) && criterion$abstract > 0) {
            criteria_fields$abstract <- criterion$abstract / 100
          }
          if (!is.null(criterion$journal) && !is.na(criterion$journal) && criterion$journal > 0) {
            criteria_fields$journal <- criterion$journal / 100
          }
          if (!is.null(criterion$pages) && !is.na(criterion$pages) && criterion$pages > 0) {
            criteria_fields$pages <- criterion$pages / 100
          }
          if (!is.null(criterion$volume) && !is.na(criterion$volume) && criterion$volume > 0) {
            criteria_fields$volume <- criterion$volume / 100
          }
          if (!is.null(criterion$number) && !is.na(criterion$number) && criterion$number > 0) {
            criteria_fields$number <- criterion$number / 100
          }
          if (!is.null(criterion$isbn) && !is.na(criterion$isbn) && criterion$isbn > 0) {
            criteria_fields$isbn <- criterion$isbn / 100
          }
          if (!is.null(criterion$doi) && !is.na(criterion$doi) && criterion$doi > 0) {
            criteria_fields$doi <- criterion$doi / 100
          }
          
          # Only include criterion if it has at least one field
          if (length(criteria_fields) > 0) {
            criterion_result <- list(
              name = ifelse(is.null(criterion$name) || criterion$name == "", "Unnamed Criterion", criterion$name),
              criteria = criteria_fields
            )
            # Debug: log the converted criterion (always log first criterion)
            if (length(validation_criteria) == 0 || criterion_result$name == criteria_list[[1]]$name) {
              message("Converted criterion '", criterion_result$name, "': ", 
                      paste(paste(names(criteria_fields), ">=", criteria_fields), collapse=", "))
              message("  (Converted from percentages: ", 
                      paste(paste(names(criteria_fields), "=", 
                                  sapply(names(criteria_fields), function(f) {
                                    val <- criterion[[f]]
                                    ifelse(is.null(val) || is.na(val), "NA", paste0(val, "%"))
                                  })), collapse=", "), ")")
            }
            criterion_result
          } else {
            NULL
          }
        })
        
        # Remove NULL entries (empty criteria)
        validation_criteria <- validation_criteria[!sapply(validation_criteria, is.null)]
        
        # Debug: log final validation criteria structure
        if (length(validation_criteria) > 0) {
          message("Final validation criteria: ", length(validation_criteria), " criteria")
          message("First criterion: name='", validation_criteria[[1]]$name, 
                  "', fields: ", paste(names(validation_criteria[[1]]$criteria), collapse=", "))
          if (length(validation_criteria[[1]]$criteria) > 0) {
            first_field <- names(validation_criteria[[1]]$criteria)[1]
            message("  First field '", first_field, "' threshold: ", validation_criteria[[1]]$criteria[[first_field]])
          }
        }
        
        if (length(validation_criteria) == 0) {
          show_toastr(
            "No Valid Criteria",
            "All criteria are empty. Please set at least one field threshold above 0%. Using default ASySD criteria instead.",
            type = "warning"
          )
          validation_criteria <- NULL
        }
      }
      
      # For now, use default blocking rounds (custom blocking rounds UI coming later)
      blocking_rounds <- NULL  # NULL means use defaults
    }
    # If use_custom is FALSE, blocking_rounds and validation_criteria remain NULL
    # which will cause dedup_citations to use defaults but still track statistics
    
    # Perform deduplication (always use custom path to get statistics)
    # Try to call with new parameters, fall back if package hasn't been reloaded
    dedup_results <- NULL
    tryCatch({
      dedup_results <- CiteSource::dedup_citations(
        rv$upload_df, 
        manual = TRUE, 
        show_unknown_tags = TRUE,
        use_custom = TRUE,  # Always TRUE to get statistics
        blocking_rounds = blocking_rounds,  # NULL = use defaults
        validation_criteria = validation_criteria  # NULL = use defaults
      )
    }, error = function(e) {
      # If new parameters aren't recognized, package needs to be reloaded
      # Fall back to standard ASySD call
      if (grepl("unused argument", e$message, ignore.case = TRUE)) {
        show_toastr(
          "Package Reload Required",
          "Please restart R and reload the CiteSource package for custom thresholds to work. Using default ASySD for now.",
          type = "warning"
        )
        # Use standard ASySD call
        dedup_results <<- CiteSource::dedup_citations(
          rv$upload_df, 
          manual = TRUE, 
          show_unknown_tags = TRUE
        )
        # Set empty stats since we can't get them from default ASySD
        rv$dedup_stats <<- list(
          blocking_round_stats = data.frame(
            round_number = integer(),
            round_name = character(),
            pair_count = integer(),
            stringsAsFactors = FALSE
          ),
          validation_stats = data.frame(
            criterion_name = character(),
            pair_count = integer(),
            stringsAsFactors = FALSE
          )
        )
      } else {
        # For other errors, show the error and use fallback
        show_toastr(
          "Deduplication Error",
          paste("Error during deduplication:", e$message, "Falling back to default ASySD."),
          type = "error"
        )
        tryCatch({
          dedup_results <<- CiteSource::dedup_citations(
            rv$upload_df, 
            manual = TRUE, 
            show_unknown_tags = TRUE
          )
          # Initialize empty stats structure for fallback
          rv$dedup_stats <<- list(
            blocking_round_stats = data.frame(
              round_number = integer(),
              round_name = character(),
              pair_count = integer(),
              stringsAsFactors = FALSE
            ),
            validation_stats = data.frame(
              criterion_name = character(),
              pair_count = integer(),
              stringsAsFactors = FALSE
            )
          )
          message("Fallback to default ASySD - statistics not available")
        }, error = function(e2) {
          stop("Failed to perform deduplication even with fallback: ", e2$message)
        })
      }
    })
    
    # Check if dedup_results was successfully created
    if (is.null(dedup_results)) {
      stop("Deduplication failed and no fallback result available")
    }
    
    # Ensure manual_dedup is a dataframe, even if empty
    if (is.null(dedup_results$manual_dedup) || nrow(dedup_results$manual_dedup) == 0) {
      # Create empty dataframe with expected structure
      rv$pairs_to_check <- data.frame()
    } else {
    rv$pairs_to_check <- dedup_results$manual_dedup
    }
    rv$latest_unique <- dedup_results$unique
    
    # Store statistics (should always be available when use_custom=TRUE)
    if (!is.null(dedup_results$stats)) {
      rv$dedup_stats <- dedup_results$stats
    } else {
      # Fallback: try to get from attributes
      stats_attr <- attr(dedup_results$unique, "dedup_stats")
      if (!is.null(stats_attr)) {
        rv$dedup_stats <- stats_attr
      } else {
        # Check if dedup_results itself has stats (for manual=TRUE case)
        if (is.list(dedup_results) && "stats" %in% names(dedup_results)) {
          rv$dedup_stats <- dedup_results$stats
        } else {
          # Last resort: empty stats structure
          rv$dedup_stats <- list(
            blocking_round_stats = data.frame(
              round_number = integer(),
              round_name = character(),
              pair_count = integer(),
              stringsAsFactors = FALSE
            ),
            validation_stats = data.frame(
              criterion_name = character(),
              pair_count = integer(),
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
    
    rv$n_unique <- count_unique(rv$latest_unique)  # Generate the n_unique data
    
    # Generate a summary message based on deduplication results
    n_citations <- nrow(rv$upload_df)
    n_unique_records <- nrow(rv$latest_unique)  # Use latest_unique, not n_unique (which is expanded)
    n_duplicates_removed <- n_citations - n_unique_records
    n_pairs_manual <- nrow(rv$pairs_to_check)
    
    # Format numbers with commas for readability
    n_citations_formatted <- format(n_citations, big.mark = ",", scientific = FALSE)
    n_unique_records_formatted <- format(n_unique_records, big.mark = ",", scientific = FALSE)
    n_duplicates_removed_formatted <- format(n_duplicates_removed, big.mark = ",", scientific = FALSE)
    
    message <- if (n_pairs_manual > 0) {
      paste0(
        "Total citations uploaded: ", n_citations_formatted, "\n",
        "Unique citations after deduplication: ", n_unique_records_formatted, "\n",
        "Duplicates removed: ", n_duplicates_removed_formatted, "\n\n",
        n_pairs_manual, " potential duplicate pair(s) require manual review. ",
        "Head to the manual deduplication tab to check them."
      )
    } else {
      paste0(
        "Total citations uploaded: ", n_citations_formatted, "\n",
        "Unique citations after deduplication: ", n_unique_records_formatted, "\n",
        "Duplicates removed: ", n_duplicates_removed_formatted, "\n\n",
        "No potential duplicates for manual review. You can proceed to the visualization tab."
      )
    }
    
    show_toastr("Auto-deduplication complete", message, type = "success")
  })
  
  ## Manual deduplication -----
  

  # Action button: remove manually selected duplicates
  shiny::observeEvent(input$manualdedupsubmit, {
    
    # Combine selections from both card view and table view
    selected_indices <- unique(c(
      input$manual_dedup_dt_rows_selected,
      rv$selected_pairs_card
    ))
    
    if (length(selected_indices) == 0) {
      show_toastr("Oops!", "You haven't selected any duplicate pairs to remove.", type = "error")
      return()
    }
    
    # Move selected pairs to removed list
    new_removed <- rv$pairs_to_check[selected_indices, ]
    rv$pairs_to_check <- rv$pairs_to_check[-selected_indices, ]
    
    # --- Initialize the column to prevent "Unknown column" warning ---
    new_removed$field_preferences <- NA_character_
    
    # Attach field preferences to each removed pair
    for (i in seq_len(nrow(new_removed))) {
      # Use the ORIGINAL row index to look up preferences
      # (If the dataframe was reordered, we need the stable ID)
      pair_idx <- if("original_row_index" %in% names(new_removed)) new_removed$original_row_index[i] else selected_indices[i]
      
      # Fallback if original_row_index is missing (use the selected index directly)
      # In the card view logic, we used original_row_index as the key
      prefs <- get_pair_preferences(pair_idx)
      
      # Add preferences as a new column if any preferences are set
      if (length(prefs) > 0) {
        new_removed$field_preferences[i] <- jsonlite::toJSON(prefs, auto_unbox = TRUE)
      }
    }
    
    # Update global removed list
    rv$pairs_removed <- dplyr::bind_rows(rv$pairs_removed, new_removed)
    
    # Clear card view selections
    rv$selected_pairs_card <- integer(0)
    
    # Perform the manual deduplication in CiteSource
    after <- CiteSource::dedup_citations_add_manual(rv$latest_unique,
                                                    additional_pairs = new_removed)
    
    # --- Apply the field preferences to the merged data ---
    after <- apply_field_preferences(after, new_removed, rv$latest_unique)
    
    # update latest unique df reactive value
    rv$latest_unique <- after
    
    # Show success message
    show_toastr(
      "Manual deduplication complete",
      paste("Removed", nrow(new_removed), "duplicate pair(s)."),
      type = "success"
    )
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
  
  # Track selected pairs for card view
  rv$selected_pairs_card <- integer(0)
  rv$current_pair_index <- 1
  
  # Track field preferences for each pair (e.g., rv$field_preferences$"pair_123"$author = "A")
  rv$field_preferences <- list()
  
  # Function to get or initialize preferences for a pair
  get_pair_preferences <- function(pair_row_idx) {
    key <- as.character(pair_row_idx)
    if (!key %in% names(rv$field_preferences)) {
      rv$field_preferences[[key]] <- list()
    }
    rv$field_preferences[[key]]
  }
  
  # Function to set preference for a field in a pair
  # Function to set preference for a field in a pair
  set_field_preference <- function(pair_row_idx, field, preference) {
    key <- as.character(pair_row_idx)
    if (!key %in% names(rv$field_preferences)) {
      rv$field_preferences[[key]] <- list()
    }
    
    if (preference == "clear") {
      # Remove the specific field preference
      rv$field_preferences[[key]][[field]] <- NULL
      # Clean up if list is empty
      if (length(rv$field_preferences[[key]]) == 0) {
        rv$field_preferences[[key]] <- NULL
      }
    } else {
      rv$field_preferences[[key]][[field]] <- preference
    }
  }
  
  # --- Handle the custom JavaScript button click ---
  shiny::observeEvent(input$field_preference_click, {
    click_data <- input$field_preference_click
    req(click_data)
    
    # 1. Update the preference (A, B, or clear)
    set_field_preference(click_data$pair_idx, click_data$field, click_data$record)
    
    # 2. Auto-mark as duplicate logic
    # If the user selected A or B (not clearing), automatically mark the pair as duplicate
    if (click_data$record != "clear") {
      
      # We need to map the pair_idx (string from JS) back to the numeric original_row_index
      # In the JS we passed the original_row_index as the pair_idx ID
      row_idx <- as.numeric(click_data$pair_idx)
      
      if (!is.na(row_idx) && !row_idx %in% rv$selected_pairs_card) {
        rv$selected_pairs_card <- unique(c(rv$selected_pairs_card, row_idx))
        
        # Optional: Show a subtle notification
        show_toastr("Pair Marked", "Pair automatically marked as duplicate.", type = "info")
      }
    }
  })  
  # Calculate similarity score for a pair
  calculate_similarity <- function(pair_row) {
    # Fields to compare (with weights)
    fields <- list(
      list(name = "title", weight = 0.3),
      list(name = "author", weight = 0.2),
      list(name = "doi", weight = 0.2),
      list(name = "year", weight = 0.1),
      list(name = "journal", weight = 0.1),
      list(name = "abstract", weight = 0.05),
      list(name = "pages", weight = 0.025),
      list(name = "volume", weight = 0.025)
    )
    
    total_score <- 0
    total_weight <- 0
    
    for (field in fields) {
      field_name <- field$name
      weight <- field$weight
      col1 <- paste0(field_name, "1")
      col2 <- paste0(field_name, "2")
      
      if (col1 %in% names(pair_row) && col2 %in% names(pair_row)) {
        val1 <- as.character(pair_row[[col1]])
        val2 <- as.character(pair_row[[col2]])
        
        # Handle NA values
        if (is.na(val1) || val1 == "" || val1 == "NA") val1 <- ""
        if (is.na(val2) || val2 == "" || val2 == "NA") val2 <- ""
        
        if (val1 == "" && val2 == "") {
          # Both missing - neutral (don't count)
          next
        } else if (val1 == "" || val2 == "") {
          # One missing - low score
          score <- 0.2
        } else {
          # Normalize strings for comparison
          val1_norm <- tolower(trimws(val1))
          val2_norm <- tolower(trimws(val2))
          
          if (val1_norm == val2_norm) {
            score <- 1.0
          } else {
            # Use string similarity (simple approach)
            # Calculate Levenshtein-like similarity
            max_len <- max(nchar(val1_norm), nchar(val2_norm))
            if (max_len == 0) {
              score <- 1.0
            } else {
              # Simple substring matching
              if (grepl(val1_norm, val2_norm, fixed = TRUE) || grepl(val2_norm, val1_norm, fixed = TRUE)) {
                score <- 0.8
              } else {
                # Calculate character overlap
                chars1 <- strsplit(val1_norm, "")[[1]]
                chars2 <- strsplit(val2_norm, "")[[1]]
                common <- length(intersect(chars1, chars2))
                total_chars <- length(union(chars1, chars2))
                score <- if (total_chars > 0) common / total_chars else 0
              }
            }
          }
        }
        
        total_score <- total_score + (score * weight)
        total_weight <- total_weight + weight
      }
    }
    
    # Normalize by total weight used
    if (total_weight > 0) {
      return(round((total_score / total_weight) * 100))
    } else {
      return(0)
    }
  }
  
  # Reactive: Filtered pairs based on similarity score
  filtered_pairs <- shiny::reactive({
    if (nrow(rv$pairs_to_check) == 0) {
      return(data.frame())
    }
    
    pairs <- rv$pairs_to_check
    min_similarity <- if (is.null(input$similarity_filter)) 0 else input$similarity_filter
    sort_order <- if (is.null(input$similarity_sort)) "desc" else input$similarity_sort
    
    # Calculate similarity for each pair
    similarity_scores <- sapply(1:nrow(pairs), function(i) {
      calculate_similarity(pairs[i, ])
    })
    
    # Add row numbers as a column for matching
    pairs$original_row_index <- 1:nrow(pairs)
    pairs$similarity_score <- similarity_scores
    pairs <- pairs[similarity_scores >= min_similarity, ]
    
    # Sort by similarity based on user selection
    if (nrow(pairs) > 0) {
      if (sort_order == "desc") {
        pairs <- pairs[order(-pairs$similarity_score), ]
      } else {
        pairs <- pairs[order(pairs$similarity_score), ]
      }
    }
    
    pairs
  })
  
  # Update current pair index max when filtered pairs change
  shiny::observe({
    filtered <- filtered_pairs()
    if (nrow(filtered) > 0) {
      shiny::updateNumericInput(
        session = session,
        inputId = "current_pair_index",
        max = nrow(filtered)
      )
      # Reset to 1 if current index is out of bounds
      if (rv$current_pair_index > nrow(filtered)) {
        rv$current_pair_index <- 1
        shiny::updateNumericInput(
          session = session,
          inputId = "current_pair_index",
          value = 1
        )
      }
    }
  })
  
  # Update current pair index from input
  shiny::observeEvent(input$current_pair_index, {
    filtered <- filtered_pairs()
    if (nrow(filtered) > 0 && input$current_pair_index >= 1 && input$current_pair_index <= nrow(filtered)) {
      rv$current_pair_index <- input$current_pair_index
    }
  })
  
  # Navigation: Previous pair
  shiny::observeEvent(input$dedup_prev_pair, {
    if (rv$current_pair_index > 1) {
      rv$current_pair_index <- rv$current_pair_index - 1
      shiny::updateNumericInput(
        session = session,
        inputId = "current_pair_index",
        value = rv$current_pair_index
      )
    }
  })
  
  # Navigation: Next pair
  shiny::observeEvent(input$dedup_next_pair, {
    filtered <- filtered_pairs()
    if (nrow(filtered) > 0 && rv$current_pair_index < nrow(filtered)) {
      rv$current_pair_index <- rv$current_pair_index + 1
      shiny::updateNumericInput(
        session = session,
        inputId = "current_pair_index",
        value = rv$current_pair_index
      )
    }
  })
  
  # Mark current pair as duplicate
  shiny::observeEvent(input$dedup_mark_duplicate, {
    filtered <- filtered_pairs()
    if (nrow(filtered) > 0 && rv$current_pair_index >= 1 && rv$current_pair_index <= nrow(filtered)) {
      # Get the actual row index in rv$pairs_to_check using original_row_index
      pair_row <- filtered[rv$current_pair_index, ]
      if ("original_row_index" %in% names(pair_row)) {
        row_idx <- pair_row$original_row_index
        rv$selected_pairs_card <- unique(c(rv$selected_pairs_card, row_idx))
        # Move to next pair
        if (rv$current_pair_index < nrow(filtered)) {
          rv$current_pair_index <- rv$current_pair_index + 1
          shiny::updateNumericInput(
            session = session,
            inputId = "current_pair_index",
            value = rv$current_pair_index
          )
        }
      }
    }
  })
  
  # Mark current pair as NOT duplicate
  shiny::observeEvent(input$dedup_mark_not_duplicate, {
    filtered <- filtered_pairs()
    if (nrow(filtered) > 0 && rv$current_pair_index < nrow(filtered)) {
      # Just move to next pair
      rv$current_pair_index <- rv$current_pair_index + 1
      shiny::updateNumericInput(
        session = session,
        inputId = "current_pair_index",
        value = rv$current_pair_index
      )
    }
  })
  
  # Update button visibility based on selections
  shiny::observe({
    if (length(rv$selected_pairs_card) > 0) {
      shinyjs::show("manualdedupsubmit")
    } else {
      shinyjs::hide("manualdedupsubmit")
    }
  })
  
  # if rows selected in manual dedup table, make button appear
  observe({
    selected_rows <- input$manual_dedup_dt_rows_selected
    if (length(selected_rows) > 0 || length(rv$selected_pairs_card) > 0) {
      shinyjs::show("manualdedupsubmit")  # Show the button when rows are selected
    } else {
      shinyjs::hide("manualdedupsubmit")  # Hide the button when no rows are selected
    }
  })
  
  # Output: manual dedup datatable
  manual_dedup_data <- reactive({
    
    # Check if pairs_to_check exists and has data
    if (is.null(rv$pairs_to_check) || nrow(rv$pairs_to_check) == 0) {
      return(data.frame())
    }
    
    # Get all columns, don't assume there are exactly 36
    data <- rv$pairs_to_check
    selected_cols <- input$manual_dedup_cols
    
    # Define the desired base order
    core_col_order <- c("author", "title", "year", "journal", "abstract",
                                "doi", "pages", "source", "label")
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
    if (length(match_number_cols_to_add) > 0 && nrow(ordered_data) > 0) {
      ordered_data <- cbind(ordered_data, data[, match_number_cols_to_add, drop = FALSE])
    }
    
    ordered_data
    
  })
  output$manual_dedup_dt <- DT::renderDataTable({
    
    data <- manual_dedup_data()
    
    # If data is empty, return empty table
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No pairs require manual review."),
        options = list(paging = FALSE, searching = FALSE, info = FALSE),
        rownames = FALSE
      ))
    }
    
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
    n_pairs <- nrow(rv$pairs_to_check)
    if (n_pairs == 0) {
      "No pairs require manual deduplication."
    } else {
      paste(n_pairs, "pair(s) of citations require manual deduplication. Review the pairs below using either the card view (recommended) or table view.")
    }
  })
  
  # Custom blocking rounds UI
  output$custom_blocking_ui <- shiny::renderUI({
    # Only show if custom thresholds are enabled
    if (!isTRUE(input$use_custom_thresholds)) {
      return(shiny::div())
    }
    
    # Get default blocking rounds to display
    # Try to get from the package, fall back to hardcoded if not available
    default_rounds <- tryCatch({
      CiteSource:::get_default_blocking_rounds()
    }, error = function(e) {
      # Fallback to hardcoded defaults
      list(
        "Round 1 (Broad)" = list(
          c("title", "pages"),
          c("title", "author"),
          c("title", "abstract"),
          c("doi")
        ),
        "Round 2 (Bibliographic)" = list(
          c("author", "year", "pages"),
          c("journal", "volume", "pages"),
          c("isbn", "volume", "pages"),
          c("title", "isbn")
        ),
        "Round 3 (Numeric)" = list(
          c("year", "pages", "volume"),
          c("year", "number", "volume"),
          c("year", "pages", "number")
        ),
        "Round 4 (Loose)" = list(
          c("author", "year"),
          c("year", "title"),
          c("title", "volume"),
          c("title", "journal")
        )
      )
    })
    
    shiny::div(
      shiny::p(style = "margin-bottom: 15px; font-weight: 500;", 
               "Blocking rounds identify potential duplicate pairs by matching on specific field combinations."),
      shiny::p(style = "font-size: 0.9em; color: #666; margin-bottom: 20px;", 
               "Currently using default ASySD blocking rounds. Custom blocking round editor coming soon."),
      
      # Display default blocking rounds for reference
      shiny::h6("Default Blocking Rounds (Currently Active):"),
      shiny::div(
        style = "max-height: 400px; overflow-y: auto; margin-bottom: 15px;",
        lapply(seq_along(default_rounds), function(i) {
          round_name <- names(default_rounds)[i]
          round_combos <- default_rounds[[i]]
          
          shiny::div(
            style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
            shiny::tags$strong(round_name, ":"),
            shiny::tags$ul(
              style = "margin-top: 5px; margin-bottom: 0;",
              lapply(round_combos, function(combo) {
                shiny::tags$li(paste(combo, collapse = " & "))
              })
            )
          )
        })
      ),
      shiny::p(style = "font-size: 0.85em; color: #666; font-style: italic;",
               "Note: Custom blocking rounds editor will allow you to add, remove, or modify these rounds in a future update.")
    )
  })
  
  # Observer to ensure defaults are loaded when custom thresholds are enabled
  # This runs ONLY when the toggle changes, not when criteria are modified
  # Only loads defaults ONCE when toggle is first enabled, never reloads after user edits
  shiny::observeEvent(input$use_custom_thresholds, {
    if (isTRUE(input$use_custom_thresholds)) {
      # CRITICAL: Check flags FIRST before doing anything
      # If user has manually edited, NEVER reload defaults
      if (isTRUE(rv$criteria_manually_edited)) {
        message("Observer: Criteria manually edited, skipping default load")
        return()  # Exit early, don't reload
      }
      
      # Only check/load defaults if we haven't already loaded them
      if (!isTRUE(rv$defaults_loaded)) {
        # Use isolate to prevent re-running when rv$custom_validation_criteria changes
        criteria_list <- shiny::isolate(rv$custom_validation_criteria)
        
        # Check if we need to load defaults
        needs_defaults <- FALSE
        
        if (is.null(criteria_list) || length(criteria_list) == 0) {
          needs_defaults <- TRUE
        } else if (length(criteria_list) > 0) {
          # Check if criteria look like defaults
          first_name <- criteria_list[[1]]$name
          if (is.null(first_name) || first_name == "" || 
              grepl("^Criterion \\d+$", first_name) || 
              grepl("^New Criterion \\d+$", first_name)) {
            needs_defaults <- TRUE
          } else {
            # Criteria look like defaults, mark as loaded
            rv$defaults_loaded <- TRUE
          }
        }
        
        # Load defaults if needed
        if (needs_defaults) {
          criteria_list <- load_default_validation_criteria()
          if (length(criteria_list) > 0) {
            # Defensive: ensure all values are numeric
            criteria_list <- lapply(criteria_list, function(crit) {
              for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
                if (field %in% names(crit)) {
                  if (is.logical(crit[[field]]) && is.na(crit[[field]])) {
                    crit[[field]] <- NA_real_
                  } else if (!is.numeric(crit[[field]])) {
                    num_val <- suppressWarnings(as.numeric(crit[[field]]))
                    crit[[field]] <- if (is.na(num_val)) NA_real_ else num_val
                  }
                }
              }
              crit
            })
            rv$custom_validation_criteria <- criteria_list
            rv$defaults_loaded <- TRUE
            message("Observer: Loaded ", length(criteria_list), " default criteria")
          }
        }
      }
    } else {
      # When toggle is off, reset the flags so defaults can be loaded again when toggled back on
      rv$defaults_loaded <- FALSE
      rv$criteria_manually_edited <- FALSE
    }
  }, ignoreInit = TRUE)
  
  # Separate observer to handle logical NA conversion (runs when criteria change, but doesn't reload defaults)
  # IMPORTANT: Only runs if criteria_manually_edited is FALSE (i.e., during initial load only)
  shiny::observe({
    # Only run if custom thresholds are enabled, defaults are loaded, AND user hasn't manually edited
    # This prevents it from running after deletions/edits
    if (isTRUE(input$use_custom_thresholds) && isTRUE(rv$defaults_loaded) && !isTRUE(rv$criteria_manually_edited)) {
      criteria_list <- rv$custom_validation_criteria
      if (length(criteria_list) > 0 && "title" %in% names(criteria_list[[1]])) {
        if (is.logical(criteria_list[[1]]$title) && is.na(criteria_list[[1]]$title)) {
          # Convert logical NAs to numeric NAs (but don't reload defaults)
          criteria_list <- lapply(criteria_list, function(crit) {
            for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
              if (field %in% names(crit)) {
                if (is.logical(crit[[field]]) && is.na(crit[[field]])) {
                  crit[[field]] <- NA_real_
                } else if (!is.numeric(crit[[field]])) {
                  num_val <- suppressWarnings(as.numeric(crit[[field]]))
                  crit[[field]] <- if (is.na(num_val)) NA_real_ else num_val
                }
              }
            }
            crit
          })
          rv$custom_validation_criteria <- criteria_list
        }
      }
    }
  })
  
  # Custom validation criteria UI
  output$custom_validation_ui <- shiny::renderUI({
    # Only show if custom thresholds are enabled
    if (!isTRUE(input$use_custom_thresholds)) {
      return(shiny::div())
    }
    
    # Get criteria from reactive values (don't modify them here!)
    criteria_list <- rv$custom_validation_criteria
    
    # If still empty, show message
    if (is.null(criteria_list) || length(criteria_list) == 0) {
      return(shiny::div(
        shiny::p("Loading default validation criteria...")
      ))
    }
    
    shiny::div(
      shiny::p(style = "margin-bottom: 10px; font-weight: 500; font-size: 0.9em;", 
               "Edit validation criteria below. Each criterion defines thresholds for different fields. A pair needs to match only ONE criterion to be considered a duplicate."),
      shiny::p(style = "font-size: 0.85em; color: #666; margin-bottom: 10px;", 
               "Tip: Leave a field blank or set to 0 to exclude it from that criterion."),
      
      # Action buttons at top
      shiny::fluidRow(
        shiny::column(6,
          shinyWidgets::actionBttn(
            inputId = "add_criterion",
            label = "Add New Criterion",
            style = "jelly",
            color = "success",
            size = "sm",
            icon = shiny::icon("plus")
          )
        ),
        shiny::column(6,
          shinyWidgets::actionBttn(
            inputId = "reset_criteria",
            label = "Reset to Defaults",
            style = "jelly",
            color = "warning",
            size = "sm",
            icon = shiny::icon("undo")
          )
        )
      ),
      shiny::br(),
      
      # Header row for field labels
      shiny::div(
        style = "margin-bottom: 5px; padding: 5px; background-color: #e9ecef; border-radius: 3px; font-size: 0.85em; font-weight: 500;",
        shiny::fluidRow(
          shiny::column(2, shiny::tags$strong("Criterion Name")),
          shiny::column(1, shiny::tags$strong("title")),
          shiny::column(1, shiny::tags$strong("author")),
          shiny::column(1, shiny::tags$strong("abstract")),
          shiny::column(1, shiny::tags$strong("journal")),
          shiny::column(1, shiny::tags$strong("pages")),
          shiny::column(1, shiny::tags$strong("volume")),
          shiny::column(1, shiny::tags$strong("number")),
          shiny::column(1, shiny::tags$strong("isbn")),
          shiny::column(1, shiny::tags$strong("doi")),
          shiny::column(1, shiny::tags$strong(""))
        )
      ),
      
      # Scrollable list of criteria
      shiny::div(
        style = "max-height: 600px; overflow-y: auto;",
        lapply(seq_along(criteria_list), function(i) {
          criterion <- criteria_list[[i]]
          criterion_id <- paste0("criterion_", i)
          
          # Helper function to get field value for numericInput
          # numericInput can accept NA_real_ which will display as blank
          get_field_val <- function(field_name) {
            if (!field_name %in% names(criterion)) {
              return(NA_real_)
            }
            val <- criterion[[field_name]]
            if (is.null(val)) {
              return(NA_real_)
            }
            if (length(val) == 0) {
              return(NA_real_)
            }
            # Convert logical NA to numeric NA
            if (is.logical(val) && is.na(val)) {
              return(NA_real_)
            }
            if (is.na(val)) {
              # If it's already NA, ensure it's numeric
              return(NA_real_)
            }
            # Ensure it's numeric
            num_val <- suppressWarnings(as.numeric(val))
            if (is.na(num_val)) {
              return(NA_real_)
            }
            return(num_val)
          }
          
          shiny::div(
            style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
            # Compact single-row layout
            shiny::fluidRow(
              # Criterion name (wider)
              shiny::column(2,
                shiny::textInput(
                  inputId = paste0(criterion_id, "_name"),
                  label = NULL,
                  value = ifelse(is.null(criterion$name) || criterion$name == "", "", criterion$name),
                  placeholder = "Name",
                  width = "100%"
                )
              ),
              # All field inputs in one row (smaller width)
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_title"),
                  label = "title",
                  value = get_field_val("title"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_author"),
                  label = "author",
                  value = get_field_val("author"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_abstract"),
                  label = "abstract",
                  value = get_field_val("abstract"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_journal"),
                  label = "journal",
                  value = get_field_val("journal"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_pages"),
                  label = "pages",
                  value = get_field_val("pages"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_volume"),
                  label = "volume",
                  value = get_field_val("volume"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_number"),
                  label = "number",
                  value = get_field_val("number"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_isbn"),
                  label = "isbn",
                  value = get_field_val("isbn"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              shiny::column(1,
                shiny::numericInput(
                  inputId = paste0(criterion_id, "_doi"),
                  label = "doi",
                  value = get_field_val("doi"),
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              # Remove button
              shiny::column(1,
                shiny::div(
                  style = "margin-top: 25px; text-align: center;",
                  shinyWidgets::actionBttn(
                    inputId = paste0("remove_criterion_", i),
                    label = NULL,
                    icon = shiny::icon("trash"),
                    style = "jelly",
                    color = "danger",
                    size = "xs"
                  )
                )
              )
            )
          )
        })
      )
    )
  })
  
  # Observer: Sync criterion input changes to reactive values
  # Use debounce to avoid too frequent updates
  # IMPORTANT: This reactive should depend on INPUT values, not rv$custom_validation_criteria
  # We use isolate() to read rv$custom_validation_criteria to get the structure, but
  # the reactive should trigger on input changes, not reactive value changes
  sync_criteria_inputs <- shiny::reactive({
    if (!isTRUE(input$use_custom_thresholds)) {
      return(NULL)
    }
    
    # Use isolate to get the structure without creating a reactive dependency
    # This prevents the reactive from re-running when rv$custom_validation_criteria changes
    criteria_list <- shiny::isolate(rv$custom_validation_criteria)
    if (is.null(criteria_list) || length(criteria_list) == 0) {
      return(NULL)
    }
    
    # Check if inputs exist - if not, don't sync (prevents creating "Criterion 1" entries)
    first_criterion_id <- paste0("criterion_1_name")
    first_title_id <- paste0("criterion_1_title")
    if (is.null(input[[first_criterion_id]])) {
      # Inputs don't exist yet, return NULL to prevent sync
      return(NULL)
    }
    
    # Make this reactive depend on input values by accessing them
    # This ensures the reactive re-runs when ANY input value changes
    # Access all input values to create reactive dependencies (but don't use the values yet)
    num_criteria <- length(criteria_list)
    for (i in 1:min(num_criteria, 50)) {  # Check up to 50 criteria
      criterion_id <- paste0("criterion_", i)
      # Access all input fields to create reactive dependencies
      # This makes the reactive depend on these inputs
      tryCatch({
        input[[paste0(criterion_id, "_name")]]
        input[[paste0(criterion_id, "_title")]]
        input[[paste0(criterion_id, "_author")]]
        input[[paste0(criterion_id, "_abstract")]]
        input[[paste0(criterion_id, "_journal")]]
        input[[paste0(criterion_id, "_pages")]]
        input[[paste0(criterion_id, "_volume")]]
        input[[paste0(criterion_id, "_number")]]
        input[[paste0(criterion_id, "_isbn")]]
        input[[paste0(criterion_id, "_doi")]]
      }, error = function(e) {
        # Input doesn't exist for this criterion, skip
      })
    }
    
    # Debug: Log what we're reading from inputs (for first criterion)
    title_val <- input[[first_title_id]]
    if (!is.null(title_val)) {
      message("Sync reactive triggered: Reading criterion_1_title from input: ", title_val)
    }
    
    # Update each criterion from inputs
    updated_criteria <- lapply(seq_along(criteria_list), function(i) {
      criterion_id <- paste0("criterion_", i)
      
      # Get name - preserve existing name if input is empty or unchanged
      name_input <- input[[paste0(criterion_id, "_name")]]
      existing_name <- if (i <= length(criteria_list) && !is.null(criteria_list[[i]]$name)) {
        criteria_list[[i]]$name
      } else {
        ""
      }
      
      # Preserve existing name unless user explicitly changes it
      if (is.null(name_input) || name_input == "") {
        # Input is empty - preserve existing name, or use generic only if no name exists
        name <- ifelse(existing_name == "", paste0("Criterion ", i), existing_name)
      } else if (name_input == existing_name) {
        # Input matches existing - preserve it
        name <- existing_name
      } else if (grepl("^Criterion \\d+$", name_input) && existing_name != "") {
        # Input is generic but we have an existing name - preserve existing
        name <- existing_name
      } else {
        # User has explicitly changed the name - use the new input
        name <- name_input
      }
      
      # Get field values
      # Accessing input values here creates reactive dependencies
      get_field_value <- function(field) {
        input_id <- paste0(criterion_id, "_", field)
        # Access the input - this creates a reactive dependency
        val <- input[[input_id]]
        # Debug: Log first criterion's first field to verify inputs are being read
        if (i == 1 && field == "title" && !is.null(val)) {
          message("Sync reactive: Reading input for criterion_1_", field, ": ", val)
        }
        if (is.null(val) || is.na(val) || val <= 0) {
          return(NA_real_)
        }
        return(as.numeric(val))
      }
      
      list(
        name = name,
        title = get_field_value("title"),
        author = get_field_value("author"),
        abstract = get_field_value("abstract"),
        journal = get_field_value("journal"),
        pages = get_field_value("pages"),
        volume = get_field_value("volume"),
        number = get_field_value("number"),
        isbn = get_field_value("isbn"),
        doi = get_field_value("doi")
      )
    })
    
    return(updated_criteria)
  })
  
  # Debounce the sync to avoid too frequent updates
  sync_criteria_debounced <- shiny::debounce(sync_criteria_inputs, millis = 500)
  
  # Create a reactive that tracks when inputs change
  # This will trigger the observer when any input changes
  input_change_tracker <- shiny::reactive({
    if (!isTRUE(input$use_custom_thresholds)) {
      return(NULL)
    }
    # Access a sample of inputs to create dependencies
    # This reactive will fire when any of these inputs change
    criteria_list <- shiny::isolate(rv$custom_validation_criteria)
    if (is.null(criteria_list) || length(criteria_list) == 0) {
      return(NULL)
    }
    # Access first criterion's inputs to create dependencies
    tryCatch({
      input[[paste0("criterion_1_title")]]
      input[[paste0("criterion_1_author")]]
      input[[paste0("criterion_1_abstract")]]
    }, error = function(e) NULL)
    return(Sys.time())  # Return timestamp to ensure reactive fires
  })
  
  shiny::observe({
    # Make observer depend on input changes
    input_change_tracker()
    
    # Check if sync should be unlocked based on timestamp
    if (!is.null(rv$unlock_sync_at) && Sys.time() >= rv$unlock_sync_at) {
      rv$sync_locked <- FALSE
      rv$unlock_sync_at <- NULL
      message("Sync unlocked after deletion (2 second delay)")
    }
    
    # CRITICAL: Don't sync if sync is locked (e.g., right after deletion)
    # This prevents reading stale inputs during UI re-render
    if (isTRUE(rv$sync_locked)) {
      message("Sync observer: Sync is locked, skipping")
      return()  # Exit early, don't sync
    }
    
    # Sync criteria from inputs to reactive values
    # Always preserve existing names unless user explicitly changes them
    # Calling sync_criteria_debounced() here creates a reactive dependency
    # The observer will re-run when the debounced reactive changes
    updated <- sync_criteria_debounced()
    
    # Debug: Always log what we got from the debounced reactive
    if (!is.null(updated) && length(updated) > 0) {
      message("Sync observer: Got ", length(updated), " criteria from debounced reactive")
      if (length(updated) > 0) {
        message("Sync observer: First criterion title value: ", updated[[1]]$title)
      }
    } else {
      message("Sync observer: debounced reactive returned NULL or empty")
    }
    
    if (!is.null(updated) && !is.null(rv$custom_validation_criteria) && length(updated) > 0) {
      # Only update if inputs exist and criteria count matches
      if (length(updated) == length(rv$custom_validation_criteria)) {
        message("Sync observer: Counts match (", length(updated), " criteria)")
        # Preserve names: if updated name is empty or generic, keep existing name
        for (i in seq_along(updated)) {
          updated_name <- updated[[i]]$name
          existing_name <- if (i <= length(rv$custom_validation_criteria) && 
                               !is.null(rv$custom_validation_criteria[[i]]$name)) {
            rv$custom_validation_criteria[[i]]$name
          } else {
            ""
          }
          
          # If updated name is empty, generic ("Criterion X"), or unchanged, preserve existing
          if (is.null(updated_name) || updated_name == "" || 
              grepl("^Criterion \\d+$", updated_name) ||
              (existing_name != "" && updated_name == existing_name)) {
            if (existing_name != "") {
              updated[[i]]$name <- existing_name
            }
          }
        }
        
        # Debug: Log what we're syncing (for first criterion only, and only if values changed)
        if (length(updated) > 0 && length(rv$custom_validation_criteria) > 0) {
          first_updated <- updated[[1]]
          first_existing <- rv$custom_validation_criteria[[1]]
          
          # Check if values actually changed
          values_changed <- FALSE
          for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
            updated_val <- first_updated[[field]]
            existing_val <- first_existing[[field]]
            if (!identical(updated_val, existing_val)) {
              values_changed <- TRUE
              break
            }
          }
          
          if (values_changed) {
            message("Sync: Updating criterion '", first_updated$name, 
                    "' - title: ", first_updated$title, 
                    ", author: ", first_updated$author,
                    " (was: title=", first_existing$title, ", author=", first_existing$author, ")")
          }
        }
        
        # Only update if something actually changed (excluding name preservation)
        # Use a more lenient comparison that ignores list attributes
        lists_identical <- tryCatch({
          identical(updated, rv$custom_validation_criteria)
        }, error = function(e) {
          FALSE  # If comparison fails, assume they're different
        })
        
        if (!lists_identical) {
          message("Sync observer: Lists are different, updating reactive values")
          message("Sync observer: Before update - first criterion title: ", rv$custom_validation_criteria[[1]]$title)
          rv$custom_validation_criteria <- updated
          message("Sync observer: After update - first criterion title: ", rv$custom_validation_criteria[[1]]$title)
          if (!isTRUE(rv$criteria_manually_edited)) {
            rv$criteria_manually_edited <- TRUE  # Mark as manually edited when user changes values
          }
        } else {
          message("Sync observer: Lists are identical, skipping update")
        }
      } else {
        # Debug: Log when counts don't match
        if (!is.null(updated) && !is.null(rv$custom_validation_criteria)) {
          message("Sync: Count mismatch - updated has ", length(updated), 
                  " criteria, reactive has ", length(rv$custom_validation_criteria))
        }
      }
    } else {
      # Debug: Log when sync returns NULL
      if (is.null(updated)) {
        message("Sync: sync_criteria_debounced() returned NULL (inputs may not exist yet)")
      }
    }
  })
  
  # Observer: Add new criterion
  shiny::observeEvent(input$add_criterion, {
    new_criterion <- list(
      name = paste0("New Criterion ", length(rv$custom_validation_criteria) + 1),
      title = NA_real_,
      author = NA_real_,
      abstract = NA_real_,
      journal = NA_real_,
      pages = NA_real_,
      volume = NA_real_,
      number = NA_real_,
      isbn = NA_real_,
      doi = NA_real_
    )
    rv$custom_validation_criteria <- c(rv$custom_validation_criteria, list(new_criterion))
    rv$criteria_manually_edited <- TRUE  # Mark as manually edited to prevent auto-reload
  })
  
  # Observer: Remove criterion (handle dynamically)
  # Use a reactive value to track which criterion to remove
  rv$criterion_to_remove <- NULL
  
  # Create observers for remove buttons (up to 50 criteria)
  lapply(1:50, function(i) {
    shiny::observeEvent(input[[paste0("remove_criterion_", i)]], {
      if (!is.null(rv$custom_validation_criteria) && i <= length(rv$custom_validation_criteria)) {
        criterion_name <- rv$custom_validation_criteria[[i]]$name
        if (is.null(criterion_name) || criterion_name == "") {
          criterion_name <- paste0("Criterion ", i)
        }
        shiny::showModal(shiny::modalDialog(
          title = "Remove Criterion",
          paste("Are you sure you want to remove '", criterion_name, "'?"),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("confirm_remove_criterion", "Remove", class = "btn-danger")
          )
        ))
        rv$criterion_to_remove <- i
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
  
  # Confirm remove
  shiny::observeEvent(input$confirm_remove_criterion, {
    if (!is.null(rv$criterion_to_remove) && rv$criterion_to_remove <= length(rv$custom_validation_criteria)) {
      # Store index before removing
      index_to_remove <- rv$criterion_to_remove
      
      # CRITICAL: Set flags FIRST, before any reactive updates
      # This prevents any observers from reloading defaults or syncing stale inputs
      rv$criteria_manually_edited <- TRUE
      rv$defaults_loaded <- TRUE  # Also ensure defaults_loaded is TRUE to prevent reload
      rv$sync_locked <- TRUE  # Lock sync to prevent reading stale inputs during UI re-render
      
      # Now remove the criterion
      rv$custom_validation_criteria <- rv$custom_validation_criteria[-index_to_remove]
      rv$criterion_to_remove <- NULL
      
      message("Removed criterion at index ", index_to_remove, 
              ". Criteria manually edited flag set to TRUE. Sync locked. Remaining criteria: ", 
              length(rv$custom_validation_criteria))
      
      shiny::removeModal()
      
      # Schedule unlock after UI re-renders (2 seconds should be enough)
      # Set a timestamp for when sync should be unlocked
      rv$unlock_sync_at <- Sys.time() + 2  # Unlock in 2 seconds
    }
  })
  
  # Observer: Reset to defaults
  shiny::observeEvent(input$reset_criteria, {
    shiny::showModal(shiny::modalDialog(
      title = "Reset to Defaults",
      "This will replace all your custom criteria with the default ASySD criteria. Continue?",
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("confirm_reset_criteria", "Reset", class = "btn-warning")
      )
    ))
  })
  
  shiny::observeEvent(input$confirm_reset_criteria, {
    defaults <- load_default_validation_criteria()
    if (length(defaults) > 0) {
      # Defensive: ensure all values are numeric (convert any logical NAs to numeric NAs)
      defaults <- lapply(defaults, function(crit) {
        for (field in c("title", "author", "abstract", "journal", "pages", "volume", "number", "isbn", "doi")) {
          if (field %in% names(crit)) {
            if (is.logical(crit[[field]]) && is.na(crit[[field]])) {
              crit[[field]] <- NA_real_
            } else if (!is.numeric(crit[[field]])) {
              num_val <- suppressWarnings(as.numeric(crit[[field]]))
              crit[[field]] <- if (is.na(num_val)) NA_real_ else num_val
            }
          }
        }
        crit
      })
      rv$custom_validation_criteria <- defaults
      rv$defaults_loaded <- TRUE  # Mark as loaded after reset
      rv$criteria_manually_edited <- FALSE  # Reset flag since we're back to defaults
    } else {
      show_toastr(
        "Error Loading Defaults",
        "Could not load default ASySD criteria. Please reload the CiteSource package.",
        type = "error"
      )
    }
    shiny::removeModal()
  })
  
  # Note: Defaults loading is now handled by the main observer above
  
  # Show statistics flag - show if stats exist (even if empty)
  output$show_dedup_stats <- shiny::reactive({
    !is.null(rv$dedup_stats)
  })
  shiny::outputOptions(output, "show_dedup_stats", suspendWhenHidden = FALSE)
  
  
  # Deduplication statistics table
  output$dedup_statistics_table <- shiny::renderUI({
    if (is.null(rv$dedup_stats)) {
      return(shiny::div(
        shiny::p(style = "color: #666; font-style: italic;", 
                 "Statistics will appear here after deduplication completes.")
      ))
    }
    
    stats <- rv$dedup_stats
    blocking_stats <- stats$blocking_round_stats
    validation_stats <- stats$validation_stats
    
    # Check if stats are empty dataframes
    blocking_empty <- is.null(blocking_stats) || (is.data.frame(blocking_stats) && nrow(blocking_stats) == 0)
    validation_empty <- is.null(validation_stats) || (is.data.frame(validation_stats) && nrow(validation_stats) == 0)
    
    shiny::tagList(
      # Blocking rounds statistics
      shiny::h6("Blocking Rounds Statistics"),
      DT::DTOutput("blocking_stats_table"),
      if (blocking_empty) {
        shiny::p(style = "color: #666; font-style: italic; margin-top: 10px;", 
                 "No pairs were identified in any blocking round.")
      },
      
      shiny::br(),
      
      # Validation criteria statistics
      shiny::h6("Validation Criteria Statistics"),
      DT::DTOutput("validation_stats_table"),
      if (validation_empty) {
        shiny::p(style = "color: #666; font-style: italic; margin-top: 10px;", 
                 "No pairs were confirmed as duplicates by any validation criterion.")
      }
    )
  })
  
  # Blocking statistics table
  output$blocking_stats_table <- DT::renderDataTable({
    if (is.null(rv$dedup_stats) || is.null(rv$dedup_stats$blocking_round_stats)) {
      # Return empty table with proper structure
      empty_df <- data.frame(
        Round = character(),
        `Pairs Identified` = integer(),
        stringsAsFactors = FALSE
      )
      return(DT::datatable(empty_df, options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE))
    }
    
    stats <- rv$dedup_stats$blocking_round_stats
    
    # If stats dataframe is empty, return empty table
    if (nrow(stats) == 0) {
      empty_df <- data.frame(
        Round = character(),
        `Pairs Identified` = integer(),
        stringsAsFactors = FALSE
      )
      return(DT::datatable(empty_df, options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE))
    }
    
    stats <- stats %>%
      dplyr::arrange(round_number) %>%  # Ensure proper ordering first
      dplyr::mutate(
        Round = round_name,  # round_name already includes "Round X (Description)"
        `Pairs Identified` = pair_count
      ) %>%
      dplyr::select(Round, `Pairs Identified`)
    
    DT::datatable(
      stats,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Validation statistics table
  output$validation_stats_table <- DT::renderDataTable({
    if (is.null(rv$dedup_stats) || is.null(rv$dedup_stats$validation_stats)) {
      # Return empty table with proper structure
      empty_df <- data.frame(
        `Validation Criterion` = character(),
        `Pairs Confirmed as Duplicates` = integer(),
        stringsAsFactors = FALSE
      )
      return(DT::datatable(empty_df, options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE))
    }
    
    stats <- rv$dedup_stats$validation_stats
    
    # If stats dataframe is empty, return empty table
    if (nrow(stats) == 0) {
      empty_df <- data.frame(
        `Validation Criterion` = character(),
        `Pairs Confirmed as Duplicates` = integer(),
        stringsAsFactors = FALSE
      )
      return(DT::datatable(empty_df, options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE))
    }
    
    stats <- stats %>%
      dplyr::arrange(dplyr::desc(pair_count)) %>%
      dplyr::mutate(
        `Validation Criterion` = criterion_name,
        `Pairs Confirmed as Duplicates` = pair_count
      ) %>%
      dplyr::select(`Validation Criterion`, `Pairs Confirmed as Duplicates`)
    
    DT::datatable(
      stats,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        order = list(list(1, 'desc'))
      ),
      rownames = FALSE
    )
  })
  
  ## How Deduplication works tab
  
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
  
  # Progress indicator for card view (compact version in filter panel)
  output$dedup_progress <- shiny::renderUI({
    filtered <- filtered_pairs()
    if (nrow(filtered) == 0) {
      return(shiny::div())
    }
    
    current <- rv$current_pair_index
    total <- nrow(filtered)
    selected_count <- length(rv$selected_pairs_card)
    
    shiny::div(
      style = "padding-top: 5px;",
      shiny::tags$p(
        style = "margin: 0 0 5px 0; font-size: 0.95em; font-weight: bold; color: #23395B;",
        paste("Pair", current, "of", total)
      ),
      shiny::tags$p(
        style = "margin: 0 0 8px 0; font-size: 0.85em; color: #666;",
        paste(selected_count, "selected")
      ),
      shiny::tags$div(
        style = paste0("width: 100%; height: 6px; background-color: #e0e0e0; border-radius: 3px; margin-bottom: 8px;"),
        shiny::tags$div(
          style = paste0("width: ", round((current / total) * 100), "%; height: 100%; background-color: #008080; border-radius: 3px; transition: width 0.3s;")
        )
      ),
      shiny::div(
        style = "text-align: center;",
        shinyWidgets::actionBttn(
          inputId = "dedup_prev_pair",
          label = "",
          icon = shiny::icon("chevron-left"),
          style = "jelly",
          color = "primary",
          size = "xs"
        ) %>% htmltools::tagAppendAttributes(style = "margin-right: 5px;"),
        shinyWidgets::actionBttn(
          inputId = "dedup_next_pair",
          label = "",
          icon = shiny::icon("chevron-right"),
          style = "jelly",
          color = "primary",
          size = "xs"
        )
      )
    )
  })
  
  # Helper function to build field with preference selector
  build_field_with_preference <- function(field, val1, val2, comparison, is_record_a, pair_row_idx) {
    field_class <- paste0("dedup-field ", comparison$status)
    field_value <- if (is_record_a) comparison$val1 else comparison$val2
    
    # Fields where user might want to override (when values differ)
    selectable_fields <- c("author", "abstract", "title", "journal")
    is_selectable <- field %in% selectable_fields && (comparison$status == "different" || comparison$status == "missing")
    
    # Get current preference
    prefs <- get_pair_preferences(pair_row_idx)
    current_pref <- prefs[[field]]
    
    # Determine if this field is preferred
    is_preferred <- FALSE
    if (!is.null(current_pref) && length(current_pref) > 0) {
      is_preferred <- if (is_record_a) {
        current_pref == "A"
      } else {
        current_pref == "B"
      }
      # Handle case where comparison returns NA or empty
      if (is.na(is_preferred)) is_preferred <- FALSE
    }
    
    # Determine default selection (which would be kept by ASySD)
    # For selectable fields with "different" or "missing" status, show which is default
    is_default <- FALSE
    show_default_badge <- FALSE

    if (is_selectable && (comparison$status == "different" || comparison$status == "missing")) {
      val1_len <- nchar(as.character(comparison$val1))
      val2_len <- nchar(as.character(comparison$val2))
      if (comparison$status == "missing") {
        # Show badge on the non-missing value
        if (comparison$val1 != "N/A" && is_record_a) {
          is_default <- TRUE
        } else if (comparison$val2 != "N/A" && !is_record_a) {
          is_default <- TRUE
        } else {
          is_default <- FALSE
        }
        show_default_badge <- is_default
      } else {
        # 'different' logic as before
        if (val1_len > val2_len) {
          is_default <- is_record_a
        } else if (val2_len > val1_len) {
          is_default <- !is_record_a
        } else {
          is_default <- is_record_a
        }
        show_default_badge <- TRUE
      }
    }
    
    record_label <- if (is_record_a) "Record A" else "Record B"
    
    shiny::tags$div(
      class = paste0("dedup-field ", comparison$status, if (isTRUE(is_preferred)) " field-preferred" else ""),
      style = if (isTRUE(is_preferred)) "background-color: #f0f8ff; border-left: 3px solid #008080;" else "",
      shiny::tags$div(
        class = "dedup-field-label",
        shiny::tags$span(stringr::str_to_title(field), ":"),
        if (is_selectable) {
          shiny::tagList(
            shiny::actionButton(
              paste0("field_pref_", pair_row_idx, "_", field, "_", if (is_record_a) "A" else "B"),
              label = shiny::tagList(shiny::icon("check"), if (isTRUE(is_preferred)) "Selected" else "Use This"),
              class = paste0("btn-field-preference", if (isTRUE(is_preferred)) " selected" else ""),
              style = if (isTRUE(is_preferred)) "background-color: white; color: #2d8659; border: 2px solid #2d8659; font-weight: bold;" else "background-color: white; color: #333; border: 1px solid #ddd;",
              size = "sm"
            ),
            if (show_default_badge && isTRUE(is_default)) {
              shiny::tags$span(
                class = "default-indicator",
                style = "margin-left: 6px;",
                shiny::icon("star"),
                "Default"
              )
            }
          )
        }
      ),
      shiny::tags$div(
        class = "dedup-field-value",
        field_value
      )
    )
  }
  
  # Helper function to compare field values
  compare_field <- function(val1, val2) {
    # Normalize values
    if (is.na(val1) || val1 == "" || val1 == "NA") val1 <- ""
    if (is.na(val2) || val2 == "" || val2 == "NA") val2 <- ""
    
    if (val1 == "" && val2 == "") {
      return(list(status = "missing", val1 = "N/A", val2 = "N/A"))
    } else if (val1 == "") {
      return(list(status = "missing", val1 = "N/A", val2 = val2))
    } else if (val2 == "") {
      return(list(status = "missing", val1 = val1, val2 = "N/A"))
    } else {
      val1_norm <- tolower(trimws(val1))
      val2_norm <- tolower(trimws(val2))
      
      if (val1_norm == val2_norm) {
        return(list(status = "match", val1 = val1, val2 = val2))
      } else {
        return(list(status = "different", val1 = val1, val2 = val2))
      }
    }
  }
  
  # Card view display
  output$dedup_card_view <- shiny::renderUI({
    filtered <- filtered_pairs()
    
    if (nrow(filtered) == 0) {
      return(
        shiny::wellPanel(
          style = "text-align: center; padding: 40px;",
          shiny::tags$p(
            style = "font-size: 1.2em; color: #666;",
            "No pairs match the current similarity filter. Try lowering the minimum similarity score."
          )
        )
      )
    }
    
    if (rv$current_pair_index < 1 || rv$current_pair_index > nrow(filtered)) {
      return(shiny::div())
    }
    
    pair <- filtered[rv$current_pair_index, ]
    similarity <- pair$similarity_score
    
    # Determine similarity badge class
    similarity_class <- if (similarity >= 80) {
      "dedup-similarity-high"
    } else if (similarity >= 50) {
      "dedup-similarity-medium"
    } else {
      "dedup-similarity-low"
    }
    
    # Fields to display - always show all fields to ensure alignment
    fields_to_show <- c("title", "author", "year", "journal", "doi", "pages", "volume", "abstract", "source", "label")
    
    # Get pair row index for preference tracking
    pair_row_idx <- if ("original_row_index" %in% names(pair)) pair$original_row_index else rv$current_pair_index
    
    # Build record A card - always include all fields for alignment
    record_a_fields <- lapply(fields_to_show, function(field) {
      col1 <- paste0(field, "1")
      col2 <- paste0(field, "2")
      
      # Always create the field div, even if columns don't exist
      val1 <- if (col1 %in% names(pair)) pair[[col1]] else ""
      val2 <- if (col2 %in% names(pair)) pair[[col2]] else ""
      
      comparison <- compare_field(val1, val2)
      
      build_field_with_preference(field, val1, val2, comparison, TRUE, pair_row_idx)
    })
    
    # Build record B card - always include all fields for alignment
    record_b_fields <- lapply(fields_to_show, function(field) {
      col1 <- paste0(field, "1")
      col2 <- paste0(field, "2")
      
      # Always create the field div, even if columns don't exist
      val1 <- if (col1 %in% names(pair)) pair[[col1]] else ""
      val2 <- if (col2 %in% names(pair)) pair[[col2]] else ""
      
      comparison <- compare_field(val1, val2)
      
      build_field_with_preference(field, val1, val2, comparison, FALSE, pair_row_idx)
    })
    
    # Check if current pair is selected
    is_selected <- !is.null(pair_row_idx) && pair_row_idx %in% rv$selected_pairs_card
    
    shiny::fluidRow(
      shiny::column(
        12,
        # Action toolbar at the top
        shiny::wellPanel(
          style = "background-color: #f8f9fa; padding: 12px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::tags$p(
                style = "margin: 0; font-size: 0.9em;",
                shiny::tags$span(style = "background-color: #d4edda; padding: 3px 10px; border-radius: 3px; margin-right: 5px;", "Green = Match"),
                shiny::tags$span(style = "background-color: #fff3cd; padding: 3px 10px; border-radius: 3px; margin-right: 5px;", "Yellow = Different"),
                shiny::tags$span(style = "background-color: #f8d7da; padding: 3px 10px; border-radius: 3px;", "Red = Missing")
              ), shiny::tags$div(
                style = "margin-top: 8px; font-size: 0.85em; color: #555; font-style: italic;",
                shiny::icon("info-circle"), " Tip: Selecting a specific value ('Use This') automatically marks the pair as a duplicate."
              )
            ),
            shiny::column(
              6,
              shiny::div(
                style = "text-align: right;",
                shiny::tags$strong(
                  style = "margin-right: 10px; color: #23395B; font-size: 0.9em;",
                  "Quick Actions:"
                ),
                shinyWidgets::actionBttn(
                  inputId = "dedup_mark_duplicate",
                  label = "Duplicate",
                  icon = shiny::icon("check"),
                  style = "jelly",
                  color = "success",
                  size = "sm"
                ) %>% htmltools::tagAppendAttributes(style = "background-color: #82D173; margin-right: 6px;"),
                shinyWidgets::actionBttn(
                  inputId = "dedup_mark_not_duplicate",
                  label = "Not Duplicate",
                  icon = shiny::icon("times"),
                  style = "jelly",
                  color = "danger",
                  size = "sm"
                ) %>% htmltools::tagAppendAttributes(style = "background-color: #dc3545; margin-right: 6px;"),
                shinyWidgets::actionBttn(
                  inputId = "dedup_skip",
                  label = "Skip",
                  icon = shiny::icon("forward"),
                  style = "jelly",
                  color = "warning",
                  size = "sm"
                )
              )
            )
          )
        ),
        # Similarity badge
        shiny::div(
          class = paste("dedup-similarity-badge", similarity_class),
          paste("Similarity:", similarity, "%")
        ),
        shiny::br(),
        # Two cards side by side - using equal height columns
        shiny::tags$div(
          class = "dedup-cards-container",
          shiny::fluidRow(
            shiny::column(
              6,
              style = "display: flex; flex-direction: column; padding-left: 7.5px; padding-right: 7.5px;",
              shiny::div(
                class = "dedup-card record-a",
                style = "flex: 1;",
                shiny::tags$h6(
                  style = "margin-top: 0; margin-bottom: 8px; color: #008080; font-size: 1em; flex-shrink: 0;",
                  shiny::icon("file-alt"), " Record A",
                  if (is_selected) {
                    shiny::tags$span(
                      style = "float: right; color: #82D173; font-size: 0.9em;",
                      shiny::icon("check-circle"), " Selected"
                    )
                  }
                ),
                shiny::div(
                  class = "dedup-card-body",
                  record_a_fields
                )
              )
            ),
            shiny::column(
              6,
              style = "display: flex; flex-direction: column; padding-left: 7.5px; padding-right: 7.5px;",
              shiny::div(
                class = "dedup-card record-b",
                style = "flex: 1;",
                shiny::tags$h6(
                  style = "margin-top: 0; margin-bottom: 8px; color: #23395B; font-size: 1em; flex-shrink: 0;",
                  shiny::icon("file-alt"), " Record B",
                  if (is_selected) {
                    shiny::tags$span(
                      style = "float: right; color: #82D173; font-size: 0.9em;",
                      shiny::icon("check-circle"), " Selected"
                    )
                  }
                ),
                shiny::div(
                  class = "dedup-card-body",
                  record_b_fields
                )
              )
            )
          )
        )
      )
    )
  })
  

  # --- Handle the custom JavaScript button click ---
  shiny::observeEvent(input$field_preference_click, {
    click_data <- input$field_preference_click
    
    # Check if data exists
    req(click_data)
    
    # Log for debugging (optional, prints to R console)
     message("Click received: Pair ", click_data$pair_idx, 
             " Field: ", click_data$field, 
             " Record: ", click_data$record)
    
    # Save the preference to the reactive value
    set_field_preference(click_data$pair_idx, click_data$field, click_data$record)
  })  
  # Skip button handler
  shiny::observeEvent(input$dedup_skip, {
    filtered <- filtered_pairs()
    if (nrow(filtered) > 0 && rv$current_pair_index < nrow(filtered)) {
      rv$current_pair_index <- rv$current_pair_index + 1
      shiny::updateNumericInput(
        session = session,
        inputId = "current_pair_index",
        value = rv$current_pair_index
      )
    }
  })
  
  # Function to apply field preferences to merged records
  apply_field_preferences <- function(merged_data, pairs_removed, original_unique) {
    if (nrow(pairs_removed) == 0) return(merged_data)
    
    # --- Detect the correct ID column name ---
    # CiteSource sometimes uses 'record_id' and sometimes 'duplicate_id'
    id_col <- "record_id"
    if (!"record_id" %in% names(merged_data)) {
      if ("duplicate_id" %in% names(merged_data)) {
        id_col <- "duplicate_id"
      } else {
        warning("apply_field_preferences: Could not find record_id or duplicate_id in merged data.")
        return(merged_data)
      }
    }
    
    # Iterate through every removed pair to check for preferences
    for (i in seq_len(nrow(pairs_removed))) {
      pair <- pairs_removed[i, ]
      
      # 1. Decode the JSON preferences
      prefs_json <- if ("field_preferences" %in% names(pair)) pair$field_preferences else "{}"
      if (is.na(prefs_json) || prefs_json == "") prefs_json <- "{}"
      
      prefs <- tryCatch(
        jsonlite::fromJSON(prefs_json),
        error = function(e) list()
      )
      
      # 2. If user made specific selections for this pair
      if (length(prefs) > 0) {
        
        # Identify the ID of the record that SURVIVED in the final dataset
        # handle tibble vs dataframe access safely
        id1 <- if("record_id1" %in% names(pair)) pair[["record_id1"]] else pair[["record_id.x"]]
        id2 <- if("record_id2" %in% names(pair)) pair[["record_id2"]] else pair[["record_id.y"]]
        
        # Check if we are dealing with NAs
        if (is.null(id1)) id1 <- NA
        if (is.null(id2)) id2 <- NA
        
        # Find the row index of the survivor in the merged dataset
        # accessing the column dynamically using [[id_col]]
        target_row <- which(merged_data[[id_col]] == id1)
        if (length(target_row) == 0) {
          target_row <- which(merged_data[[id_col]] == id2)
        }
        
        # 3. Overwrite the data
        if (length(target_row) > 0) {
          for (field in names(prefs)) {
            choice <- prefs[[field]] # "A" or "B"
            
            # Determine which column in the pair contained the chosen value
            source_col <- paste0(field, if (choice == "A") "1" else "2")
            
            # If that column exists in the pair data
            if (source_col %in% names(pair)) {
              new_value <- pair[[source_col]]
              
              # Force this value into the final merged dataset
              # Ensure we aren't writing NULL or weird types
              if (field %in% names(merged_data)) {
                if(is.null(new_value) || length(new_value) == 0) new_value <- NA
                merged_data[target_row, field] <- new_value
              }
            }
          }
        }
      }
    }
    
    return(merged_data)
  }
  
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
    
    # Separate rows for source and label using helper function
    df_long <- df %>%
      CiteSource:::expand_metadata_columns(columns = c("cite_source", "cite_label"))
    
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
      show_toastr("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error"
      )
      shiny::req(FALSE)
    }
    
    filtered_data <- unique_filtered_visual()
    
    if (nrow(filtered_data) == 0) {
      # Create an informative empty plot
      empty_plot <- plotly::plot_ly() %>%
        plotly::add_annotations(
          text = paste(
            "No data matches your current filters.",
            "<br><br>",
            "<b>Try:</b>",
            "<br>• Deselecting some filters",
            "<br>• Checking that your selected sources/labels/strings exist in your data",
            "<br>• Ensuring your comparison type matches your filter selections"
          ),
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        plotly::layout(
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE)
        )
      return(empty_plot)
    }
    
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
      show_toastr("Data needed",
                  "Please import and deduplicate your citations first.",
                  type = "error"
      )
      shiny::req(FALSE)
    }
    
    filtered_data <- unique_filtered_visual()
    
    if (nrow(filtered_data) == 0) {
      # Create informative empty plot
      plot.new()
      text(0.5, 0.7, "No data matches your current filters.", 
           cex = 1.2, font = 2)
      text(0.5, 0.5, "Try deselecting some filters or", cex = 1)
      text(0.5, 0.45, "checking that your selections exist in your data.", cex = 1)
      return()
    }
    
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
      show_toastr("Data needed",
                             "Please import and deduplicate your citations first.",
                             type = "error")
      shiny::req(FALSE, cancelOutput = TRUE)
    }
    
    # Use the reactive data specifically prepared for this plot
    plot_data <- unique_separated_phase()
    
    # Check if the prepared data is empty after filtering
    if (nrow(plot_data) == 0) {
      # Display a helpful message instead of just cancelling
      plot.new()
      text(0.5, 0.7, "No data matches your current filters for Phase Analysis.", 
           cex = 1.2, font = 2)
      text(0.5, 0.5, "Try:", cex = 1.1, font = 2)
      text(0.5, 0.4, "• Deselecting some source or label filters", cex = 1)
      text(0.5, 0.35, "• Checking that your selected items exist in your data", cex = 1)
      text(0.5, 0.3, "• Ensuring you have data with both sources and labels", cex = 1)
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
    
    # Separate cite_source column using helper function
    df_long_source <- df_filtered_wide %>%
      dplyr::select(duplicate_id, cite_source, cite_label, cite_string) %>% 
      CiteSource:::expand_single_metadata_column("cite_source") %>%
      dplyr::filter(cite_source != "unknown")  # Exclude "unknown" - records from screened/final phases aren't search sources
    
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
      show_toastr("Data needed", "Please import and deduplicate your citations first.", type = "error")
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
    
    # Check if base data is loaded
    if (!is.data.frame(rv$latest_unique) || nrow(rv$latest_unique) == 0) {
      show_toastr(
        "Data needed",
        "Please import and deduplicate your citations first.",
        type = "error"
      )
      shiny::req(FALSE)
    }
    
    unique_citations <- unique_filtered_table()
    
    # Check if data is available after filtering
    if (nrow(unique_citations) == 0) {
      show_toastr(
        "No data available",
        "No records match your current filter selections. Please adjust your filters and try again.",
        type = "warning"
      )
      shiny::req(FALSE)
    }
    
    # The table is only for phase comparison, include "final" in labels for comparison
    if (!any(stringr::str_detect(tolower(unique_citations$cite_label), "final"))) {
      show_toastr(
        "Missing required data",
        "The Precision/Sensitivity table requires data labeled as 'final' to compare against other phases. Please ensure you have uploaded and labeled data with a 'final' phase, or adjust your filter selections to include records with 'final' labels.",
        type = "error"
      )
      shiny::req(FALSE)
    }
    
    unique_citations <- unique_filtered_table()
    phase_counts <- calculate_phase_records(unique_citations, n_unique, "cite_source")
    create_precision_sensitivity_table(phase_counts)
  }) %>% shiny::bindEvent(input$generatePrecisionTable)
  
  
  # Rendering the record-level table ----
  output$reviewTab <- DT::renderDataTable({
    
    if (nrow(rv$latest_unique) == 0) {
      show_toastr("Data needed",
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
  
  # Reactive to check if data is available for export
  export_data_available <- shiny::reactive({
    is.data.frame(rv$latest_unique) && nrow(rv$latest_unique) > 0
  })
  
  # Observe download button clicks to show errors before download starts
  shiny::observeEvent(input$downloadCsv, {
    if (!export_data_available()) {
      show_toastr(
        "No data to download",
        "Please import and deduplicate your citations before downloading. There is no data available to export.",
        type = "error"
      )
    }
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$downloadBib, {
    if (!export_data_available()) {
      show_toastr(
        "No data to download",
        "Please import and deduplicate your citations before downloading. There is no data available to export.",
        type = "error"
      )
    }
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$downloadRis, {
    if (!export_data_available()) {
      show_toastr(
        "No data to download",
        "Please import and deduplicate your citations before downloading. There is no data available to export.",
        type = "error"
      )
    }
  }, ignoreInit = TRUE)
  
  # Downloadable CSV ----
  output$downloadCsv <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Validate data is available
      shiny::validate(
        shiny::need(export_data_available(), 
                   "No data available. Please import and deduplicate citations first.")
      )
      
      # Attempt to write the file
      tryCatch({
        write.csv(rv$latest_unique, file, row.names = FALSE)
      }, error = function(e) {
        # Write error message to file so user can see what went wrong
        error_msg <- paste("Error creating CSV file:", e$message, 
                          "\n\nPlease ensure you have imported and deduplicated your citations, then try again.")
        writeLines(error_msg, file)
        # Also show toastr notification
        show_toastr(
          "CSV export failed",
          paste("An error occurred while creating the CSV file:", e$message, "Please try again."),
          type = "error"
        )
      })
    }
  )
  
  # Downloadable BibTeX ----
  output$downloadBib <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".bib", sep = "")
    },
    content = function(file) {
      # Validate data is available
      shiny::validate(
        shiny::need(export_data_available(), 
                   "No data available. Please import and deduplicate citations first.")
      )
      
      # Attempt to export
      tryCatch({
        export_bib(rv$latest_unique, file)
      }, error = function(e) {
        # Write error message to file
        error_msg <- paste("% Error creating BibTeX file:", e$message,
                          "\n% Please ensure you have imported and deduplicated your citations, then try again.")
        writeLines(error_msg, file)
        # Show toastr notification
        show_toastr(
          "BibTeX export failed",
          paste("An error occurred while creating the BibTeX file:", e$message, "Please try again."),
          type = "error"
        )
      })
    }
  )
  
  # Downloadable RIS ----
  output$downloadRis <- shiny::downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".ris", sep = "")
    },
    content = function(file) {
      # Validate data is available
      shiny::validate(
        shiny::need(export_data_available(), 
                   "No data available. Please import and deduplicate citations first.")
      )
      
      # Attempt to export
      tryCatch({
        export_ris(rv$latest_unique, file)
      }, error = function(e) {
        # Write error message to file
        error_msg <- paste("ER  - Error creating RIS file:", e$message,
                          "\nER  - Please ensure you have imported and deduplicated your citations, then try again.")
        writeLines(error_msg, file)
        # Show toastr notification
        show_toastr(
          "RIS export failed",
          paste("An error occurred while creating the RIS file:", e$message, "Please try again."),
          type = "error"
        )
      })
    }
  )
}

# Create Shiny app ----
shiny::shinyApp(ui, server)
