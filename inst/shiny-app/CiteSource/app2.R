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
    
    # Perform deduplication
    dedup_results <- CiteSource::dedup_citations(rv$upload_df, manual = TRUE, show_unknown_tags = TRUE)
    rv$pairs_to_check <- dedup_results$manual_dedup
    rv$latest_unique <- dedup_results$unique
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
    
    data <- rv$pairs_to_check[,1:36]
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
    n_pairs <- nrow(rv$pairs_to_check)
    if (n_pairs == 0) {
      "No pairs require manual deduplication."
    } else {
      paste(n_pairs, "pair(s) of citations require manual deduplication. Review the pairs below using either the card view (recommended) or table view.")
    }
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
