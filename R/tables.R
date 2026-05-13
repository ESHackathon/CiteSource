#' Record-level table
#'
#' Creates a per-record table that shows which sources (and/or labels/strings) each item was found in.
#' @return A tibble or DataTable containing the per-record table that shows which sources (and/or labels/strings) each item was found in.
#' @param citations A deduplicated tibble as returned by `dedup_citations()`.
#' @param include Which metadata should be included in the table? Defaults to 'sources', can be replaced or expanded with 'labels' and/or 'strings'
#' @param include_empty Should records with empty metadata (e.g., no information on 'sources') be included in the table? Defaults to FALSE.
#' @param indicator_presence How should it be indicated that a value is present in a source/label/string? Defaults to TRUE in tibbles and a tickmark in DT tables
#' @param indicator_absence How should it be indicated that a value is *not* present in a source/label/string? Defaults to FALSE in tibbles and a cross in DT tables
#' @param return Either a `tibble` that can be exported, e.g. as a csv, or a DataTable (`DT`) that allows for interactive exploration. Note that the DataTable allows
#' users to download a .csv file; in that file, presence and absence is always indicated as TRUE and FALSE to prevent issues with character encodings.
#' @export
#' @examples
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations and compare sources
#' unique_citations <- dedup_citations(examplecitations)
#' 
#' unique_citations |> 
#' dplyr::filter(stringr::str_detect(cite_label, "final"))  |> 
#' record_level_table(return = "DT")

record_level_table <- function(citations, include = "sources", include_empty = TRUE, return = c("tibble", "DT"), indicator_presence = NULL, indicator_absence = NULL) {
  
  if (!is.data.frame(citations) || nrow(citations) == 0) stop("Citations must be a tibble and cannot have 0 entries")
  
  if (is.null(indicator_absence)) {
    indicator_absence <- switch(return[1],
                                tibble = FALSE,
                                DT = "&#x2717;"
    )
  }
  if (is.null(indicator_presence)) {
    indicator_presence <- switch(return[1],
                                 tibble = TRUE,
                                 DT = "&#10004;"
    )
  }
  
  sources <- compare_sources(citations, comp_type = include)
  
  if (nrow(sources) == 0) {
    warning("Citations provided contain no information on ", include, ". NA will be displayed, but check whether you intended to do a different comparison")
    sources <- tibble::tibble(duplicate_id = citations$duplicate_id)
    sources[[paste0(stringr::str_sub(include, 1, -2), "__NA")]] <- TRUE
  }
  
  if (!include_empty == TRUE) {
    citations <- citations |> dplyr::filter(.data$duplicate_id %in% sources$duplicate_id)
  }
  
  if (! "url" %in% colnames(citations)) {
    citations$url <- NA_character_
  }
  
  citations <- citations |>
    dplyr::mutate(
      citation = generate_apa_citation(.data$author, .data$year),
      reference = generate_apa_reference(.data$author, .data$year, .data$title, .data$journal, .data$volume, .data$issue, .data$doi, .data$url),
      html_reference = generate_apa_reference(.data$author, .data$year, .data$title, .data$journal, .data$volume, .data$issue, .data$doi, .data$url, return_html = TRUE)
    ) |>
    # Arrange by first author to avoid issue with initials
    dplyr::arrange(stringr::str_extract(.data$author, "^.*?,"), .data$citation) |>
    dplyr::select("duplicate_id", "citation", "reference", "html_reference") |>
    dplyr::left_join(sources, by = "duplicate_id")
  
  indicator_presence <- as.character(indicator_presence)
  indicator_absence <- as.character(indicator_absence)
  
  to_display <- citations |>
    dplyr::select(-(1:4)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(.x, indicator_presence, indicator_absence))) |>
    dplyr::rename_with(~ paste0(.x, " ")) # Add space to keep column names unique
  
  citations <- dplyr::bind_cols(citations, to_display)
  
  if (return[1] == "DT") {
    if (!rlang::is_installed("DT")) {
      warning('DT can only be returned when the DT package is installed. Please run install.packages("DT")')
      return(citations)
    } else {
      headings <- purrr::map(include, function(type) {
        type <- stringr::str_sub(type, 1, -2)
        values <- names(citations) |>
          stringr::str_subset(glue::glue("^{type}__")) |>
          stringr::str_remove(glue::glue("^{type}__"))
        list(type = type |> stringr::str_to_title(), values = values)
      }) |> purrr::transpose()
      
      sketch <- htmltools::tags$table(
        class = "display",
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(rowspan = 2, colspan = 2, htmltools::HTML("&nbsp;")),
            htmltools::tags$th(rowspan = 2, colspan = 2, "Citation"),
            purrr::map2(headings$type, lengths(headings$values), ~ htmltools::tags$th(colspan = .y, .x))
          ),
          htmltools::tags$tr(
            purrr::map(unlist(headings$values), ~ htmltools::tags$th(.x))
          )
        ),
        htmltools::tags$tfoot(
          htmltools::tags$td(colspan = 4 + length(unlist(headings$values)), htmltools::HTML("Click on the &oplus; to view the full reference"))
        )
      )
      
      citations |>
        dplyr::select(-"duplicate_id", -"reference") |>
        (\(df) cbind(" " = "&oplus;", df))() |>
        DT::datatable(
          escape = FALSE,
          extensions = "Buttons",
          options = list(
            pageLength = 10,
            lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
            columnDefs = list(
              list(visible = FALSE, targets = c(0, 3:(3 + ncol(to_display)))),
              list(orderable = FALSE, className = "details-control", targets = 1)
            ),
            dom = "lBfrtip",
            buttons =
              list("print", list(
                extend = "csv", filename = "CiteSource_record_summary",
                text = "Download csv",
                exportOptions = list(
                  columns = c(0, 2:(3 + ncol(to_display))),
                  modifier = list(page = "all")
                )
                
              ))
          ), container = sketch,
          callback = DT::JS("
              table.column(1).nodes().to$().css({cursor: 'pointer'});
              var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\">' +
                        d[3];
              };
              table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                  row.child.hide();
                  td.html('&oplus;');
                } else {
                  row.child(format(row.data())).show();
                  td.html('&CircleMinus;');
                }
              });")
        )
    }
  } else {
    citations |>
      dplyr::select(1:3, dplyr::matches(" "), -"html_reference") |>
      dplyr::rename_with(stringr::str_trim)
  }
}

#' Contribution summary table
#'
#' Create a summary table to show the contribution of each source and the overall performance of the search. For this to work,
#' labels need to be used that contrast a "search" stage with one or more later stages.
#' 
#' @return A tibble containing the contribution summary table, which shows the contribution of each source and the overall performance of the search
#' @param citations A deduplicated tibble as returned by `dedup_citations()`.
#' @param comparison_type Either "sources" to summarise and assess sources or "strings" to consider strings.
#' @param search_label One or multiple labels that identify initial search results (default: "search") - if multiple labels are provided, they are merged.
#' @param screening_label One or multiple label that identify screened records (default: "final") - if multiple are provided, each is compared to the search stage.
#' @param top_n Number of sources/strings to display, based on the number of total records they contributed at the search stage. Note that calculations and totals will still be based on all citations. Defaults to NULL, then all sources/strings are displayed.
#' @export
#' @examples
#' if (interactive()) {
#' # Load example data from the package
#' examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#' examplecitations <- readRDS(examplecitations_path)
#'
#' # Deduplicate citations and compare sources
#' unique_citations <- dedup_citations(examplecitations)
#' 
#' unique_citations |> 
#' dplyr::filter(stringr::str_detect(cite_label, "final"))  |> 
#' record_level_table(return = "DT")
#' citation_summary_table(unique_citations, screening_label = c("screened", "final"))
#' }

citation_summary_table <- function(citations, comparison_type = "sources", search_label = "search", screening_label = "final", top_n = NULL) {
  if (!comparison_type %in% c("sources", "strings")) stop('comparison_type needs to be "sources" or "strings"')

  labels <- unique(citations$cite_label) |> paste(collapse = "  ")

  if (!(stringr::str_detect(labels, search_label) && all(stringr::str_detect(labels, screening_label)))) stop("`search_label` and `screening_label` must refer to values of `cite_label` in `citations`")

  comparison_type_singular <- stringr::str_sub(comparison_type, 1, -2)

  # Merge multiple search labels
  for (i in seq_along(search_label)) {
    citations <- citations |>
      dplyr::mutate(cite_label = stringr::str_replace_all(cite_label, paste0("(^|( ))(", search_label[i], ")(,|$)"), "\\1search\\4"))
  }

  citations_spread <- compare_sources(citations, c("labels", comparison_type))

  citations_long <- citations_spread |>
    tidyr::pivot_longer(-.data$duplicate_id, values_to = "bool") |>
    dplyr::filter(.data$bool == TRUE) |>
    dplyr::select(-"bool") |>
    tidyr::separate(.data$name, c("type", "value")) |>
    dplyr::filter((.data$type == "label" & .data$value %in% c("search", screening_label)) |
      .data$type == comparison_type_singular)


  types <- as.factor(paste0("_", unique(citations_long$type))) |> levels()

  yield_dfs <- citations_long |>
    (\(x) split(x, x$type))() |>
    purrr::reduce(dplyr::left_join, by = "duplicate_id", suffix = types) |>
    dplyr::select(-dplyr::starts_with("type_")) |>
    (\(x) split(x, x$value_label))()

  yields <- purrr::map_dfr(yield_dfs, .id = "stage", function(df) {
    indicators <- df |>
      tidyr::pivot_wider(
        names_from = .data[[glue::glue("value_{comparison_type_singular}")]],
        values_from = .data[[glue::glue("value_{comparison_type_singular}")]], values_fn = ~TRUE,
        values_fill = FALSE
      ) |>
      dplyr::select(-dplyr::all_of(c("duplicate_id", "value_label")))

    clusters <- names(indicators)

    purrr::map_dfr(clusters, function(cluster) {
      rs <- indicators |>
        dplyr::filter(.data[[cluster]] == TRUE) |>
        rowSums()
      tibble::tibble(!!comparison_type := cluster, Records_total = length(rs), Records_unique = sum(rs == 1))
    })
  })


  yield_dfs_dedup <- purrr::map_dfr(yield_dfs, .id = "stage", ~ .x |>
    dplyr::select("duplicate_id") |>
    unique()) |>
    dplyr::group_by(.data$stage) |>
    dplyr::summarise(stage_total = dplyr::n())

  yields <- yields |>
    dplyr::left_join(yield_dfs_dedup, by = "stage") |>
    dplyr::group_by(.data$stage) |>
    dplyr::mutate(Sensitivity = .data$Records_total / .data$stage_total) |>
    dplyr::ungroup() |>
    dplyr::select(-"stage_total")

  total_row <- yield_dfs_dedup |>
    dplyr::rename(Records_total = .data$stage_total) |>
    dplyr::mutate(!!comparison_type := "Total", Sensitivity = NA) |>
    dplyr::left_join(yields |>
      dplyr::group_by(.data$stage) |>
      dplyr::summarise(Records_unique = sum(.data$Records_unique)), by = "stage")
  yields <- dplyr::bind_rows(yields, total_row)

  search_results <- yields |> dplyr::filter(.data$stage == search_label)


  yields <- yields |>
    dplyr::filter(.data$stage != search_label) |>
    dplyr::left_join(
      search_results |> dplyr::select(-"stage", total_search = .data$Records_total, -c("Records_unique":"Sensitivity")),
      by = comparison_type
    ) |>
    dplyr::mutate(Precision = .data$Records_total / .data$total_search) |>
    dplyr::select(-"total_search") |>
    dplyr::bind_rows(search_results) |>
    dplyr::group_by(.data$stage) |>
    dplyr::arrange(-.data$Records_total) |>
    dplyr::ungroup() |>
    dplyr::mutate(total_total = dplyr::if_else(.data[[comparison_type]] == "Total", .data$Records_total, NA_integer_)) |>
    tidyr::fill(.data$total_total) |>
    dplyr::mutate(Contribution_unique = .data$Records_unique / .data$total_total) |>
    dplyr::select(-"total_total") |>
    dplyr::group_by(.data$stage) |>
    dplyr::arrange(.data[[comparison_type]] == "Total") |>
    dplyr::ungroup()

  if (!is.null(top_n)) {
    if (length(unique(search_results[[comparison_type]])) <= top_n) {
      top_n <- NULL
    } else {
      to_display <- search_results |>
        dplyr::arrange(-.data$Records_total) |>
        dplyr::slice_head(n = top_n + 1) |> # +1 to include Total row
        dplyr::pull(!!rlang::sym(comparison_type))

      yields <- yields |>
        dplyr::filter(.data[[comparison_type]] %in% to_display)
    }
  }

  tab <- yields |>
    dplyr::relocate("Sensitivity", "Precision", .after = dplyr::last_col()) |>
    dplyr::rename_with(stringr::str_to_title) |>
    dplyr::group_by(.data$Stage) |>
    gt::gt() |>
    gt::fmt_percent(5:7) |>
    gt::sub_missing() |>
    gt::tab_spanner_delim("_") |>
    gt::fmt_number(3:4, decimals = 0) |>
    gt::tab_options(row_group.background.color = "grey") |>
    gt::tab_style(
      locations = gt::cells_column_labels(columns = gt::everything()),
      style = list(
        gt::cell_borders(sides = c("bottom"), weight = gt::px(3)),
        gt::cell_borders(sides = c("top"), weight = gt::px(2)),
        gt::cell_text(weight = "bold")
      )
    ) |>
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = list(
        gt::cell_borders(sides = "top", weight = gt::px(2)),
        gt::cell_borders(sides = "bottom", weight = NULL),
        gt::cell_text(weight = "bold", style = "italic")
      )
    ) |>
    gt::tab_footnote("After deduplication", gt::cells_body(
      columns = dplyr::all_of(stringr::str_to_title(comparison_type)),
      rows = !!rlang::sym(stringr::str_to_title(comparison_type)) == "Total"
    )) |>
    gt::tab_source_note(gt::md(glue::glue(" \n **Included fields:**
    * **_Total_ records** are all records returned by that {comparison_type_singular}, while **_unique_ records** are found in only that {comparison_type_singular} (or, in the Total rows, in only one {comparison_type_singular}).
    * The **unique contribution** is the share of records only found in that {comparison_type_singular} (or, in the Total rows, in only one {comparison_type_singular}).\n
    * **Sensitivity** is the share of all (deduplicated) records retained at that stage compared to the total number found in that particular {comparison_type_singular}.\n
    * **Precision** is the share of initial records in that {comparison_type_singular} that are retained for inclusion at that stage.")))

  if (!is.null(top_n)) {
    tab <- tab |> gt::tab_source_note(gt::md(glue::glue("NB: For clarity, **only the top-{top_n} {comparison_type} are displayed**. Totals and percentages, however, are based on *all* {comparison_type}.")))
  }
  tab
}

#' Generate author-year citations for record table
#'
#' Creates the author-year citations (e.g., `Miller (2020)`) for the record-level table. This requires a lot of disambiguation
#' when similar author-year combinations appear. This function largely follows APA style, yet abbreviates citations where more than
#' five authors would have to be listed.
#'
#' @param authors Character vector of authors. Needs to follow Last, First and Last, First etc format (as returned by dedup)
#' @param year Vector of publication years
#' @return A character vector of citations
#' @noRd


generate_apa_citation <- function(authors, year) {
  id <- seq_along(authors)
  # Extract last names and initials
  processed_names <- tibble::tibble(id = id, authors = authors, year = year) |>
    dplyr::mutate(
      last_names = authors |> stringr::str_split(pattern = " and ") |> purrr::map(~ stringr::str_remove(.x, ",.*$")),
      initials = authors |> stringr::str_split(pattern = " and ") |> purrr::map(~ stringr::str_remove(.x, "^.*?,") |>
        stringr::str_remove_all("\\.") |>
        stringr::str_trim() |>
        stringr::str_split(pattern = " ") |>
        purrr::map(stringr::str_trunc, 1, ellipsis = "") |>
        purrr::map(stringr::str_c, collapse = ". ") |>
        purrr::flatten_chr() |>
        paste0("."))
    )

  # If last name does not uniquely describe authors, first author should be disambiguated in APA style
  # Here implemented by comparing initials -
  # False positives where some sources contain two names (or initials) while others include 1
  # False negative where same initial refers to different names
  # Appears to be best balance for now - further options and instructions could be provided
  # Need this to ensure that last_names and initialed_names retain same length


  last_name <- processed_names |>
    dplyr::pull(.data$last_names) |>
    unlist()

  initialed_name <- purrr::map2(
    dplyr::pull(processed_names, .data$initials), 
    dplyr::pull(processed_names, .data$last_names),
    ~ paste(.x, .y)
  ) |> unlist()

  authors <- tibble::tibble(last_name, initialed_name)
  duplicated_last_names <- last_name[duplicated(last_name)]
  # Identify which last names appear with different initials
  to_disambiguate <- authors |>
    dplyr::filter(.data$last_name %in% duplicated_last_names) |>
    dplyr::group_by(.data$last_name) |>
    dplyr::summarise(disambiguate = dplyr::n_distinct(.data$initialed_name) > 1) |>
    dplyr::filter(.data$disambiguate == TRUE) |>
    dplyr::pull(.data$last_name)
  # Replace those last names with initials
processed_names$last_names <- 
  purrr::map2(processed_names$last_names, processed_names$initials, \(last_names, initials) {
    last_names[last_names %in% to_disambiguate] <- 
      paste(initials[last_names %in% to_disambiguate], last_names[last_names %in% to_disambiguate])
    last_names
  })

  # Create simple citations
  citations <- processed_names |>
    dplyr::mutate(
      N_authors = purrr::map_int(.data$last_names, length),
      year = as.character(.data$year),
      citation = purrr::map2_chr(.data$last_names, .data$year, function(ln, yr) {
        n <- length(ln)
        if (n == 1) paste0(ln[1], " (", yr, ")")
        else if (n == 2) paste0(ln[1], " & ", ln[2], " (", yr, ")")
        else paste0(ln[1], " et al. (", yr, ")")
      })
    )

  # Disambiguate
  citations_unambiguous <- citations |>
    dplyr::filter(!(duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

  citations_ambiguous <- citations |>
    dplyr::filter((duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

  if (nrow(citations_ambiguous) > 0) {
    citations_ambiguous <- purrr::map_dfr(unique(citations_ambiguous$citation), function(current_citation) {
      group <- citations_ambiguous |>
        dplyr::filter(.data$citation == current_citation) |>
        dplyr::mutate(author_names = purrr::map(.data$last_names, stringr::str_c())) |>
        dplyr::arrange(dplyr::desc(.data$author_names))

      # Case 1: multiple publications by same author(s) in same year - add letters
      if (dplyr::n_distinct(group$author_names) == 1) {
        group <- group |>
          dplyr::mutate(year = paste0(year, letters[seq_len(dplyr::n())])) |>
          dplyr::mutate(
            N_authors = purrr::map_int(.data$last_names, length),
            citation = purrr::map2_chr(.data$last_names, .data$year, function(ln, yr) {
              n <- length(ln)
              if (n == 1) paste0(ln[1], " (", yr, ")")
              else if (n == 2) paste0(ln[1], " & ", ln[2], " (", yr, ")")
              else paste0(ln[1], " et al. (", yr, ")")
            })
          )
      } else {
        # Case 2: distinct authors
        # Find maximum number of common authors
        common <- group$last_names |>
          utils::combn(2, simplify = FALSE) |>
          purrr::map_int(~{
            len <- min(length(.x[[1]]), length(.x[[2]]))
            comparison <- .x[[1]][1:len] == .x[[2]][1:len]
            ifelse(any(!comparison), which(!comparison)[1] - 1, len)
          }) |>
          max()
        
        group <- group |>
          dplyr::mutate(citation = purrr::pmap_chr(
            list(.data$last_names, .data$year, .data$N_authors),
            function(ln, yr, n) {
              if ((common < 5 || n < 5) && n < common + 3) {
                paste0(glue::glue_collapse(ln, ", ", last = " & "), " (", yr, ")")
              } else if (common < 5 || n < 5) {
                paste0(glue::glue_collapse(ln[1:(common + 1)], ", "), " et al. (", yr, ")")
              } else if (n < common + 2) {
                paste0(ln[1], " ... ", ln[common], "  & ", ln[common + 1], " (", yr, ")")
              } else if (n < common + 3) {
                paste0(ln[1], " ... ", ln[common + 1], "  & ", ln[common + 2], " (", yr, ")")
              } else {
                paste0(ln[1], " ... ", ln[common + 1], " et al. (", yr, ")")
              }
            }
          ))
      }
      group
    })

    citations_still_ambiguous <- citations_ambiguous |>
      dplyr::filter((duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_ambiguous |>
      dplyr::filter(!(duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE)))))

    # If some of Case 2 were in fact Case 1s (e.g., more than 2 authors with same names), they need to be further disambiguated
    citations_still_ambiguous <- citations_still_ambiguous |>
      dplyr::group_by(.data$citation) |>
      dplyr::mutate(letter = letters[seq_len(dplyr::n())]) |>
      dplyr::ungroup() |>
      dplyr::mutate(citation = stringr::str_replace(.data$citation, "(\\d)\\)", paste0("\\1", .data$letter, ")")))

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_still_ambiguous)
  }
  dplyr::left_join(tibble::tibble(id), citations_unambiguous, by = "id") |>
    dplyr::pull(.data$citation)
}


#' Generate apa-style reference for record table
#'
#' Creates the full references based on the deduplicated data - for instance to show as additional detail in the
#' record-level table. Citations are generally formatted like those for journal articles - if we want more formats,
#' we should rely on a specialist package, but this seems ok for now. Page numbers are not included for now, as they
#' do not seem to be returned clearly in deduplication?
#'
#' @param authors Character vector of authors. Needs to follow Last, First and Last, First etc format (as returned by dedup)
#' @param year Vector of publication years
#' @param title Vector of titles
#' @param source Journal or other publication
#' @param volume Volume - can be blank
#' @param issue Issue - can be blank.
#' @param doi - will be converted to link format
#' @param weblink - will be coalesced with DOI; DOI will be given preference.
#' @param return_html Should HTML-formatted reference (with link and journal in italics) be returned
#' @param format_journal_case Should source be formatted to title case (otherwise, journal titles are often in ALL CAPS or similar)
#' @return A character vector of references
#' @noRd


generate_apa_reference <- function(authors, year, title, source, volume, issue, doi, weblink, return_html = FALSE, format_journal_case = TRUE) {
  
  id <- seq_along(authors)
  
  # Helper function to handle NULL and missing columns
  handle_missing <- function(arg, arg_name) {
    result <- tryCatch({
      if (is.null(arg)) stop()
      arg
    }, error = function(e) {
      warning("Column ", arg_name, " missing from citation data when generating references", call. = FALSE)
      NA_character_
    })
    result
  }
  
  # Apply the helper function to each other than authors
  year <- handle_missing(year, "year")
  title <- handle_missing(title, "title")
  source <- handle_missing(source, "source")
  volume <- handle_missing(volume, "volume")
  issue <- handle_missing(issue, "issue")
  doi <- handle_missing(doi, "doi")
  weblink <- handle_missing(weblink, "weblink")
  
  # Extract last names and initials
  citations <- tibble::tibble(id, authors, year, title, source, volume, issue, doi, weblink) |>
    dplyr::mutate(dplyr::across(c(dplyr::everything(), -.data$id), .fns = ~ as.character(.x) |> dplyr::na_if(""))) |>
    dplyr::mutate(
      last_names = authors |> stringr::str_split(pattern = " and ") |> purrr::map(~ stringr::str_remove(.x, ",.*$")),
      initials = authors |> stringr::str_split(pattern = " and ") |> purrr::map(~{
        out <- .x
        out[!stringr::str_detect(.x, ",")] <- ""
        
        .x <- .x[stringr::str_detect(.x, ",")]
        

        out[!out == ""] <- stringr::str_remove(.x, "^.*?,") |>
                                                                                    stringr::str_remove_all("\\.") |>
                                                                                    stringr::str_trim() |>
                                                                                    stringr::str_split(pattern = " ") |>
                                                                                    purrr::map(stringr::str_trunc, 1, ellipsis = "") |>
                                                                                    purrr::map(stringr::str_c, collapse = ". ") |>
                                                                                    purrr::flatten_chr() |>
                                                                                    paste0(".")
        out
        })
  
    )
  
  # Merge initials to names
  citations$initialed_names <- citations |>
    dplyr::select("last_names", "initials") |>
    as.list() |>
    purrr::transpose() |>
    purrr::map(~ {
       purrr::map2_chr(.x$last_names, .x$initials, ~ if (.y == "" | is.na(.y)) .x else paste(.x, .y, sep = ", "))
    })

  if (format_journal_case) {
    citations <- citations |> dplyr::mutate(source = stringr::str_to_title(source))
  }

  # Helper function to deal with missing values
  nNA <- function(x, ..., alt = "", pre = "") {
    ifelse(is.na(x), alt, paste0(pre, x, ...))
  }

  # Compose references

  citations <- citations |> dplyr::mutate(
    doi = dplyr::case_when(
      is.na(doi)                          ~ NA_character_,
      stringr::str_detect(doi, "http")    ~ doi,
      TRUE                                ~ paste0("https://doi.org/", doi)
    ),
    link = dplyr::coalesce(doi, weblink)
  )
  
  if (return_html) {
    purrr::pmap_chr(
      list(citations$initialed_names, citations$year, citations$title,
           citations$source, citations$volume, citations$issue, citations$link),
      function(initialed_names, year, title, source, volume, issue, link) {
        paste0(
          glue::glue_collapse(initialed_names, ", ", last = " & "),
          " (", year, "). ",
          nNA(title, "."), " ",
          nNA(source, pre = "<i>", "</i>"),
          nNA(volume, pre = "<i>, ", "</i>"),
          nNA(issue, pre = "(", ")"),
          ". ",
          nNA(link, pre = '<a href="', '" target="_blank" rel="noopener noreferrer">'),
          nNA(link, "</a>")
        )
      }
    )
  } else {
    purrr::pmap_chr(
      list(citations$initialed_names, citations$year, citations$title,
           citations$source, citations$volume, citations$issue, citations$link),
      function(initialed_names, year, title, source, volume, issue, link) {
        paste0(
          glue::glue_collapse(initialed_names, ", ", last = " & "),
          " (", year, "). ",
          nNA(title, "."), " ",
          nNA(source),
          nNA(volume, pre = ", "),
          nNA(issue, pre = "(", ")"),
          ". ",
          nNA(link)
        )
      }
    )
  }
}


#' Initial Record Table
#'
#' This function generates a formatted table displaying the record counts
#' for each citation source, including the number of records imported and
#' the distinct records after deduplication.
#'
#' @param data A data frame containing the record counts for each citation source.
#'   It must include columns `Source`, `Records_Imported`, and `Distinct_Records`.
#'
#' @return A `gt` table object summarizing the record counts for each citation source.
#'
#' @details
#' The function checks if the input data frame is empty and returns an empty `gt` table
#' if no data is present. Otherwise, it generates a formatted table with labeled columns
#' and adds footnotes explaining the meaning of each column.
#'
#' @export
#'
#' @examples
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Source3"),
#'   Records_Imported = c(100, 150, 250),
#'   Distinct_Records = c(90, 140, 230)
#' )
#' create_initial_record_table(sample_data)
create_initial_record_table <- function(data) {
  if (nrow(data) == 0) {
    return(gt::gt(data.frame(Source = character(), `Records Imported` = integer(), `Distinct Records` = integer())))
  }

  data |>
    gt::gt(rowname_col = "Source") |>
    gt::tab_header(title = "Record Counts") |>
    gt::cols_label(
      Records_Imported = "Records Imported",
      Distinct_Records = "Distinct Records"
    ) |>
    gt::tab_footnote(
      footnote = "Number of records imported from each source.",
      locations = gt::cells_column_labels(columns = c("Records_Imported"))
    ) |>
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(columns = c("Distinct_Records"))
    )
}


#' Create a Detailed Record Table
#'
#' This function generates a formatted summary table using the `gt` package,
#' which displays detailed counts for each citation source. The table includes
#' columns for the number of records imported, distinct records, unique records,
#' non-unique records, and various contribution percentages. Data from the
#' function calculate_detailed_records is pre-formatted for this table.
#'
#' @param data A data frame containing the detailed counts for each citation source.
#'   The data frame must include the following columns:
#'   - `Source`: The name of the citation source.
#'   - `Records Imported`: The total number of records imported from the source.
#'   - `Distinct Records`: The number of distinct records after deduplication within the source.
#'   - `Unique Records`: The number of records unique to that source.
#'   - `Non-unique Records`: The number of records found in at least one other source.
#'   - `Source Contribution %`: The percentage contribution of each source to the total distinct records.
#'   - `Source Unique Contribution %`: The percentage contribution of each source to the total unique records.
#'   - `Source Unique %`: The percentage of records from each source that were unique.
#'
#' @return A `gt` table object summarizing the detailed record counts for each citation source.
#'
#' @export
#'
#' @examples
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Total"),
#'   `Records Imported` = c(100, 150, 250),
#'   `Distinct Records` = c(90, 140, 230),
#'   `Unique Records` = c(50, 70, 120),
#'   `Non-unique Records` = c(40, 70, 110),
#'   `Source Contribution %` = c("39.1%", "60.9%", "100%"),
#'   `Source Unique Contribution %` = c("41.7%", "58.3%", "100%"),
#'   `Source Unique %` = c("55.6%", "50%", "52.2%"),
#'   check.names = FALSE
#' )
#' create_detailed_record_table(sample_data)
create_detailed_record_table <- function(data) {
  required_columns <- c("Source", "Records Imported", "Distinct Records",
                        "Unique Records", "Non-unique Records",
                        "Source Contribution %", "Source Unique Contribution %",
                        "Source Unique %")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("The following required columns are missing from the data: ",
         paste(missing_columns, collapse = ", "))
  }

  data |>
    gt::gt(rowname_col = "Source") |>
    gt::tab_header(title = "Record Summary") |>
    gt::cols_label(
      `Records Imported` = "Records Imported",
      `Distinct Records` = "Distinct Records",
      `Unique Records` = "Unique Records",
      `Non-unique Records` = "Non-unique Records",
      `Source Contribution %` = "Source Contribution %",
      `Source Unique Contribution %` = "Source Unique Contribution %",
      `Source Unique %` = "Source Unique %"
    ) |>
    gt::tab_footnote(
      footnote = "Number of raw records imported from each database.",
      locations = gt::cells_column_labels(columns = "Records Imported")
    ) |>
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(columns = "Distinct Records")
    ) |>
    gt::tab_footnote(
      footnote = "Number of records not found in another source.",
      locations = gt::cells_column_labels(columns = "Unique Records")
    ) |>
    gt::tab_footnote(
      footnote = "Number of records found in at least one other source.",
      locations = gt::cells_column_labels(columns = "Non-unique Records")
    ) |>
    gt::tab_footnote(
      footnote = "Percent distinct records contributed to the total number of distinct records.",
      locations = gt::cells_column_labels(columns = "Source Contribution %")
    ) |>
    gt::tab_footnote(
      footnote = "Percent of unique records contributed to the total unique records.",
      locations = gt::cells_column_labels(columns = "Source Unique Contribution %")
    ) |>
    gt::tab_footnote(
      footnote = "Percentage of records that were unique from each source.",
      locations = gt::cells_column_labels(columns = "Source Unique %")
    ) |>
    gt::tab_footnote(
      footnote = "Total citations discovered (after internal and cross-source deduplication).",
      locations = gt::cells_body(columns = "Distinct Records", rows = "Total")
    )
}


#' Count and Precision/Sensitivity Table
#'
#' This function generates a formatted table that displays the precision
#' and sensitivity (recall) metrics for each citation source, along with
#' distinct records and phase-specific counts such as "screened" and "final".
#'
#' @param data A data frame containing phase-specific counts and calculated metrics
#'   for each citation source. It must include columns such as `Source`,
#'   `Distinct_Records`, `final`, `Precision`, `Recall`, and optionally `screened`.
#'
#' @return A `gt` table object summarizing the precision and sensitivity
#'   metrics for each citation source, with relevant footnotes and labels.
#'
#' @details
#' The function first checks whether all values in the `screened` column are zero.
#' If so, the column is removed from the table. The table is then generated
#' using the `gt` package, with labeled columns and footnotes explaining the metrics.
#'
#' @export
#'
#' @examples
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Total"),
#'   Distinct_Records = c(100, 150, 250),
#'   final = c(80, 120, 200),
#'   Precision = c(80.0, 80.0, 80.0),
#'   Recall = c(40.0, 60.0, 100.0),
#'   screened = c(90, 140, 230)
#' )
#' create_precision_sensitivity_table(sample_data)
create_precision_sensitivity_table <- function(data) {
  all_zero_screened <- all(data$screened == 0)

  if (all_zero_screened) {
    data <- data[ , !(names(data) %in% "screened")]
  }

  gt_table <- data |>
    gt::gt(rowname_col = "Source") |>
    gt::tab_header(title = "Record Counts & Precision/Sensitivity") |>
    gt::cols_label(
      `Distinct_Records` = "Distinct Records",
      final = "Final Included",
      Precision = "Precision",
      Recall = "Sensitivity/Recall"
    ) |>
    gt::cols_align(align = "right", columns = c("final", "Precision", "Recall"))

  if (!all_zero_screened) {
    gt_table <- gt_table |>
      gt::cols_label(screened = "Screened Included") |>
      gt::cols_align(align = "right", columns = "screened") |>
      gt::tab_footnote(
        footnote = "Number of citations included after title/abstract screening.",
        locations = gt::cells_column_labels(columns = "screened")
      ) |>
      gt::tab_footnote(
        footnote = "Total citations included after Ti/Ab Screening.",
        locations = gt::cells_body(columns = "screened", rows = "Total")
      )
  }

  gt_table |>
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(columns = "Distinct_Records")
    ) |>
    gt::tab_footnote(
      footnote = "Number of citations included after full text screening.",
      locations = gt::cells_column_labels(columns = "final")
    ) |>
    gt::tab_footnote(
      footnote = "Number of final included citations / Number of distinct records.",
      locations = gt::cells_column_labels(columns = "Precision")
    ) |>
    gt::tab_footnote(
      footnote = "Number of final included citations / Total number of final included citations.",
      locations = gt::cells_column_labels(columns = "Recall")
    ) |>
    gt::tab_footnote(
      footnote = "Total citations discovered (after internal and cross-source deduplication).",
      locations = gt::cells_body(columns = "Distinct_Records", rows = "Total")
    ) |>
    gt::tab_footnote(
      footnote = "Total citations included after full text screening.",
      locations = gt::cells_body(columns = "final", rows = "Total")
    ) |>
    gt::tab_footnote(
      footnote = "Overall Precision = Number of final included citations / Total distinct records.",
      locations = gt::cells_body(columns = "Precision", rows = "Total")
    )
}
