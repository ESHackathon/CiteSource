#' Record-level table
#'
#' Creates a per-record table that shows which sources (and/or labels/strings) each item was found in.
#'
#' @param citations A deduplicated tibble as returned by `dedup_citations()`.
#' @param include Which metadata should be included in the table? Defaults to 'sources', can be replaced or expanded with 'labels' and/or 'strings'
#' @param include_empty Should records with empty metadata (e.g., no information on 'sources') be included in the table? Defaults to FALSE.
#' @param indicator_presence How should it be indicated that a value is present in a source/label/string? Defaults to TRUE in tibbles and a tickmark in DT tables
#' @param indicator_absence How should it be indicated that a value is *not* present in a source/label/string? Defaults to FALSE in tibbles and a cross in DT tables
#' @param return Either a `tibble` that can be exported, e.g. as a csv, or a DataTable (`DT`) that allows for interactive exploration. Note that the DataTable allows
#' users to download a .csv file; in that file, presence and absence is always indicated as TRUE and FALSE to prevent issues with character encodings.
#' @export

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

  if (!include_empty == TRUE) {
    citations <- citations %>% dplyr::filter(.data$duplicate_id %in% sources$duplicate_id)
  }

  citations <- citations %>%
    dplyr::mutate(
      citation = generate_apa_citation(.data$author, .data$year),
      reference = generate_apa_reference(.data$author, .data$year, .data$title, .data$source, .data$volume, .data$issue, .data$doi, .data$url),
      html_reference = generate_apa_reference(.data$author, .data$year, .data$title, .data$source, .data$volume, .data$issue, .data$doi, .data$url, return_html = TRUE)
    ) %>%
    # Arrange by first author to avoid issue with initials
    dplyr::arrange(stringr::str_extract(.data$author, "^.*?,"), .data$citation) %>%
    dplyr::select("duplicate_id", "citation", "reference", "html_reference") %>%
    dplyr::left_join(sources, by = "duplicate_id")

  indicator_presence <- as.character(indicator_presence)
  indicator_absence <- as.character(indicator_absence)

  to_display <- citations %>%
    dplyr::select(-(1:4)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(.x, indicator_presence, indicator_absence))) %>%
    dplyr::rename_with(~ paste0(.x, " ")) # Add space to keep column names unique

  citations <- dplyr::bind_cols(citations, to_display)

  if (return[1] == "DT") {
    if (!rlang::is_installed("DT")) {
      warning('DT can only be returned when the DT package is installed. Please run install.packages("DT")')
      return(citations)
    } else {
      headings <- purrr::map(include, function(type) {
        type <- stringr::str_sub(type, 1, -2)
        values <- names(citations) %>%
          stringr::str_subset(glue::glue("^{type}__")) %>%
          stringr::str_remove(glue::glue("^{type}__"))
        list(type = type %>% stringr::str_to_title(), values = values)
      }) %>% purrr::transpose()

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

      browser()

      citations %>%
        dplyr::select(-"duplicate_id", -"reference") %>%
        cbind(" " = "&oplus;", .) %>%
        DT::datatable(
          escape = FALSE,
          extensions = "Buttons",
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = c(0, 3:(3 + ncol(to_display)))),
              list(orderable = FALSE, className = "details-control", targets = 1)
            ),
            dom = "Bfrtip",
            buttons =
              list("print", list(
                extend = "csv", filename = "CiteSource_record_summary",
                text = "Download csv",
                exportOptions = list(columns = c(0, 2:(3 + ncol(to_display))))
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
    citations %>%
      dplyr::select(1:3, dplyr::matches(" "), -"html_reference") %>%
      dplyr::rename_with(stringr::str_trim)
  }
}

#' Contribution summary table
#'
#' Create a summary table to show the contribution of each source and the overall performance of the search. For this to work,
#' labels need to be used that contrast a "search" stage with one or more later stages.
#'
#' @param citations A deduplicated tibble as returned by `dedup_citations()`.
#' @param comparison_type Either "sources" to summarise and assess sources or "strings" to consider strings.
#' @param search_label One or multiple labels that identify initial search results (default: "search") - if multiple labels are provided, they are merged.
#' @param screening_label One or multiple label that identify screened records (default: "final") - if multiple are provided, each is compared to the search stage.
#' @param top_n Number of sources/strings to display, based on the number of total records they contributed at the search stage. Note that calculations and totals will still be based on all citations. Defaults to NULL, then all sources/strings are displayed.
#' @export

citation_summary_table <- function(citations, comparison_type = "sources", search_label = "search", screening_label = "final", top_n = NULL) {
  if (!comparison_type %in% c("sources", "strings")) stop('comparison_type needs to be "sources" or "strings"')

  labels <- unique(citations$cite_label) %>% paste(collapse = "  ")

  if (!(stringr::str_detect(labels, search_label) && all(stringr::str_detect(labels, screening_label)))) stop("`search_label` and `screening_label` must refer to values of `cite_label` in `citations`")

  comparison_type_singular <- stringr::str_sub(comparison_type, 1, -2)

  # Merge multiple search labels
  for (i in seq_along(search_label)) {
    citations <- citations %>%
      dplyr::mutate(cite_label = stringr::str_replace_all(cite_label, paste0("(^|( ))(", search_label[i], ")(,|$)"), "\\1search\\4"))
  }

  citations_spread <- compare_sources(citations, c("labels", comparison_type))

  citations_long <- citations_spread %>%
    tidyr::pivot_longer(-.data$duplicate_id, values_to = "bool") %>%
    dplyr::filter(.data$bool == TRUE) %>%
    dplyr::select(-"bool") %>%
    tidyr::separate(.data$name, c("type", "value")) %>%
    dplyr::filter((.data$type == "label" & .data$value %in% c("search", screening_label)) |
      .data$type == comparison_type_singular)


  types <- as.factor(paste0("_", unique(citations_long$type))) %>% levels()

  yield_dfs <- citations_long %>%
    split(.$type) %>%
    purrr::reduce(dplyr::left_join, by = "duplicate_id", suffix = types) %>%
    dplyr::select(-dplyr::starts_with("type_")) %>%
    split(.$value_label)

  yields <- purrr::map_dfr(yield_dfs, .id = "stage", function(df) {
    indicators <- df %>%
      tidyr::pivot_wider(
        names_from = .data[[glue::glue("value_{comparison_type_singular}")]],
        values_from = .data[[glue::glue("value_{comparison_type_singular}")]], values_fn = ~TRUE,
        values_fill = FALSE
      ) %>%
      dplyr::select(-dplyr::all_of(c("duplicate_id", "value_label")))

    clusters <- names(indicators)

    purrr::map_dfr(clusters, function(cluster) {
      rs <- indicators %>%
        dplyr::filter(.data[[cluster]] == TRUE) %>%
        rowSums()
      tibble::tibble(!!comparison_type := cluster, Records_total = length(rs), Records_unique = sum(rs == 1))
    })
  })


  yield_dfs_dedup <- purrr::map_dfr(yield_dfs, .id = "stage", ~ .x %>%
    dplyr::select("duplicate_id") %>%
    unique()) %>%
    dplyr::group_by(.data$stage) %>%
    dplyr::summarise(stage_total = dplyr::n())

  yields <- yields %>%
    dplyr::left_join(yield_dfs_dedup, by = "stage") %>%
    dplyr::group_by(.data$stage) %>%
    dplyr::mutate(Sensitivity = .data$Records_total / .data$stage_total) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"stage_total")

  yields <- yield_dfs_dedup %>%
    dplyr::rename(Records_total = .data$stage_total) %>%
    dplyr::mutate(!!comparison_type := "Total", Sensitivity = NA) %>%
    dplyr::left_join(yields %>%
      dplyr::group_by(.data$stage) %>%
      dplyr::summarise(Records_unique = sum(.data$Records_unique)), by = "stage") %>%
    dplyr::bind_rows(yields, .)

  search_results <- yields %>% dplyr::filter(.data$stage == search_label)


  yields <- yields %>%
    dplyr::filter(.data$stage != search_label) %>%
    dplyr::left_join(
      search_results %>% dplyr::select(-"stage", total_search = .data$Records_total, -c("Records_unique":"Sensitivity")),
      by = comparison_type
    ) %>%
    dplyr::mutate(Precision = .data$Records_total / .data$total_search) %>%
    dplyr::select(-"total_search") %>%
    dplyr::bind_rows(search_results) %>%
    dplyr::group_by(.data$stage) %>%
    dplyr::arrange(-.data$Records_total) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(total_total = dplyr::if_else(.data[[comparison_type]] == "Total", .data$Records_total, NA_integer_)) %>%
    tidyr::fill(.data$total_total) %>%
    dplyr::mutate(Contribution_unique = .data$Records_unique / .data$total_total) %>%
    dplyr::select(-"total_total") %>%
    dplyr::group_by(.data$stage) %>%
    dplyr::arrange(.data[[comparison_type]] == "Total") %>%
    dplyr::ungroup()

  if (!is.null(top_n)) {
    if (length(unique(search_results[[comparison_type]])) <= top_n) {
      top_n <- NULL
    } else {
      to_display <- search_results %>%
        dplyr::arrange(-.data$Records_total) %>%
        dplyr::slice_head(n = top_n + 1) %>% # +1 to include Total row
        dplyr::pull(!!rlang::sym(comparison_type))

      yields <- yields %>%
        dplyr::filter(.data[[comparison_type]] %in% to_display)
    }
  }

  tab <- yields %>%
    dplyr::relocate("Sensitivity", "Precision", .after = dplyr::last_col()) %>%
    dplyr::rename_with(stringr::str_to_title) %>%
    dplyr::group_by(.data$Stage) %>%
    gt::gt() %>%
    gt::fmt_percent(5:7) %>%
    gt::sub_missing() %>%
    gt::tab_spanner_delim("_") %>%
    gt::fmt_number(3:4, decimals = 0) %>%
    gt::tab_options(row_group.background.color = "grey") %>%
    gt::tab_style(
      locations = gt::cells_column_labels(columns = gt::everything()),
      style = list(
        gt::cell_borders(sides = c("bottom"), weight = gt::px(3)),
        gt::cell_borders(sides = c("top"), weight = gt::px(2)),
        gt::cell_text(weight = "bold")
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = list(
        gt::cell_borders(sides = "top", weight = gt::px(2)),
        gt::cell_borders(sides = "bottom", weight = NULL),
        gt::cell_text(weight = "bold", style = "italic")
      )
    ) %>%
    gt::tab_footnote("After deduplication", gt::cells_body(
      columns = dplyr::all_of(stringr::str_to_title(comparison_type)),
      rows = !!rlang::sym(stringr::str_to_title(comparison_type)) == "Total"
    )) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_source_note(gt::md(glue::glue(" \n **Included fields:**
    * **_Total_ records** are all records returned by that {comparison_type_singular}, while **_unique_ records** are found in only that {comparison_type_singular} (or, in the Total rows, in only one {comparison_type_singular}).
    * The **unique contribution** is the share of records only found in that {comparison_type_singular} (or, in the Total rows, in only one {comparison_type_singular}).\n
    * **Sensitivity** is the share of all (deduplicated) records retained at that stage compared to the total number found in that particular {comparison_type_singular}.\n
    * **Precision** is the share of initial records in that {comparison_type_singular} that are retained for inclusion at that stage.")))

  if (!is.null(top_n)) {
    tab <- tab %>% gt::tab_source_note(gt::md(glue::glue("NB: For clarity, **only the top-{top_n} {comparison_type} are displayed**. Totals and percentages, however, are based on *all* {comparison_type}.")))
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
  processed_names <- tibble::tibble(id = id, authors = authors, year = year) %>%
    mutate(
      last_names = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, ",.*$")),
      initials = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, "^.*?,") %>%
        stringr::str_remove_all("\\.") %>%
        stringr::str_trim() %>%
        stringr::str_split(pattern = " ") %>%
        purrr::map(stringr::str_trunc, 1, ellipsis = "") %>%
        purrr::map(stringr::str_c, collapse = ". ") %>%
        purrr::flatten_chr() %>%
        paste0("."))
    )

  # If last name does not uniquely describe authors, first author should be disambiguated in APA style
  # Here implemented by comparing initials -
  # False positives where some sources contain two names (or initials) while others include 1
  # False negative where same initial refers to different names
  # Appears to be best balance for now - further options and instructions could be provided

  # Need this to ensure that last_names and initialed_names retain same length


  last_name <- processed_names %>%
    dplyr::pull(.data$last_names) %>%
    unlist()

  initialed_name_list <- processed_names %>%
    dplyr::select("last_names", "initials") %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::map(~ paste(.x[[2]], .x[[1]]))

  initialed_name <- initialed_name_list %>% unlist()
  authors <- tibble::tibble(last_name, initialed_name)
  duplicated_last_names <- last_name[duplicated(last_name)]
  # Identify which last names appear with different initials
  to_disambiguate <- authors %>%
    dplyr::filter(.data$last_name %in% duplicated_last_names) %>%
    dplyr::group_by(.data$last_name) %>%
    dplyr::summarise(disambiguate = dplyr::n_distinct(.data$initialed_name) > 1) %>%
    dplyr::filter(.data$disambiguate == TRUE) %>%
    dplyr::pull(.data$last_name)

  # Replace those last names with initials
  for (i in seq_along(processed_names$last_names)) {
    if (processed_names$last_names[[i]][[1]] %in% to_disambiguate) {
      processed_names$last_names[[i]][[1]] <- initialed_name_list[[i]][[1]]
    }
  }

  # Create simple citations
  citations <- processed_names %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      N_authors = length(.data$last_names),
      year = as.character(.data$year),
      citation = dplyr::case_when(
        .data$N_authors == 1 ~ glue::glue("{last_names[1]} ({year})"),
        .data$N_authors == 2 ~ glue::glue("{last_names[1]} & {last_names[2]} ({year})"),
        .data$N_authors > 2 ~ glue::glue("{last_names[1]} et al. ({year})")
      )
    ) %>%
    dplyr::ungroup()

  # Disambiguate
  citations_unambiguous <- citations %>%
    dplyr::filter(!(duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

  citations_ambiguous <- citations %>%
    dplyr::filter((duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

  if (nrow(citations_ambiguous) > 0) {
    citations_ambiguous <- purrr::map_dfr(unique(citations_ambiguous$citation), function(current_citation) {
      group <- citations_ambiguous %>%
        dplyr::filter(.data$citation == current_citation) %>%
        dplyr::mutate(author_names = purrr::map(.data$last_names, stringr::str_c())) %>%
        dplyr::arrange(dplyr::desc(.data$author_names))

      # Case 1: multiple publications by same author(s) in same year - add letters
      if (dplyr::n_distinct(group$author_names) == 1) {
        group %>%
          dplyr::mutate(year = paste0(year, letters[1:dplyr::n()])) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            N_authors = length(.data$last_names),
            citation = dplyr::case_when(
              .data$N_authors == 1 ~ glue::glue("{last_names[1]} ({year})"),
              .data$N_authors == 2 ~ glue::glue("{last_names[1]} & {last_names[2]} ({year})"),
              .data$N_authors > 2 ~ glue::glue("{last_names[1]} et al. ({year})")
            )
          ) %>%
          dplyr::ungroup()
      } else {
        # Case 2: distinct authors
        # Find maximum number of common authors
        pairs <- utils::combn(group$last_names, 2)
        common <- numeric()
        for (i in seq_len(ncol(pairs))) {
          first <- pairs[[1, i]]
          second <- pairs[[2, i]]
          len <- min(length(first), length(second))
          comparison <- suppressWarnings(first[1:len] == second[1:len])
          # Common authors: either those before first divergence, or all
          common <- c(common, dplyr::coalesce(which(comparison == FALSE)[1] - 1, length(comparison)))
        }
        common <- max(common)

        group %>%
          dplyr::rowwise() %>%
          dplyr::mutate(citation = dplyr::case_when(
            (common < 5 | .data$N_authors < 5) & N_authors < common + 3 ~ glue::glue("{glue::glue_collapse(last_names, ', ', last = ' & ')} ({year})"),
            (common < 5 | .data$N_authors < 5) ~ glue::glue("{glue::glue_collapse(last_names[1:(common+1)], ', ')} et al. ({year})"),
            common >= 5 & .data$N_authors < common + 2 ~ glue::glue("{last_names[1]} ... {last_names[common]}  & {last_names[common+1]} ({year})"),
            common >= 5 & .data$N_authors < common + 3 ~ glue::glue("{last_names[1]} ... {last_names[common+1]}  & {last_names[common+2]} ({year})"),
            common >= 5 ~ glue::glue("{last_names[1]} ... {last_names[common+1]} et al. ({year})")
          )) %>%
          dplyr::ungroup()
      }
    })

    citations_still_ambiguous <- citations_ambiguous %>%
      dplyr::filter((duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_ambiguous %>%
      dplyr::filter(!(duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE)))))

    # If some of Case 2 were in fact Case 1s (e.g., more than 2 authors with same names), they need to be further disambiguated
    citations_still_ambiguous <- citations_still_ambiguous %>%
      dplyr::group_by(.data$citation) %>%
      dplyr::mutate(letter = letters[1:dplyr::n()]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(citation = stringr::str_replace(.data$citation, "([:digit:])\\)", glue::glue("\\1{letter})"))) %>%
      dplyr::ungroup()

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_still_ambiguous)
  }
  citations_unambiguous %>%
    dplyr::left_join(tibble::tibble(id), ., by = "id") %>%
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
  # Extract last names and initials
  citations <- tibble::tibble(id, authors, year, title, source, volume, issue, doi, weblink) %>%
    dplyr::mutate(dplyr::across(c(dplyr::everything(), -.data$id), .fns = ~ dplyr::na_if(.x, ""))) %>%
    mutate(
      last_names = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, ",.*$")),
      initials = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, "^.*?,") %>%
                                                                                    stringr::str_remove_all("\\.") %>%
                                                                                    stringr::str_trim() %>%
                                                                                    stringr::str_split(pattern = " ") %>%
                                                                                    purrr::map(stringr::str_trunc, 1, ellipsis = "") %>%
                                                                                    purrr::map(stringr::str_c, collapse = ". ") %>%
                                                                                    purrr::flatten_chr() %>%
                                                                                    paste0("."))
    )
  
  # Merge initials to names
  citations$initialed_names <- citations %>%
    dplyr::select("last_names", "initials") %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::map(~ paste(.x[[1]], .x[[2]], sep = ", "))

  if (format_journal_case) {
    citations <- citations %>% dplyr::mutate(source = stringr::str_to_title(source))
  }

  # Helper function to deal with missing values
  nNA <- function(x, ..., alt = "", pre = "") {
    ifelse(is.na(x), alt, paste0(pre, x, ...))
  }

  # Compose references

  citations <- citations %>% dplyr::mutate(
    doi = dplyr::if_else(stringr::str_detect(doi, "http"), doi, paste0("https://doi.org/", doi)),
    link = dplyr::coalesce(doi, weblink)
  )

  if (return_html) {
    citations %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        reference = glue::glue("
                              {glue::glue_collapse(initialed_names, ', ', last = ' & ')} ({year}). {title}. {nNA(source, pre = '<i>', '</i>')}{nNA(volume, pre = '<i>, ', '</i>')}{nNA(issue, pre = '(', ')')}. {nNA(link, pre = '<a href = \"', '\">')}{nNA(link, '</a>')}
                                                     ")
      ) %>%
      dplyr::pull(.data$reference)
  } else {
    citations %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        reference = glue::glue("
                              {glue::glue_collapse(initialed_names, ', ', last = ' & ')} ({year}). {title}. {nNA(source)}{nNA(volume, pre = ', ')}{nNA(issue, pre = '(', ')')}. {nNA(link)}
                                                     ")
      ) %>%
      dplyr::pull(.data$reference)
  }
}
