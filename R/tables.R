#' Record-level table
#'
#' Creates a per-record table that shows which sources (and/or labels/strings) each item was found in.
#'
#' @param citations A deduplicated tibble as returned by `dedup_citations()`.
#' @param include Which metadata should be included in the table? Defaults to 'source', can be replaced or expanded with 'label' and/or 'string'
#' @return A tibble that can be turned into a pretty table with (e.g.) the `gt`-package
#' @export

record_level_table <- function(citations, include = "source") {
  
  warning("Not yet implemented.")
  
}

#' Summary table
#'
#' Create a summary table to show the contribution of each source and the overall performance of the search. For this to work, 
#' labels need to be used that contrast a "search" stage with one or more later stages.
#'
#' @param citations A deduplicated tibble as returned by `dedup_citations()`.
#' @param comparison_type Either "sources" to summarise and assess sources or "strings" to consider strings.
#' @param search_label One or multiple labels that identify initial search results (default: "search") - if multiple labels are provided, they are merged.
#' @param screening_label One or multiple label that identify screened records (default: "final") - if multiple are provided, each is compared to the search stage.
#' @export

citation_summary_table <- function(citations, comparison_type = "sources", search_label = "search", screening_label = "final") {
  
  if (!comparison_type %in% c("sources", "strings")) stop('comparison_type needs to be "sources" or "strings"')
  
  comparison_type_singular <- stringr::str_sub(comparison_type, 1, -2)
  
  # Merge multiple search labels
  for (i in seq_along(search_label)) {
    citations <- citations %>% 
      dplyr::mutate(cite_label = stringr::str_replace_all(cite_label, paste0("(^|( ))(", search_label[i], ")(,|$)"), "\\1search\\4"))
  }
  
  citations_spread <- compare_sources(citations, c("labels", comparison_type))
  
  citations_long <- citations_spread %>% tidyr::pivot_longer(-.data$duplicate_id, values_to = "bool") %>% 
    dplyr::filter(.data$bool == TRUE) %>% dplyr::select(-"bool") %>%  
    tidyr::separate(.data$name, c("type", "value")) %>% 
    dplyr::filter((.data$type == "label" & .data$value %in% c("search", screening_label)) |
                    .data$type == comparison_type_singular) 
  
  
  types <- as.factor(paste0("_", unique(citations_long$type))) %>% levels()
  
  yield_dfs <- citations_long %>% split(.$type) %>% 
    purrr::reduce(dplyr::left_join, by = "duplicate_id", suffix = types) %>% 
    dplyr::select(-dplyr::starts_with("type_")) %>% 
    split(.$value_label)
  
  yields <- purrr::map_dfr(yield_dfs, .id = "stage", function(df) {
    indicators <- df %>% tidyr::pivot_wider(names_from = .data$value_source, 
                              values_from = .data$value_source, values_fn = ~TRUE,
                              values_fill = FALSE) %>% 
      dplyr::select(-dplyr::all_of(c("duplicate_id", "value_label")))
    
    clusters <- names(indicators)
    
    purrr::map_dfr(clusters, function(cluster) {
      rs <- indicators %>% dplyr::filter(.data[[cluster]] == TRUE) %>% rowSums()
      tibble::tibble(!!comparison_type := cluster, Records_total = length(rs), Records_unique = sum(rs == 1), 
                     Records_crossover = .data$Records_total - Records_unique)
    })
    
  })
  
  yields <- yields %>% 
    dplyr::group_by(.data$stage) %>% 
    mutate(sensitivity = .data$Records_total / sum(.data$Records_total)) %>% 
    dplyr::ungroup() %>% 
   dplyr::bind_rows(dplyr::group_by(., .data$stage) %>% dplyr::summarise(!!comparison_type := "Total", dplyr::across(tidyselect::where(is.numeric), sum), sensitivity = NA))
  
  search_results <- yields %>% dplyr::filter(.data$stage == "search") 
  
  yields <- yields %>% dplyr::filter(.data$stage != "search") %>% dplyr::left_join(
    search_results %>% dplyr::select(-"stage", total_search = .data$Records_total, -c("Records_unique":"sensitivity")), by = comparison_type) %>% 
    dplyr::mutate(precision = .data$Records_total / .data$total_search) %>% 
    dplyr::select(-"total_search") %>% 
    dplyr::bind_rows(search_results) %>% 
    dplyr::arrange(-.data$Records_total) %>% 
    dplyr::mutate(total_total = dplyr::if_else(sources == "Total", Records_total, NA_integer_)) %>% 
    tidyr::fill(total_total) %>% 
    dplyr::mutate(Contribution_total = dplyr::if_else(sources == "Total", NA_real_, Records_total/total_total), 
                  Contribution_unique = dplyr::if_else(sources == "Total", NA_real_, Records_unique/total_total)) %>% 
    dplyr::select(-"total_total")
  
  yields %>% dplyr::group_by(.data$stage) %>% gt::gt() %>% 
    gt::fmt_percent(6:9) %>% gt::sub_missing() %>% 
    gt::tab_spanner_delim("_") %>% 
    gt::fmt_number(3:5, decimals = 0)
  
}