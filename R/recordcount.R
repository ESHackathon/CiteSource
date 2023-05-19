#' Count Sources Function
#' 
#' This function counts the unique citations per source.
#'
#' @param df A data frame containing the raw data.
#' @param db_colname The name of the column in `df` that contains the database names.
#' 
#' @return A data frame of counts by source.
#' 
#' @export
count_sources <- function(df, db_colname) {
  db_counts <- df %>%
    dplyr::pull(!!sym(db_colname)) %>%
    strsplit(", ") %>%
    lapply(unique) %>%  
    unlist() %>%
    trimws() %>%
    table() %>%
    as.data.frame()
  
  colnames(db_counts) <- c("Source", "count")
  
  return(db_counts)
}

#' Extend Citation Counts Function
#' 
#' This function extends the base citation counts with unique/non-unique records and contribution metrics.
#'
#' @param citation_counts A data frame returned by `count_sources()`.
#' @param unique_data A data frame of unique citations.
#'
#' @return A data frame of extended citation counts.
#'
#' @export
extend_citation_counts <- function(citation_counts, unique_data) {
  n_unique_citations_count <- unique_data %>%
    dplyr::filter(cite_label == "search") %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(`Unique records` = sum(unique)) %>%
    dplyr::filter(cite_source != "") %>%
    dplyr::arrange(cite_source) %>%
    dplyr::rename(Source = cite_source)
  
  citation_counts <- dplyr::left_join(citation_counts, n_unique_citations_count, by = "Source")
  
  citation_counts <- citation_counts %>%
    dplyr::mutate(`Non-unique Records` = `Distinct Records` - `Unique records`)
  
  citation_counts <- citation_counts %>%
    dplyr::mutate_at(vars(`Distinct Records`, `Records Imported`, `Unique records`, `Non-unique Records`), as.numeric)
  
  citation_counts <- citation_counts %>%
    dplyr::mutate(
      `Source Contribution %` = `Distinct Records` / sum(`Distinct Records`, na.rm = TRUE),
      `Source Unique Contribution %` = `Unique records` / sum(`Unique records`, na.rm = TRUE),
      `Source Unique %` = `Unique records` / `Distinct Records`
    ) %>%
    dplyr::mutate_at(vars(`Source Contribution %`, `Source Unique Contribution %`, `Source Unique %`), 
                     ~scales::percent(., accuracy = 0.1))
  
  totals <- c("Total", 
              sum(citation_counts$`Records Imported`, na.rm = TRUE),
              sum(citation_counts$`Distinct Records`, na.rm = TRUE),
              sum(citation_counts$`Unique records`, na.rm = TRUE),
              sum(citation_counts$`Non-unique Records`, na.rm = TRUE),
              "-", "-", "-")
  
  citation_counts <- rbind(citation_counts, totals)
  
  return(citation_counts)
}

#' Create Combined Counts Function
#' 
#' This function combines the base citation counts with screening and final phases, and calculates precision/recall.
#' The totals computation includes the use of "⁶" and "⁷" superscripts which represent the counts of 'screened' and 'final' labels respectively in the citations data frame.
#'
#' @param citation_counts A data frame returned by `extend_citation_counts()`.
#' @param #' unique_citations A data frame of unique citations.
#' @param citations A data frame of all citations.
#'
#' @return A data frame of combined citation counts.
#'
#' @export
create_combined_counts <- function(citation_counts, unique_citations, citations) {
  count_source_phase <- function(source_phase_df) {
    source_phase_df <- source_phase_df %>%
      tidyr::separate_rows(cite_source, sep = ",") %>%
      tidyr::separate_rows(cite_label, sep = ",") %>%
      dplyr::mutate(cite_source = stringr::str_trim(cite_source),
                    cite_label = stringr::str_trim(cite_label)) %>%
      dplyr::filter(cite_source != "unknown") %>%
      dplyr::mutate(screened = ifelse(cite_label == "screened", 1, 0),
                    final = ifelse(cite_label == "final", 1, 0)) %>%
      dplyr::group_by(cite_source) %>%
      dplyr::summarise(screened = sum(screened),
                       final = sum(final),
                       .groups = "drop") %>%
      dplyr::rename(Source = cite_source)
    
    return(source_phase_df)
  }
  
  source_phase <- count_source_phase(unique_citations)
  
  combined_counts <- dplyr::left_join(citation_counts, source_phase, by = "Source")
  
  combined_counts[is.na(combined_counts)] <- 0
  
  combined_counts <- combined_counts %>%
    dplyr::mutate(Precision = ifelse(`Distinct Records` != 0, round((final / `Distinct Records`) * 100, 2), 0))
  
  total_final <- sum(combined_counts$final)
  for(i in 1:nrow(combined_counts)) {
    combined_counts$Recall[i] <- round((combined_counts$final[i] / total_final) * 100, 2)
  }
  
  totals <- c("Total", 
              sum(combined_counts$`Distinct Records`, na.rm = TRUE),
              paste0(sum(citations$cite_label == "screened"), "⁶"),
              paste0(sum(citations$cite_label == "final"), "⁷"),
              "-", 
              "-")
  
  combined_counts <- rbind(combined_counts, totals)
  
  combined_counts <- combined_counts %>%
    dplyr::filter(Source != "unknown")
  
  return(combined_counts)
}

