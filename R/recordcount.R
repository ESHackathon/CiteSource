#' Calculate Counts of Records
#'
#' This function calculates the distinct record count and initial citations count for each source in the input data frames.
#' The function then combines these counts into one table, converts the necessary columns to numeric, adds a row for the totals, and returns the resulting data frame.
#'
#' @param df1 A data frame representing unique citations. Must include a column matching `db_colname` which will be used for count calculations.
#' @param df2 A data frame representing citations. Must include a column matching `db_colname` which will be used for count calculations.
#' @param db_colname The column name that exists in both `df1` and `df2` which will be used to calculate counts. Should be a character string.
#' 
#' @return A data frame with the counts of records for each source in the input data frames. Includes columns for the source, records imported, distinct records, and a total row.

calculate_counts <- function(df1, df2, db_colname) {
  # Internal function to count sources
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
  
  # Calculate distinct record count for each database
  distinct_count <- count_sources(df1, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  
  # Count the citations in the uploaded .ris data and remove blank rows
  initial_citations_count <- count_sources(df2, db_colname)
  colnames(initial_citations_count) <- c("Source", "Records Imported")
  
  # Combine the counts into one table
  citation_counts <- dplyr::left_join(initial_citations_count, distinct_count, by = "Source")
  
  # Convert the necessary columns to numeric
  citation_counts$`Distinct Records` <- as.numeric(citation_counts$`Distinct Records`)
  citation_counts$`Records Imported` <- as.numeric(citation_counts$`Records Imported`)
  
  # Convert the Source column to character
  citation_counts$Source <- as.character(citation_counts$Source)
  
  # Calculate totals
  totals <- c("Total", 
              sum(citation_counts$`Records Imported`, na.rm = TRUE),
              sum(citation_counts$`Distinct Records`, na.rm = TRUE))
  
  # Add the total row
  citation_counts <- rbind(citation_counts, totals)
  
  return(citation_counts)
}

#' calculate_all_counts
#'
#' This function takes in three data frames and a column name, and calculates various
#' counts related to citations from different sources. The counts include the number of
#' distinct records, imported records, unique records, non-unique records, and various percentages.
#' The function returns a data frame summarizing the counts and percentages, 
#' including totals for each metric.
#'
#' @param unique_citations A data frame representing unique citations.
#' @param citations A data frame representing citations.
#' @param n_unique A data frame with unique citations where the label of citation is "search".
#' @param db_colname A character string representing the name of the database column in the data frame.
#' 
#' @return A data frame summarizing the counts and percentages, including totals for each metric.
#' The columns of the data frame are: 'Source', 'Records Imported', 'Distinct Records',
#' 'Unique records', 'Non-unique Records', 'Source Contribution %',
#' 'Source Unique Contribution %', 'Source Unique %'.
#'
#' @examples
#' \dontrun{
#' all_counts <- calculate_all_counts(unique_citations, citations, n_unique, "cite_source")
#' }
#' @export
#'
calculate_all_counts <- function(unique_citations, citations, n_unique, db_colname) {
  # Function to count occurrences of each database
  count_sources_all <- function(df, db_colname) {
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
  
  # Calculate Distinct Records count for each database
  distinct_count <- count_sources_all(unique_citations, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  
  # Count the citations in the uploaded .ris data and remove blank rows
  initial_citations_count <- count_sources_all(citations, db_colname)
  colnames(initial_citations_count) <- c("Source", "Records Imported")
  
  # Count the citations in the n_unique data, remove blank rows, and exclude rows without 'search' in cite_label
  n_unique_citations_count <- n_unique %>%
    dplyr::filter(cite_label == "search") %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(`Unique records` = sum(unique)) %>%
    dplyr::filter(cite_source != "") %>%
    dplyr::arrange(cite_source) %>%
    dplyr::rename(Source = cite_source)
  
  # Combine the counts into one table
  citation_counts <- dplyr::left_join(initial_citations_count, distinct_count, by = "Source") %>%
    dplyr::left_join(n_unique_citations_count, by = "Source")
  
  # Add the "Non-unique Records" column directly
  citation_counts <- citation_counts %>%
    dplyr::mutate(`Non-unique Records` = `Distinct Records` - `Unique records`)
  
  # Convert the necessary columns to numeric
  citation_counts$`Distinct Records` <- as.numeric(citation_counts$`Distinct Records`)
  citation_counts$`Records Imported` <- as.numeric(citation_counts$`Records Imported`)
  citation_counts$`Unique records` <- as.numeric(citation_counts$`Unique records`)
  citation_counts$`Non-unique Records` <- as.numeric(citation_counts$`Non-unique Records`)
  
  # Add the calculations
  citation_counts <- citation_counts %>%
    dplyr::mutate(`Source Contribution %` = `Distinct Records` / sum(`Distinct Records`, na.rm = TRUE),
                  `Source Unique Contribution %` = `Unique records` / sum(`Unique records`, na.rm = TRUE),
                  `Source Unique %` = `Unique records` / `Distinct Records`)
  
  citation_counts <- citation_counts %>%
    dplyr::mutate(
      `Source Contribution %` = as.numeric(`Source Contribution %`),
      `Source Unique Contribution %` = as.numeric(`Source Unique Contribution %`),
      `Source Unique %` = as.numeric(`Source Unique %`)
    ) %>%
    dplyr::mutate(
      `Source Contribution %` = scales::percent(`Source Contribution %`, accuracy = 0.1),
      `Source Unique Contribution %` = scales::percent(`Source Unique Contribution %`, accuracy = 0.1),
      `Source Unique %` = scales::percent(`Source Unique %`, accuracy = 0.1))
  
  # Calculate totals
  totals <- c("Total", 
              sum(citation_counts$`Records Imported`, na.rm = TRUE),
              sum(citation_counts$`Distinct Records`, na.rm = TRUE),
              sum(citation_counts$`Unique records`, na.rm = TRUE),
              sum(citation_counts$`Non-unique Records`, na.rm = TRUE),
              "-", "-", "-")
  
  # Add the total row
  citation_counts <- rbind(citation_counts, totals)
  
  return(citation_counts)
}

#' Calculate Counts with Phase
#'
#' This function calculates the counts, precision, and recall for each source in a dataset 
#' of citations with respect to 'Screened' and 'Final' phases.
#'
#' @param unique_citations A data frame of unique citations.
#' @param citations A data frame of all citations.
#' @param db_colname A string specifying the database column name in the citation data frames.
#'
#' @return A data frame that includes counts, precision, and recall calculated for each source.

calculate_counts_with_phase <- function(unique_citations, citations, db_colname) {
  # Function to count occurrences of each source in the 'Screened' and 'Final' phases
  count_source_phase <- function(source_phase_df, db_colname) {
    source_phase_df <- source_phase_df %>%
      tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",") %>%
      tidyr::separate_rows(cite_label, sep = ",") %>%
      dplyr::mutate(!!rlang::sym(db_colname) := stringr::str_trim(!!rlang::sym(db_colname)),
                    cite_label = stringr::str_trim(cite_label)) %>%
      dplyr::filter(!!rlang::sym(db_colname) != "unknown") %>%
      dplyr::mutate(screened = ifelse(cite_label == "screened", 1, 0),
                    final = ifelse(cite_label == "final", 1, 0)) %>%
      dplyr::group_by(!!rlang::sym(db_colname)) %>%
      dplyr::summarise(screened = sum(screened),
                       final = sum(final),
                       .groups = "drop") %>%
      dplyr::rename(Source = !!rlang::sym(db_colname))
    
    return(source_phase_df)
  }
  
  # Function to count occurrences of each database
  count_sources <- function(source_phase_df, db_colname) {
    db_counts <- source_phase_df %>%
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
  
  source_phase <- count_source_phase(unique_citations, db_colname)
  distinct_count <- count_sources(unique_citations, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  
  distinct_count$`Distinct Records` <- as.numeric(distinct_count$`Distinct Records`)
  distinct_count$Source <- as.character(distinct_count$Source)
  
  # Combine the results
  combined_counts <- dplyr::left_join(distinct_count, source_phase, by = "Source")
  
  # Replace NA values with 0
  combined_counts[is.na(combined_counts)] <- 0
  
  # Calculate Precision
  combined_counts <- combined_counts %>%
    dplyr::mutate(Precision = ifelse(`Distinct Records` != 0, round((final / `Distinct Records`) * 100, 2), 0))  # Multiply by 100 and round
  
  # Calculate Recall manually in a loop
  total_final <- sum(combined_counts$final)
  for(i in 1:nrow(combined_counts)) {
    combined_counts$Recall[i] <- round((combined_counts$final[i] / total_final) * 100, 2)  # Multiply by 100 and round
  }
  
  # Calculate totals
  totals <- c("Total", 
              sum(combined_counts$`Distinct Records`, na.rm = TRUE),
              paste0(sum(citations$cite_label == "screened"), "⁶"),
              paste0(sum(citations$cite_label == "final"), "⁷"),
              "-",  # Precision total doesn't make sense
              "-")  # Recall total doesn't make sense
  
  # Add the total row
  combined_counts <- rbind(combined_counts, totals)
  
  # Remove the 'unknown' row - this should be corrected and unnecessary soon
  combined_counts <- combined_counts %>%
    dplyr::filter(Source != "unknown")
  
  return(combined_counts)
}