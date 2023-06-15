#' Count sources function
#' Count database source occurrences in a column
#'
#' This function counts the occurrences of different database sources present in a specific column of a dataframe. 
#' It returns a dataframe with the counts for each source.
#'
#' @param df Dataframe. The dataframe containing the data to be analyzed.
#' @param db_colname Character. The name of the column containing the database source information.
#'
#' @return A dataframe with the names of the sources and their corresponding counts.

count_sources <- function(df, db_colname) {
# Pull out the database column, split it into multiple elements if there are commas,
# create a list of unique elements, unlist it to a vector, remove white spaces, and count occurrences
  db_counts <- df %>%
    dplyr::pull(!!rlang::sym(db_colname)) %>%
    strsplit(", ") %>%
    lapply(unique) %>%
    unlist() %>%
    trimws() %>%
    table() %>%
    as.data.frame()
  return(db_counts)
}

#' Record counts function
#' Calculate and combine counts of distinct records and imported records for each database
#'
#' This function calculates the counts of distinct records and records imported for each database source. 
#' It combines these counts into one dataframe and calculates the total for each count type.
#'
#' @export
#' @param df1 Dataframe. The dataframe for calculating distinct records count.
#' @param df2 Dataframe. The dataframe for calculating records imported count.
#' @param db_colname Character. The name of the column containing the database source information.
#'
#' @return A dataframe with counts of distinct records and imported records for each source, including total counts.

record_counts <- function(df1, df2, db_colname) {
  # Count distinct record sources
  distinct_count <- count_sources(df1, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  distinct_count$`Distinct Records` <- as.numeric(distinct_count$`Distinct Records`)
  
  # Count initial imported record sources
  initial_citations_count <- count_sources(df2, db_colname)
  colnames(initial_citations_count) <- c("Source", "Records Imported")
  initial_citations_count$`Records Imported` <- as.numeric(initial_citations_count$`Records Imported`)
  
  # Merge the two counts into a single dataframe
  citation_counts <- dplyr::left_join(initial_citations_count, distinct_count, by = "Source")
  
  # Convert the Source column to character
  citation_counts$Source <- as.character(citation_counts$Source)
 
  # Create 'totals' dataframe with same structure
  totals <- citation_counts[FALSE, ]  # This creates an empty df with the same structure
  
  # Calculate the totals
  total_records_imported <- sum(citation_counts$`Records Imported`, na.rm = TRUE)
  total_distinct_records <- sum(citation_counts$`Distinct Records`, na.rm = TRUE)
  
  # Use add_row() to add the totals to the dataframe
  citation_counts <- tibble::add_row(citation_counts, 
                                     Source = "Total", 
                                     `Records Imported` = total_records_imported,
                                     `Distinct Records` = total_distinct_records)
  
  return(citation_counts)
}

#' Calculate record counts function
#' Calculate and combine counts of distinct records, imported records, and unique records for each database
#'
#' This function calculates the counts of distinct records, records imported, and unique records for each database source. 
#' It combines these counts into one dataframe and calculates several ratios and percentages related to the unique and distinct counts.
#' It also calculates the total for each count type.
#'
#' @export
#' @param unique_citations Dataframe. The dataframe for calculating distinct records count.
#' @param citations Dataframe. The dataframe for calculating records imported count.
#' @param n_unique Dataframe. The dataframe for calculating unique records count.
#' @param db_colname Character. The name of the column containing the database source information.
#'
#' @return A dataframe with counts of distinct records, imported records, and unique records for each source, including total counts and several calculated ratios and percentages.

calculate_record_counts <- function(unique_citations, citations, n_unique, db_colname) {
  
  # Calculate the count of distinct records for each database source and convert the count to numeric.
  distinct_count <- count_sources(unique_citations, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  distinct_count$`Distinct Records` <- as.numeric(distinct_count$`Distinct Records`)
  
  # Count the number of records imported from each source in the initial citations data. Also, convert these counts to numeric.
  initial_citations_count <- count_sources(citations, db_colname)
  colnames(initial_citations_count) <- c("Source", "Records Imported")
  initial_citations_count$`Records Imported` <- as.numeric(initial_citations_count$`Records Imported`)
  
  # Filter n_unique data to only include records with 'search' as the citation label.
  # Then count the unique records in each source, convert these counts to numeric, and rename the column.
  n_unique_citations_count <- n_unique %>%
    dplyr::filter(.data$cite_label == "search") %>%
    dplyr::group_by(.data$cite_source) %>%
    dplyr::summarise("Unique records" = sum(.data$unique)) %>%
    dplyr::filter(.data$cite_source != "") %>%
    dplyr::arrange(.data$cite_source) %>%
    dplyr::rename(Source = .data$cite_source)
  n_unique_citations_count$`Unique records` <- as.numeric(n_unique_citations_count$`Unique records`)
  
  # Merge the three counts (initial, distinct, unique) into a single dataframe.
  citation_counts <- dplyr::left_join(initial_citations_count, distinct_count, by = "Source") %>%
    dplyr::left_join(n_unique_citations_count, by = "Source")
  
  # Calculate the number of non-unique records by subtracting the number of unique records from the total records.
  citation_counts <- citation_counts %>%
    dplyr::mutate("Non-unique Records" = .data$`Distinct Records` - .data$`Unique records`)
  citation_counts$`Non-unique Records` <- as.numeric(citation_counts$`Non-unique Records`)
  
  # Calculate and add three percentages: the contribution of each source to the total,
  # the contribution of unique records of each source to the total unique records,
  # and the proportion of unique records in each source's distinct records.
  citation_counts <- citation_counts %>%
    dplyr::mutate("Source Contribution %" = .data$`Distinct Records` / sum(.data$`Distinct Records`, na.rm = TRUE),
                  "Source Unique Contribution %" = .data$`Unique records` / sum(.data$`Unique records`, na.rm = TRUE),
                  "Source Unique %" = .data$`Unique records` / .data$`Distinct Records`)
  
  citation_counts <- citation_counts %>%
    dplyr::mutate(
      `Source Contribution %` = as.numeric(.data$`Source Contribution %`),
      `Source Unique Contribution %` = as.numeric(.data$`Source Unique Contribution %`),
      `Source Unique %` = as.numeric(.data$`Source Unique %`)
    ) %>%
    dplyr::mutate(
      `Source Contribution %` = scales::percent(.data$`Source Contribution %`, accuracy = 0.1),
      `Source Unique Contribution %` = scales::percent(.data$`Source Unique Contribution %`, accuracy = 0.1),
      `Source Unique %` = scales::percent(.data$`Source Unique %`, accuracy = 0.1))
  
  # Calculate the totals
  total_records_imported <- sum(citation_counts$`Records Imported`, na.rm = TRUE)
  total_distinct_records <- sum(citation_counts$`Distinct Records`, na.rm = TRUE)
  total_unique_records <- sum(citation_counts$`Unique records`, na.rm = TRUE)
  total_nonunique_records <- sum(citation_counts$`Non-unique Records`, na.rm = TRUE)
  
  # Add totals to the citation_counts dataframe
  calculated_counts <- tibble::add_row(citation_counts, 
                                       Source = "Total", 
                                       `Records Imported` = total_records_imported,
                                       `Distinct Records` = total_distinct_records,
                                       `Unique records` = total_unique_records,
                                       `Non-unique Records` = total_nonunique_records)
  
  # Return the final counts dataframe which includes initial, distinct, and unique record counts and percentage contribution of each source to the totals.
  return(calculated_counts)
}


#' Calculate phase counts, precision, and recall
#'
#' This function calculates counts for different phases and calculates precision and recall
#' for each source based on unique citations and citations dataframe.
#'
#' @export
#' @param unique_citations A dataframe containing unique citations with phase information.
#' @param citations A dataframe containing all citations with phase information.
#' @param db_colname The name of the column representing the source database.
#'
#' @return A dataframe containing distinct counts, counts for different phases, precision,
#' and recall for each source, as well as totals.

calculate_phase_count <- function(unique_citations, citations, db_colname) {
  count_source_phase <- function(source_phase_df, db_colname) {
    source_phase_df <- source_phase_df %>%
      tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",") %>%
      tidyr::separate_rows(cite_label, sep = ",") %>%
      dplyr::mutate(!!rlang::sym(db_colname) := stringr::str_trim(!!rlang::sym(db_colname)),
                    cite_label = stringr::str_trim(cite_label)) %>%
      dplyr::mutate(screened = ifelse(.data$cite_label == "screened", 1, 0),
                    final = ifelse(.data$cite_label == "final", 1, 0)) %>%
      dplyr::group_by(!!rlang::sym(db_colname)) %>%
      dplyr::summarise(screened = sum(.data$screened),
                       final = sum(.data$final),
                       .groups = "drop") %>%
      dplyr::rename(Source = !!rlang::sym(db_colname))
    
    return(source_phase_df)
  }


  
  source_phase <- count_source_phase(unique_citations, db_colname)
  
  distinct_count <- count_sources(unique_citations, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  
  distinct_count$`Distinct Records` <- as.numeric(distinct_count$`Distinct Records`)
  distinct_count$Source <- as.character(distinct_count$Source)
  
  combined_counts <- dplyr::left_join(distinct_count, source_phase, by = "Source")
  combined_counts[is.na(combined_counts)] <- 0
  
  combined_counts <- combined_counts %>%
    dplyr::mutate(Precision = ifelse(.data$`Distinct Records` != 0, round((.data$final / .data$`Distinct Records`) * 100, 2), 0))
  
  # Calculate total_final before the loop
  total_final <- sum(citations$cite_label == "final")
  
  for(i in 1:nrow(combined_counts)) {
    combined_counts$Recall[i] <- round((combined_counts$final[i] / total_final) * 100, 2)
  }
  
  totals <- c("Total", 
              sum(combined_counts$`Distinct Records`, na.rm = TRUE),
              paste0(sum(citations$cite_label == "screened")),
              total_final,
              "-",
              "-")
  combined_counts <- rbind(combined_counts, totals)
  
  return(combined_counts)
}