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
    dplyr::pull(!!sym(db_colname)) %>%
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
  # Calculate and add total row
  totals <- data.frame("Source" = "Total", 
                       "Records Imported" = sum(citation_counts$`Records Imported`, na.rm = TRUE),
                       "Distinct Records" = sum(citation_counts$`Distinct Records`, na.rm = TRUE))
  # Append total row to the dataframe
  citation_counts <- rbind(citation_counts, totals)
  
  
  return(citation_counts)
}

#' Calculate counts fucntion
#' Calculate and combine counts of distinct records, imported records, and unique records for each database
#'
#' This function calculates the counts of distinct records, records imported, and unique records for each database source. 
#' It combines these counts into one dataframe and calculates several ratios and percentages related to the unique and distinct counts.
#' It also calculates the total for each count type.
#'
#' @param unique_citations Dataframe. The dataframe for calculating distinct records count.
#' @param citations Dataframe. The dataframe for calculating records imported count.
#' @param n_unique Dataframe. The dataframe for calculating unique records count.
#' @param db_colname Character. The name of the column containing the database source information.
#'
#' @return A dataframe with counts of distinct records, imported records, and unique records for each source, including total counts and several calculated ratios and percentages.

calculate_record_counts <- function(unique_citations, citations, n_unique, db_colname) {
  
  # Calculate the count of distinct records for each database source 
  # and convert the count to numeric.
  distinct_count <- count_sources(unique_citations, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  distinct_count$`Distinct Records` <- as.numeric(distinct_count$`Distinct Records`)
  
  # Count the number of records imported from each source in the initial citations data.
  # Also, convert these counts to numeric.
  initial_citations_count <- count_sources(citations, db_colname)
  colnames(initial_citations_count) <- c("Source", "Records Imported")
  initial_citations_count$`Records Imported` <- as.numeric(initial_citations_count$`Records Imported`)
  
  # Filter n_unique data to only include records with 'search' as the citation label.
  # Then count the unique records in each source, convert these counts to numeric, and rename the column.
  n_unique_citations_count <- n_unique %>%
    dplyr::filter(cite_label == "search") %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(`Unique records` = sum(unique)) %>%
    dplyr::filter(cite_source != "") %>%
    dplyr::arrange(cite_source) %>%
    dplyr::rename(Source = cite_source)
  n_unique_citations_count$`Unique records` <- as.numeric(n_unique_citations_count$`Unique records`)
  
  # Merge the three counts (initial, distinct, unique) into a single dataframe.
  citation_counts <- dplyr::left_join(initial_citations_count, distinct_count, by = "Source") %>%
    dplyr::left_join(n_unique_citations_count, by = "Source")
  
  # Calculate the number of non-unique records by subtracting the number of unique records from the total records.
  citation_counts <- citation_counts %>%
    dplyr::mutate(`Non-unique Records` = `Distinct Records` - `Unique records`)
  citation_counts$`Non-unique Records` <- as.numeric(citation_counts$`Non-unique Records`)
  
  # Calculate and add three percentages: the contribution of each source to the total, 
  # the contribution of unique records of each source to the total unique records,
  # and the proportion of unique records in each source's distinct records.
  citation_counts <- citation_counts %>%
    dplyr::mutate(`Source Contribution %` = `Distinct Records` / sum(`Distinct Records`, na.rm = TRUE),
                  `Source Unique Contribution %` = `Unique records` / sum(`Unique records`, na.rm = TRUE),
                  `Source Unique %` = `Unique records` / `Distinct Records`) %>%
    dplyr::mutate(
      `Source Contribution %` = as.numeric(`Source Contribution %`),
      `Source Unique Contribution %` = as.numeric(`Source Unique Contribution %`),
      `Source Unique %` = as.numeric(`Source Unique %`)
    ) %>%
    dplyr::mutate(
      `Source Contribution %` = scales::percent(`Source Contribution %`, accuracy = 0.1),
      `Source Unique Contribution %` = scales::percent(`Source Unique Contribution %`, accuracy = 0.1),
      `Source Unique %` = scales::percent(`Source Unique %`, accuracy = 0.1))
  
  # Calculate the sum totals for each numeric column
  totals <- data.frame("Source" = "Total", 
                       "Records Imported" = sum(citation_counts$`Records Imported`, na.rm = TRUE),
                       "Distinct Records" = sum(citation_counts$`Distinct Records`, na.rm = TRUE),
                       "Unique records" = sum(citation_counts$`Unique records`, na.rm = TRUE),
                       "Non-unique Records" = sum(citation_counts$`Non-unique Records`, na.rm = TRUE))
  
  # Add a row to the citation_counts dataframe with the totals calculated above.
  citation_counts <- dplyr::bind_rows(citation_counts, totals)
  
  # Return the final counts dataframe which includes initial, distinct, and unique record counts 
  # and percentage contribution of each source to the totals.
  return(citation_counts)
}


#' Calculate phase counts function
#' Calculate and combine counts of distinct records for each database and counts of 'Screened' and 'Final' labels
#'
#' This function calculates the counts of distinct records for each database source and the counts of 'Screened' and 'Final' labels. 
#' It combines these counts into one dataframe, calculates precision and recall, and also calculates total counts for each count type.
#'
#' @param unique_citations Dataframe. The dataframe for calculating distinct records count and also counts of 'Screened' and 'Final' labels.
#' @param citations Dataframe. Unused in the current implementation.
#' @param db_colname Character. The name of the column containing the database source information.
#'
#' @return A dataframe with counts of distinct records, 'Screened' and 'Final' labels for each source, including total counts, precision and recall.

calculate_phase_count <- function(unique_citations, citations, db_colname) {
  # This function calculates counts for different phases and precision and recall
  
  # Nested helper function to count occurrences of each source in 'Screened' and 'Final' phases
  count_source_phase <- function(source_phase_df, db_colname) {
    # Take a dataframe and a database column name as inputs.
    
    source_phase_df <- source_phase_df %>%
      # Separate rows by the database column and cite_label, which should contain phase information.
      tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",") %>%
      tidyr::separate_rows(cite_label, sep = ",") %>%
      # Trim any extra spaces from the database column and cite_label.
      dplyr::mutate(!!rlang::sym(db_colname) := stringr::str_trim(!!rlang::sym(db_colname)),
                    cite_label = stringr::str_trim(cite_label)) %>%
      # Filter out any rows where the database column is 'unknown'.
      dplyr::filter(!!rlang::sym(db_colname) != "unknown") %>%
      # Create two new columns, screened and final, which are binary flags indicating the phase.
      dplyr::mutate(screened = ifelse(cite_label == "screened", 1, 0),
                    final = ifelse(cite_label == "final", 1, 0)) %>%
      # Group by the database column and calculates the sum of the screened and final columns.
      dplyr::group_by(!!rlang::sym(db_colname)) %>%
      dplyr::summarise(screened = sum(screened),
                       final = sum(final),
                       .groups = "drop") %>%
      # Rename the database column to 'Source'.
      dplyr::rename(Source = !!rlang::sym(db_colname))
    
    # Converts the screened and final columns to numeric.
    source_phase_df$screened <- as.numeric(source_phase_df$screened)
    source_phase_df$final <- as.numeric(source_phase_df$final)
    
    # Returns the updated dataframe.
    return(source_phase_df)
  }
  
  # Count the occurrences of each source in the 'Screened' and 'Final' phases.
  source_phase <- count_source_phase(unique_citations, db_colname)
  
  # Compute the distinct counts of each source.
  distinct_count <- count_sources(unique_citations, db_colname)
  colnames(distinct_count) <- c("Source", "Distinct Records")
  distinct_count$`Distinct Records` <- as.numeric(distinct_count$`Distinct Records`)
  
  # Combine the distinct count and the phase count into one table.
  combined_counts <- dplyr::left_join(distinct_count, source_phase, by = "Source")
  
  # Replace any NA values in the table with 0.
  combined_counts[is.na(combined_counts)] <- 0
  
  # Calculate the Precision and Recall for each source.
  combined_counts <- combined_counts %>%
    dplyr::mutate(
      Precision = ifelse(`Distinct Records` != 0, round((final / `Distinct Records`) * 100, 2), 0),
      Recall = ifelse(sum(final, na.rm = TRUE) != 0, round((final / sum(final, na.rm = TRUE)) * 100, 2), 0)
    )
  
  # Calculate the total counts for all sources combined.
  totals <- data.frame(
    "Source" = "Total", 
    "Distinct Records" = sum(combined_counts$`Distinct Records`, na.rm = TRUE),
    "screened" = sum(combined_counts$screened, na.rm = TRUE),
    "final" = sum(combined_counts$final, na.rm = TRUE)
  )
  
  # Add the total row to the end of the combined counts dataframe.
  combined_counts <- dplyr::bind_rows(combined_counts, totals)
  
  # Remove the 'unknown' row as this is likely an error or placeholder that is no longer necessary.
  combined_counts <- combined_counts %>%
    dplyr::filter(Source != "unknown")
  
  # Return the final counts dataframe, which contains distinct counts, counts for different phases,
  # and the precision and recall for each source, as well as totals.
  return(combined_counts)
}

  
