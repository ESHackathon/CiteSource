#' Calculate Initial Records Unique Citations
#'
#' This function processes a dataset of unique citations, expands the `cite_source` column,
#' filters based on user-specified labels (if provided), and then calculates the number
#' of records imported and distinct records for each citation source. It also adds a
#' total row summarizing these counts.
#'
#' @param unique_citations A data frame containing the unique citations. 
#'   It must contain the columns `cite_source`, `cite_label`, and `duplicate_id`.
#' @param labels_to_include An optional character vector of labels to filter the citations. 
#'   If provided, only citations matching these labels will be included in the counts.
#'   Default is NULL, meaning no filtering will be applied.
#'
#' @return A data frame containing the counts of `Records Imported` and `Distinct Records` 
#'   for each citation source. The data frame also includes a "Total" row summing 
#'   the counts across all sources.
#'
#' @details 
#' The function first checks if the required columns are present in the input data frame.
#' It then expands the `cite_source` column to handle multiple sources listed in a 
#' single row and filters the dataset based on the provided labels (if any).
#' The function calculates the number of records imported (total rows) and the number 
#' of distinct records (unique `duplicate_id` values) for each citation source.
#' Finally, a total row is added to summarize the counts across all sources.
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' unique_citations <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   cite_label = c("Label1", "Label2", "Label3"),
#'   duplicate_id = c(1, 2, 3)
#' )
#' calculate_initial_records(unique_citations)
calculate_initial_records <- function(unique_citations, labels_to_include = NULL) {
  
  # Explicitly define variables as NULL to avoid binding warnings
  duplicate_id <- NULL
  
  # Check if necessary columns exist
  required_columns <- c("cite_source", "cite_label", "duplicate_id")
  if (!all(required_columns %in% colnames(unique_citations))) {
    stop("The dataset does not contain the required columns.")
  }
  
  # Split and expand the cite_source column
  df_expanded <- unique_citations %>%
    tidyr::separate_rows(cite_source, sep = ",") %>%
    dplyr::mutate(cite_source = trimws(cite_source))
  
  # Filter by user-specified labels if provided
  if (!is.null(labels_to_include) && length(labels_to_include) > 0) {
    pattern <- paste(labels_to_include, collapse = "|")
    df_filtered <- df_expanded %>%
      dplyr::filter(grepl(pattern, cite_label, ignore.case = TRUE))
  } else {
    df_filtered <- df_expanded
  }
  
  # Check if df_filtered is empty
  if (nrow(df_filtered) == 0) {
    return(data.frame(Source = character(), Records_Imported = integer(), Distinct_Records = integer()))
  }
  
  records_imported <- df_filtered %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(Records_Imported = n(), .groups = 'drop')
  
  # Count the unique duplicate_id values for each source to determine the "Distinct Records"
  distinct_records <- df_filtered %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(Distinct_Records = n_distinct(duplicate_id), .groups = 'drop')
  
  # Merge the two dataframes to get the final result
  initial_counts <- dplyr::left_join(records_imported, distinct_records, by = "cite_source")
  
  # Calculate the total counts
  total_records_imported <- sum(initial_counts$Records_Imported)
  total_distinct_records <- sum(initial_counts$Distinct_Records)
  
  # Add the total counts to the result dataframe
  total_row <- data.frame(cite_source = "Total",
                          Records_Imported = total_records_imported,
                          Distinct_Records = total_distinct_records)
  initial_counts <- dplyr::bind_rows(initial_counts, total_row)
  
  # Rename columns for consistency with gt table
  initial_counts <- initial_counts %>%
    dplyr::rename(Source = cite_source)
  
  # Return the final result
  return(initial_counts)
}



#' Calculate Detailed Record Counts
#' 
#' @description
#' This function processes a dataset and expands the 'cite_source' column, filters on
#' user-specified labels (if provided), and calculates detailed counts such as the records imported,
#' distinct records, unique records, non-unique records, and several percentage contributions for 
#' each citation source/method it also adds a total row summarizing these counts.
#' 
#' @param unique_citations A data frame containing unique citations. 
#'   The data frame must include the columns `cite_source`, `cite_label`, and `duplicate_id`.
#' @param n_unique A data frame containing counts of unique records, typically filtered 
#'   by specific criteria (e.g., `cite_label == "search"`).
#' @param labels_to_include An optional character vector of labels to filter the citations. 
#'   If provided, only citations matching these labels will be included in the counts.
#'   if 'NULL' all labels are included. Default is 'NULL'.
#'
#' @return A data frame with detailed counts for each citation source, including:
#'   - `Records Imported`: Total number of records imported.
#'   - `Distinct Records`: Number of distinct records after deduplication.
#'   - `Unique Records`: Number of unique records specific to a source.
#'   - `Non-unique Records`: Number of records found in other sources.
#'   - `Source Contribution %`: Percentage contribution of each source to the total distinct records.
#'   - `Source Unique Contribution %`: Percentage contribution of each source to the total unique records.
#'   - `Source Unique %`: Percentage of unique records within the distinct records for each source.
#'
#' @details 
#' The function first checks if the required columns are present in the input data frames.
#' It then expands the `cite_source` column, filters the data based on the provided labels (if any),
#' and calculates various counts and percentages for each citation source. The function also adds 
#' a total row summarizing these counts across all sources.
#'
#' @import dplyr
#' @import tidyr
#' @import scales
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' unique_citations <- data.frame(
#'   cite_source = c("Source1, Source2", "Source2", "Source3"),
#'   cite_label = c("Label1", "Label2", "Label1"),
#'   duplicate_id = c(1, 2, 3)
#' )
#' n_unique <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   cite_label = c("search", "search", "search"),
#'   unique = c(10, 20, 30)
#' )
#' calculate_detailed_records(unique_citations, n_unique, labels_to_include = "search")
calculate_detailed_records <- function(unique_citations, n_unique, labels_to_include = NULL) {
  
  # Explicitly define variables as NULL to avoid binding warnings
  cite_source <- NULL
  cite_label <- NULL
  duplicate_id <- NULL
  'Distinct Records' <- NULL
  'Unique Records' <- NULL
  Non_unique_Records <- NULL
  'Source Contribution %' <- NULL
  'Source Unique Contribution %' <- NULL
  'Source Unique %' <- NULL
  
  # Check if necessary columns exist in unique_citations
  required_columns <- c("cite_source", "cite_label", "duplicate_id")
  if (!all(required_columns %in% colnames(unique_citations))) {
    stop("The dataset does not contain the required columns.")
  }
  
  # Split and expand the cite_source column
  df_expanded <- unique_citations %>%
    tidyr::separate_rows(cite_source, sep = ",") %>%
    dplyr::mutate(cite_source = trimws(cite_source))
  
  # Filter by user-specified labels if provided
  if (!is.null(labels_to_include) && length(labels_to_include) > 0) {
    pattern <- paste(labels_to_include, collapse = "|")
    df_filtered <- df_expanded %>%
      dplyr::filter(grepl(pattern, cite_label, ignore.case = TRUE))
  } else {
    df_filtered <- df_expanded
  }
  
  # Check if df_filtered is empty
  if (nrow(df_filtered) == 0) {
    return(data.frame(Source = character(), 
                      `Records Imported` = integer(), 
                      `Distinct Records` = integer(), 
                      `Unique Records` = integer(), 
                      `Non-unique Records` = integer()
    )
    )
  }
  
  # Count the occurrences of each source to determine the "Records Imported"
  records_imported <- df_filtered %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(`Records Imported` = n(), .groups = 'drop')
  
  # Count the unique duplicate_id values for each source to determine the "Distinct Records"
  distinct_records <- df_filtered %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(`Distinct Records` = dplyr::n_distinct(duplicate_id), .groups = 'drop')
  
  # Filter n_unique data to only include records with 'search' as the citation label
  n_unique_citations_count <- n_unique %>%
    dplyr::filter(cite_label == "search") %>%
    dplyr::group_by(cite_source) %>%
    dplyr::summarise(`Unique Records` = sum(unique), .groups = 'drop') %>%
    dplyr::filter(cite_source != "") %>%
    dplyr::arrange(cite_source)
  
  # Merge the three counts (initial, distinct, unique) into a single dataframe
  detailed_counts <- dplyr::left_join(records_imported, distinct_records, by = "cite_source") %>%
    dplyr::left_join(n_unique_citations_count, by = "cite_source")
  
  # Calculate the number of non-unique records by subtracting the number of unique records from the distinct records
  detailed_counts <- detailed_counts %>%
    dplyr::mutate(`Non-unique Records` = `Distinct Records` - `Unique Records`)
  
  # Calculate and add three percentages: the contribution of each source to the total,
  # the contribution of unique records of each source to the total unique records,
  # and the proportion of unique records in each source's distinct records
  detailed_counts <- detailed_counts %>%
    dplyr::mutate(`Source Contribution %` = `Distinct Records` / sum(`Distinct Records`, na.rm = TRUE),
                  `Source Unique Contribution %` = `Unique Records` / sum(`Unique Records`, na.rm = TRUE),
                  `Source Unique %` = `Unique Records` / `Distinct Records`)
  
  # Convert percentage columns to numeric first and then to percentage format
  detailed_counts <- detailed_counts %>%
    dplyr::mutate(
      `Source Contribution %` = scales::percent(as.numeric(`Source Contribution %`), accuracy = 0.1),
      `Source Unique Contribution %` = scales::percent(as.numeric(`Source Unique Contribution %`), accuracy = 0.1),
      `Source Unique %` = scales::percent(as.numeric(`Source Unique %`), accuracy = 0.1)
    )
  
  # Calculate the totals
  total_records_imported <- sum(detailed_counts$`Records Imported`, na.rm = TRUE)
  total_distinct_records <- nrow(unique_citations)
  total_unique_records <- sum(detailed_counts$`Unique Records`, na.rm = TRUE)
  total_nonunique_records <- sum(detailed_counts$`Non-unique Records`, na.rm = TRUE)
  
  # Add totals to the detailed_counts dataframe
  detailed_counts <- tibble::add_row(detailed_counts, 
                                     cite_source = "Total", 
                                     `Records Imported` = total_records_imported,
                                     `Distinct Records` = total_distinct_records,
                                     `Unique Records` = total_unique_records,
                                     `Non-unique Records` = total_nonunique_records)
  
  # Rename columns for consistency
  detailed_counts <- detailed_counts %>%
    dplyr::rename(Source = cite_source)
  
  # Return the final counts dataframe
  return(detailed_counts)
}


#' Calculate Phase Counts with Precision and Recall
#'
#' This function calculates the distinct record counts, as well as screened 
#' and final record counts, for each citation source across different phases 
#' (e.g., "screened", "final"). It also calculates precision and recall metrics 
#' for each source.
#'
#' @param unique_citations A data frame containing unique citations. 
#'   It must include the columns `cite_source`, `cite_label`, and `duplicate_id`.
#' @param n_unique A data frame containing counts of unique records. 
#'   Typically filtered by specific criteria, such as `cite_label == "search"`.
#' @param db_colname The name of the column representing the citation source 
#'   in the `unique_citations` data frame.
#'
#' @return A data frame with phase counts and calculated precision and recall 
#'   for each citation source, including:
#'   - `Distinct Records`: The count of distinct records per source.
#'   - `screened`: The count of records in the "screened" phase.
#'   - `final`: The count of records in the "final" phase.
#'   - `Precision`: The precision metric calculated as `final / Distinct Records`.
#'   - `Recall`: The recall metric calculated as `final / Total final records`.
#'
#' @details 
#' The function starts by calculating the total distinct records, as well as 
#' the total "screened" and "final" records across all sources. It then 
#' calculates distinct counts for each source, followed by counts for "screened" 
#' and "final" records. Finally, it calculates precision and recall metrics and 
#' adds a total row summarizing these counts across all sources.
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' unique_citations <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   cite_label = c("screened","screened", "final"),
#'   duplicate_id = c(1, 2, 3)
#' )
#' n_unique <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   unique = c(10, 20, 30)
#' )
#' calculate_phase_records(unique_citations, n_unique, "cite_source")
calculate_phase_records <- function(unique_citations, n_unique, db_colname) {
  
  # Explicitly define variables as NULL to avoid binding warnings
  duplicate_id <- NULL
  Source <- NULL
  screened <- NULL
  final <- NULL
  Distinct_Records <- NULL
  
  
  # Step 1: Calculate and store the totals before expanding
  total_distinct_records <- dplyr::n_distinct(unique_citations$duplicate_id)
  
  # Split the cite_label column and count any occurrence of "screened" and "final"
  total_screened <- unique_citations %>%
    tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
    dplyr::filter(cite_label == "screened") %>%
    nrow()
  
  total_final <- unique_citations %>%
    tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
    dplyr::filter(cite_label == "final") %>%
    nrow()
  
  # Step 2: Proceed with the regular calculation for distinct records by source
  distinct_count <- unique_citations %>%
    tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",\\s*") %>%
    dplyr::filter(!(!!rlang::sym(db_colname) == "unknown" | !!rlang::sym(db_colname) == "")) %>%
    dplyr::group_by(!!rlang::sym(db_colname)) %>%
    dplyr::summarise(Distinct_Records = dplyr::n_distinct(duplicate_id), .groups = "drop") %>%
    dplyr::rename(Source = !!rlang::sym(db_colname))
  
  # Calculate the number of "screened" and "final" records for each source after expanding
  source_phase <- unique_citations %>%
    dplyr::select(!!rlang::sym(db_colname), cite_label, duplicate_id) %>%
    tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",\\s*") %>%
    tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
    dplyr::distinct() %>%
    dplyr::filter(!(!!rlang::sym(db_colname) == "unknown" | !!rlang::sym(db_colname) == "")) %>%
    dplyr::mutate(screened = ifelse(cite_label == "screened", 1, 0),
                  final = ifelse(cite_label == "final", 1, 0)) %>%
    dplyr::group_by(!!rlang::sym(db_colname)) %>%
    dplyr::summarise(screened = sum(screened),
                     final = sum(final),
                     .groups = "drop") %>%
    dplyr::rename(Source = !!rlang::sym(db_colname))
  
  # Combine the distinct counts with the source_phase
  combined_counts <- dplyr::left_join(distinct_count, source_phase, by = "Source")
  combined_counts[is.na(combined_counts)] <- 0
  
  # Step 3: Calculate Precision and Recall
  combined_counts <- combined_counts %>%
    dplyr::mutate(
      Precision = ifelse(Distinct_Records != 0, round((final / Distinct_Records) * 100, 2), 0)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Recall = ifelse(total_final != 0, round((final / total_final) * 100, 2), 0)
    ) %>%
    dplyr::ungroup()
  
  # Step 4: Calculate the total row using the pre-expansion totals
  totals <- tibble::tibble(
    Source = "Total", 
    Distinct_Records = total_distinct_records,  # Using the pre-expansion distinct count
    screened = total_screened,  # Using the correct total for screened
    final = total_final,  # Using the correct total for final
    Precision = ifelse(total_distinct_records != 0, round((total_final / total_distinct_records) * 100, 2), 0),
    Recall = NA  # Recall set to NA for the total row
  )
  
  phase_counts <- dplyr::bind_rows(combined_counts, totals)
  
  return(phase_counts)
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
#' @import gt
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Source3"),
#'   Records_Imported = c(100, 150, 250),
#'   Distinct_Records = c(90, 140, 230)
#' )
#' create_initial_record_table(sample_data)
create_initial_record_table <- function(data) {
  # Check if data is empty
  if (nrow(data) == 0) {
    return(gt::gt(data.frame(Source = character(), `Records Imported` = integer(), `Distinct Records` = integer())))
  }
  
  # Create the initial gt table
  data %>%
    gt::gt(rowname_col = "Source") %>%
    gt::tab_header(title = "Record Counts") %>%
    
    # Label the columns
    gt::cols_label(
      Records_Imported = "Records Imported",
      Distinct_Records = "Distinct Records"
    ) %>%
    
    # Add footnote for "Records Imported"
    gt::tab_footnote(
      footnote = "Number of records imported from each source.",
      locations = gt::cells_column_labels(
        columns = c("Records_Imported")
      )
    ) %>%
    
    # Add footnote for "Distinct Records"
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(
        columns = c("Distinct_Records")
      )
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
#' @details 
#' The function checks for the presence of all required columns in the input data frame. 
#' If any required columns are missing, the function stops and returns an error message 
#' specifying the missing columns. This ensures that the input data is correctly formatted 
#' before attempting to generate the table.
#'
#' The generated table includes a header and footnotes that provide additional context 
#' for each column, explaining the meaning of the data presented.
#'
#' @import gt
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Source3", "Total"),
#'   `Records Imported` = c(100, 150, 250, 500),
#'   `Distinct Records` = c(90, 140, 230, 460),
#'   `Unique Records` = c(50, 70, 120, 240),
#'   `Non-unique Records` = c(40, 70, 110, 220),
#'   `Source Contribution %` = c("39.1%", "60.9%", "100%", "100%"),
#'   `Source Unique Contribution %` = c("41.7%", "58.3%", "100%", "100%"),
#'   `Source Unique %` = c("55.6%", "50%", "52.2%", "52.2%"),
#'   check.names = FALSE
#' )
#' 
#' # Create the detailed record table
#' create_detailed_record_table(sample_data)
create_detailed_record_table <- function(data) {
  # Define the required columns for the table
  required_columns <- c("Source", "Records Imported", "Distinct Records", 
                        "Unique Records", "Non-unique Records", 
                        "Source Contribution %", "Source Unique Contribution %", 
                        "Source Unique %")
  
  # Check if all required columns are present in the data
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("The following required columns are missing from the data: ", 
         paste(missing_columns, collapse = ", "))
  }
  
  # Create the gt table using the provided data
  data %>%
    gt::gt(rowname_col = "Source") %>%
    
    # Add a title to the table
    gt::tab_header(title = "Record Summary") %>%
    
    # Label the columns as per their data role
    gt::cols_label(
      `Records Imported` = "Records Imported",
      `Distinct Records` = "Distinct Records",
      `Unique Records` = "Unique Records",
      `Non-unique Records` = "Non-unique Records",
      `Source Contribution %` = "Source Contribution %",
      `Source Unique Contribution %` = "Source Unique Contribution %",
      `Source Unique %` = "Source Unique %" 
    ) %>%
    
    # Add explanatory footnotes for the columns
    gt::tab_footnote(
      footnote = "Number of raw records imported from each database.",
      locations = gt::cells_column_labels(
        columns = "Records Imported"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(
        columns = "Distinct Records"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Number of records not found in another source.",
      locations = gt::cells_column_labels(
        columns = "Unique Records"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Number of records found in at least one other source.",
      locations = gt::cells_column_labels(
        columns = "Non-unique Records"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Percent distinct records contributed to the total number of distinct records.",
      locations = gt::cells_column_labels(
        columns = "Source Contribution %"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Percent of unique records contributed to the total unique records.",
      locations = gt::cells_column_labels(
        columns = "Source Unique Contribution %"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Percentage of records that were unique from each source.",
      locations = gt::cells_column_labels(
        columns = "Source Unique %"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Total citations discovered (after internal and cross-source deduplication).",
      locations = gt::cells_body(
        columns = "Distinct Records",
        rows = "Total"
      )
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
#' @import gt
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
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
  # First, we check if all values in the "screened" column are 0
  all_zero_screened <- all(data$screened == 0)
  
  # If all values are zero, we remove the "screened" column from the data
  if (all_zero_screened) {
    data <- data[ , !(names(data) %in% "screened")]
  }
  
  # Create the initial gt table
  gt_table <- data %>%
    gt::gt(rowname_col = "Source") %>%
    gt::tab_header(title = "Record Counts & Precision/Sensitivity") %>%
    
    # Label the columns
    gt::cols_label(
      `Distinct_Records` = "Distinct Records",
      final = "Final Included",
      Precision = "Precision",
      Recall = "Sensitivity/Recall"
    ) %>%
    
    # Align columns to the right
    gt::cols_align(
      align = "right",
      columns = c("final", "Precision", "Recall")
    )
  
  # If the "screened" column isn't all zeros, add its specific labels, alignment, and footnotes
  if (!all_zero_screened) {
    gt_table <- gt_table %>%
      gt::cols_label(screened = "Screened Included") %>%
      gt::cols_align(align = "right", columns = "screened") %>%
      gt::tab_footnote(
        footnote = "Number of citations included after title/abstract screening.",
        locations = gt::cells_column_labels(columns = "screened")
      ) %>%
      gt::tab_footnote(
        footnote = "Total citations included after Ti/Ab Screening.",
        locations = gt::cells_body(columns = "screened", rows = "Total")
      )
  }
  
  # Add remaining footnotes and return the gt_table
  gt_table %>%
    # Add footnotes for the columns
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(columns = "Distinct_Records")
    ) %>%
    gt::tab_footnote(
      footnote = "Number of citations included after full text screening.",
      locations = gt::cells_column_labels(columns = "final")
    ) %>%
    gt::tab_footnote(
      footnote = "Number of final included citations / Number of distinct records.",
      locations = gt::cells_column_labels(columns = "Precision")
    ) %>%
    gt::tab_footnote(
      footnote = "Number of final included citations / Total number of final included citations.",
      locations = gt::cells_column_labels(columns = "Recall")
    ) %>%
    gt::tab_footnote(
      footnote = "Total citations discovered (after internal and cross-source deduplication).",
      locations = gt::cells_body(columns = "Distinct_Records", rows = "Total")
    ) %>%
    gt::tab_footnote(
      footnote = "Total citations included after full text screening.",
      locations = gt::cells_body(columns = "final", rows = "Total")
    ) %>%
    gt::tab_footnote(
      footnote = "Overall Precision = Number of final included citations / Total distinct records.",
      locations = gt::cells_body(columns = "Precision", rows = "Total")
    )
}
