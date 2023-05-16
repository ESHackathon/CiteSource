#' Count Sources
#'
#' @param df A data frame with the data to be processed.
#' @param db_colname A character string specifying the column name which contains the database source.
#'
#' @return A data frame with columns 'Source' and 'count'.
#' @export
#'
#' @examples
#'
#' count_sources(df = data, db_colname = "database_source")
count_sources <- function(df, db_colname) {
  db_counts <- df %>%
    dplyr::pull(!!rlang::sym(db_colname)) %>%
    strsplit(", ") %>%
    lapply(unique) %>%  
    unlist() %>%
    trimws() %>%
    table() %>%
    as.data.frame()
  
  colnames(db_counts) <- c("Source", "count")
  
  return(db_counts)
}


#' Calcultate Source Counts
#'
#' This function processes citation data to generate a dataframe of citation counts.
#'
#' @param unique_citations A dataframe of unique citations.
#' @param citations A dataframe of all citations.
#' @param n_unique A dataframe of unique citation counts.
#'
#' @return A dataframe of calculated citation counts.
#' 
#' @examples
#' # Assuming unique_citations, citations, and n_unique are dataframes with appropriate structure
#' processed_data <- calculate_source_counts(unique_citations, citations, n_unique)
#' 
#' @export
calculate_source_counts <- function(unique_citations, citations, n_unique) {
  # Calculate Distinct Records count for each database
  distinct_count <- count_sources(unique_citations, "cite_source")
  colnames(distinct_count) <- c("Source", "Distinct Records")
  
  # Count the citations in the uploaded .ris data
  initial_citations_count <- count_sources(citations, "cite_source")
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
      `Source Unique %` = scales::percent(`Source Unique %`, accuracy = 0.1)
    )
  
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

                  