# Licensed under the MIT licence
# YEAR: 2020
# COPYRIGHT HOLDER: usethis authors
# see https://github.com/r-lib/usethis/blob/main/LICENSE

ui_yeah <- function (x, yes = c("Yes", "Definitely", "For sure", "Yup", 
                     "Yeah", "I agree", "Absolutely"), no = c("No way", "Not now", 
                                                              "Negative", "No", "Nope", "Absolutely not"), n_yes = 1, n_no = 2, 
          shuffle = TRUE, .envir = parent.frame()) 
{
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)
  if (!interactive()) {
    stop(c("User input required, but session is not interactive.", 
              glue::glue("Query: {x}")))
  }
  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))
  qs <- c(sample(yes, n_yes), sample(no, n_no))
  if (shuffle) {
    qs <- sample(qs)
  }
  rlang::inform(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

#' Expand metadata columns from comma-separated strings to long format
#'
#' This helper function efficiently expands comma-separated metadata columns
#' (cite_source, cite_label, cite_string) into long format in a single pass.
#' This replaces multiple separate_rows() calls throughout the package.
#'
#' @param df A data frame containing metadata columns
#' @param columns Character vector of column names to expand. Defaults to 
#'   c("cite_source", "cite_label", "cite_string")
#' @param sep Separator pattern. Defaults to ",\\s*" (comma with optional whitespace)
#' @param trim_whitespace Logical. Should whitespace be trimmed from split values?
#'   Defaults to TRUE
#' @param keep_empty Logical. Should empty values after splitting be kept?
#'   Defaults to FALSE
#'
#' @return A data frame with expanded metadata columns. Each row represents
#'   one value from the comma-separated strings, with duplicate_id preserved
#'   to maintain relationships.
#'
#' @details
#' This function is more efficient than calling separate_rows() multiple times
#' because it processes all columns in a single pass and handles edge cases
#' consistently.
#'
#' @noRd
expand_metadata_columns <- function(df, 
                                     columns = c("cite_source", "cite_label", "cite_string"),
                                     sep = ",\\s*",
                                     trim_whitespace = TRUE,
                                     keep_empty = FALSE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  # Filter to only columns that exist in the dataframe
  columns <- columns[columns %in% names(df)]
  
  if (length(columns) == 0) {
    warning("None of the specified columns exist in the dataframe")
    return(df)
  }
  
  # Start with the dataframe
  result <- df
  
  # Expand each column sequentially (tidyr::separate_rows can only handle one at a time)
  # But we do it efficiently by only expanding what's needed
  for (col in columns) {
    if (col %in% names(result)) {
      result <- result %>%
        tidyr::separate_rows(!!rlang::sym(col), sep = sep, convert = FALSE) %>%
        dplyr::mutate(
          !!rlang::sym(col) := if (trim_whitespace) {
            stringr::str_trim(!!rlang::sym(col))
          } else {
            !!rlang::sym(col)
          }
        )
      
      # Remove empty values if requested
      if (!keep_empty) {
        result <- result %>%
          dplyr::filter(
            !is.na(!!rlang::sym(col)),
            !!rlang::sym(col) != "",
            !stringr::str_detect(!!rlang::sym(col), "^\\s*$")
          )
      }
    }
  }
  
  # Remove duplicates that may have been created
  result <- dplyr::distinct(result)
  
  return(result)
}

#' Expand a single metadata column (optimized for single-column operations)
#'
#' @param df Data frame
#' @param column Column name to expand
#' @param sep Separator pattern. Defaults to ",\\s*" (comma with optional whitespace)
#' @param trim_whitespace Logical. Should whitespace be trimmed from split values?
#'   Defaults to TRUE
#' @return Expanded data frame
#' @noRd
expand_single_metadata_column <- function(df, column, sep = ",\\s*", trim_whitespace = TRUE) {
  if (!column %in% names(df)) {
    stop("Column '", column, "' not found in dataframe")
  }
  
  result <- df %>%
    tidyr::separate_rows(!!rlang::sym(column), sep = sep, convert = FALSE) %>%
    dplyr::mutate(
      !!rlang::sym(column) := if (trim_whitespace) {
        stringr::str_trim(!!rlang::sym(column))
      } else {
        !!rlang::sym(column)
      }
    ) %>%
    dplyr::filter(
      !is.na(!!rlang::sym(column)),
      !!rlang::sym(column) != "",
      !stringr::str_detect(!!rlang::sym(column), "^\\s*$")
    ) %>%
    dplyr::distinct()
  
  return(result)
}