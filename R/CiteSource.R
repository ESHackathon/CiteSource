#' CiteSource: A package to compare sources of citation records
#'
#' The CiteSource package supports evidence aggregation by helping with the
#' processing of results of various searches in different sources. It allows to
#' deduplicate results while retaining meta-data on where those results were
#' found and then enables users to compare the contribution of different sources.
"_PACKAGE"

#' @importFrom rlang .data := as_name enquo inform is_installed quo_is_null sym
#' @importFrom dplyr across all_of anti_join any_of arrange bind_cols bind_rows
#'   case_when coalesce desc distinct everything filter first group_by if_else
#'   last_col left_join matches mutate n n_distinct na_if pull relocate rename
#'   rename_with right_join row_number rowwise select slice_head starts_with
#'   summarise transmute ungroup
#' @importFrom tidyr fill pivot_longer pivot_wider replace_na separate
#'   separate_rows unite
#' @importFrom gt cell_borders cell_text cells_body cells_column_labels
#'   cells_column_spanners cols_align cols_label fmt_number fmt_percent gt md px
#'   sub_missing tab_footnote tab_header tab_options tab_source_note
#'   tab_spanner_delim tab_style
#' @importFrom scales percent trans_format
#' @importFrom tidyselect where
NULL

utils::globalVariables(c(
  # Column names used in NSE contexts (dplyr/tidyr unquoted)
  "abstract", "abstract1", "abstract2",
  "author", "author1", "author2",
  "ComponentID",
  "doi", "doi1", "doi2",
  "duplicate_id", "duplicate_id.x", "duplicate_id.y",
  "id1", "id2",
  "isbn", "isbn1", "isbn2",
  "journal", "journal1", "journal2",
  "label", "label1", "label2",
  "max_id", "min_id",
  "number", "number1", "number2",
  "pages", "pages1", "pages2",
  "record_id", "record_id1", "record_id2", "record_ids",
  "result",
  "Source",
  "source1", "source2",
  "title", "title1", "title2",
  "volume", "volume1", "volume2",
  "year", "year1", "year2"
))

# Declare . as global variable to suppress NSE warnings
utils::globalVariables(".")

key_fields <- c("author", "title", "year", "journal", "abstract", "doi", "number", "pages",
                "volume", "isbn", "record_id", "label", "source", "issue", "url",
                "issn", "start_page", "ID")
