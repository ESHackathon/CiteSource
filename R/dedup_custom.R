#' Custom ASySD wrapper functions with configurable blocking and validation
#' 
#' These functions provide the ability to customize blocking rounds and validation
#' criteria for deduplication, while tracking statistics about which criteria
#' identified duplicate pairs.
#' 
#' @noRd

#' Get default blocking rounds
#' @return List of blocking rounds matching ASySD defaults
get_default_blocking_rounds <- function() {
  list(
    "Round 1 (Broad)" = list(
      c("title", "pages"),
      c("title", "author"),
      c("title", "abstract"),
      c("doi")
    ),
    "Round 2 (Bibliographic)" = list(
      c("author", "year", "pages"),
      c("journal", "volume", "pages"),
      c("isbn", "volume", "pages"),
      c("title", "isbn")
    ),
    "Round 3 (Numeric)" = list(
      c("year", "pages", "volume"),
      c("year", "number", "volume"),
      c("year", "pages", "number")
    ),
    "Round 4 (Loose)" = list(
      c("author", "year"),
      c("year", "title"),
      c("title", "volume"),
      c("title", "journal")
    )
  )
}

#' Get default validation criteria
#' @return List of validation criteria with thresholds matching ASySD defaults
get_default_validation_criteria <- function() {
  list(
    list(
      name = "Strict Bibliographic 1",
      criteria = list(
        pages = 0.8, volume = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.5, isbn = 0.99
      )
    ),
    list(
      name = "Strict Bibliographic 2",
      criteria = list(
        pages = 0.8, volume = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.5, journal = 0.6
      )
    ),
    list(
      name = "Strict Bibliographic 3",
      criteria = list(
        pages = 0.8, number = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.5, journal = 0.6
      )
    ),
    list(
      name = "Strict Bibliographic 4",
      criteria = list(
        volume = 0.8, number = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.5, journal = 0.6
      )
    ),
    list(
      name = "Abstract Heavy 1",
      criteria = list(
        volume = 0.8, number = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.8
      )
    ),
    list(
      name = "Abstract Heavy 2",
      criteria = list(
        volume = 0.8, pages = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.8
      )
    ),
    list(
      name = "Abstract Heavy 3",
      criteria = list(
        pages = 0.8, number = 0.8, title = 0.9, abstract = 0.9, 
        author = 0.8
      )
    ),
    list(
      name = "DOI Specific",
      criteria = list(
        doi = 0.95, author = 0.75, title = 0.9
      )
    ),
    list(
      name = "Complex Metadata 1",
      criteria = list(
        title = 0.8, abstract = 0.9, volume = 0.85, journal = 0.65, 
        author = 0.9
      )
    ),
    list(
      name = "Complex Metadata 2",
      criteria = list(
        title = 0.9, abstract = 0.8, volume = 0.85, journal = 0.65, 
        author = 0.9
      )
    ),
    list(
      name = "Strict Journal + Abstract 1",
      criteria = list(
        pages = 0.8, volume = 0.8, title = 0.9, abstract = 0.8, 
        author = 0.9, journal = 0.75
      )
    ),
    list(
      name = "Strict Journal + Abstract 2",
      criteria = list(
        pages = 0.8, number = 0.8, title = 0.9, abstract = 0.8, 
        author = 0.9, journal = 0.75
      )
    ),
    list(
      name = "Strict Journal + Abstract 3",
      criteria = list(
        volume = 0.8, number = 0.8, title = 0.9, abstract = 0.8, 
        author = 0.9, journal = 0.75
      )
    ),
    list(
      name = "High Confidence Metadata 1",
      criteria = list(
        title = 0.9, author = 0.9, abstract = 0.9, journal = 0.7
      )
    ),
    list(
      name = "High Confidence Metadata 2",
      criteria = list(
        title = 0.9, author = 0.9, abstract = 0.9, isbn = 0.99
      )
    ),
    list(
      name = "High Numeric Confidence 1",
      criteria = list(
        pages = 0.9, number = 0.9, title = 0.9, author = 0.8, journal = 0.6
      )
    ),
    list(
      name = "High Numeric Confidence 2",
      criteria = list(
        number = 0.9, volume = 0.9, title = 0.9, author = 0.9, isbn = 0.99
      )
    ),
    list(
      name = "High Numeric Confidence 3",
      criteria = list(
        pages = 0.9, volume = 0.9, title = 0.9, author = 0.8, journal = 0.6
      )
    ),
    list(
      name = "High Numeric Confidence 4",
      criteria = list(
        pages = 0.9, number = 0.9, title = 0.9, author = 0.8, isbn = 0.99
      )
    ),
    list(
      name = "Title & Journal/ISBN 1",
      criteria = list(
        pages = 0.8, volume = 0.8, title = 0.95, author = 0.8, journal = 0.9
      )
    ),
    list(
      name = "Title & Journal/ISBN 2",
      criteria = list(
        number = 0.8, volume = 0.8, title = 0.95, author = 0.8, journal = 0.9
      )
    ),
    list(
      name = "Title & Journal/ISBN 3",
      criteria = list(
        number = 0.8, pages = 0.8, title = 0.95, author = 0.8, journal = 0.9
      )
    ),
    list(
      name = "Title & Journal/ISBN 4",
      criteria = list(
        pages = 0.8, volume = 0.8, title = 0.95, author = 0.8, isbn = 0.99
      )
    )
  )
}

#' Map field names to column indices for blocking
#' @param field_names Character vector of field names
#' @return Numeric vector of column indices
map_fields_to_indices <- function(field_names) {
  field_map <- c(
    author = 1, title = 2, year = 3, journal = 4, abstract = 5,
    doi = 6, number = 7, pages = 8, volume = 9, isbn = 10
  )
  indices <- field_map[field_names]
  if (any(is.na(indices))) {
    stop("Unknown field names: ", paste(field_names[is.na(indices)], collapse = ", "))
  }
  as.numeric(indices)
}

#' Custom match_citations with configurable blocking rounds
#' @param formatted_citations Formatted citation data
#' @param blocking_rounds List of blocking rounds
#' @return List with pairs dataframe and blocking_round_stats
custom_match_citations <- function(formatted_citations, blocking_rounds) {
  # Import RecordLinkage functions
  if (!requireNamespace("RecordLinkage", quietly = TRUE)) {
    stop("RecordLinkage package is required")
  }
  
  # Ensure formatted_citations doesn't have logical columns that might cause issues
  # Convert any logical columns to character
  for (col in names(formatted_citations)) {
    if (is.logical(formatted_citations[[col]])) {
      formatted_citations[[col]] <- as.character(formatted_citations[[col]])
    }
  }
  
  all_pairs_list <- list()
  blocking_round_stats <- data.frame(
    round_number = integer(),
    round_name = character(),
    pair_count = integer(),
    stringsAsFactors = FALSE
  )
  
  round_num <- 1
  for (round_name in names(blocking_rounds)) {
    round_combinations <- blocking_rounds[[round_name]]
    
    # Convert combinations to blockfld format (list of index vectors)
    # Handle each combination separately, like ASySD does
    round_pairs_list <- list()
    for (combo in round_combinations) {
      block_indices <- map_fields_to_indices(combo)
      
      # Skip if indices are invalid
      if (any(is.na(block_indices)) || length(block_indices) == 0) {
        next
      }
      
      # Run compare.dedup for this specific combination
      # Ensure exclude columns actually exist in the data
      exclude_cols <- c("record_id", "source", "label")
      exclude_cols <- exclude_cols[exclude_cols %in% names(formatted_citations)]
      
      tryCatch({
        # For compare.dedup, blockfld format depends on number of fields:
        # - Single field: integer or list with integer
        # - Multiple fields: list with vector of integers (blocking on all fields together)
        if (length(block_indices) == 1) {
          # Single field blocking
          blockfld_param <- as.integer(block_indices[1])
        } else {
          # Multiple fields blocking together - must be list with vector
          blockfld_param <- list(as.integer(block_indices))
        }
        
        newpairs <- RecordLinkage::compare.dedup(
          formatted_citations,
          blockfld = blockfld_param,
          exclude = if (length(exclude_cols) > 0) exclude_cols else NULL
        )
        if (!is.null(newpairs) && !is.null(newpairs$pairs) && nrow(newpairs$pairs) > 0) {
          linkedpairs <- as.data.frame(newpairs$pairs)
          if (nrow(linkedpairs) > 0) {
            round_pairs_list[[length(round_pairs_list) + 1]] <- linkedpairs
          }
        }
      }, error = function(e) {
        # Log the error for debugging but continue
        warning("Error in compare.dedup for combination ", paste(combo, collapse = " & "), ": ", e$message)
      })
    }
    
    # Combine all pairs from this round
    if (length(round_pairs_list) > 0) {
      round_pairs <- do.call(rbind, round_pairs_list)
      round_pairs <- unique(round_pairs)
      
      if (nrow(round_pairs) > 0) {
        all_pairs_list[[round_name]] <- round_pairs
        
        # Track statistics
        blocking_round_stats <- rbind(
          blocking_round_stats,
          data.frame(
            round_number = round_num,
            round_name = round_name,
            pair_count = nrow(round_pairs),
            stringsAsFactors = FALSE
          )
        )
      } else {
        # Still track the round even if no pairs found (for completeness)
        blocking_round_stats <- rbind(
          blocking_round_stats,
          data.frame(
            round_number = round_num,
            round_name = round_name,
            pair_count = 0L,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # Track round even if no pairs found in any combination
    if (length(round_pairs_list) == 0) {
      blocking_round_stats <- rbind(
        blocking_round_stats,
        data.frame(
          round_number = round_num,
          round_name = round_name,
          pair_count = 0L,
          stringsAsFactors = FALSE
        )
      )
    }
    
    round_num <- round_num + 1
  }
  
  # Combine all pairs from all rounds
  if (length(all_pairs_list) > 0) {
    pairs <- do.call(rbind, all_pairs_list)
    pairs <- unique(pairs)
  } else {
    pairs <- NULL
  }
  
  # Debug: Print statistics summary (only if significant)
  if (nrow(blocking_round_stats) > 0 && sum(blocking_round_stats$pair_count) > 0) {
    message("Blocking round statistics: ", nrow(blocking_round_stats), " rounds, ", 
            sum(blocking_round_stats$pair_count), " total pairs identified")
  }
  
  # Add metadata columns (similar to original match_citations)
  if (!is.null(pairs) && nrow(pairs) > 0) {
    pairs <- pairs %>%
      dplyr::mutate(
        author1 = formatted_citations$author[id1],
        author2 = formatted_citations$author[id2],
        title1 = formatted_citations$title[id1],
        title2 = formatted_citations$title[id2],
        abstract1 = formatted_citations$abstract[id1],
        abstract2 = formatted_citations$abstract[id2],
        doi1 = formatted_citations$doi[id1],
        doi2 = formatted_citations$doi[id2],
        year1 = formatted_citations$year[id1],
        year2 = formatted_citations$year[id2],
        number1 = formatted_citations$number[id1],
        number2 = formatted_citations$number[id2],
        pages1 = formatted_citations$pages[id1],
        pages2 = formatted_citations$pages[id2],
        volume1 = formatted_citations$volume[id1],
        volume2 = formatted_citations$volume[id2],
        journal1 = formatted_citations$journal[id1],
        journal2 = formatted_citations$journal[id2],
        isbn1 = formatted_citations$isbn[id1],
        isbn2 = formatted_citations$isbn[id2],
        record_id1 = formatted_citations$record_id[id1],
        record_id2 = formatted_citations$record_id[id2],
        label1 = formatted_citations$label[id1],
        label2 = formatted_citations$label[id2],
        source1 = formatted_citations$source[id1],
        source2 = formatted_citations$source[id2]
      )
    
    # Calculate similarity scores using jarowinkler
    jarowinkler <- RecordLinkage::jarowinkler
    
    # Calculate similarity scores
    pairs$author <- mapply(jarowinkler, pairs$author1, pairs$author2)
    pairs$title <- mapply(jarowinkler, pairs$title1, pairs$title2)
    pairs$abstract <- mapply(jarowinkler, pairs$abstract1, pairs$abstract2)
    pairs$year <- mapply(jarowinkler, pairs$year1, pairs$year2)
    pairs$pages <- mapply(jarowinkler, pairs$pages1, pairs$pages2)
    pairs$number <- mapply(jarowinkler, pairs$number1, pairs$number2)
    pairs$volume <- mapply(jarowinkler, pairs$volume1, pairs$volume2)
    pairs$journal <- mapply(jarowinkler, pairs$journal1, pairs$journal2)
    pairs$isbn <- mapply(jarowinkler, pairs$isbn1, pairs$isbn2)
    pairs$doi <- mapply(jarowinkler, pairs$doi1, pairs$doi2)
    
    # Handle missing values
    pairs <- pairs %>%
      dplyr::mutate(
        abstract = ifelse(is.na(abstract1) & is.na(abstract2), 0, abstract),
        pages = ifelse(is.na(pages1) & is.na(pages2), 1, pages),
        volume = ifelse(is.na(volume1) & is.na(volume2), 1, volume),
        number = ifelse(is.na(number1) & is.na(number2), 1, number),
        doi = ifelse(is.na(doi1) & is.na(doi2), 0, doi),
        isbn = ifelse(is.na(isbn1) & is.na(isbn2), 0, isbn),
        year = ifelse(is.na(year1) & is.na(year2), 0, year),
        journal = ifelse(is.na(journal1) & is.na(journal2), 0, journal)
      )
  }
  
  return(list(
    pairs = pairs,
    blocking_round_stats = blocking_round_stats
  ))
}

#' Custom identify_true_matches with configurable validation criteria
#' @param pairs Citation pairs with similarity scores
#' @param validation_criteria List of validation criteria with thresholds
#' @return List with true_pairs, maybe_pairs, and validation_stats
custom_identify_true_matches <- function(pairs, validation_criteria) {
  if (is.null(pairs) || nrow(pairs) == 0) {
    return(list(
      true_pairs = NULL,
      maybe_pairs = NULL,
      validation_stats = data.frame(
        criterion_name = character(),
        pair_count = integer(),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  # Build filter conditions for each validation criterion
  # Track statistics for each criterion separately
  validation_stats <- data.frame(
    criterion_name = character(),
    pair_count = integer(),
    stringsAsFactors = FALSE
  )
  
  # Track which pairs match each criterion (for statistics)
  # Then combine all matching pairs (for true_pairs)
  all_matching_pairs_list <- list()
  
  for (criterion in validation_criteria) {
    criterion_name <- criterion$name
    thresholds <- criterion$criteria
    
    # Debug: log criterion details
    if (nrow(validation_stats) == 0) {
      message("Processing criterion '", criterion_name, "' with ", length(thresholds), " field thresholds:")
      for (field in names(thresholds)) {
        message("  ", field, " >= ", thresholds[[field]])
      }
      message("Starting with ", nrow(pairs), " pairs")
    }
    
    # Build filter condition dynamically - start with all pairs
    filtered_pairs <- pairs
    initial_count <- nrow(filtered_pairs)
    
    for (field in names(thresholds)) {
      threshold <- thresholds[[field]]
      if (field %in% names(filtered_pairs)) {
        before_filter <- nrow(filtered_pairs)
        
        # Check if this field has any non-zero, non-NA values
        # If all values are 0 or NA, the field has no data and should be skipped
        non_zero_count <- sum(!is.na(filtered_pairs[[field]]) & filtered_pairs[[field]] > 0, na.rm = TRUE)
        na_count <- sum(is.na(filtered_pairs[[field]]))
        zero_count <- sum(filtered_pairs[[field]] == 0, na.rm = TRUE)
        
        # If field has no data (all zeros or NAs), skip this field requirement
        # This allows criteria to work even when some fields are missing
        if (non_zero_count == 0 && before_filter > 0) {
          if (nrow(validation_stats) == 0) {
            message("  Warning: Field '", field, "' has no data (all ", before_filter, 
                    " pairs have 0 or NA). Skipping this field requirement.")
          }
          # Skip filtering for this field - continue to next field
          after_filter <- before_filter
        } else {
          # Field has data, apply the threshold filter
          if (nrow(validation_stats) == 0 && na_count > 0) {
            message("  Info: ", na_count, " pairs have NA values for ", field, " (will be excluded)")
          }
          if (nrow(validation_stats) == 0 && zero_count > 0 && threshold > 0) {
            message("  Info: ", zero_count, " pairs have 0 similarity for ", field, 
                    " (missing data) - these won't match threshold ", threshold)
          }
          
          # Use >= instead of > to include pairs that exactly meet the threshold
          # This matches the expected behavior: if threshold is 0.9, pairs with score >= 0.9 should match
          # Handle NA values - they should not match (filter them out)
          # IMPORTANT: For very low thresholds (like 0.01), we need to be careful about zero values
          # Zero values typically indicate missing data, so they should be excluded unless threshold is 0
          if (threshold == 0) {
            # If threshold is 0, only exclude NA values (allow zero values)
            filtered_pairs <- filtered_pairs %>%
              dplyr::filter(!is.na(.data[[field]]))
          } else {
            # If threshold > 0, exclude both NA and values below threshold
            filtered_pairs <- filtered_pairs %>%
              dplyr::filter(!is.na(.data[[field]]) & .data[[field]] >= threshold)
          }
          after_filter <- nrow(filtered_pairs)
        }
        # Debug: log filtering steps for first few criteria only (and only if significant reduction)
        if (nrow(validation_stats) < 3 && (before_filter - after_filter) > 100) {
          message("Criterion '", criterion_name, "': Filtering by ", field, " >= ", threshold, 
                  " reduced pairs from ", before_filter, " to ", after_filter)
        }
        # Debug: also log for first criterion regardless of reduction size
        if (nrow(validation_stats) == 0 && before_filter > 0) {
          message("Criterion '", criterion_name, "': Filtering by ", field, " >= ", threshold, 
                  " reduced pairs from ", before_filter, " to ", after_filter)
          # Show sample of scores BEFORE filter to understand what we're working with
          if (before_filter > 0) {
            sample_before <- head(pairs[[field]], min(10, nrow(pairs)))
            message("  Sample ", field, " scores BEFORE filter: ", paste(round(sample_before, 3), collapse=", "))
            message("  Min score: ", round(min(pairs[[field]], na.rm=TRUE), 3), 
                    ", Max score: ", round(max(pairs[[field]], na.rm=TRUE), 3),
                    ", Mean score: ", round(mean(pairs[[field]], na.rm=TRUE), 3))
          }
          # Show sample of scores after filter
          if (after_filter > 0) {
            sample_after <- head(filtered_pairs[[field]], min(5, nrow(filtered_pairs)))
            message("  Sample ", field, " scores AFTER filter: ", paste(round(sample_after, 3), collapse=", "))
          } else {
            message("  WARNING: No pairs passed the ", field, " >= ", threshold, " filter!")
          }
        }
      } else {
        # Debug: warn if field not found (only for first criterion)
        if (nrow(validation_stats) == 0) {
          message("Criterion '", criterion_name, "': Field '", field, "' not found in pairs dataframe")
          message("  Available fields: ", paste(names(filtered_pairs), collapse=", "))
        }
      }
    }
    
    # Track statistics (count pairs matching this criterion)
    # Always track, even if 0 pairs match
    pair_count <- nrow(filtered_pairs)
    validation_stats <- rbind(
      validation_stats,
      data.frame(
        criterion_name = criterion_name,
        pair_count = pair_count,
        stringsAsFactors = FALSE
      )
    )
    
    # Debug: Always log for first criterion, and log if pair_count is 0
    if (nrow(validation_stats) == 1 || pair_count == 0) {
      message("Criterion '", criterion_name, "': ", pair_count, " pairs match (from ", initial_count, " total pairs)")
      if (pair_count == 0 && initial_count > 0) {
        message("  WARNING: No pairs matched this criterion despite starting with ", initial_count, " pairs!")
        message("  This suggests the thresholds may be too strict or similarity scores are very low.")
      }
    }
    
    # Store pairs for this criterion (will combine later) if any match
    if (nrow(filtered_pairs) > 0) {
      all_matching_pairs_list[[criterion_name]] <- filtered_pairs
    }
  }
  
  # Combine all true pairs (remove duplicates - a pair might match multiple criteria)
  if (length(all_matching_pairs_list) > 0) {
    true_pairs <- do.call(rbind, all_matching_pairs_list)
    # Remove duplicates based on record_id1 and record_id2
    true_pairs <- true_pairs %>%
      dplyr::distinct(record_id1, record_id2, .keep_all = TRUE)
  } else {
    true_pairs <- NULL
  }
  
  # Debug: Print validation statistics summary (only if significant)
  if (nrow(validation_stats) > 0) {
    total_pairs <- sum(validation_stats$pair_count)
    if (total_pairs > 0) {
      message("Validation statistics: ", nrow(validation_stats), " criteria, ", total_pairs, 
              " total pairs confirmed (note: pairs may match multiple criteria)")
      # Show top criteria only if there are significant matches
      top_criteria <- validation_stats %>% 
        dplyr::arrange(dplyr::desc(pair_count)) %>% 
        dplyr::slice_head(n = 3)
      if (nrow(top_criteria) > 0 && top_criteria$pair_count[1] > 0) {
        message("Top criteria: ", top_criteria$criterion_name[1], " (", top_criteria$pair_count[1], " pairs)")
      }
    }
  }
  
  # Apply DOI and year mismatch filters (from original ASySD logic)
  if (!is.null(true_pairs) && nrow(true_pairs) > 0) {
    # Find papers with low matching dois
    true_pairs_mismatch_doi <- true_pairs %>%
      dplyr::filter(!(is.na(doi) | doi == 0 | doi > 0.99)) %>%
      dplyr::filter(!(title > 0.9 & abstract > 0.9 & (journal > 0.9 | isbn > 0.9)))
    
    # Remove papers with low matching dois
    true_pairs <- true_pairs %>%
      dplyr::filter(is.na(doi) | doi > 0.99 | doi == 0 | (title > 0.9 & abstract > 0.9 & (journal > 0.9 | isbn > 0.9)))
    
    # Handle year mismatches
    true_pairs$year1 <- as.numeric(as.character(true_pairs$year1))
    true_pairs$year2 <- as.numeric(as.character(true_pairs$year2))
    year_mismatch <- true_pairs[which(true_pairs$year1 != true_pairs$year2), ]
    year_mismatch_minor1 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2 + 1), ]
    year_mismatch_minor2 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2 - 1), ]
    year_mismatch_minor <- unique(rbind(year_mismatch_minor1, year_mismatch_minor2))
    year_mismatch_major <- year_mismatch[which(!rownames(year_mismatch) %in% rownames(year_mismatch_minor)), ]
    
    true_pairs <- true_pairs[which(!rownames(true_pairs) %in% rownames(year_mismatch_major)), ]
    true_pairs <- unique(true_pairs)
    
    # Select relevant columns
    true_pairs <- true_pairs %>%
      dplyr::select(author1, author2, title1, title2, year1, year2, journal1, journal2, 
                    doi1, doi2, record_id1, record_id2)
  }
  
  # Get maybe_pairs (for manual review) - using default ASySD criteria
  maybe_pairs <- pairs %>%
    dplyr::filter(
      (title > 0.85 & author > 0.75) |
        (title > 0.80 & abstract > 0.80) |
        (title > 0.80 & isbn > 0.99) |
        (title > 0.80 & journal > 0.80)
    ) %>%
    dplyr::filter(doi > 0.99 | doi == 0 | is.na(doi)) %>%
    dplyr::filter(!(as.numeric(year1) - as.numeric(year2) > 1)) %>%
    dplyr::filter(!(as.numeric(year2) - as.numeric(year1) > 1))
  
  maybe_pairs$record_id1 <- as.character(maybe_pairs$record_id1)
  maybe_pairs$record_id2 <- as.character(maybe_pairs$record_id2)
  
  if (!is.null(true_pairs) && nrow(true_pairs) > 0) {
    true_pairs$record_id1 <- as.character(true_pairs$record_id1)
    true_pairs$record_id2 <- as.character(true_pairs$record_id2)
    maybe_pairs <- maybe_pairs %>%
      dplyr::anti_join(true_pairs, by = c("record_id1", "record_id2"))
  }
  
  return(list(
    true_pairs = true_pairs,
    maybe_pairs = maybe_pairs,
    validation_stats = validation_stats
  ))
}

#' Custom deduplication function that uses configurable blocking and validation
#' @param raw_citations Citation dataframe
#' @param manual Whether to return manual deduplication pairs
#' @param show_unknown_tags Whether to show unknown tags
#' @param blocking_rounds Custom blocking rounds (or NULL for defaults)
#' @param validation_criteria Custom validation criteria (or NULL for defaults)
#' @return List with unique citations, manual_dedup pairs, and statistics
#' @noRd
dedup_citations_custom <- function(raw_citations, manual = TRUE, show_unknown_tags = TRUE,
                                    blocking_rounds = NULL, validation_criteria = NULL) {
  
  # Use defaults if not provided
  if (is.null(validation_criteria)) {
    message("Using default validation criteria (validation_criteria is NULL)")
  } else {
    message("Using custom validation criteria: ", length(validation_criteria), " criteria provided")
  }
  
  if (is.null(blocking_rounds)) {
    blocking_rounds <- get_default_blocking_rounds()
  }
  if (is.null(validation_criteria)) {
    validation_criteria <- get_default_validation_criteria()
  }
  
  # Use ASySD internal functions via ::: operator
  # First, prepare data using ASySD functions
  if ("duplicate_id" %in% names(raw_citations)) {
    raw_citations <- ASySD:::format_rerun(raw_citations)
  }
  
  # Handle unknown tags
  if (show_unknown_tags) {
    raw_citations <- raw_citations %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., ""))) %>%
      dplyr::mutate(label = ifelse(is.na(.data$label), "unknown", paste(.data$label))) %>%
      dplyr::mutate(source = ifelse(is.na(.data$source), "unknown", paste(.data$source)))
  } else {
    raw_citations <- raw_citations %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "")))
  }
  
  # Add missing columns using ASySD function
  raw_citations <- ASySD:::add_missing_cols(raw_citations)
  
  # Initialize result
  if (manual) {
    res <- list("manual_dedup" = data.frame())
  } else {
    res <- list()
  }
  
  # Order citations using ASySD function
  # Replicate ASySD's order_citations logic to avoid tidy eval issues with extra_merge_fields
  # This is simpler and more reliable than trying to work around ASySD's internal functions
  ordered_citations <- raw_citations %>%
    dplyr::arrange(abstract, year) %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8
  
  # Select relevant columns (matching ASySD's order_citations output)
  cols_to_select <- c("author", "title", "year", "journal", "abstract", "doi", "number", "pages", "volume", "isbn", "record_id", "label", "source")
  # Add cite_string if it exists
  if ("cite_string" %in% names(ordered_citations)) {
    cols_to_select <- c(cols_to_select, "cite_string")
  }
  ordered_citations <- ordered_citations %>%
    dplyr::select(dplyr::any_of(cols_to_select))
  
  # Format citations using ASySD function
  if (nrow(ordered_citations) == 0) {
    warning("No citations to format after ordering")
    raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
    return(list(
      unique = raw_citations, 
      manual_dedup = data.frame(), 
      stats = list(blocking_round_stats = data.frame(), validation_stats = data.frame())
    ))
  }
  
  formatted_citations <- tryCatch({
    result <- ASySD:::format_citations(ordered_citations)
    # Ensure result is a data frame
    if (!is.data.frame(result)) {
      stop("format_citations did not return a data frame")
    }
    # Convert any logical columns to character to avoid RecordLinkage issues
    for (col in names(result)) {
      if (is.logical(result[[col]])) {
        result[[col]] <- as.character(result[[col]])
      }
      # Also ensure no NA logicals
      if (is.logical(result[[col]]) && any(is.na(result[[col]]))) {
        result[[col]] <- as.character(result[[col]])
      }
    }
    result
  }, error = function(e) {
    warning("Error formatting citations: ", e$message)
    stop("Failed to format citations: ", e$message)
  })
  
  # Use custom match_citations
  match_result <- custom_match_citations(formatted_citations, blocking_rounds)
  pairs <- match_result$pairs
  blocking_round_stats <- match_result$blocking_round_stats
  
  if (is.null(pairs) || nrow(pairs) == 0) {
    warning("No duplicates detected")
    raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
    return(list(
      unique = raw_citations, 
      manual_dedup = data.frame(), 
      stats = list(blocking_round_stats = blocking_round_stats, validation_stats = data.frame())
    ))
  }
  
  # Use custom identify_true_matches
  pair_types <- custom_identify_true_matches(pairs, validation_criteria)
  true_pairs <- pair_types$true_pairs
  validation_stats <- pair_types$validation_stats
  
  if (is.null(true_pairs) || nrow(true_pairs) == 0) {
    warning("No duplicates detected!")
    raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
    return(list(
      unique = raw_citations,
      manual_dedup = data.frame(),
      stats = list(blocking_round_stats = blocking_round_stats, validation_stats = validation_stats)
    ))
  }
  
  # Generate duplicate IDs using ASySD function
  matched_pairs_with_ids <- ASySD:::generate_dup_id(true_pairs, raw_citations, keep_source = NULL, keep_label = NULL)
  
  # Merge citations using ASySD function
  # merge_metadata expects extra_merge_fields as a character vector
  res$unique <- ASySD:::merge_metadata(matched_pairs_with_ids, extra_merge_fields = c("cite_string"))
  
  # Process possible pairs for manual dedup using ASySD function
  # process_possible_pairs expects extra_merge_fields as a single character string
  if (manual) {
    res$manual_dedup <- ASySD:::process_possible_pairs(
      pair_types$maybe_pairs, 
      ordered_citations, 
      matched_pairs_with_ids, 
      extra_merge_fields = "cite_string"
    )
  }
  
  # Add statistics - ensure they're always dataframes, even if empty
  if (is.null(blocking_round_stats) || !is.data.frame(blocking_round_stats)) {
    message("Warning: blocking_round_stats is NULL or not a dataframe, creating empty structure")
    blocking_round_stats <- data.frame(
      round_number = integer(),
      round_name = character(),
      pair_count = integer(),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(validation_stats) || !is.data.frame(validation_stats)) {
    message("Warning: validation_stats is NULL or not a dataframe, creating empty structure")
    validation_stats <- data.frame(
      criterion_name = character(),
      pair_count = integer(),
      stringsAsFactors = FALSE
    )
  }
  
  # Add statistics to result
  res$stats <- list(
    blocking_round_stats = blocking_round_stats,
    validation_stats = validation_stats
  )
  
  # Make sure data is ungrouped
  res$unique <- res$unique %>% dplyr::ungroup()
  
  return(res)
}

