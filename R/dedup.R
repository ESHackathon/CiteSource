#' Deduplicate citations (ASySD Modified Function)
#'
#' This function deduplicates citation data. Note that duplicates are assumed to published
#' in the same source, so pre-prints and similar results will not be identified here.
#'
#' @export
#' @param raw_citations Citation dataframe with relevant columns
#' @param manual_dedup Logical value. Do you want to retrieve dataframe for manual deduplication?
#' @param merge_citations Logical value. Do you want to merge matching citations?
#' @param preferred_source citation source user wants to preferentially retain in dataset
#' @return A list of 2 dataframes - unique citations and citations to be manually deduplicated if that option is selected
#' @examples
#' if (interactive()) {
#'   # Load example data from the package
#'   file_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
#'   examplecitations <- readRDS(file_path)
#'
#'   # Deduplicate citations and compare sources
#'   dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
#'   unique_citations <- dedup_results$unique
#'   compare_sources(unique_citations, comp_type = "sources")
#' }

dedup_citations <- function(raw_citations, manual_dedup = FALSE, merge_citations = FALSE, preferred_source = NULL) {
  message("formatting data...")
  raw_citations <- add_cols(raw_citations, c("record_id", "cite_label", "cite_source", "cite_string"))

  # initialize/reset record_id
  raw_citations$record_id <- ""
  
  # rename or coalesce columns
  targets <- c("journal", "number", "pages", "isbn")
  sources <- c("source", "issue", "start_page", "issn")
  raw_citations <- add_cols(raw_citations, sources)
  
  for (i in seq_along(targets)) {
    if (targets[i] %in% names(raw_citations)) {
      raw_citations[[targets[i]]] <- dplyr::coalesce(raw_citations[[targets[i]]], raw_citations[[sources[i]]])
    }  else {
      raw_citations[[targets[i]]] <- raw_citations[[sources[i]]]
    }
  }
  
  raw_citations_with_id <- add_id_citations(raw_citations)

  formatted_citations <- format_citations(raw_citations_with_id)

  message("identifying potential duplicates...")
  pairs <- match_citations(formatted_citations)

  pair_types <- identify_true_matches(pairs)

  true_pairs <- pair_types$true_pairs
  message("identified duplicates!")

  maybe_pairs <- pair_types$maybe_pairs
  matched_pairs_with_ids <- generate_dup_id(true_pairs, formatted_citations)

  if (manual_dedup == TRUE) {
    manual_dedup <- get_manual_dedup_list(maybe_pairs, formatted_citations, pairs)
  } else {
    manual_dedup <- NULL
  }

  if (merge_citations == TRUE) {
    message("merging citations...")

    unique_citations_with_metadata <- merge_metadata(raw_citations_with_id, matched_pairs_with_ids)
  } else {
    unique_citations_with_metadata <- keep_one_unique_citation(raw_citations_with_id, matched_pairs_with_ids, preferred_source)
  }

  return(list(
    "unique" = unique_citations_with_metadata,
    "manual_dedup" = manual_dedup
  ))
}

#' ####------ Add columns ------ ####

#' This function adds citesource columns to citation data if missing
#' @param raw_citations Citation dataframe with relevant columns
#' @param cname column names which are required in dataframe
#' @return Dataframe of citations with id
#' @noRd
add_cols <- function(raw_citations, cname) {
  add <- cname[!cname %in% names(raw_citations)]

  if (length(add) != 0) raw_citations[add] <- NA
  raw_citations
}


#' ####------ Assign id ------ ####

#' This function adds an id to citation data if missing
#' @param raw_citations Citation dataframe with relevant columns
#' @return Dataframe of citations with id
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @noRd
add_id_citations <- function(raw_citations) {
  names(raw_citations) <- snakecase::to_any_case(names(raw_citations), case = c("snake"))

  raw_citations_with_id <- raw_citations %>%
    mutate(record_id = ifelse(is.na(.data$record_id) | .data$record_id == "", as.character(dplyr::row_number() + 1000), paste(.data$record_id)))
}

#### ------ Format citation data ------ ####

#' This function formats citation data for deduplication
#' @param raw_citations_with_id Citation dataframe with relevant columns and id column
#' @return Dataframe of formatted citations with id
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @noRd
format_citations <- function(raw_citations_with_id) {
  # arrange by Year and presence of an Abstract - we want to keep newer records and records with an abstract preferentially
  formatted_citations <- raw_citations_with_id %>%
    dplyr::arrange(dplyr::desc(.data$year), .data$abstract)

  # select relevant columns
  formatted_citations <- formatted_citations %>%
    dplyr::select(
      .data$author, .data$title, .data$year, .data$journal, .data$abstract,
      .data$doi, .data$number, .data$pages, .data$volume, .data$isbn, .data$record_id, .data$cite_label, .data$cite_source,
      .data$cite_string
    )

  # make sure author is a character
  formatted_citations$author <- as.character(formatted_citations$author)

  # Fix author formatting so similar
  formatted_citations <- formatted_citations %>%
    mutate(author = ifelse(is.na(.data$author) | .data$author == "" | .data$author == "Anonymous", "Unknown", .data$author))

  # Make all upper case
  formatted_citations <- as.data.frame(sapply(formatted_citations, toupper))

  # get rid of punctuation and differnces in doi formatting
  formatted_citations["doi"] <- sapply(formatted_citations["doi"], function(x) gsub("%28", "(", x))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("%29", ")", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTP://DX.DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTPS://DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTPS://DX.DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTP://DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI: ", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI:", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI", "", x)))

  formatted_citations["title"] <- as.data.frame(sapply(formatted_citations["title"], function(x) gsub("[[:punct:]]", "", x)))
  formatted_citations["year"] <- as.data.frame(sapply(formatted_citations["year"], function(x) gsub("[[:punct:]]", "", x)))
  formatted_citations["abstract"] <- as.data.frame(sapply(formatted_citations["abstract"], function(x) gsub("[[:punct:]]", "", x)))

  formatted_citations["isbn"] <- as.data.frame(sapply(formatted_citations["isbn"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  formatted_citations["isbn"] <- as.data.frame(sapply(formatted_citations["isbn"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  formatted_citations <- formatted_citations %>%
    dplyr::filter(!is.na(.data$record_id))

  # sort out NA / missing data formatting for optimal matching
  formatted_citations <- formatted_citations %>%
    dplyr::mutate(dplyr::across(
      c(
        .data$author, .data$year, .data$title, .data$number,
        .data$volume, .data$pages, .data$abstract, .data$doi,
        .data$journal, .data$isbn
      ),
      ~ ifelse(.x == "NA", NA, paste(.x))
    ))

  formatted_citations <- formatted_citations %>%
    dplyr::select(
      .data$author, .data$title, .data$year, .data$journal, .data$abstract,
      .data$doi, .data$number, .data$pages, .data$volume, .data$isbn,
      .data$record_id, .data$cite_source, .data$cite_label, .data$cite_string
    )

  return(formatted_citations)
}

#### ------ Identify all possible matching pairs of citations ------ ####

#' This function identifies matching pairs of citations
#' @param formatted_citations Formatted citation dataframe with relevant columns and id column
#' @return Dataframe of citation pairs
#' @importFrom dplyr mutate
#' @noRd

match_citations <- function(formatted_citations) {
  # ROUND 1: run compare.dedup function and block by title&pages OR title&author OR title&abstract OR doi
  try(newpairs <- RecordLinkage::compare.dedup(formatted_citations, blockfld = list(c(2, 8), c(1, 2), c(2, 5), 6), exclude = c("record_id", "cite_source", "cite_string", "cite_label")), silent = TRUE)

  # Create df of pairs
  try(linkedpairs <- as.data.frame(newpairs$pairs), silent = TRUE)

  # ROUND 2: run compare.dedup function and block by author&year&pages OR journal&volume&pages or isbn&volume&pages OR title&isbn
  try(newpairs2 <- RecordLinkage::compare.dedup(formatted_citations, blockfld = list(c(1, 3, 8), c(4, 9, 8), c(10, 9, 8), c(2, 10)), exclude = c("record_id", "cite_source", "cite_string", "cite_label")), silent = TRUE)

  # Create df of pairs
  try(linkedpairs2 <- as.data.frame(newpairs2$pairs), silent = TRUE)

  # ROUND 3: run compare.dedup function and block by year&pages&volume OR year&number&volume or year&pages&number
  try(newpairs3 <- RecordLinkage::compare.dedup(formatted_citations, blockfld = list(c(3, 8, 9), c(3, 7, 9), c(3, 8, 7)), exclude = c("record_id", "cite_source", "cite_string", "cite_label")), silent = TRUE)

  # Create df of pairs
  try(linkedpairs3 <- as.data.frame(newpairs3$pairs), silent = TRUE)

  # ROUND 4: run compare.dedup function and block by author&year OR year&title OR title&volume OR title&journal
  try(newpairs4 <- RecordLinkage::compare.dedup(formatted_citations, blockfld = list(c(1, 3), c(3, 2), c(2, 9), c(2, 4)), exclude = c("record_id", "cite_source", "cite_string", "cite_label")), silent = TRUE)

  # Create df of pairs
  try(linkedpairs4 <- as.data.frame(newpairs4$pairs), silent = TRUE)

  # Combine all possible pairs
  pairs <- rbind(
    get0("linkedpairs"),
    get0("linkedpairs2"),
    get0("linkedpairs3"),
    get0("linkedpairs4")
  )

  pairs <- unique(pairs)

  # Obtain metadata for matching pairs
  pairs <- pairs %>%
    dplyr::mutate(author2 = formatted_citations$author[.data$id2]) %>%
    dplyr::mutate(author1 = formatted_citations$author[.data$id1]) %>%
    dplyr::mutate(title1 = formatted_citations$title[.data$id1]) %>%
    dplyr::mutate(title2 = formatted_citations$title[.data$id2]) %>%
    dplyr::mutate(abstract1 = formatted_citations$abstract[.data$id1]) %>%
    dplyr::mutate(abstract2 = formatted_citations$abstract[.data$id2]) %>%
    dplyr::mutate(doi1 = formatted_citations$doi[.data$id1]) %>%
    dplyr::mutate(doi2 = formatted_citations$doi[.data$id2]) %>%
    dplyr::mutate(year1 = formatted_citations$year[.data$id1]) %>%
    dplyr::mutate(year2 = formatted_citations$year[.data$id2]) %>%
    dplyr::mutate(number1 = formatted_citations$number[.data$id1]) %>%
    dplyr::mutate(number2 = formatted_citations$number[.data$id2]) %>%
    dplyr::mutate(pages1 = formatted_citations$pages[.data$id1]) %>%
    dplyr::mutate(pages2 = formatted_citations$pages[.data$id2]) %>%
    dplyr::mutate(volume1 = formatted_citations$volume[.data$id1]) %>%
    dplyr::mutate(volume2 = formatted_citations$volume[.data$id2]) %>%
    dplyr::mutate(journal1 = formatted_citations$journal[.data$id1]) %>%
    dplyr::mutate(journal2 = formatted_citations$journal[.data$id2]) %>%
    dplyr::mutate(isbn1 = formatted_citations$isbn[.data$id1]) %>%
    dplyr::mutate(isbn2 = formatted_citations$isbn[.data$id2]) %>%
    dplyr::mutate(record_id1 = formatted_citations$record_id[.data$id1]) %>%
    dplyr::mutate(record_id2 = formatted_citations$record_id[.data$id2]) %>%
    dplyr::mutate(cite_labels1 = formatted_citations$cite_label[.data$id1]) %>%
    dplyr::mutate(cite_labels2 = formatted_citations$cite_label[.data$id2]) %>%
    dplyr::mutate(cite_sources1 = formatted_citations$cite_source[.data$id1]) %>%
    dplyr::mutate(cite_sources2 = formatted_citations$cite_source[.data$id2]) %>%
    dplyr::mutate(cite_strings1 = formatted_citations$cite_string[.data$id1]) %>%
    dplyr::mutate(cite_strings2 = formatted_citations$cite_string[.data$id2])

  selected_vars <- c(
    "id1", "id2", "author1", "author2", "author", "title1",
    "title2", "title", "abstract1", "abstract2", "abstract",
    "year1", "year2", "year", "number1", "number2", "number",
    "pages1", "pages2", "pages", "volume1", "volume2", "volume",
    "journal1", "journal2", "journal", "isbn", "isbn1", "isbn2",
    "doi1", "doi2", "doi", "record_id1", "record_id2", "cite_labels1",
    "cite_labels2", "cite_sources1", "cite_sources2", "cite_strings1", "cite_strings2"
  )

  pairs <- pairs %>%
    dplyr::select(dplyr::all_of(selected_vars))

  numCores <- parallel::detectCores()

  try(pairs$author <- mapply(RecordLinkage::jarowinkler, pairs$author1, pairs$author2), silent = TRUE)
  try(pairs$title <- mapply(RecordLinkage::jarowinkler, pairs$title1, pairs$title2), silent = TRUE)
  try(pairs$abstract <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$abstract1, pairs$abstract2, mc.cores = numCores), silent = TRUE)
  try(pairs$year <- mapply(RecordLinkage::jarowinkler, pairs$year1, pairs$year2), silent = TRUE)
  try(pairs$number <- mapply(RecordLinkage::jarowinkler, pairs$number1, pairs$number2), silent = TRUE)
  try(pairs$volume <- mapply(RecordLinkage::jarowinkler, pairs$volume1, pairs$volume2), silent = TRUE)
  try(pairs$journal <- mapply(RecordLinkage::jarowinkler, pairs$journal1, pairs$journal2), silent = TRUE)
  try(pairs$isbn <- mapply(RecordLinkage::jarowinkler, pairs$isbn1, pairs$isbn2), silent = TRUE)
  try(pairs$doi <- mapply(RecordLinkage::jarowinkler, pairs$doi1, pairs$doi2), silent = TRUE)

  pairs <- pairs %>%
    dplyr::mutate(abstract = ifelse(is.na(.data$abstract1) & is.na(.data$abstract2), 0, .data$abstract)) %>%
    dplyr::mutate(pages = ifelse(is.na(.data$pages1) & is.na(.data$pages2), 1, .data$pages)) %>%
    dplyr::mutate(volume = ifelse(is.na(.data$volume1) & is.na(.data$volume2), 1, .data$volume)) %>%
    dplyr::mutate(number = ifelse(is.na(.data$number1) & is.na(.data$number2), 1, .data$number)) %>%
    dplyr::mutate(doi = ifelse(is.na(.data$doi1) & is.na(.data$doi2), 0, .data$doi)) %>%
    dplyr::mutate(isbn = ifelse(is.na(.data$isbn1) & is.na(.data$isbn2), 0, .data$isbn))
}


#' This function identifies true pairs from matching pairs of citations and pairs which may be duplicates - for manual deduplication
#' @param pairs citation matches which may be duplicates
#' @return Dataframe of true citation pairs
#' @noRd

identify_true_matches <- function(pairs) {
  #### ------ Filter matching pairs - retain correct matches ------ ####
  true_pairs <- pairs %>%
    dplyr::filter(
      (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.90 & .data$abstract > 0.90 & .data$author > 0.50 & .data$isbn > 0.99) |
        (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.90 & .data$abstract > 0.90 & .data$author > 0.50 & .data$journal > 0.6) |
        (.data$pages > 0.8 & .data$number > 0.8 & .data$title > 0.90 & .data$abstract > 0.90 & .data$author > 0.50 & .data$journal > 0.6) |
        (.data$volume > 0.8 & .data$number > 0.8 & .data$title > 0.90 & .data$abstract > 0.90 & .data$author > 0.50 & .data$journal > 0.6) |

        (.data$volume > 0.8 & .data$number > 0.8 & .data$title > 0.90 & .data$abstract > 0.90 & .data$author > 0.8) |
        (.data$volume > 0.8 & .data$pages > 0.8 & .data$title > 0.90 & .data$abstract > 0.9 & .data$author > 0.8) |
        (.data$pages > 0.8 & .data$number > 0.8 & .data$title > 0.90 & .data$abstract > 0.9 & .data$author > 0.8) |

        (.data$doi > 0.95 & .data$author > 0.75 & .data$title > 0.9) |

        (.data$title > 0.80 & .data$abstract > 0.90 & .data$volume > 0.85 & .data$journal > 0.65 & .data$author > 0.9) |
        (.data$title > 0.90 & .data$abstract > 0.80 & .data$volume > 0.85 & .data$journal > 0.65 & .data$author > 0.9) |

        (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.90 & .data$abstract > 0.8 & .data$author > 0.9 & .data$journal > 0.75) |
        (.data$pages > 0.8 & .data$number > 0.8 & .data$title > 0.90 & .data$abstract > 0.80 & .data$author > 0.9 & .data$journal > 0.75) |
        (.data$volume > 0.8 & .data$number > 0.8 & .data$title > 0.90 & .data$abstract > 0.8 & .data$author > 0.9 & .data$journal > 0.75) |

        (.data$title > 0.9 & .data$author > 0.9 & .data$abstract > 0.9 & .data$journal > 0.7) |
        (.data$title > 0.9 & .data$author > 0.9 & .data$abstract > 0.9 & .data$isbn > 0.99) |

        (.data$pages > 0.9 & .data$number > 0.9 & .data$title > 0.90 & .data$author > 0.80 & .data$journal > 0.6) |
        (.data$number > 0.9 & .data$volume > 0.9 & .data$title > 0.90 & .data$author > 0.90 & .data$journal > 0.6) |
        (.data$pages > 0.9 & .data$volume > 0.9 & .data$title > 0.90 & .data$author > 0.80 & .data$journal > 0.6) |
        (.data$pages > 0.9 & .data$number > 0.9 & .data$title > 0.90 & .data$author > 0.80 & .data$isbn > 0.99) |
        (.data$pages > 0.9 & .data$number > 0.9 & .data$title > 0.90 & .data$author > 0.80 & .data$isbn > 0.99) |
        (.data$pages > 0.9 & .data$number > 0.9 & .data$title > 0.90 & .data$author > 0.80 & .data$isbn > 0.99) |

        (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.95 & .data$author > 0.80 & .data$journal > 0.9) |
        (.data$number > 0.8 & .data$volume > 0.8 & .data$title > 0.95 & .data$author > 0.80 & .data$journal > 0.9) |
        (.data$number > 0.8 & .data$pages > 0.8 & .data$title > 0.95 & .data$author > 0.80 & .data$journal > 0.9) |
        (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.95 & .data$author > 0.80 & .data$isbn > 0.99) |
        (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.95 & .data$author > 0.80 & .data$isbn > 0.99) |
        (.data$pages > 0.8 & .data$volume > 0.8 & .data$title > 0.95 & .data$author > 0.80 & .data$isbn > 0.99)
    )

  # Find papers with low matching dois - often indicates FALSE positive matches
  true_pairs_mismatch_doi <- true_pairs %>%
    dplyr::filter(!(is.na(.data$doi) | .data$doi == 0 | .data$doi > 0.99)) %>%
    dplyr::filter(!(.data$title > 0.9 & .data$abstract > 0.9 & (.data$journal | .data$isbn > 0.9)))

  # Remove papers with low matching dois from filtered matched
  true_pairs <- true_pairs %>%
    dplyr::filter(is.na(.data$doi) | .data$doi > 0.99 | .data$doi == 0 | (.data$title > 0.9 & .data$abstract > 0.9 & (.data$journal | .data$isbn > 0.9)))

  true_pairs <- unique(true_pairs)

  # Make year numeric, then find matches where year differs
  true_pairs$year1 <- as.numeric(as.character(true_pairs$year1))
  true_pairs$year2 <- as.numeric(as.character(true_pairs$year2))
  year_mismatch <- true_pairs[which(true_pairs$year1 != true_pairs$year2), ]
  year_mismatch_minor1 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2 + 1), ]
  year_mismatch_minor2 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2 - 1), ]

  year_mismatch_minor <- rbind(year_mismatch_minor1, year_mismatch_minor2)
  year_mismatch_minor <- unique(year_mismatch_minor)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  year_mismatch_major <- year_mismatch[which(!rownames(year_mismatch) %in% rownames(year_mismatch_minor)), ]

  true_pairs <- true_pairs[which(!rownames(true_pairs) %in% rownames(year_mismatch_major)), ]

  true_pairs <- unique(true_pairs)

  true_pairs$record_id1 <- as.character(true_pairs$record_id1)
  true_pairs$record_id2 <- as.character(true_pairs$record_id2)

  # Get potential duplicates for manual deduplication
  maybe_pairs <- rbind(true_pairs_mismatch_doi, year_mismatch_major)

  return(list(
    "true_pairs" = true_pairs,
    "maybe_pairs" = maybe_pairs
  ))
}

#' This function generates a duplicate ID for sets of matching citations
#' @param true_pairs citation matches which are true duplicates'
#' @param formatted_citations formatted citation data
#' @return Dataframe of formatted citation data with duplicate id
#' @importFrom dplyr filter
#' @noRd
generate_dup_id <- function(true_pairs, formatted_citations) {
  # generate duplicate IDs
  dup_ids <- true_pairs %>%
    dplyr::group_by(.data$record_id1) %>%
    dplyr::mutate(duplicate_id = dplyr::first(.data$record_id1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$record_id2) %>%
    dplyr::mutate(duplicate_id = dplyr::first(.data$duplicate_id)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$record_id1, .data$record_id2, .data$duplicate_id) %>%
    unique()

  citations_with_dup_id1 <- dplyr::inner_join(formatted_citations, dup_ids, by = c("record_id" = "record_id1")) %>%
    dplyr::select(-.data$record_id2) %>%
    unique()

  citations_with_dup_id2 <- dplyr::inner_join(formatted_citations, dup_ids, by = c("record_id" = "record_id2")) %>%
    dplyr::select(-.data$record_id1) %>%
    unique()

  citations_with_dup_id <- rbind(citations_with_dup_id1, citations_with_dup_id2) %>% unique()

  unique_citations <- formatted_citations %>%
    dplyr::filter(!.data$record_id %in% citations_with_dup_id$record_id) %>%
    mutate(duplicate_id = .data$record_id)

  citations_with_dup_id <- rbind(unique_citations, citations_with_dup_id)
  matched_pairs_with_ids <- unique(citations_with_dup_id)

  return(matched_pairs_with_ids)
}

# Remove duplicate papers ----------------------------------------------
#' This function retains one citation in a set of matching records
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @param raw_citations_with_id original citation data with ids
#' @param preferred_source citation source user wants to preferentially retain in dataset
#' @return Dataframe of citation data with duplicate citation rows removed
#' @noRd
keep_one_unique_citation <- function(raw_citations_with_id, matched_pairs_with_ids, preferred_source) {
  duplicate_id <- matched_pairs_with_ids %>%
    dplyr::select(.data$duplicate_id, .data$record_id) %>%
    unique()

  all_metadata_with_duplicate_id <- dplyr::left_join(duplicate_id, raw_citations_with_id)

  if (!is.null(preferred_source)) {
    preferred_source <- toupper(preferred_source)

    citations_with_dup_id_pick <- all_metadata_with_duplicate_id %>%
      dplyr::mutate_all(~ replace(., . == "NA", NA)) %>%
      dplyr::group_by(duplicate_id) %>%
      dplyr::arrange(.data$doi, .data$abstract) %>%
      dplyr::mutate(order = ifelse(.data$cite_source == preferred_source, 1, 2)) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-order, -preferred_source) %>%
      dplyr::slice_head()
  } else {
    citations_with_dup_id_pick <- all_metadata_with_duplicate_id %>%
      dplyr::mutate_all(~ replace(., . == "NA", NA)) %>%
      dplyr::group_by(duplicate_id) %>%
      dplyr::arrange(.data$doi, .data$abstract) %>%
      dplyr::slice_head()
  }
}

#' This function ouputs potentially matching citations for manual deduplication
#' @param maybe_pairs citation matches which are may be duplicates'
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @param pairs of potential duplicates
#' @return Dataframe of matching citations for manual evaluation
#' @noRd
get_manual_dedup_list <- function(maybe_pairs, matched_pairs_with_ids, pairs) {
  maybe_also_pairs <- pairs %>%
    dplyr::filter(.data$record_id1 %in% matched_pairs_with_ids$record_id &
      .data$record_id2 %in% matched_pairs_with_ids$record_id) %>%
    dplyr::filter(.data$doi > 0.99 |
      .data$title > 0.85 & .data$author > 0.75 |
      .data$title > 0.80 & .data$abstract > 0.80 |
      .data$title > 0.80 & .data$isbn > 0.99 |
      .data$title > 0.80 & .data$journal > 0.80)

  # Add in problem doi matching pairs and different year data in ManualDedup
  maybe_pairs <- rbind(maybe_pairs, maybe_also_pairs)
  maybe_pairs <- unique(maybe_pairs)

  maybe_pairs %>%
    dplyr::filter(.data$record_id1 %in% matched_pairs_with_ids$record_id &
      .data$record_id2 %in% matched_pairs_with_ids$record_id)
}

#' This function merges duplicates into a single citation
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @param raw_citations_with_id  original citation data with unique ids
#' @return Dataframe of formatted citation data with duplicate id
#' @noRd
merge_metadata <- function(raw_citations_with_id, matched_pairs_with_ids) {
  # get df of duplicate ids and record ids
  duplicate_id <- matched_pairs_with_ids %>%
    dplyr::select(.data$duplicate_id, .data$record_id) %>%
    unique()

  # make character
  duplicate_id$record_id <- as.character(duplicate_id$record_id)

  # join duplicate id to raw citation metadata (e.g. title, author, journal)
  all_metadata_with_duplicate_id <- dplyr::left_join(duplicate_id, raw_citations_with_id, by = "record_id")

  # extra merging is required
  # record A = record B, record B = record C, BUT if no link to indicate A=C, need to ensure that A,B,C are all part of the same group (gets complicated quickly)
  citations_with_dup_id_merged <- all_metadata_with_duplicate_id %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) %>%
    dplyr::mutate_all(~ replace(., . == "NA", NA)) %>% # replace NA
    dplyr::group_by(.data$record_id) %>%
    dplyr::arrange(.data$duplicate_id) %>%
    dplyr::add_count() # get count of duplicate ids assigned to a single record ID (happens when A = B, A = C, A = D for example, duplicate ID for A could be both D and B

  citations_with_dup_id_merged <- citations_with_dup_id_merged %>%
    mutate(duplicate_id = ifelse(.data$n > 1, dplyr::first(.data$duplicate_id), paste(.data$duplicate_id))) %>% # when more than 1 duplicate id for one record id, make duplicate ID the FIRST one.
    mutate(duplicate_id = ifelse(.data$n == 1 & .data$duplicate_id %in% citations_with_dup_id_merged$record_id,
      paste(citations_with_dup_id_merged$duplicate_id[which(citations_with_dup_id_merged$record_id == duplicate_id)]),
      paste0(.data$duplicate_id)
    )) %>% # when only 1 record id to 1 duplicate ID, check for other instances of the duplicate ID in the record ID column, then paste the duplicate ID THAT record has- linking together all the studies in one duplicatee group
    dplyr::group_by(duplicate_id) %>% # group by duplicate id
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ trimws(paste(na.omit(.), collapse = ";;;")))) %>% # merge all rows with same dup id, dont merge NA values
    dplyr::mutate(dplyr::across(c(dplyr::everything(), -.data$cite_label, -.data$cite_string, -.data$cite_source, -.data$record_id), gsub, pattern = ";;;.*", replacement = "")) %>% # remove extra values in each col, keep first one only
    dplyr::mutate(dplyr::across(.data$cite_label, gsub, pattern = ";;;", replacement = ", ")) %>%
    dplyr::mutate(dplyr::across(.data$cite_string, gsub, pattern = ";;;", replacement = ", ")) %>%
    dplyr::mutate(dplyr::across(.data$cite_source, gsub, pattern = ";;;", replacement = ", ")) %>%
    dplyr::mutate(dplyr::across(.data$record_id, gsub, pattern = ";;;", replacement = ", ")) %>% # replace separator to comma
    dplyr::ungroup() %>%
    dplyr::rename(record_ids = "record_id") %>%
    dplyr::select(-"n") %>%
    dplyr::ungroup()
}
