# Vendored from the ASySD package (camaradesuk/ASySD)
# Original authors: Kaitlyn Hair, Naomi Spence, and contributors (CAMARADES group,
#   University of Edinburgh). See: https://github.com/camaradesuk/ASySD
# License: GPL (>= 3)
#
# ASySD is not available on CRAN. These functions have been incorporated directly
# into CiteSource to remove the GitHub-only dependency while preserving all
# deduplication functionality. The deduplication algorithm and all logic below
# are the work of the original ASySD authors; changes are limited to pipe style,
# deprecated-dplyr updates, and explicit package namespacing.

# ---- Internal helpers -------------------------------------------------------

#' @noRd
add_id_citations <- function(raw_citations) {
  raw_citations |>
    dplyr::mutate(record_id = as.character(dplyr::row_number() + 1000))
}

#' @noRd
order_citations <- function(raw_citations, extra_merge_fields = NULL) {
  raw_citations |>
    dplyr::arrange(abstract, year) |>
    dplyr::mutate(dplyr::across(where(is.character), utf8::utf8_encode)) |>
    dplyr::select(author, title, year, journal, abstract, doi, number, pages,
                  volume, isbn, record_id, label, source, {{ extra_merge_fields }})
}

#' @noRd
format_citations <- function(raw_citations) {
  raw_citations$author <- as.character(raw_citations$author)

  raw_citations <- raw_citations |>
    dplyr::mutate(
      author = dplyr::case_when(
        .data$author == "" | is.na(.data$author) |
          .data$author %in% c("Anonymous", "Anonymous.", "[Anonymous] A") ~ "Unknown",
        TRUE ~ .data$author
      )
    ) |>
    dplyr::mutate(dplyr::across(where(is.character), utf8::utf8_encode))

  raw_citations$pages <- gsub("--", "-", raw_citations$pages)

  # Bring source/label/record_id to the end so toupper is not applied to them
  formatted_citations <- raw_citations |>
    dplyr::select(!c(source, label, record_id), source, label, record_id)

  to_col <- ncol(formatted_citations) - 3
  formatted_citations[, 1:to_col] <- as.data.frame(
    sapply(formatted_citations[, 1:to_col], toupper)
  )

  formatted_citations[["doi"]] <- gsub("%28",               "(",  formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("%29",               ")",  formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("HTTP://DX\\.DOI\\.ORG/", "", formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("HTTPS://DOI\\.ORG/",     "", formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("HTTPS://DX\\.DOI\\.ORG/","", formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("HTTP://DOI\\.ORG/",      "", formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("DOI: ",                  "", formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("DOI:",                   "", formatted_citations[["doi"]])
  formatted_citations[["doi"]] <- gsub("DOI",                    "", formatted_citations[["doi"]])

  formatted_citations[["title"]]    <- gsub("[[:punct:]]", "", formatted_citations[["title"]])
  formatted_citations[["year"]]     <- gsub("[[:punct:]]", "", formatted_citations[["year"]])
  formatted_citations[["abstract"]] <- gsub("[[:punct:]]", "", formatted_citations[["abstract"]])
  formatted_citations[["isbn"]]     <- gsub("[[:space:]]\\(PRINT\\).*",      "", formatted_citations[["isbn"]])
  formatted_citations[["isbn"]]     <- gsub("[[:space:]]\\(ELECTRONIC\\).*", "", formatted_citations[["isbn"]])

  formatted_citations <- formatted_citations |>
    dplyr::filter(!is.na(record_id)) |>
    dplyr::mutate(
      author   = ifelse(.data$author   == "NA", NA, .data$author),
      year     = ifelse(.data$year     == "NA", NA, .data$year),
      title    = ifelse(.data$title    == "NA", NA, .data$title),
      number   = ifelse(.data$number   == "NA", NA, .data$number),
      volume   = ifelse(.data$volume   == "NA", NA, .data$volume),
      pages    = ifelse(.data$pages    == "NA", NA, .data$pages),
      abstract = ifelse(.data$abstract == "NA", NA, .data$abstract),
      doi      = ifelse(.data$doi      == "NA", NA, .data$doi),
      journal  = ifelse(.data$journal  == "NA", NA, .data$journal),
      isbn     = ifelse(.data$isbn     == "",   NA, .data$isbn)
    ) |>
    dplyr::select(author, title, year, journal, abstract, doi, number, pages,
                  volume, isbn, record_id, source, label)

  formatted_citations$record_id <- as.character(formatted_citations$record_id)
  formatted_citations
}

#' @noRd
match_citations <- function(formatted_citations) {

  # Round 1: block by title+pages, title+author, title+abstract, doi
  try(newpairs  <- RecordLinkage::compare.dedup(
    formatted_citations,
    blockfld = list(c(2, 8), c(1, 2), c(2, 5), 6),
    exclude  = c("record_id", "source", "label")
  ), silent = TRUE)
  try(linkedpairs  <- as.data.frame(newpairs$pairs),  silent = TRUE)

  # Round 2: block by author+year+pages, journal+volume+pages, isbn+volume+pages, title+isbn
  try(newpairs2 <- RecordLinkage::compare.dedup(
    formatted_citations,
    blockfld = list(c(1, 3, 8), c(4, 9, 8), c(10, 9, 8), c(2, 10)),
    exclude  = c("record_id", "source", "label")
  ), silent = TRUE)
  try(linkedpairs2 <- as.data.frame(newpairs2$pairs), silent = TRUE)

  # Round 3: block by year+pages+volume, year+number+volume, year+pages+number
  try(newpairs3 <- RecordLinkage::compare.dedup(
    formatted_citations,
    blockfld = list(c(3, 8, 9), c(3, 7, 9), c(3, 8, 7)),
    exclude  = c("record_id", "source", "label")
  ), silent = TRUE)
  try(linkedpairs3 <- as.data.frame(newpairs3$pairs), silent = TRUE)

  # Round 4: block by author+year, year+title, title+volume, title+journal
  try(newpairs4 <- RecordLinkage::compare.dedup(
    formatted_citations,
    blockfld = list(c(1, 3), c(3, 2), c(2, 9), c(2, 4)),
    exclude  = c("record_id", "source", "label")
  ), silent = TRUE)
  try(linkedpairs4 <- as.data.frame(newpairs4$pairs), silent = TRUE)

  pairs <- unique(rbind(
    get0("linkedpairs"), get0("linkedpairs2"),
    get0("linkedpairs3"), get0("linkedpairs4")
  ))

  if (is.null(pairs)) return(NULL)

  pairs <- pairs |>
    dplyr::mutate(
      author1    = formatted_citations$author[id1],
      author2    = formatted_citations$author[id2],
      title1     = formatted_citations$title[id1],
      title2     = formatted_citations$title[id2],
      abstract1  = formatted_citations$abstract[id1],
      abstract2  = formatted_citations$abstract[id2],
      doi1       = formatted_citations$doi[id1],
      doi2       = formatted_citations$doi[id2],
      year1      = formatted_citations$year[id1],
      year2      = formatted_citations$year[id2],
      number1    = formatted_citations$number[id1],
      number2    = formatted_citations$number[id2],
      pages1     = formatted_citations$pages[id1],
      pages2     = formatted_citations$pages[id2],
      volume1    = formatted_citations$volume[id1],
      volume2    = formatted_citations$volume[id2],
      journal1   = formatted_citations$journal[id1],
      journal2   = formatted_citations$journal[id2],
      isbn1      = formatted_citations$isbn[id1],
      isbn2      = formatted_citations$isbn[id2],
      record_id1 = formatted_citations$record_id[id1],
      record_id2 = formatted_citations$record_id[id2],
      label1     = formatted_citations$label[id1],
      label2     = formatted_citations$label[id2],
      source1    = formatted_citations$source[id1],
      source2    = formatted_citations$source[id2]
    ) |>
    dplyr::select(
      id1, id2, author1, author2, author, title1, title2, title,
      abstract1, abstract2, abstract, year1, year2, year,
      number1, number2, number, pages1, pages2, pages,
      volume1, volume2, volume, journal1, journal2, journal,
      isbn, isbn1, isbn2, doi1, doi2, doi,
      record_id1, record_id2, label1, label2, source1, source2
    )

  numCores <- parallelly::availableCores()

  if (.Platform$OS.type != "unix") {
    try(pairs$author   <- mapply(RecordLinkage::jarowinkler, pairs$author1,   pairs$author2),   silent = TRUE)
    try(pairs$title    <- mapply(RecordLinkage::jarowinkler, pairs$title1,    pairs$title2),    silent = TRUE)
    try(pairs$abstract <- mapply(RecordLinkage::jarowinkler, pairs$abstract1, pairs$abstract2), silent = TRUE)
    try(pairs$year     <- mapply(RecordLinkage::jarowinkler, pairs$year1,     pairs$year2),     silent = TRUE)
    try(pairs$pages    <- mapply(RecordLinkage::jarowinkler, pairs$pages1,    pairs$pages2),    silent = TRUE)
    try(pairs$number   <- mapply(RecordLinkage::jarowinkler, pairs$number1,   pairs$number2),   silent = TRUE)
    try(pairs$volume   <- mapply(RecordLinkage::jarowinkler, pairs$volume1,   pairs$volume2),   silent = TRUE)
    try(pairs$journal  <- mapply(RecordLinkage::jarowinkler, pairs$journal1,  pairs$journal2),  silent = TRUE)
    try(pairs$isbn     <- mapply(RecordLinkage::jarowinkler, pairs$isbn1,     pairs$isbn2),     silent = TRUE)
    try(pairs$doi      <- mapply(RecordLinkage::jarowinkler, pairs$doi1,      pairs$doi2),      silent = TRUE)
  } else {
    suppressWarnings({
      try(pairs$author   <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$author1,   pairs$author2,   mc.cores = numCores), silent = TRUE)
      try(pairs$title    <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$title1,    pairs$title2,    mc.cores = numCores), silent = TRUE)
      try(pairs$abstract <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$abstract1, pairs$abstract2, mc.cores = numCores), silent = TRUE)
      try(pairs$year     <- mapply(RecordLinkage::jarowinkler, pairs$year1,    pairs$year2),    silent = TRUE)
      try(pairs$pages    <- mapply(RecordLinkage::jarowinkler, pairs$pages1,   pairs$pages2),   silent = TRUE)
      try(pairs$number   <- mapply(RecordLinkage::jarowinkler, pairs$number1,  pairs$number2),  silent = TRUE)
      try(pairs$volume   <- mapply(RecordLinkage::jarowinkler, pairs$volume1,  pairs$volume2),  silent = TRUE)
      try(pairs$journal  <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$journal1,  pairs$journal2,  mc.cores = numCores), silent = TRUE)
      try(pairs$isbn     <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$isbn1,     pairs$isbn2,     mc.cores = numCores), silent = TRUE)
      try(pairs$doi      <- parallel::mcmapply(RecordLinkage::jarowinkler, pairs$doi1,      pairs$doi2,      mc.cores = numCores), silent = TRUE)
    })
  }

  pairs |>
    dplyr::mutate(
      abstract = ifelse(is.na(.data$abstract1) & is.na(.data$abstract2), 0, .data$abstract),
      pages    = ifelse(is.na(.data$pages1)    & is.na(.data$pages2),    1, .data$pages),
      volume   = ifelse(is.na(.data$volume1)   & is.na(.data$volume2),   1, .data$volume),
      number   = ifelse(is.na(.data$number1)   & is.na(.data$number2),   1, .data$number),
      doi      = ifelse(is.na(.data$doi1)      & is.na(.data$doi2),      0, .data$doi),
      isbn     = ifelse(is.na(.data$isbn1)     & is.na(.data$isbn2),     0, .data$isbn),
      year     = ifelse(is.na(.data$year1)     & is.na(.data$year2),     0, .data$year),
      journal  = ifelse(is.na(.data$journal1)  & is.na(.data$journal2),  0, .data$journal)
    )
}

#' @noRd
identify_true_matches <- function(pairs) {

  true_pairs <- pairs |>
    dplyr::filter(
      (pages > 0.8 & volume > 0.8 & title > 0.90 & abstract > 0.90 & author > 0.50 & isbn > 0.99) |
      (pages > 0.8 & volume > 0.8 & title > 0.90 & abstract > 0.90 & author > 0.50 & journal > 0.6) |
      (pages > 0.8 & number > 0.8 & title > 0.90 & abstract > 0.90 & author > 0.50 & journal > 0.6) |
      (volume > 0.8 & number > 0.8 & title > 0.90 & abstract > 0.90 & author > 0.50 & journal > 0.6) |
      (volume > 0.8 & number > 0.8 & title > 0.90 & abstract > 0.90 & author > 0.8) |
      (volume > 0.8 & pages > 0.8  & title > 0.90 & abstract > 0.9  & author > 0.8) |
      (pages > 0.8  & number > 0.8 & title > 0.90 & abstract > 0.9  & author > 0.8) |
      (doi > 0.95   & author > 0.75 & title > 0.9) |
      (title > 0.80 & abstract > 0.90 & volume > 0.85 & journal > 0.65 & author > 0.9) |
      (title > 0.90 & abstract > 0.80 & volume > 0.85 & journal > 0.65 & author > 0.9) |
      (pages > 0.8  & volume > 0.8  & title > 0.90 & abstract > 0.8  & author > 0.9 & journal > 0.75) |
      (pages > 0.8  & number > 0.8  & title > 0.90 & abstract > 0.80 & author > 0.9 & journal > 0.75) |
      (volume > 0.8 & number > 0.8  & title > 0.90 & abstract > 0.8  & author > 0.9 & journal > 0.75) |
      (title > 0.9  & author > 0.9  & abstract > 0.9 & journal > 0.7) |
      (title > 0.9  & author > 0.9  & abstract > 0.9 & isbn > 0.99) |
      (pages > 0.9  & number > 0.9  & title > 0.90 & author > 0.80 & journal > 0.6) |
      (number > 0.9 & volume > 0.9  & title > 0.90 & author > 0.90 & isbn > 0.99) |
      (pages > 0.9  & volume > 0.9  & title > 0.90 & author > 0.80 & journal > 0.6) |
      (pages > 0.9  & number > 0.9  & title > 0.90 & author > 0.80 & isbn > 0.99) |
      (pages > 0.8  & volume > 0.8  & title > 0.95 & author > 0.80 & journal > 0.9) |
      (number > 0.8 & volume > 0.8  & title > 0.95 & author > 0.80 & journal > 0.9) |
      (number > 0.8 & pages > 0.8   & title > 0.95 & author > 0.80 & journal > 0.9) |
      (pages > 0.8  & volume > 0.8  & title > 0.95 & author > 0.80 & isbn > 0.99)
    )

  # Flag pairs where DOI similarity is present but low — likely false positives
  true_pairs_mismatch_doi <- true_pairs |>
    dplyr::filter(!(is.na(doi) | doi == 0 | doi > 0.99)) |>
    dplyr::filter(!(title > 0.9 & abstract > 0.9 & (journal | isbn > 0.9)))

  true_pairs <- true_pairs |>
    dplyr::filter(is.na(doi) | doi > 0.99 | doi == 0 | (title > 0.9 & abstract > 0.9 & (journal | isbn > 0.9))) |>
    unique()

  # Remove pairs where year differs by more than 1
  true_pairs$year1 <- as.numeric(as.character(true_pairs$year1))
  true_pairs$year2 <- as.numeric(as.character(true_pairs$year2))
  year_mismatch       <- true_pairs[which(true_pairs$year1 != true_pairs$year2), ]
  year_mismatch_minor <- unique(rbind(
    year_mismatch[which(year_mismatch$year1 == year_mismatch$year2 + 1), ],
    year_mismatch[which(year_mismatch$year1 == year_mismatch$year2 - 1), ]
  ))
  year_mismatch_major <- year_mismatch[!rownames(year_mismatch) %in% rownames(year_mismatch_minor), ]
  true_pairs <- unique(true_pairs[!rownames(true_pairs) %in% rownames(year_mismatch_major), ])

  # Potential duplicates for manual review
  maybe_pairs <- pairs |>
    dplyr::filter(
      (title > 0.85 & author > 0.75) |
      (title > 0.80 & abstract > 0.80) |
      (title > 0.80 & isbn > 0.99) |
      (title > 0.80 & journal > 0.80)
    ) |>
    dplyr::filter(doi > 0.99 | doi == 0 | is.na(doi)) |>
    dplyr::filter(!(as.numeric(year1) - as.numeric(year2) > 1)) |>
    dplyr::filter(!(as.numeric(year2) - as.numeric(year1) > 1))

  maybe_pairs$record_id1 <- as.character(maybe_pairs$record_id1)
  maybe_pairs$record_id2 <- as.character(maybe_pairs$record_id2)
  true_pairs$record_id1  <- as.character(true_pairs$record_id1)
  true_pairs$record_id2  <- as.character(true_pairs$record_id2)

  maybe_pairs <- dplyr::anti_join(maybe_pairs, true_pairs, by = c("record_id1", "record_id2"))
  maybe_pairs <- unique(rbind(maybe_pairs, true_pairs_mismatch_doi, year_mismatch_major))

  true_pairs <- true_pairs |>
    dplyr::select(author1, author2, title1, title2, year1, year2,
                  journal1, journal2, doi1, doi2, record_id1, record_id2)

  list(true_pairs = true_pairs, maybe_pairs = maybe_pairs)
}

#' @noRd
generate_dup_id <- function(true_pairs, raw_citations, keep_source, keep_label,
                            post_auto_dedup = FALSE) {
  if (post_auto_dedup) {
    true_pairs_small <- dplyr::select(true_pairs, duplicate_id.x, duplicate_id.y) |> unique()
    g  <- igraph::graph_from_data_frame(true_pairs_small, directed = FALSE)
    cc <- igraph::components(g)
    true_pairs_small$ComponentID <- cc$membership[match(true_pairs_small$duplicate_id.x, names(cc$membership))]

    duplicate_id <- true_pairs_small |>
      dplyr::group_by(ComponentID) |>
      tidyr::unite(record_ids, duplicate_id.x, duplicate_id.y, sep = ", ") |>
      dplyr::summarise(record_id = paste(record_ids, collapse = ", "), .groups = "drop") |>
      tidyr::separate_rows(record_id, sep = ", ") |>
      dplyr::distinct()

    duplicate_id$record_id  <- as.character(duplicate_id$record_id)
    raw_citations$record_id <- as.character(raw_citations$record_id)

    duplicate_id <- duplicate_id |>
      dplyr::right_join(raw_citations, by = "record_id") |>
      dplyr::mutate(ComponentID = ifelse(
        is.na(ComponentID),
        paste0(max(duplicate_id$ComponentID) + dplyr::row_number()),
        ComponentID
      ))
    duplicate_id <- unique(duplicate_id)

  } else {
    true_pairs_small <- dplyr::select(true_pairs, record_id1, record_id2) |> unique()
    g  <- igraph::graph_from_data_frame(true_pairs_small, directed = FALSE)
    cc <- igraph::components(g)
    true_pairs_small$ComponentID <- cc$membership[match(true_pairs_small$record_id1, names(cc$membership))]

    raw_citations <- raw_citations |>
      dplyr::mutate(record_id = as.character(.data$record_id))

    duplicate_id <- true_pairs_small |>
      dplyr::group_by(ComponentID) |>
      tidyr::unite(record_ids, record_id1, record_id2, sep = ", ") |>
      dplyr::summarise(record_id = paste(record_ids, collapse = ", "), .groups = "drop") |>
      tidyr::separate_rows(record_id, sep = ", ") |>
      dplyr::distinct()

    duplicate_id$record_id  <- as.character(duplicate_id$record_id)
    raw_citations$record_id <- as.character(raw_citations$record_id)

    duplicate_id <- duplicate_id |>
      dplyr::right_join(raw_citations, by = "record_id") |>
      dplyr::mutate(ComponentID = ifelse(
        is.na(ComponentID),
        paste0(max(duplicate_id$ComponentID) + dplyr::row_number()),
        ComponentID
      ))
  }

  if (!is.null(keep_label)) {
    order <- unique(raw_citations$label)
    order <- c(order[order == keep_label], order[order != keep_label])

    citations_duplicate_id <- duplicate_id |>
      dplyr::group_by(.data$ComponentID) |>
      dplyr::arrange(factor(.data$label, levels = order)) |>
      dplyr::mutate(duplicate_id = dplyr::first(.data$record_id)) |>
      dplyr::ungroup() |>
      dplyr::select(-ComponentID)

  } else if (!is.null(keep_source)) {
    order <- unique(raw_citations$source)
    order <- c(order[order == keep_source], order[order != keep_source])

    citations_duplicate_id <- duplicate_id |>
      dplyr::group_by(.data$ComponentID) |>
      dplyr::arrange(factor(.data$source, levels = order)) |>
      dplyr::mutate(duplicate_id = dplyr::first(.data$record_id)) |>
      dplyr::ungroup() |>
      dplyr::select(-ComponentID)

  } else {
    citations_duplicate_id <- duplicate_id |>
      dplyr::group_by(ComponentID) |>
      dplyr::arrange(record_id) |>
      dplyr::mutate(duplicate_id = dplyr::first(.data$record_id)) |>
      dplyr::ungroup() |>
      dplyr::select(-ComponentID)
  }

  citations_duplicate_id
}

#' @noRd
process_possible_pairs <- function(maybe_pairs, ordered_citations,
                                   matched_pairs_with_ids, extra_merge_fields) {
  maybe_pairs <- maybe_pairs |>
    dplyr::mutate(
      author1    = ordered_citations$author[id1],
      author2    = ordered_citations$author[id2],
      title1     = ordered_citations$title[id1],
      title2     = ordered_citations$title[id2],
      abstract1  = ordered_citations$abstract[id1],
      abstract2  = ordered_citations$abstract[id2],
      doi1       = ordered_citations$doi[id1],
      doi2       = ordered_citations$doi[id2],
      year1      = ordered_citations$year[id1],
      year2      = ordered_citations$year[id2],
      number1    = ordered_citations$number[id1],
      number2    = ordered_citations$number[id2],
      pages1     = ordered_citations$pages[id1],
      pages2     = ordered_citations$pages[id2],
      volume1    = ordered_citations$volume[id1],
      volume2    = ordered_citations$volume[id2],
      journal1   = ordered_citations$journal[id1],
      journal2   = ordered_citations$journal[id2],
      isbn1      = ordered_citations$isbn[id1],
      isbn2      = ordered_citations$isbn[id2],
      record_id1 = ordered_citations$record_id[id1],
      record_id2 = ordered_citations$record_id[id2],
      label1     = ordered_citations$label[id1],
      label2     = ordered_citations$label[id2],
      source1    = ordered_citations$source[id1],
      source2    = ordered_citations$source[id2]
    )

  if (!is.null(extra_merge_fields)) {
    maybe_pairs <- maybe_pairs |>
      dplyr::mutate(
        !!paste0(extra_merge_fields, 1) := ordered_citations[[extra_merge_fields]][id1],
        !!paste0(extra_merge_fields, 2) := ordered_citations[[extra_merge_fields]][id2]
      ) |>
      dplyr::select(
        author1, author2, author, title1, title2, title,
        abstract1, abstract2, abstract, year1, year2, year,
        number1, number2, number, pages1, pages2, pages,
        volume1, volume2, volume, journal1, journal2, journal,
        isbn, isbn1, isbn2, doi1, doi2, doi,
        record_id1, record_id2, label1, label2, source1, source2,
        dplyr::starts_with(paste0(extra_merge_fields))
      )
  } else {
    maybe_pairs <- maybe_pairs |>
      dplyr::select(
        author1, author2, author, title1, title2, title,
        abstract1, abstract2, abstract, year1, year2, year,
        number1, number2, number, pages1, pages2, pages,
        volume1, volume2, volume, journal1, journal2, journal,
        isbn, isbn1, isbn2, doi1, doi2, doi,
        record_id1, record_id2, label1, label2, source1, source2
      )
  }

  ids <- matched_pairs_with_ids |>
    dplyr::select(duplicate_id, record_id)

  maybe_pairs <- dplyr::left_join(maybe_pairs, ids, by = c("record_id1" = "record_id"))
  maybe_pairs <- dplyr::left_join(maybe_pairs, ids, by = c("record_id2" = "record_id"))

  maybe_pairs <- maybe_pairs |>
    dplyr::mutate(match = ifelse(duplicate_id.x == duplicate_id.y, TRUE, FALSE)) |>
    dplyr::filter(match == FALSE) |>
    dplyr::group_by(.data$duplicate_id.x, .data$duplicate_id.y) |>
    dplyr::slice_head() |>
    dplyr::ungroup()

  if (length(maybe_pairs$record_id1) == 0) return(maybe_pairs)

  unique_pairs <- maybe_pairs |>
    dplyr::select(duplicate_id.x, duplicate_id.y) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      min_id = min(duplicate_id.x, duplicate_id.y),
      max_id = max(duplicate_id.x, duplicate_id.y)
    ) |>
    dplyr::group_by(min_id, max_id) |>
    dplyr::slice_head() |>
    dplyr::mutate(unique = TRUE)

  maybe_pairs |>
    dplyr::left_join(unique_pairs, by = c("duplicate_id.x", "duplicate_id.y")) |>
    dplyr::filter(!is.na(unique)) |>
    dplyr::select(-unique)
}

#' @noRd
add_missing_cols <- function(raw_citations) {
  if (!"record_id" %in% names(raw_citations)) {
    warning("Search does not contain a record_id column. A record_id will be created using row numbers")
    raw_citations <- add_id_citations(raw_citations)
  } else if (any(is.na(raw_citations$record_id)) | any(raw_citations$record_id == "")) {
    warning("Search contains missing values for the record_id column. A record_id will be created using row numbers")
    raw_citations <- add_id_citations(raw_citations)
  } else if (length(unique(raw_citations$record_id)) != nrow(raw_citations)) {
    warning("The record_id column is not unique. A record_id will be created using row numbers")
    raw_citations <- add_id_citations(raw_citations)
  }

  cols         <- c("author", "year", "journal", "doi", "title", "pages", "volume",
                    "number", "abstract", "record_id", "isbn", "label", "source")
  essential    <- c("author", "year", "journal", "doi", "title", "pages", "volume",
                    "number", "abstract", "record_id", "isbn")
  all_missing  <- cols[!cols %in% colnames(raw_citations)]
  missing_ess  <- essential[!essential %in% colnames(raw_citations)]

  if (length(missing_ess) > 0) {
    warning(paste0("The following columns are missing: ", paste(missing_ess, collapse = ", ")))
    message("Setting missing cols to NA")
  }
  raw_citations[all_missing] <- NA
  raw_citations$record_id    <- as.character(raw_citations$record_id)
  raw_citations
}

#' @noRd
remove_string_dups <- function(string) {
  elements <- unlist(strsplit(string, ", "))
  paste(unique(elements), collapse = ", ")
}

#' @noRd
format_rerun <- function(raw_citations) {
  if ("duplicate_id" %in% names(raw_citations)) {
    message("Re-running deduplication on dataset...")
    raw_citations |> dplyr::rename(record_id = duplicate_id)
  } else {
    raw_citations
  }
}

#' @noRd
keep_one_unique_citation <- function(true_pairs_with_ids) {
  true_pairs_with_ids |>
    dplyr::group_by(duplicate_id) |>
    dplyr::slice_head()
}

#' @noRd
merge_metadata <- function(matched_pairs_with_ids, extra_merge_fields) {
  if (!"record_ids" %in% names(matched_pairs_with_ids)) {
    matched_pairs_with_ids$record_ids <- matched_pairs_with_ids$record_id
  }

  merge_fields <- c("record_ids", "label", "source", extra_merge_fields)

  paste_unless_blank_or_na <- function(x) {
    if (all(is.na(x)))  return(NA)
    if (all(x == ""))   return("")
    paste(stats::na.omit(x), collapse = ";;;")
  }

  result <- matched_pairs_with_ids |>
    dplyr::select(-record_id) |>
    dplyr::mutate(dplyr::across(where(is.character), utf8::utf8_encode)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., . == "NA", NA))) |>
    dplyr::group_by(.data$duplicate_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ trimws(paste_unless_blank_or_na(.x))),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(
      c(dplyr::everything(), -{{ merge_fields }}),
      ~ gsub(.x, pattern = ";;;.*", replacement = "")
    )) |>
    dplyr::mutate(dplyr::across(
      {{ merge_fields }},
      ~ gsub(.x, pattern = ";;;", replacement = ", ")
    ))

  result$record_ids <- sapply(result$record_ids, remove_string_dups)
  result
}

# ---- Main deduplication functions (from ASySD) ------------------------------

#' Deduplicate citations (internal ASySD implementation)
#'
#' @noRd
asys_dedup_citations <- function(raw_citations, manual_dedup = TRUE,
                                 merge_citations = TRUE, keep_source = NULL,
                                 keep_label = NULL, extra_merge_fields = NULL,
                                 show_unknown_tags = TRUE, user_input = NA) {

  shiny_progress <- if ("shiny" %in% .packages(all.available = TRUE)) {
    shiny::isRunning()
  } else {
    FALSE
  }

  raw_citations <- format_rerun(raw_citations)

  if (shiny_progress) {

    res <- shiny::withProgress(message = "formatting data...", value = 0, {

      if (show_unknown_tags) {
        raw_citations <- raw_citations |>
          dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., ""))) |>
          dplyr::mutate(label  = ifelse(is.na(.data$label),  "unknown", paste(.data$label))) |>
          dplyr::mutate(source = ifelse(is.na(.data$source), "unknown", paste(.data$source))) |>
          dplyr::mutate(dplyr::across({{ extra_merge_fields }}, ~ replace(., is.na(.), "unknown")))
      } else {
        raw_citations <- raw_citations |>
          dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "")))
      }

      raw_citations <- add_missing_cols(raw_citations)
      res <- if (manual_dedup) list(manual_dedup = data.frame()) else list()

      ordered_citations   <- order_citations(raw_citations, extra_merge_fields)
      formatted_citations <- format_citations(ordered_citations)
      shiny::incProgress(0.2, message = "identifying potential duplicates...")

      pairs <- match_citations(formatted_citations)
      shiny::incProgress(0.4)

      if (is.null(pairs) || length(pairs$record_id1) == 0) {
        warning("No duplicates detected")
        raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
        res$unique <- raw_citations
        return(res)
      }

      pair_types <- identify_true_matches(pairs)
      true_pairs <- pair_types$true_pairs
      shiny::incProgress(0.5)

      if (is.null(true_pairs) || length(true_pairs$record_id1) == 0) {
        warning("No duplicates detected!")
        raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
        res$unique <- raw_citations
        return(res)
      }

      shiny::incProgress(0.6, message = "merging duplicate citations...")
      matched_pairs_with_ids <- generate_dup_id(true_pairs, raw_citations, keep_source, keep_label)

      if (merge_citations) {
        res$unique <- raw_citations <- merge_metadata(matched_pairs_with_ids, extra_merge_fields)
      } else {
        res$unique <- raw_citations <- keep_one_unique_citation(matched_pairs_with_ids)
      }

      if (manual_dedup) {
        shiny::incProgress(0.8, message = "flagging potential pairs for manual dedup...")
        res$manual_dedup <- process_possible_pairs(
          pair_types$maybe_pairs, ordered_citations, matched_pairs_with_ids, extra_merge_fields
        )
      }

      shiny::incProgress(1)
      res
    })

    res$unique <- dplyr::ungroup(res$unique)
    return(res)

  } else {

    cols          <- c("author", "year", "journal", "doi", "title", "pages", "volume",
                       "number", "abstract", "record_id", "isbn", "label", "source")
    essential     <- c("author", "year", "journal", "doi", "title", "pages", "volume",
                       "number", "abstract", "record_id", "isbn")
    all_missing   <- cols[!cols %in% colnames(raw_citations)]
    missing_ess   <- essential[!essential %in% colnames(raw_citations)]

    if (length(missing_ess) > 0) {
      message(paste("Warning: The following columns are missing:", paste(missing_ess, collapse = ", ")))
      if (is.na(user_input)) {
        user_input <- utils::menu(c("Yes", "No"), title = "Are you sure you want to proceed?")
      }
      if (user_input == "1") {
        message("formatting data...")
      } else {
        return("Halting dedup...")
      }
    } else {
      message("formatting data...")
    }

    raw_citations[all_missing] <- NA

    if (show_unknown_tags) {
      raw_citations <- raw_citations |>
        dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., ""))) |>
        dplyr::mutate(label  = ifelse(is.na(.data$label),  "unknown", paste(.data$label))) |>
        dplyr::mutate(source = ifelse(is.na(.data$source), "unknown", paste(.data$source))) |>
        dplyr::mutate(dplyr::across({{ extra_merge_fields }}, ~ replace(., is.na(.), "unknown")))
    } else {
      raw_citations <- raw_citations |>
        dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "")))
    }

    raw_citations <- add_missing_cols(raw_citations)
    ordered_citations   <- order_citations(raw_citations)
    formatted_citations <- format_citations(ordered_citations)

    message("identifying potential duplicates...")
    pairs <- match_citations(formatted_citations)

    res <- if (manual_dedup) list(manual_dedup = data.frame()) else list()

    if (is.null(pairs) || length(pairs$record_id1) == 0) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
      res$unique <- raw_citations
      return(res)
    }

    pair_types <- identify_true_matches(pairs)
    true_pairs <- pair_types$true_pairs

    if (is.null(true_pairs) || length(true_pairs$record_id1) == 0) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
      res$unique <- raw_citations
      return(res)
    }

    message("identified duplicates!")

    suppressMessages({
      matched_pairs_with_ids <- generate_dup_id(true_pairs, raw_citations, keep_source, keep_label)
    })

    suppressMessages({
      if (merge_citations) {
        res$unique <- merge_metadata(matched_pairs_with_ids, extra_merge_fields)
      } else {
        res$unique <- keep_one_unique_citation(matched_pairs_with_ids)
      }
    })

    if (manual_dedup) {
      message("flagging potential pairs for manual dedup...")
      res$manual_dedup <- process_possible_pairs(
        pair_types$maybe_pairs, ordered_citations, matched_pairs_with_ids, extra_merge_fields
      )
    }

    res$unique <- dplyr::ungroup(res$unique)

    n_unique <- length(unique(res$unique$duplicate_id))
    n_start  <- length(unique(formatted_citations$record_id))
    n_dups   <- n_start - n_unique

    message(paste(n_start,  "citations loaded..."))
    message(paste(n_dups,   "duplicate citations removed..."))
    message(paste(n_unique, "unique citations remaining!"))

    res
  }
}

#' Add manual duplicate pairs (internal ASySD implementation)
#'
#' @noRd
asys_dedup_citations_add_manual <- function(unique_citations, merge_citations = TRUE,
                                            keep_source = NULL, keep_label = NULL,
                                            additional_pairs, extra_merge_fields = NULL,
                                            show_unknown_tags = TRUE) {
  if ("result" %in% names(additional_pairs)) {
    additional_pairs <- dplyr::filter(additional_pairs, result == "match")
    if (nrow(additional_pairs) == 0) {
      message("Beware: if additional_pairs contains a `result` column, only those with a value of `match` will be merged. Currently, this means that there are no pairs to be merged.")
      return(unique_citations)
    }
  }

  unique_citations <- unique_citations |>
    dplyr::rename(record_id = duplicate_id)

  # Only reprocess records actually involved in the new manual pairs.
  # Unaffected records are already in their final merged state and can be
  # passed through unchanged, avoiding an O(N) merge over the full dataset.
  involved_ids <- unique(c(
    as.character(additional_pairs$duplicate_id.x),
    as.character(additional_pairs$duplicate_id.y)
  ))

  affected   <- unique_citations |> dplyr::filter(record_id %in% involved_ids)
  unaffected <- unique_citations |>
    dplyr::filter(!record_id %in% involved_ids) |>
    dplyr::rename(duplicate_id = record_id)

  res             <- generate_dup_id(additional_pairs, affected, keep_source, keep_label,
                                     post_auto_dedup = TRUE)
  merged_affected <- merge_metadata(res, extra_merge_fields)

  dplyr::bind_rows(merged_affected, unaffected)
}
