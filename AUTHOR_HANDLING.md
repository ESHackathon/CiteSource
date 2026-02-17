# Author Name Handling in CiteSource: Complete Documentation

## Table of Contents
1. [Overview](#overview)
2. [Author Data Format Expectations](#author-data-format-expectations)
3. [Import Functions](#import-functions)
4. [Data Cleaning Functions](#data-cleaning-functions)
5. [Deduplication Functions](#deduplication-functions)
6. [Citation Generation Functions](#citation-generation-functions)
7. [Export Functions](#export-functions)
8. [Data Conversion Functions](#data-conversion-functions)

---

## Overview

The CiteSource package processes author names through multiple stages:
1. **Import**: Reading from RIS, BibTeX, CSV, or TSV files
2. **Cleaning**: Standardizing delimiters and formats
3. **Deduplication**: Formatting for matching and comparison
4. **Citation Generation**: Parsing for APA-style citations and references
5. **Export**: Converting back to bibliographic formats

**Expected Author Format**: `"Last, First and Last, First"` (comma-separated last name and first name within each author, " and " separating multiple authors)

---

## Author Data Format Expectations

### Standard Format
- Individual authors: `"Last, First"` or `"Last, First Middle"`
- Multiple authors: `"Last1, First1 and Last2, First2"`
- Delimiter between authors: `" and "` (lowercase with spaces)
- Delimiter within author: `,` (comma separating last name from first name)

### Variations Handled
- `" AND "` → converted to `" and "`
- `" & "` → converted to `" and "`
- Commas between author names → converted to `" and "` when followed by 2+ letters

---

## Import Functions

### 1. `read_citations()` - Main Import Function

**Location**: `R/import.R`  
**Purpose**: Main entry point for importing citation files. Calls lower-level parsing functions.

**Full Function Code**:

```r
read_citations <- function(files = NULL,
                           cite_sources = NULL,
                           cite_strings = NULL,
                           cite_labels = NULL,
                           metadata = NULL,
                           verbose = TRUE,
                           tag_naming = "best_guess",
                           only_key_fields = TRUE) {
  
  if (is.character(only_key_fields)) {
    only_key_fields <- union(key_fields, only_key_fields)
  } else if (only_key_fields == TRUE) {
    only_key_fields <- key_fields
  } else if (!only_key_fields == FALSE) {
    stop("only_key_fields must be TRUE, FALSE or a character vector")
  } else {
    only_key_fields <- NULL
  }

  if (is.null(files) && is.null(metadata)) stop("Either files or metadata must be specified.")
  if (!is.null(files) && !is.null(metadata)) stop("files and metadata cannot both be specified.")
  
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata)) stop("metadata must be a tibble/dataframe.")
    if (!("files" %in% colnames(metadata))) stop("metadata must contain at least a `files` column")
    files <- metadata[["files"]]
    cite_sources <- metadata[["cite_sources"]]
    cite_strings <- metadata[["cite_strings"]]
    cite_labels <- metadata[["cite_labels"]]
  }
  
  
  if (is.null(cite_sources)) {
    cite_sources <- purrr::map_chr(files, ~ tools::file_path_sans_ext(basename(.x)))

    if (any(duplicated(cite_sources))) {
      cite_sources <- make.unique(cite_sources, sep = "_")
      message("Some file names were duplicated. Therefore, their cite_source values are distinguished by suffixes (_1 etc). For greater clarity, specify cite_sources explicitly or rename files.")
    }
  }

  if (length(files) != length(cite_sources)) {
    stop("Files and origins cite_sources be of equal length")
  }
  if (!is.null(cite_strings)) {
    if (length(cite_sources) != length(cite_strings)) {
      stop("Cite_sources and cite_strings must be of equal length")
    }
  }
  if (!is.null(cite_labels)) {
    if (length(cite_sources) != length(cite_labels)) {
      stop("Cite_sources and cite_labels must be of equal length")
    }
  }

  contains_commas <- any(stringr::str_detect(c(cite_sources, cite_labels, cite_strings), ","))

  if (!is.na(contains_commas) && contains_commas) {
    stop("',' must not be used in cite_source, cite_labels or cite_strings (or filenames if these are not specified)")
  }

  # Need to import files separately to add origin, platform, and searches
  ref_list <- purrr::map(files,
                         \(x) synthesisr_read_refs(x,  tag_naming = tag_naming, select_fields = only_key_fields),
                         .progress = list(  total = 100, 
                                            format = "Importing files {cli::pb_bar} {cli::pb_percent}")
  )

  # Drop empty citations
  ref_list <- lapply(
    ref_list,
    function(data) data[rowSums(is.na(data)) != (ncol(data) - 1), ]
  )

  ref_counts <- numeric(length(files))

  for (i in seq_along(files)) {
    ref_counts[i] <- nrow(ref_list[[i]])
  }

  for (index in seq_len(length(files))) {
    ref_list[[index]]$cite_source <- cite_sources[[index]]
    if (!is.null(cite_strings)) {
      ref_list[[index]]$cite_string <- cite_strings[[index]]
    }
    if (!is.null(cite_labels)) {
      ref_list[[index]]$cite_label <- cite_labels[[index]]
    }
  }

  if (verbose) {
    report <- data.frame(
      file = basename(files),
      cite_source = cite_sources,
      cite_string = if (is.null(cite_strings)) NA_character_ else cite_strings,
      cite_label = if (is.null(cite_labels)) NA_character_ else cite_labels,
      citations = ref_counts
    )

    message("Import completed - with the following details:")
    message(paste0(utils::capture.output(report), collapse = "\n"))
  }

  ref_list %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows)
  
}
```

**Author Handling**: This function calls `synthesisr_read_refs()` which handles the actual parsing. Author data passes through unchanged at this level.

---

### 2. `synthesisr_read_refs()` - File Reading Wrapper

**Location**: `R/import_export_helpers.R`  
**Purpose**: Wrapper function that reads bibliographic files and calls format-specific parsers.

**Full Function Code**:

```r
synthesisr_read_refs <- function(
    filename,
    tag_naming = "best_guess",
    return_df = TRUE,
    verbose = FALSE,
    select_fields = NULL) {
  if (missing(filename)) {
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if (any(!file_check)) {
    stop("file not found")
  }

  if (length(filename) > 1) {
    result_list <- lapply(filename, function(a) {
      read_ref(
        filename = a,
        tag_naming = tag_naming,
        return_df = return_df,
        select_fields = select_fields,
        verbose = verbose
      )
    })
    names(result_list) <- filename

    # drop any unrecognized file types
    null_check <- unlist(lapply(result_list, is.null))
    if (any(null_check)) {
      result_list <- result_list[-which(null_check)]
    }

    if (return_df) {
      result <- merge_columns(result_list)
      result$filename <- unlist(
        lapply(seq_len(length(result_list)),
          function(a, data) {
            rep(names(data)[a], nrow(data[[a]]))
          },
          data = result_list
        )
      )
      return(result)
    } else {
      result <- do.call(c, result_list)
      return(result)
    }
  } else { # i.e. if only one filename given
    return(
      read_ref(
        filename,
        tag_naming = tag_naming,
        return_df = return_df,
        select_fields = select_fields,
        verbose = verbose
      )
    )
  }
}
```

**Author Handling**: Passes data to `read_ref()` which calls format-specific parsers.

---

### 3. `parse_ris()` - RIS File Parser

**Location**: `R/import_export_helpers.R`  
**Purpose**: Parses RIS format files and consolidates author fields.

**Full Function Code** (author-relevant sections highlighted):

```r
parse_ris <- function(x, tag_naming = "best_guess") {
  x <- prep_ris(x, detect_delimiter(x), type = "generic")

  # create the appropriate lookup file for the specified tag
  if (inherits(tag_naming, "data.frame")) {
    if (!any(colnames(tag_naming) == "order")) {
      tag_naming$order <- seq_len(nrow(tag_naming))
    }
    code_lookup_thisfile <- tag_naming
  } else {
    if (tag_naming == "none") {
      ris_vals <- unique(x$ris)
      code_lookup_thisfile <- data.frame(
        code = ris_vals,
        field = ris_vals,
        order = seq_along(ris_vals),
        stringsAsFactors = FALSE
      )
    } else if (tag_naming == "best_guess") {
      code_lookup_thisfile <- detect_lookup(tags = unique(x$ris))
    } else if (any(c("wos", "scopus", "ovid", "asp", "synthesisr") == tag_naming)) {
      rows <- which(synthesisr_code_lookup[, paste0("ris_", tag_naming)])
      code_lookup_thisfile <- synthesisr_code_lookup[
        rows,
        c("code", "order", "field")
      ]
    }
  }

  # merge data with lookup info, to provide bib-style tags
  x_merge <- merge(x,
    code_lookup_thisfile,
    by.x = "ris",
    by.y = "code",
    all.x = TRUE,
    all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]

  # find a way to store missing .bib data rather than discard
  if (any(is.na(x_merge$field))) {
    rows_tr <- which(is.na(x_merge$field))
    x_merge$field[rows_tr] <- x_merge$ris[rows_tr]

    # ensure all headings have an order
    if (all(is.na(x_merge$order))) {
      start_val <- 0
    } else {
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(as.factor(x_merge$ris[rows_tr])) + start_val
  }

  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x_merge$text)
  if (any(year_check > 0)) {
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x_merge$text[check_rows])

    # for entries with a bib entry labelled year, check that there arent multiple years
    if (any(x_merge$field[check_rows] == "year", na.rm = TRUE)) {
      # check for repeated year information
      year_freq <- xtabs(~ref, data = x_merge[which(x_merge$field == "year"), ])
      if (any(year_freq > 1)) {
        year_df <- x_merge[which(x_merge$field == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean)))^2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x_merge$ris != names(which.min(year_4)[1]) &
            x_merge$field == "year"
        )
        x_merge$field[incorrect_rows] <- "year_additional"
      }
    } else {
      possible_rows <- which(
        year_strings > 0 &
          year_strings <= as.numeric(format(Sys.Date(), "%Y")) + 1
      )
      tag_frequencies <- as.data.frame(
        xtabs(~ x_merge$ris[check_rows[possible_rows]]),
        stringsAsFactors = FALSE
      )
      colnames(tag_frequencies) <- c("tag", "n")
      # now work out what proportion of each tag contain year data
      # compare against number of references to determine likelihood of being 'the' year tag
      tag_frequencies$prop <- tag_frequencies$n / (max(x_merge$ref) + 1) # number of references
      if (any(tag_frequencies$prop > 0.9)) {
        year_tag <- tag_frequencies$tag[which.max(tag_frequencies$prop)]
        rows.tr <- which(x_merge$ris == year_tag)
        x_merge$field[rows.tr] <- "year"
        x_merge$row_order[rows.tr] <- 3
      }
    }
  }

  # ensure author data from a single ris tag
  if (any(x_merge$field == "author")) {
    lookup.tags <- xtabs(~ x_merge$ris[which(x_merge$field == "author")])
    if (length(lookup.tags) > 1) {
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x_merge$ris %in% replace_tags)
      x_merge$field[replace_rows] <- x_merge$ris[replace_rows]
      if (all(is.na(x_merge$row_order))) {
        start_val <- 0
      } else {
        start_val <- max(x_merge$row_order, na.rm = TRUE)
      }
      x_merge$row_order[replace_rows] <- start_val + as.numeric(
        as.factor(x_merge$ris[replace_rows])
      )
    }
  }

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "ris", "text", "order")], x_merge$ref)

  # there is an issue with date accessed creating non-existing records
  # removing datasets with 1 row fixes this
  if (any(unlist(lapply(x_split, nrow)) == 1)) {
    x_split <- x_split[-which(unlist(lapply(x_split, nrow)) == 1)]
  }

  # convert to list format
  x_final <- lapply(x_split, function(a) {
    result <- split(a$text, a$field)
    # YEAR
    if (any(names(result) == "year")) {
      if (any(nchar(result$year) >= 4)) {
        year_check <- regexpr("\\d{4}", result$year)
        if (any(year_check > 0)) {
          result$year <- substr(
            x = result$year[which(year_check > 0)],
            start = year_check[1],
            stop = year_check[1] + 3
          )
        } else {
          result$year <- ""
        }
      } else {
        result$year <- ""
      }
    }
    # TITLE
    if (any(names(result) == "title")) {
      if (length(result$title) > 1) {
        if (result$title[1] == result$title[2]) {
          result$title <- result$title[1]
        } else {
          result$title <- paste(result$title, collapse = " ")
        }
      }
      result$title <- gsub("\\s+", " ", result$title) # remove multiple spaces
      result$title <- sub("\\.$", "", result$title) # remove final full stops
    }
    # JOURNAL
    if (any(names(result) == "journal")) {
      unique_journals <- unique(result$journal)
      if (length(unique_journals) > 1) {
        unique_journals <- unique_journals[order(
          nchar(unique_journals),
          decreasing = FALSE
        )]
        result$journal <- unique_journals[1]
        result$journal_secondary <- paste(
          unique_journals[c(2:length(unique_journals))],
          collapse = "; "
        )
      } else {
        result$journal <- unique_journals
      }
      result$journal <- gsub("  ", " ", result$journal)
      result$journal <- sub("\\.$", "", result$journal)
    }
    # ABSTRACT
    if (length(result$abstract > 1)) {
      result$abstract <- paste(result$abstract, collapse = " ")
      result$abstract <- gsub("\\s+", " ", result$abstract) # remove multiple spaces
    }
    # PAGE NUMBER
    if (any(names(result) == "pages")) {
      if (length(result$pages) > 1) {
        result$pages <- paste(sort(result$pages), collapse = "-")
      }
    }

    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b) {
        b[1]
      }))
    )
    return(result[result_order])
  })

  # names(x_final) <- seq_along(x_final)
  class(x_final) <- "bibliography"
  return(x_final)
}
```

**Author Handling** (lines 351-367):
- Consolidates multiple author RIS tags into a single "author" field
- If multiple RIS tags map to "author", keeps the most common one
- Other author tags are renamed to their original RIS tag names
- Author data is preserved as-is from the RIS file

---

### 4. `parse_bibtex()` - BibTeX File Parser

**Location**: `R/import_export_helpers.R`  
**Purpose**: Parses BibTeX format files and splits author strings.

**Full Function Code** (author-relevant section highlighted):

```r
parse_bibtex <- function(x) {
  ### Remove lines that start with a percentage symbol (comments)
  x <- grep("^\\s*%.*",
    x,
    invert = TRUE,
    value = TRUE
  )

  # which lines start with @article?
  group_vec <- rep(0, length(x))
  row_id <- which(regexpr("^@", x) == 1)
  group_vec[row_id] <- 1
  group_vec <- cumsum(group_vec)

  # work out row names
  ref_names <- gsub(".*\\{|,$", "", x[row_id])
  ref_type <- gsub(".*@|\\{.*", "", x[row_id])

  # split by reference
  x_split <- split(x[-row_id], group_vec[-row_id])
  length_vals <- unlist(lapply(x_split, length))

  # Changed this to > 1 based on https://github.com/mjwestgate/synthesisr/issues/26
  # Not sure if this introduces new issues
  x_split <- x_split[which(length_vals > 1)]
  x_final <- lapply(x_split, function(z) {
    # first use a stringent lookup term to locate only tagged rows
    delimiter_lookup <- regexpr(
      "^[[:blank:]]*([[:alnum:]]|[[:punct:]])+[[:blank:]]*=[[:blank:]]*\\{+",
      z
    )
    delimiter_rows <- which(delimiter_lookup != -1)
    other_rows <- which(delimiter_lookup == -1)
    delimiters <- data.frame(
      row = delimiter_rows,
      location = regexpr("=", z[delimiter_rows])
    )
    split_tags <- apply(delimiters, 1, function(a, lookup) {
      c(
        row = as.numeric(a[1]),
        tag = substr(
          x = lookup[a[1]],
          start = 1,
          stop = a[2] - 1
        ),
        value = substr(
          x = lookup[a[1]],
          start = a[2] + 1,
          stop = nchar(lookup[a[1]])
        )
      )
    },
    lookup = z
    )
    entry_dframe <- rbind(
      as.data.frame(
        t(split_tags),
        stringsAsFactors = FALSE
      ),
      data.frame(
        row = other_rows,
        tag = NA,
        value = z[other_rows],
        stringsAsFactors = FALSE
      )
    )
    entry_dframe$row <- as.numeric(entry_dframe$row)
    entry_dframe <- entry_dframe[order(entry_dframe$row), c("tag", "value")]

    if (any(entry_dframe$value == "}")) {
      entry_dframe <- entry_dframe[seq_len(which(entry_dframe$value == "}")[1] - 1), ]
    }
    if (any(entry_dframe$value == "")) {
      entry_dframe <- entry_dframe[-which(entry_dframe$value == ""), ]
    }

    # remove whitespace
    entry_dframe <- as.data.frame(
      lapply(entry_dframe, trimws),
      stringsAsFactors = FALSE
    )
    # remove 1 or more opening brackets
    entry_dframe$value <- gsub("^\\{+", "", entry_dframe$value)
    # remove 1 or more closing brackets followed by zero or more punctuation marks
    entry_dframe$value <- gsub("\\}+[[:punct:]]*$", "", entry_dframe$value)

    # convert each entry to a list
    label_group <- rep(0, nrow(entry_dframe))
    tag_rows <- which(entry_dframe$tag != "")
    label_group[tag_rows] <- 1
    tag_names <- entry_dframe$tag[tag_rows]
    entry_list <- split(
      entry_dframe$value,
      cumsum(label_group) + 1
    )
    names(entry_list) <- tolower(
      gsub("^\\s+|\\s+$", "", tag_names)
    )
    entry_list <- lapply(
      entry_list,
      function(a) {
        paste(a, collapse = " ")
      }
    )
    if (any(names(entry_list) == "author")) {
      if (length(entry_list$author) == 1) {
        entry_list$author <- strsplit(entry_list$author, " and ")[[1]]
      }
    }
    return(entry_list)
  })

  # add type
  x_final <- lapply(
    seq_len(length(x_final)),
    function(a, type, data) {
      c(type = type[a], data[[a]])
    },
    type = ref_type,
    data = x_final
  )

  names(x_final) <- ref_names
  class(x_final) <- "bibliography"
  return(x_final)
}
```

**Author Handling** (lines 561-565):
- If author field exists and is a single string, splits on `" and "` to create a vector
- Assumes BibTeX authors are separated by `" and "`
- Converts author from string to character vector

---

## Data Cleaning Functions

### 5. `clean_authors()` - Author Delimiter Standardization

**Location**: `R/import_export_helpers.R`  
**Purpose**: Standardizes author delimiters to ensure consistent format.

**Full Function Code**:

```r
# Standardize author delimiters
clean_authors <- function(x) {
  if (any(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))) {
    x <- gsub("\\sAND\\s|\\s&\\s", " and ", x)
  } else {
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and ", x, perl = TRUE)
  }
  x <- gsub("\\s{2, }", " ", x)
  return(x)
}
```

**Author Handling Details**:
- **Line 1185**: Checks if any author strings contain `" and "`, `" AND "`, or `" & "`
- **Line 1186**: If found, converts `" AND "` and `" & "` to `" and "` (lowercase with spaces)
- **Line 1188**: If no explicit "and" found, converts commas followed by 2+ letters to `" and "` (handles comma-separated author lists)
- **Line 1190**: Removes multiple consecutive spaces
- **Returns**: Character vector with standardized author delimiters

**Example Transformations**:
- `"Smith, John AND Jones, Mary"` → `"Smith, John and Jones, Mary"`
- `"Smith, John & Jones, Mary"` → `"Smith, John and Jones, Mary"`
- `"Smith, John, Jones, Mary"` → `"Smith, John and Jones, Mary"` (if followed by 2+ letters)

---

### 6. `clean_df()` - Data Frame Cleaning Wrapper

**Location**: `R/import_export_helpers.R`  
**Purpose**: Cleans column names and applies `clean_authors()` if author column exists.

**Full Function Code**:

```r
# Cleans data.frames into synthesisr format
clean_df <- function(data) {
  colnames(data) <- clean_colnames(colnames(data))
  if (any(colnames(data) == "author")) {
    data$author <- clean_authors(data$author)
  }
  return(data)
}
```

**Author Handling**:
- **Line 1175**: Cleans column names (handles "authors" → "author")
- **Line 1176-1178**: If "author" column exists, applies `clean_authors()` to standardize delimiters
- Called automatically during data frame conversion from bibliography objects

---

## Deduplication Functions

### 7. `format_citations()` - Citation Formatting for Deduplication

**Location**: `ASySD info/internal.R`  
**Purpose**: Formats citations for deduplication matching, including author standardization and case conversion.

**Full Function Code**:

```r
format_citations <- function(raw_citations){

  # make sure author is a character
  raw_citations$author <- as.character(raw_citations$author)

  # Fix author formatting so similar
  raw_citations <- raw_citations %>%
    mutate(author = ifelse(.data$author=="", "Unknown", .data$author)) %>%
    mutate(author = ifelse(is.na(.data$author), "Unknown", .data$author)) %>%
    mutate(author = ifelse(.data$author=="Anonymous", "Unknown", .data$author)) %>%
    mutate(author = ifelse(.data$author=="Anonymous.", "Unknown", .data$author)) %>%
    mutate(author = ifelse(.data$author=="[Anonymous] A", "Unknown", .data$author)) %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8

  # Fix page formatting
  raw_citations$pages <- lapply(raw_citations$pages, function(x) gsub("--", "-", x))

  # Make all upper case by selecting cols in order and formatting all metadata to upper
  # Note that source, label and record id are retained - important for joining later
  formatted_citations <- raw_citations %>%
    select(everything(), source, label, record_id)

  ncol(formatted_citations)
  to_col <- ncol(formatted_citations) - 3

  formatted_citations[,1:to_col] <- as.data.frame(sapply(formatted_citations[,1:to_col], toupper))

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

  formatted_citations<-formatted_citations %>%
    filter(!is.na(record_id))

  # sort out NA / missing data formatting for optimal matching
  formatted_citations <- formatted_citations %>%
    mutate(author = ifelse(.data$author=="NA", NA, paste(.data$author))) %>%
    mutate(year = ifelse(.data$year=="NA", NA, paste(.data$year))) %>%
    mutate(title = ifelse(.data$title=="NA", NA, paste(.data$title))) %>%
    mutate(number = ifelse(.data$number=="NA", NA, paste(.data$number))) %>%
    mutate(volume = ifelse(.data$volume=="NA", NA, paste(.data$volume))) %>%
    mutate(pages = ifelse(.data$pages=="NA", NA, paste(.data$pages))) %>%
    mutate(abstract = ifelse(.data$abstract=="NA", NA, paste(.data$abstract))) %>%
    mutate(doi = ifelse(.data$doi=="NA", NA, paste(doi))) %>%
    mutate(journal = ifelse(.data$journal=="NA", NA, paste(.data$journal))) %>%
    mutate(isbn = ifelse(.data$isbn=="", NA, paste(.data$isbn)))

  formatted_citations<- formatted_citations %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, source, label)

  formatted_citations$record_id <- as.character(formatted_citations$record_id)

  return(formatted_citations)

}
```

**Author Handling Details**:
- **Line 109**: Converts author to character type
- **Lines 113-117**: Handles missing/unknown authors:
  - Empty strings → `"Unknown"`
  - `NA` → `"Unknown"`
  - `"Anonymous"`, `"Anonymous."`, `"[Anonymous] A"` → `"Unknown"`
- **Line 118**: Ensures UTF-8 encoding
- **Line 131**: Converts ALL fields (including author) to UPPERCASE for case-insensitive matching
- **Line 156**: Handles string `"NA"` by converting to actual `NA` value
- **Purpose**: Prepares author data for deduplication matching (case-insensitive, standardized missing values)

**Note**: Author names are converted to uppercase for matching, but original case is preserved in the source data.

---

## Citation Generation Functions

### 8. `generate_apa_citation()` - Short Citation Generation

**Location**: `R/tables.R`  
**Purpose**: Generates short APA-style citations (e.g., "Smith (2020)") from author and year data.

**Full Function Code**:

```r
generate_apa_citation <- function(authors, year) {
  id <- seq_along(authors)
  # Extract last names and initials
  processed_names <- tibble::tibble(id = id, authors = authors, year = year) %>%
    dplyr::mutate(
      last_names = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, ",.*$")),
      initials = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, "^.*?,") %>%
        stringr::str_remove_all("\\.") %>%
        stringr::str_trim() %>%
        stringr::str_split(pattern = " ") %>%
        purrr::map(stringr::str_trunc, 1, ellipsis = "") %>%
        purrr::map(stringr::str_c, collapse = ". ") %>%
        purrr::flatten_chr() %>%
        paste0("."))
    )

  # If last name does not uniquely describe authors, first author should be disambiguated in APA style
  # Here implemented by comparing initials -
  # False positives where some sources contain two names (or initials) while others include 1
  # False negative where same initial refers to different names
  # Appears to be best balance for now - further options and instructions could be provided
  # Need this to ensure that last_names and initialed_names retain same length


  last_name <- processed_names %>%
    dplyr::pull(.data$last_names) %>%
    unlist()

  initialed_name <- purrr::map2(
    dplyr::pull(processed_names, .data$initials), 
    dplyr::pull(processed_names, .data$last_names),
    ~ paste(.x, .y)
  ) %>% unlist()

  authors <- tibble::tibble(last_name, initialed_name)
  duplicated_last_names <- last_name[duplicated(last_name)]
  # Identify which last names appear with different initials
  to_disambiguate <- authors %>%
    dplyr::filter(.data$last_name %in% duplicated_last_names) %>%
    dplyr::group_by(.data$last_name) %>%
    dplyr::summarise(disambiguate = dplyr::n_distinct(.data$initialed_name) > 1) %>%
    dplyr::filter(.data$disambiguate == TRUE) %>%
    dplyr::pull(.data$last_name)
  # Replace those last names with initials
processed_names$last_names <- 
  purrr::map2(processed_names$last_names, processed_names$initials, \(last_names, initials) {
    last_names[last_names %in% to_disambiguate] <- 
      paste(initials[last_names %in% to_disambiguate], last_names[last_names %in% to_disambiguate])
    last_names
  })

  # Create simple citations
  citations <- processed_names %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      N_authors = length(.data$last_names),
      year = as.character(.data$year),
      citation = dplyr::case_when(
        .data$N_authors == 1 ~ glue::glue("{last_names[1]} ({year})"),
        .data$N_authors == 2 ~ glue::glue("{last_names[1]} & {last_names[2]} ({year})"),
        .data$N_authors > 2 ~ glue::glue("{last_names[1]} et al. ({year})")
      )
    ) %>%
    dplyr::ungroup()

  # Disambiguate
  citations_unambiguous <- citations %>%
    dplyr::filter(!(duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

  citations_ambiguous <- citations %>%
    dplyr::filter((duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

  if (nrow(citations_ambiguous) > 0) {
    citations_ambiguous <- purrr::map_dfr(unique(citations_ambiguous$citation), function(current_citation) {
      group <- citations_ambiguous %>%
        dplyr::filter(.data$citation == current_citation) %>%
        dplyr::mutate(author_names = purrr::map(.data$last_names, stringr::str_c())) %>%
        dplyr::arrange(dplyr::desc(.data$author_names))

      # Case 1: multiple publications by same author(s) in same year - add letters
      if (dplyr::n_distinct(group$author_names) == 1) {
        group <- group %>%
          dplyr::mutate(year = paste0(year, letters[seq_len(dplyr::n())])) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            N_authors = length(.data$last_names),
            citation = dplyr::case_when(
              .data$N_authors == 1 ~ glue::glue("{last_names[1]} ({year})"),
              .data$N_authors == 2 ~ glue::glue("{last_names[1]} & {last_names[2]} ({year})"),
              .data$N_authors > 2 ~ glue::glue("{last_names[1]} et al. ({year})")
            )
          ) %>%
          dplyr::ungroup()
      } else {
        # Case 2: distinct authors
        # Find maximum number of common authors
        common <- group$last_names %>%
          utils::combn(2, simplify = FALSE) %>%
          purrr::map_int(~{
            len <- min(length(.x[[1]]), length(.x[[2]]))
            comparison <- .x[[1]][1:len] == .x[[2]][1:len]
            ifelse(any(!comparison), which(!comparison)[1] - 1, len)
          }) %>%
          max()
        
        group <- group %>%
          dplyr::rowwise() %>%
          dplyr::mutate(citation = dplyr::case_when(
            (common < 5 | .data$N_authors < 5) & N_authors < common + 3 ~ glue::glue("{glue::glue_collapse(last_names, ', ', last = ' & ')} ({year})"),
            (common < 5 | .data$N_authors < 5) ~ glue::glue("{glue::glue_collapse(last_names[1:(common+1)], ', ')} et al. ({year})"),
            common >= 5 & .data$N_authors < common + 2 ~ glue::glue("{last_names[1]} ... {last_names[common]}  & {last_names[common+1]} ({year})"),
            common >= 5 & .data$N_authors < common + 3 ~ glue::glue("{last_names[1]} ... {last_names[common+1]}  & {last_names[common+2]} ({year})"),
            common >= 5 ~ glue::glue("{last_names[1]} ... {last_names[common+1]} et al. ({year})")
          )) %>%
          dplyr::ungroup()
      }
      group
    })

    citations_still_ambiguous <- citations_ambiguous %>%
      dplyr::filter((duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE))))

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_ambiguous %>%
      dplyr::filter(!(duplicated(.data$citation) | (duplicated(.data$citation, fromLast = TRUE)))))

    # If some of Case 2 were in fact Case 1s (e.g., more than 2 authors with same names), they need to be further disambiguated
    citations_still_ambiguous <- citations_still_ambiguous %>%
      dplyr::group_by(.data$citation) %>%
      dplyr::mutate(letter = letters[seq_len(dplyr::n())]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(citation = stringr::str_replace(.data$citation, "([:digit:])\\)", glue::glue("\\1{letter})"))) %>%
      dplyr::ungroup()

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_still_ambiguous)
  }
  citations_unambiguous %>%
    dplyr::left_join(tibble::tibble(id), ., by = "id") %>%
    dplyr::pull(.data$citation)
}
```

**Author Handling Details**:

1. **Author Parsing** (lines 357-365):
   - **Line 357**: Splits author string on `" and "` to get individual authors
   - **Last names**: Removes everything after comma (e.g., `"Smith, John"` → `"Smith"`)
   - **Initials**: 
     - Extracts text after comma
     - Removes periods
     - Trims whitespace
     - Splits on spaces
     - Takes first character of each word
     - Joins with `". "` and adds final period
     - Example: `"Smith, John Michael"` → initials `"J. M."`

2. **Disambiguation** (lines 376-401):
   - Identifies duplicate last names across citations
   - If same last name has different initials, adds initials to citation
   - Example: `"Smith (2020)"` and `"Smith (2020)"` → `"J. Smith (2020)"` and `"M. Smith (2020)"`

3. **Citation Format** (lines 409-413):
   - 1 author: `"Smith (2020)"`
   - 2 authors: `"Smith & Jones (2020)"`
   - 3+ authors: `"Smith et al. (2020)"`

4. **Ambiguity Resolution** (lines 424-485):
   - **Case 1**: Same authors, same year → adds letters: `"Smith (2020a)"`, `"Smith (2020b)"`
   - **Case 2**: Different authors with same citation → expands to show more authors

---

### 9. `generate_apa_reference()` - Full Reference Generation

**Location**: `R/tables.R`  
**Purpose**: Generates full APA-style references with all bibliographic details.

**Full Function Code**:

```r
generate_apa_reference <- function(authors, year, title, source, volume, issue, doi, weblink, return_html = FALSE, format_journal_case = TRUE) {
  
  id <- seq_along(authors)
  
  # Helper function to handle NULL and missing columns
  handle_missing <- function(arg, arg_name) {
    result <- tryCatch({
      if (is.null(arg)) stop()
      arg
    }, error = function(e) {
      warning("Column ", arg_name, " missing from citation data when generating references", call. = FALSE)
      NA_character_
    })
    result
  }
  
  # Apply the helper function to each other than authors
  year <- handle_missing(year, "year")
  title <- handle_missing(title, "title")
  source <- handle_missing(source, "source")
  volume <- handle_missing(volume, "volume")
  issue <- handle_missing(issue, "issue")
  doi <- handle_missing(doi, "doi")
  weblink <- handle_missing(weblink, "weblink")
  
  # Extract last names and initials
  citations <- tibble::tibble(id, authors, year, title, source, volume, issue, doi, weblink) %>%
    dplyr::mutate(dplyr::across(c(dplyr::everything(), -.data$id), .fns = ~ as.character(.x) %>% dplyr::na_if(""))) %>%
    dplyr::mutate(
      last_names = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~ stringr::str_remove(.x, ",.*$")),
      initials = authors %>% stringr::str_split(pattern = " and ") %>% purrr::map(~{
        out <- .x
        out[!stringr::str_detect(.x, ",")] <- ""
        
        .x <- .x[stringr::str_detect(.x, ",")]
        

        out[!out == ""] <- stringr::str_remove(.x, "^.*?,") %>%
                                                                                    stringr::str_remove_all("\\.") %>%
                                                                                    stringr::str_trim() %>%
                                                                                    stringr::str_split(pattern = " ") %>%
                                                                                    purrr::map(stringr::str_trunc, 1, ellipsis = "") %>%
                                                                                    purrr::map(stringr::str_c, collapse = ". ") %>%
                                                                                    purrr::flatten_chr() %>%
                                                                                    paste0(".")
        out
        })
  
    )
  
  # Merge initials to names
  citations$initialed_names <- citations %>%
    dplyr::select("last_names", "initials") %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::map(~ {
       purrr::map2_chr(.x$last_names, .x$initials, ~ if (.y == "" | is.na(.y)) .x else paste(.x, .y, sep = ", "))
    })

  if (format_journal_case) {
    citations <- citations %>% dplyr::mutate(source = stringr::str_to_title(source))
  }

  # Helper function to deal with missing values
  nNA <- function(x, ..., alt = "", pre = "") {
    ifelse(is.na(x), alt, paste0(pre, x, ...))
  }

  # Compose references

  citations <- citations %>% dplyr::mutate(
    doi = dplyr::if_else(stringr::str_detect(doi, "http"), doi, paste0("https://doi.org/", doi)),
    link = dplyr::coalesce(doi, weblink)
  )
  
  if (return_html) {
    citations %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        reference = glue::glue("
                               {glue::glue_collapse(initialed_names, ', ', last = ' & ')} ({year}). {nNA(title, '.')} {nNA(source, pre = '<i>', '</i>')}{nNA(volume, pre = '<i>, ', '</i>')}{nNA(issue, pre = '(', ')')}. {nNA(link, pre = '<a href=\"', '\" target=\"_blank\" rel=\"noopener noreferrer\">')}{nNA(link, '</a>')}
                                     ")
      ) %>%
      dplyr::pull(.data$reference)
  } else {
    citations %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        reference = glue::glue("
                              {glue::glue_collapse(initialed_names, ', ', last = ' & ')} ({year}). {nNA(title, '.')} {nNA(source)}{nNA(volume, pre = ', ')}{nNA(issue, pre = '(', ')')}. {nNA(link)}
                                                     ")
      ) %>%
      dplyr::pull(.data$reference)
  }
}
```

**Author Handling Details**:

1. **Author Parsing** (lines 543-560):
   - Same logic as `generate_apa_citation()`:
     - Splits on `" and "`
     - Extracts last names (removes text after comma)
     - Extracts and formats initials

2. **Initialed Names** (lines 565-571):
   - Combines last names with initials: `"Smith"` + `"J. M."` → `"Smith, J. M."`
   - If no initials, uses just last name
   - Handles missing initials gracefully

3. **Reference Format** (lines 593-594, 602-603):
   - **Plain text**: `"Smith, J. M. & Jones, M. L. (2020). Title. Journal, Volume(Issue). Link"`
   - **HTML**: Same format with journal/volume in italics and link as HTML anchor
   - Authors joined with `", "` except last author joined with `" & "`

**Example Output**:
- Input: `"Smith, John and Jones, Mary"`
- Output: `"Smith, J. & Jones, M. (2020). Title. Journal, 10(2). https://doi.org/..."`

---

## Export Functions

### 10. `write_bib()` - BibTeX Export

**Location**: `R/import_export_helpers.R`  
**Purpose**: Formats bibliography objects for BibTeX export, collapsing author vectors.

**Full Function Code**:

```r
write_bib <- function(x) {
  # process basic text
  result <- lapply(x, function(a) {
    if (any(names(a) == "author")) {
      a$author <- paste(a$author, collapse = " and ")
    }
    a <- lapply(a, function(b) {
      # ensure only one entry per value
      if (length(b) > 1) {
        paste(b, collapse = "; ")
      } else {
        b
      }
    })
    paste0(names(a), "={", a, "},") # format as text
  })

  # add article identifier info
  export <- unlist(
    lapply(seq_len(length(result)),
      function(a, source, entry_names) {
        c(
          paste0("@ARTICLE{", entry_names[a], ","),
          source[a],
          "}",
          ""
        )
      },
      source = result,
      entry_names = names(x)
    )
  )
  names(export) <- NULL
  return(export)
}
```

**Author Handling** (lines 621-623):
- If author field exists and is a vector, collapses with `" and "` separator
- Converts author from character vector to single string
- Format: `"author={Smith, John and Jones, Mary},"`

---

## Data Conversion Functions

### 11. `as.data.frame.bibliography()` - Bibliography to Data Frame

**Location**: `R/import_export_helpers.R`  
**Purpose**: Converts bibliography objects to data frames, collapsing author vectors.

**Full Function Code**:

```r
as.data.frame.bibliography <- function(x, ...) {
  # Solves https://github.com/mjwestgate/synthesisr/issues/25 - but could likely be faster
  x <- purrr::map(x, \(x) {
    x[lengths(x) == 0] <- NA
    x
  })
  
  x_list <- lapply(x, \(a) {
    a[lengths(a) > 1] <-  a[lengths(a) > 1] %>% purrr::map(\(x) paste0(x, collapse = " and "))
    return(
      as.data.frame(
        a,
        stringsAsFactors = FALSE
      )
    )
  }
  )
  
  x_dframe <- dplyr::bind_rows(x_list)
  rownames(x_dframe) <- NULL
  
  return(x_dframe)
}
```

**Author Handling** (line 1157):
- If author field has length > 1 (i.e., is a vector), collapses with `" and "` separator
- Converts author from character vector to single string
- Used when converting bibliography objects to data frames for export or display

---

### 12. `as.bibliography()` - Data Frame to Bibliography

**Location**: `R/import_export_helpers.R`  
**Purpose**: Converts data frames to bibliography objects, splitting author strings.

**Full Function Code**:

```r
as.bibliography <- function(x, ...) {
  if (!inherits(x, "data.frame")) {
    stop("as.bibliography can only be called for objects of class 'data.frame'")
  }

  x_list <- lapply(
    split(x, seq_len(nrow(x))),
    function(a) {
      a <- as.list(a)
      if (any(names(a) == "author")) {
        a$author <- strsplit(a$author, " and ")[[1]]
      }
      if (any(names(a) == "keywords")) {
        a$keywords <- strsplit(a$keywords, " and ")[[1]]
      }
      return(a)
    }
  )
  names(x_list) <- seq_len(nrow(x))
  class(x_list) <- "bibliography"
  return(x_list)
}
```

**Author Handling** (lines 1222-1223):
- If author field exists, splits on `" and "` to create character vector
- Converts author from single string to vector of individual authors
- Used when converting data frames back to bibliography format

---

## Summary of Author Data Flow

1. **Import**: Author data read from files (RIS/BibTeX/CSV) → stored as strings or vectors
2. **Cleaning**: `clean_authors()` standardizes delimiters → `" and "` format
3. **Deduplication**: `format_citations()` converts to uppercase for matching
4. **Citation Generation**: 
   - `generate_apa_citation()` parses for short citations
   - `generate_apa_reference()` parses for full references
5. **Export**: Author vectors collapsed back to strings with `" and "` separator

**Key Assumptions**:
- Authors in `"Last, First"` format
- Multiple authors separated by `" and "`
- System expects this format throughout the pipeline

---

## Function Dependencies

```
read_citations()
  └── synthesisr_read_refs()
      └── read_ref()
          ├── parse_ris() → clean_df() → clean_authors()
          ├── parse_bibtex()
          └── parse_pubmed()
              └── as.data.frame.bibliography() → clean_df() → clean_authors()

dedup_citations()
  └── format_citations() [ASySD]

record_level_table()
  ├── generate_apa_citation()
  └── generate_apa_reference()

write_refs()
  ├── write_bib()
  └── write_ris()
      └── as.bibliography()
```

---

*Document generated from CiteSource package source code analysis*

