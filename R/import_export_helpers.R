# These functions actually read and write the files. They are largely taken from 
# https://github.com/mjwestgate/synthesisr since that package appeared to be abandoned
# by the time we created CiteSource

# Copright 2020, Martin Westgate and Eliza Grames
# Licenced under GPL-3

##################################################################
##                       Import functions                       ##
##################################################################

#' Import bibliographic search results
#'
#' @description Imports common bibliographic reference formats (i.e. .bib, .ris, or .txt).
#' @param filename A path to a filename or vector of filenames containing search results to import.
#' @param tag_naming Either a length-1 character stating how should ris tags be replaced (see details for a list of options), or an object inheriting from class \code{data.frame} containing user-defined replacement tags.
#' @param return_df If TRUE (default), returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates (defaults to FALSE).
#' @details The default for argument \code{tag_naming} is \code{"best_guess"}, which estimates what database has been used for ris tag replacement, then fills any gaps with generic tags. Any tags missing from the database (i.e. \code{code_lookup}) are passed unchanged. Other options are to use tags from Web of Science (\code{"wos"}), Scopus (\code{"scopus"}), Ovid (\code{"ovid"}) or Academic Search Premier (\code{"asp"}). If a \code{data.frame} is given, then it must contain two columns: \code{"code"} listing the original tags in the source document, and \code{"field"} listing the replacement column/tag names. The \code{data.frame} may optionally include a third column named \code{"order"}, which specifies the order of columns in the resulting \code{data.frame}; otherwise this will be taken as the row order. Finally, passing \code{"none"} to \code{replace_tags} suppresses tag replacement.
#' @return Returns a data.frame or list of assembled search results.

synthesisr_read_refs <- function(
    filename,
    tag_naming = "best_guess",
    return_df = TRUE,
    verbose = FALSE
){
  
  if(missing(filename)){
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if(any(!file_check)){
    stop("file not found")
  }
  
  if(length(filename) > 1){
    result_list <- lapply(filename, function(a){
      read_ref(
        filename = a,
        tag_naming = tag_naming,
        return_df = return_df,
        verbose = verbose
      )
    })
    names(result_list) <- filename
    
    # drop any unrecognized file types
    null_check <- unlist(lapply(result_list, is.null))
    if(any(null_check)){
      result_list <- result_list[-which(null_check)]
    }
    
    if(return_df){
      result <- merge_columns(result_list)
      result$filename <- unlist(
        lapply(seq_len(length(result_list)),
               function(a, data){
                 rep(names(data)[a], nrow(data[[a]]))
               },
               data = result_list
        ))
      return(result)
    }else{
      result <- do.call(c, result_list)
      return(result)
    }
    
  }else{ # i.e. if only one filename given
    return(
      read_ref(
        filename,
        tag_naming = tag_naming,
        return_df = return_df,
        verbose = verbose
      )
    )
  }
}

# ' Internal function called by read_refs for each file
# '
# ' @description This is the underlying workhorse function that imports bibliographic files; primarily intended to be called from read_refs.
# ' @param filename A path to a filename containing search results to import.
# ' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
# ' @param verbose If TRUE, prints status updates.
# ' @return Returns a data.frame or list of assembled search results.

#' @describeIn read_refs Import a single file
read_ref <- function(
    filename,
    tag_naming = "best_guess",
    return_df = TRUE,
    verbose = FALSE
){
  old_loc <- Sys.getlocale("LC_CTYPE")
  invisible(Sys.setlocale("LC_CTYPE", "C"))
  on.exit(invisible(Sys.setlocale("LC_CTYPE", old_loc)))
  
  # error checking for replace tags
  valid_tags <- c("best_guess", "none", "wos", "scopus", "ovid", "asp", "synthesisr")
  if(inherits(tag_naming, "character")){
    if(!any(valid_tags == tag_naming)){
      stop("tag_naming should be one of 'best_guess', 'none', 'wos', 'scopus', 'ovid',  'asp' or 'synthesisr'.")
    }
  }
  if(inherits(tag_naming, "data.frame")){
    if(any(!(c("code", "field") %in% colnames(tag_naming)))){
      stop("if a data.frame is supplied to replace_tags, it must contain columns 'code' & 'field'.")
    }
  }
  
  if(verbose){cat(paste0("Reading file ", filename, " ... "))}
  x <- readLines(filename, warn = FALSE)
  parse_function <- detect_parser(x[1:min(c(length(x), 200))])
  
  if(parse_function != "unknown"){
    
    # parse correctly
    if(parse_function == "parse_ris"){
      df <- do.call(
        parse_function,
        list(x = x, tag_naming = tag_naming)
      )
    }else{
      df <- do.call(parse_function, list(x = x))
    }
    
    # return object in correct format
    if(inherits(df, "data.frame")){
      if(!return_df){df <- as.bibliography(df)}
    }else{
      if(return_df){df <- as.data.frame.bibliography(df)}
    }
    if(inherits(df, "data.frame")){df <- clean_df(df)}
    if(verbose){cat("done\n")}
    return(df)
    
  }else{
    warning(paste("file type not recognised for ", filename, " - skipping"))
  }
}  

#################################################################
##                       Parse functions                       ##
#################################################################

#' Parse bibliographic text in a variety of formats
#'
#' @description Text in standard formats - such as imported via \code{\link{readLines}} - can be parsed using a variety of standard formats. Use \code{\link{detect_parser}} to determine which is the most appropriate parser for your situation.
#' @param x A character vector containing bibliographic information in ris format.
#' @return Returns an object of class \code{bibliography} (ris, bib, or pubmed formats) or \code{data.frame} (csv or tsv).
#' @example inst/examples/parse_.R
#' @name parse_
NULL

#' @rdname parse_
parse_pubmed <- function(x){
  
  x <- prep_ris(x, detect_delimiter(x), type = "pubmed")
  
  x_merge <- merge(x,
                   synthesisr_code_lookup[
                     synthesisr_code_lookup$ris_pubmed,
                     c("code", "order", "field")
                   ],
                   by.x = "ris",
                   by.y = "code",
                   all.x = TRUE,
                   all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]
  
  # find a way to store missing .bib data rather than discard
  if(any(is.na(x_merge$field))){
    rows_tr <- which(is.na(x_merge$field))
    x_merge$field[rows_tr] <- x_merge$ris[rows_tr]
    
    # ensure all headings have an order
    if(all(is.na(x_merge$order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(as.factor(x_merge$ris[rows_tr])) + start_val
  }
  
  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "text", "order")], x_merge$ref)
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field)
    if(any(names(result) == "abstract")){
      result$abstract <- paste(result$abstract, collapse = " ")
    }
    if(any(names(result) == "address")){
      result$address <- strsplit(
        paste(result$address, collapse = " "),
        "\\.\\s"
      )[[1]]
    }
    if(any(names(result) == "title")){
      if(length(result$title) > 1){
        result$title <- paste(result$title, collapse = " ")
      }
    }
    if(any(names(result) == "term_other")){
      names(result)[which(names(result) == "term_other")] <- "keywords"
    }
    if(any(names(result) == "date_published")){
      result$year <- substr(result$date_published, start = 1, stop = 4)
    }
    if(any(names(result) == "article_id")){
      doi_check <- grepl("doi", result$article_id)
      if(any(doi_check)){
        result$doi <- strsplit(result$article_id[which(doi_check)], " ")[[1]][1]
      }
    }
    
    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
  })
  
  names(x_final) <- unlist(lapply(x_final, function(a){a$pubmed_id}))
  class(x_final) <- "bibliography"
  return(x_final)
}


#' @rdname parse_
#' @param tag_naming What format are ris tags in? Defaults to "best_guess" See \code{\link{read_refs}} for a list of accepted arguments.
parse_ris <- function(x, tag_naming = "best_guess"){
  
  x <- prep_ris(x, detect_delimiter(x), type = "generic")
  
  # create the appropriate lookup file for the specified tag
  if(inherits(tag_naming, "data.frame")){
    if(!any(colnames(tag_naming) == "order")){
      tag_naming$order <- seq_len(nrow(tag_naming))
    }
    code_lookup_thisfile <- tag_naming
  }else{
    if(tag_naming == "none"){
      ris_vals <- unique(x$ris)
      code_lookup_thisfile <- data.frame(
        code = ris_vals,
        field = ris_vals,
        order = seq_along(ris_vals),
        stringsAsFactors = FALSE
      )
    }else if(tag_naming == "best_guess"){
      code_lookup_thisfile <- detect_lookup(tags = unique(x$ris))
    }else if(any(c("wos", "scopus", "ovid", "asp", "synthesisr") == tag_naming)){
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
  if(any(is.na(x_merge$field))){
    rows_tr <- which(is.na(x_merge$field))
    x_merge$field[rows_tr] <- x_merge$ris[rows_tr]
    
    # ensure all headings have an order
    if(all(is.na(x_merge$order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(as.factor(x_merge$ris[rows_tr])) + start_val
  }
  
  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x_merge$text)
  if(any(year_check > 0)){
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x_merge$text[check_rows])
    
    # for entries with a bib entry labelled year, check that there arent multiple years
    if(any(x_merge$field[check_rows] == "year", na.rm = TRUE)){
      # check for repeated year information
      year_freq <- xtabs(~ ref, data = x_merge[which(x_merge$field == "year"), ])
      if(any(year_freq > 1)){
        year_df <- x_merge[which(x_merge$field == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean))) ^ 2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x_merge$ris != names(which.min(year_4)[1]) &
            x_merge$field == "year"
        )
        x_merge$field[incorrect_rows] <- "year_additional"
      }
    }else{
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
      tag_frequencies$prop <- tag_frequencies$n/(max(x_merge$ref)+1) # number of references
      if(any(tag_frequencies$prop > 0.9)){
        year_tag <- tag_frequencies$tag[which.max(tag_frequencies$prop)]
        rows.tr <- which(x_merge$ris == year_tag)
        x_merge$field[rows.tr] <- "year"
        x_merge$row_order[rows.tr] <- 3
      }
    }
  }
  
  # ensure author data from a single ris tag
  if(any(x_merge$field == "author")){
    lookup.tags <- xtabs( ~ x_merge$ris[which(x_merge$field == "author")])
    if(length(lookup.tags) > 1){
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x_merge$ris %in% replace_tags)
      x_merge$field[replace_rows] <- x_merge$ris[replace_rows]
      if(all(is.na(x_merge$row_order))){
        start_val <- 0
      }else{
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
  if(any(unlist(lapply(x_split, nrow))==1)){
    x_split <- x_split[  -which(unlist(lapply(x_split, nrow))==1)]
  }
  
  # convert to list format
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field)
    # YEAR
    if(any(names(result) == "year")){
      if(any(nchar(result$year) >= 4)){
        year_check <- regexpr("\\d{4}", result$year)
        if(any(year_check > 0)){
          result$year <- substr(
            x = result$year[which(year_check>0)],
            start = year_check[1],
            stop = year_check[1]+3
          )
        }else{
          result$year <- ""
        }
      }else{
        result$year <- ""
      }
    }
    # TITLE
    if(any(names(result) == "title")){
      if(length(result$title) > 1){
        if(result$title[1] == result$title[2]){
          result$title <- result$title[1]
        }else{
          result$title <- paste(result$title, collapse = " ")
        }
      }
      result$title <- gsub("\\s+", " ", result$title) # remove multiple spaces
      result$title <- sub("\\.$", "", result$title) # remove final full stops
    }
    # JOURNAL
    if(any(names(result) == "journal")){
      unique_journals <- unique(result$journal)
      if(length(unique_journals)>1){
        unique_journals <- unique_journals[order(
          nchar(unique_journals),
          decreasing = FALSE
        )]
        result$journal <- unique_journals[1]
        result$journal_secondary <- paste(
          unique_journals[c(2:length(unique_journals))],
          collapse = "; "
        )
      }else{
        result$journal <- unique_journals
      }
      result$journal <-gsub("  ", " ", result$journal)
      result$journal <-sub("\\.$", "", result$journal)
    }
    # ABSTRACT
    if(length(result$abstract > 1)){
      result$abstract <- paste(result$abstract, collapse = " ")
      result$abstract <- gsub("\\s+", " ", result$abstract) # remove multiple spaces
    }
    # PAGE NUMBER
    if(any(names(result) == "pages")){
      if(length(result$pages) > 1){
        result$pages <- paste(sort(result$pages), collapse = "-")
      }
    }
    
    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
  })
  
  # names(x_final) <- seq_along(x_final)
  class(x_final) <- "bibliography"
  return(x_final)
}


#' @rdname parse_
parse_bibtex <- function(x){
  
  ### Remove lines that start with a percentage symbol (comments)
  x <- grep("^\\s*%.*",
            x,
            invert = TRUE,
            value=TRUE)
  
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
  x_final <- lapply(x_split, function(z){
    
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
    split_tags <- apply(delimiters, 1, function(a, lookup){
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
    
    if(any(entry_dframe$value == "}")){
      entry_dframe <- entry_dframe[seq_len(which(entry_dframe$value == "}")[1]-1), ]
    }
    if(any(entry_dframe$value == "")){
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
      cumsum(label_group)+1
    )
    names(entry_list) <- tolower(
      gsub("^\\s+|\\s+$",  "", tag_names)
    )
    entry_list <- lapply(entry_list,
                         function(a){paste(a, collapse = " ")}
    )
    if(any(names(entry_list) == "author")){
      if(length(entry_list$author) == 1){
        entry_list$author <- strsplit(entry_list$author, " and ")[[1]]
      }
    }
    return(entry_list)
  })
  
  # add type
  x_final <- lapply(
    seq_len(length(x_final)),
    function(a, type, data){
      c(type = type[a], data[[a]])
    },
    type = ref_type,
    data = x_final
  )
  
  names(x_final) <- ref_names
  class(x_final) <- "bibliography"
  return(x_final)
  
}

#' @rdname parse_
parse_csv <- function(x){
  z <- read.table(
    text = x,
    header = TRUE,
    sep = ",",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    stringsAsFactors = FALSE, row.names = NULL
  )
  return(match_columns(z))
}

#' @rdname parse_
parse_tsv <- function(x){
  z <- read.table(
    text = x,
    header = TRUE,
    sep = "\t",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    stringsAsFactors = FALSE, row.names = NULL
  )
  return(match_columns(z))
}

##################################################################
##                       Export functions                       ##
##################################################################

# Parse an object of class bibliography for export in bib format
#' @describeIn write_refs Format a bib file for export
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
      } else{
        b
      }
    })
    paste0(names(a), "={", a, "},") # format as text
  })
  
  # add article identifier info
  export <- unlist(
    lapply(seq_len(length(result)),
           function(a, source, entry_names) {
             c(paste0("@ARTICLE{", entry_names[a], ","),
               source[a],
               "}",
               "")
           },
           source = result,
           entry_names = names(x)))
  names(export) <- NULL
  return(export)
  
}


# Parse an object of class bibliography for export in ris format
#' @describeIn write_refs Format a ris file for export
#' 
#' 

# Enhanced from synthesisr version to accept a tag_naming data.frame
# currently that does not work: https://github.com/mjwestgate/synthesisr/issues/23

write_ris <- function(x,
                          tag_naming = "synthesisr") {
  if (is.character(tag_naming)) {
    lookup_df <- synthesisr_code_lookup[
      synthesisr_code_lookup[, paste0("ris_", tag_naming)],
      c("code", "field")
    ]
  } else {
    lookup_df <- tag_naming[c("code", "field")]
  }
  
  
  
  result <- lapply(x, function(a, lookup) {
    # convert to tagged vector
    b <- do.call(c, a)
    b <- b[!is.na(b)]
    b <- data.frame(
      tag = c(names(b), "end"),
      entry = c(b, ""),
      stringsAsFactors = FALSE
    )
    rownames(b) <- NULL
    b$tag <- gsub("[[:digit:]]", "", b$tag)
    
    # page information needs to be treated separately
    if (any(b$tag == "pages")) {
      page_row <- which(b$tag == "pages")
      page_text <- b$entry[page_row]
      if (grepl("-", page_text)) {
        text_lookup <- list(
          regexpr("^[[:digit:]]+", page_text),
          regexpr("-[[:digit:]]+", page_text)
        )
        if (all(text_lookup > 0)) {
          text_cleaned <- unlist(lapply(
            text_lookup,
            function(b) {
              substr(page_text, b, b + attr(b, "match.length") - 1)
            }
          ))
          new_rows <- data.frame(
            tag = c("startpage", "endpage"),
            entry = gsub("[[:punct:]]", "", text_cleaned),
            stringsAsFactors = FALSE
          )
          b <- as.data.frame(rbind(
            b[c(1:(page_row - 1)), ],
            new_rows,
            b[c((page_row + 1):nrow(b)), ]
          ))
        }
      }
    }
    b$order <- seq_len(nrow(b))
    
    # substitute tags for ris format versions
    b <- merge(
      lookup,
      b,
      by.x = "field",
      by.y = "tag",
      all.x = FALSE,
      all.y = FALSE
    )
    b <- b[order(b$order), c(2:3)]
    
    # concatenate rows, return a vector of strings
    return(
      c(paste(b$code, b$entry, sep = "  - "), "ER  - ", "")
    )
  },
  lookup = lookup_df
  )
  
  export <- do.call(c, result)
  return(export)
}

#' Export data to a bibliographic format
#'
#' @description This function exports data.frames containing bibliographic information to either a .ris or .bib file.
#' @param x Either a data.frame containing bibliographic information or an object of class bibliography.
#' @param format What format should the data be exported as? Options are ris or bib.
#' @param tag_naming what naming convention should be used to write RIS files? See details for options.
#' @param file Either logical indicating whether a file should be written (defaulting to FALSE), or a character giving the name of the file to be written.
#' @return Returns a character vector containing bibliographic information in the specified format if \code{file} is FALSE, or saves output to a file if TRUE.
#' @example inst/examples/parse_.R
write_refs <- function(
    x,
    format = "ris",
    tag_naming = "synthesisr",
    file = FALSE # either logical or a character (i.e. a file name)
){
  # check input data
  if (inherits(x, "data.frame") || inherits(x, "bibliography")) {
    stop("write_bibliography only accepts objects of class 'data.frame' or 'bibliography'")
  }
  if(inherits(x, "data.frame")){
    x <- as.bibliography(x)
  }
  
  # check format
  if(!(format %in% c("ris", "bib"))){
    stop("format must be either 'ris' or 'bib'")
  }
  
  # check output format - consistent with read_refs
  if(format == "ris"){
    valid_tags <- c("best_guess", "none", "wos", "scopus", "ovid", "asp", "synthesisr")
    if(inherits(tag_naming, "character")){
      if(!any(valid_tags == tag_naming)){
        stop("tag_naming should be one of 'best_guess', 'none', 'wos', 'scopus', 'ovid',  'asp' or 'synthesisr'.")
      }
    }else if(inherits(tag_naming, "data.frame")){
      if(any(!(c("code", "field") %in% colnames(tag_naming)))){
        stop("if a data.frame is supplied to replace_tags, it must contain columns 'code' & 'field'.")
      }
    }
  }
  
  # check file information
  if(length(file) > 1){
    stop("argument 'file' should be a length-1 character or logical")
  }
  if(!inherits(file, c("logical", "character"))){
    stop("argument 'file' should be either logical or character")
  }
  if(inherits(file, "character")){
    file_out <- TRUE
    if(grepl("\\.[[:alpha:]]{2,4}$", file)){
      filename <- file
    }else{
      filename <- paste(file, format, sep = ".")
    }
  }else{ # i.e. logical
    if(file){
      file_out <- TRUE
      filename <- paste("synthesisr_bibliography", format, sep = ".")
    }else{
      file_out <- FALSE
    }
  }
  
  # write result in correct format
  export <- switch(format,
                   "bib" = {write_bib(x)},
                   "ris" = {write_ris(x, tag_naming = tag_naming)}
  )
  
  if(file_out) {
    write.table(
      export,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }else{
    invisible(return(export))
  }
}

##################################################################
##             Other helpers (code lookup / detect)             ##
##################################################################

#' Bibliographic code lookup for search results assembly
#'
#' A data frame that can be used to look up common
#' codes for different bibliographic fields across
#' databases and merge them to a common format.
#'
#' @format A data frame with 226 obs of 12 variables
#' @noRd
#'
#' \describe{
#'  \item{code}{code used in search results}
#'  \item{order}{the order in which to rank fields in assembled results}
#'  \item{category_description}{type of bibliographic data}
#'  \item{entry_description}{description of field}
#'  \item{field}{bibliographic field that codes correspond to}
#'  \item{ris_generic}{logical: If the code is used in generic ris files}
#'  \item{ris_wos}{logical: If the code is used in Web of Science ris files}
#'  \item{ris_pubmed}{logical: If the code is used in PubMed ris files}
#'  \item{ris_scopus}{logical: If the code is used in Scopus ris files}
#'  \item{ris_asp}{logical: If the code is used in Academic Search Premier ris files}
#'  \item{ris_ovid}{logical: If the code is used in Ovid ris files}
#'  \item{ris_synthesisr}{logical: If the code used in synthesisr imports & exports}}
#'
"synthesisr_code_lookup"

# Unclear which of the detect functions are actually needed here - but they are not exported, so prob not an issue
# At least `detect_parse` is essential

#' Detect file formatting information
#'
#' @description Bibliographic data can be stored in a number of different file types, meaning that detecting consistent attributes of those files is necessary if they are to be parsed accurately. These functions attempt to identify some of those key file attributes. Specifically, \code{detect_parser} determines which \code{\link{parse_}} function to use; \code{detect_delimiter} and \code{detect_lookup} identify different attributes of RIS files; and \code{detect_year} attempts to fill gaps in publication years from other information stored in a \code{data.frame}.
#' @param x A character vector containing bibliographic data
#' @param tags A character vector containing RIS tags.
#' @param df a data.frame containing bibliographic data
#' @return \code{detect_parser} and \code{detect_delimiter} return a length-1 character; \code{detect_year} returns a character vector listing estimated publication years; and \code{detect_lookup} returns a \code{data.frame}.
#' @example inst/examples/detect_.R
#' @name detect_
NULL

# internal function to calculate the proportion of lines that contain a particular regex
# called by detect_parser
proportion_delimited <- function(x, regex){
  delimiter_count <- unlist(lapply(
    gregexpr(regex, x, perl = TRUE),
    function(a){length(which(a > 0))}
  ))
  full_lines <- nchar(x, type = "bytes") > 0
  proportion <- length(which(delimiter_count > 0)) / length(which(full_lines))
  return(proportion)
}


#' @rdname detect_
detect_parser <- function(x){
  
  # calculate proportional of lines containing likely tags
  proportions <- unlist(lapply(
    c(
      ",(\"|[[:alnum:]])",
      "\t",
      "\\{|\\}",
      "(^[[:upper:]]{2,4}\\s*(-|:)\\s)|(^([[:upper:]]{2}|[[:upper:]][[:digit:]])\\s*(-|:){0,2}\\s*)"
    ),
    function(a, z){proportion_delimited(z, a)},
    z = x
  ))
  
  # if any are detection, pick the most likely one
  if(any(proportions > 0.2)){
    result <- switch(
      c("comma", "tab", "bibtex", "ris")[which.max(proportions)],
      "comma" = "parse_csv",
      "tab" = "parse_tsv",
      "bibtex" = "parse_bibtex",
      "ris" = {
        if(length(which(grepl("PMID", x))) > 0){
          "parse_pubmed"
        }else{
          "parse_ris"
        }
      }
    )
  }else{
    result <- "unknown"
  }
  return(result)
}


#' @rdname detect_
detect_delimiter <- function(x){
  if(any(grepl("^ER", x))){
    delimiter <- "endrow"
  }else{
    # special break: same character repeated >6 times, no other characters
    char_list <- strsplit(x, "")
    char_break_test <- unlist(
      lapply(char_list,
             function(a){length(unique(a)) == 1 & length(a > 6)}
      )
    )
    if(any(char_break_test)){
      delimiter <- "character"
    }else{
      # use space as a ref break (last choice)
      space_break_check <- unlist(lapply(
        char_list,
        function(a){all(a == "" | a == " ")}
      ))
      if(any(space_break_check)){
        delimiter <- "space"
      }else{
        stop("import failed: unknown reference delimiter")
      }
    }
  }
  return(delimiter)
}


#' @rdname detect_
detect_lookup <- function(
    tags # a vector of strings representing ris tags
){
  rows <- which(synthesisr::code_lookup$code %in% tags)
  ris_list <- split(
    synthesisr::code_lookup[rows, grepl("ris_", colnames(synthesisr::code_lookup))],
    synthesisr::code_lookup$code[rows]
  )
  ris_matrix <- do.call(
    rbind,
    lapply(ris_list, function(a){apply(a, 2, any)})
  )
  ris_sums <- apply(ris_matrix, 2, sum)
  best_match <- which.max(ris_sums[-1])
  best_proportion <- ris_sums[best_match + 1] / nrow(ris_matrix)
  generic_proportion <- ris_sums[1] / nrow(ris_matrix)
  # default to ris_generic if everything else is bad
  if(best_proportion < 0.75 & generic_proportion > best_proportion){
    match_df <- synthesisr::code_lookup[synthesisr::code_lookup$ris_generic, ]
  }else{ # i.e. if the 'best' match performs perfectly
    if(best_proportion > 0.99){ # i.e. a perfect match
      match_df <- synthesisr::code_lookup[
        synthesisr::code_lookup[, names(best_match)],
        
      ]
    }else{ # otherwise use the best choice, then generic to fill gaps
      rows_best <- which(
        synthesisr::code_lookup[, names(best_match)] &
          synthesisr::code_lookup$code %in% names(which(ris_matrix[, names(best_match)]))
      )
      rows_generic <- which(
        synthesisr::code_lookup$ris_generic &
          synthesisr::code_lookup$code %in% names(which(!ris_matrix[, names(best_match)]))
      )
      match_df <- synthesisr::code_lookup[c(rows_best, rows_generic), ]
    }
  }
  
  return(match_df[, c("code", "order", "field")])
}


# internal function for detect_year
guess_year <- function(x){
  number_lookup <- regexpr("[[:alnum:]]{4}", as.character(x))
  if(any(number_lookup > 0)){
    x <- x[number_lookup > 0]
    result_vec <- unlist(lapply(seq_along(x), function(a){
      substr(x[a], start = number_lookup[a], stop = number_lookup[a] + 3)
    }))
    # return(max(as.numeric(result)))
    result <- names(sort(xtabs(~result_vec), decreasing = TRUE)[1])
    return(result)
  }else{
    return(NA)
  }
}

#' @rdname detect_
detect_year <- function(df){
  if(!inherits(df, "data.frame")){
    stop(print("detect_year expects an object of class data.frame as input"))
  }
  lc_colnames <- tolower(colnames(df))
  dates <- grepl("date", lc_colnames) & !grepl("access", lc_colnames)
  if(any(dates)){
    if(any(colnames(df) == "year")) {
      result <- df$year
    }else{
      result <- rep(NA, nrow(df))
    }
    na_rows <- is.na(result)
    if(any(na_rows)){
      result[na_rows] <- unlist(lapply(
        split(df[na_rows, dates], seq_along(na_rows)),
        guess_year
      ))
    }
  }else{
    result <- rep(NA, nrow(df))
  }
  return(result)
}

# internal function used by parse_csv and parse_tsv
# ' Matches imported data to reference codes
# '
# ' @description Takes an imported data.frame and rearranges it to match lookup codes.
# ' @param df A data.frame that contains bibliographic information.
# ' @return Returns a data.frame rearranged and coded to match standard bibliographic fields, with unrecognized fields appended.
# ' @example inst/examples/match_columns.R
match_columns <- function(df){
  # figure out which columns match known tags
  hits <- as.numeric(match(synthesisr::code_lookup$code, colnames(df)))
  newcolnames <- synthesisr::code_lookup$field[
    match(colnames(df),
          synthesisr::code_lookup$code)
  ]
  colnames(df)[!is.na(newcolnames)] <- newcolnames[!is.na(newcolnames)]
  
  # rearrange data in standard(ish) order
  if(any(is.na(hits))){
    hits <- hits[!is.na(hits)]
  }
  
  # retain columns even if they did not match lookup
  retain <- append(hits, seq(1, length(df), 1)[!(seq(1, length(df), 1) %in% hits)])
  
  return(df[,retain])
}

#' Bind two or more data frames with different columns
#'
#' @description Takes two or more data.frames with different column names or different column orders and binds them to a single data.frame.
#' @param x Either a data.frame or a list of data.frames.
#' @param y A data.frame, optional if x is a list.
#' @return Returns a single data.frame with all the input data frames merged.
#' @example inst/examples/merge_columns.R
merge_columns <- function(
    x, # either a data.frame or a list of the same
    y # a data.frame, optional
){
  if(missing(x)){
    stop("object x is missing with no default")
  }
  
  if(!(inherits(x, "data.frame") || inherits(x, "list"))) {
    stop("object x must be either a data.frame or a list")    
  }

  
  if(!(inherits(x, "data.frame"))){
    if(missing(y)){
      stop("If x is a data.frame, then y must be supplied")
    }
    x <- list(x, y)
  }else{ # i.e. for lists
    if(!all(unlist(lapply(x, class)) == "data.frame")){
      stop("x must only contain data.frames")
    }
  }
  
  x <- lapply(x, remove_factors)
  
  col_names_all <- unique(unlist(lapply(x, colnames)))
  
  result_list <- lapply(x, function(a, cn){
    missing_names <- !(cn %in% colnames(a))
    if(any(missing_names)){
      new_names <- cn[missing_names]
      result <- data.frame(
        c(a, sapply(new_names, function(b){NA})),
        stringsAsFactors = FALSE)
      return(result[, cn])
    }else{
      return(a[, cn])
    }
  },
  cn = col_names_all
  )
  
  return(do.call(rbind, result_list))
  
}

# internal functions called by merge_columns
# ' Remove factors from an object
# '
# ' @description This function converts factors to characters to avoid errors with levels.
# ' @param z A data.frame
# ' @return Returns the input data.frame with all factors converted to character.
# ' @examples remove_factors(list(as.factor(c("a", "b"))))
remove_factors <- function(z){
  z[] <- lapply(z, function(x){
    if(is.factor(x)){as.character(x)}else{x}
  })
  return(z)
}

#' @rdname bibliography-class
as.data.frame.bibliography <- function(x, ...){
  # Solves https://github.com/mjwestgate/synthesisr/issues/25 - but could likely be faster
  x <- purrr::map(x, \(x) {
    x[lengths(x) == 0] <- NA
    x
  })
  cols <- unique(unlist(lapply(x, names)))
  # cols <- cols[which(cols != "further_info")]
  
  x_list <- lapply(x, function(a, cols){
    result <- lapply(cols, function(b, lookup){
      if(any(names(lookup) == b)){
        data_tr <- lookup[[b]]
        if(length(data_tr) > 1){
          data_tr <- paste0(data_tr, collapse = " and ")
        }
        return(data_tr)
      }else{
        return(NA)
      }
    },
    lookup = a)
    names(result) <- cols
    return(
      as.data.frame(
        result,
        stringsAsFactors=FALSE
      )
    )
  },
  cols = cols
  )
  
  x_dframe <- data.frame(
    do.call(rbind, x_list),
    stringsAsFactors = FALSE
  )
  rownames(x_dframe) <- NULL
  
  return(x_dframe)
}

# Cleans data.frames into synthesisr format
#' @rdname clean_
clean_df <- function(data){
  colnames(data) <- clean_colnames(colnames(data))
  if(any(colnames(data) == "author")){
    data$author <- clean_authors(data$author)
  }
  return(data)
}


# Standardize author delimiters
#' @rdname clean_
clean_authors <- function(x){
  if(any(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- gsub("\\sAND\\s|\\s&\\s", " and ", x)
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and ", x, perl = TRUE)
  }
  x <- gsub("\\s{2, }", " ", x)
  return(x)
}


# Clean common issues with column names
#' @rdname clean_
clean_colnames <- function(
    x # colnames
){
  if(inherits(x, "data.frame")){
    x <- colnames(x)
  }
  x <- sub("^(X|Y|Z)\\.+", "", x) # remove leading X
  x <- sub("^[[:punct:]]*", "", x) # leading punctuation
  x <- sub("[[:punct:]]*$", "", x) # trailing punctuation
  x <- gsub("\\.+", "_", x) # replace 1 or more dots with underscore
  non_codes <- nchar(x) > 2 # for colnames with nchar > 2, convert to lower case
  x[non_codes] <- tolower(x[non_codes])
  x <- sub("authors", "author", x) # remove plural authors
  x <- make.unique(x, sep = "_")
  x <- gsub(" ", "_", x)
  return(x)
}

#' @rdname bibliography-class
as.bibliography <- function(x, ...){
  
  if(!inherits(x, "data.frame")) {
    stop("as.bibliography can only be called for objects of class 'data.frame'")
  }
  
  x_list <- lapply(
    split(x, seq_len(nrow(x))),
    function(a){
      a <- as.list(a)
      if(any(names(a) == "author")){
        a$author <- strsplit(a$author, " and ")[[1]]
      }
      if(any(names(a) == "keywords")){
        a$keywords <- strsplit(a$keywords, " and ")[[1]]
      }
      return(a)
    }
  )
  names(x_list) <- seq_len(nrow(x))
  class(x_list) <- "bibliography"
  return(x_list)
}

# ensure multiple consecutive empty rows are removed
# This function computes the rolling sum of detections; intended for use in detect_delimiter.
rollingsum <- function(a, n = 2L){
  tail(cumsum(a) - cumsum(c(rep(0, n), head(a, -n))), -n + 1)
}


# ' Clean an RIS file for import
# '
# ' @description This function preps RIS files by cleaning common issues and converting to a common format.
# ' @param z A character vector that contains RIS bibliographic information.
# ' @param delimiter A string indicating the type of delimiter separating entries.
# ' @param type A string indicating the ris source; options are pubmed or generic.
# ' @return Returns a data.frame intended for import with parse_ris.
prep_ris <- function(
    z,
    delimiter,
    type # either "pubmed" or "generic". Not specified by user
){
  # detect tags
  if(type == "pubmed"){
    ris_regex <- "^[[:upper:]]{2,4}\\s*-\\s"
  }else{ # i.e. generic
    ris_regex <- "(^([[:upper:]]{2}|[[:upper:]][[:digit:]])\\s+)|^ER$"
    # NOTE: "^ER$" is a bug fix for .ciw end rows
  }
  tags <- regexpr(ris_regex, perl = TRUE, z)
  z_dframe <- data.frame(
    text = z,
    row = seq_along(z),
    match_length = attr(tags, "match.length"),
    stringsAsFactors = FALSE
  )
  z_list <- split(z_dframe, z_dframe$match_length)
  z_list <- lapply(z_list, function(a){
    n <- a$match_length[1]
    if(n < 0){
      result <- data.frame(
        ris = "",
        text = a$text,
        row_order = a$row,
        stringsAsFactors = FALSE
      )
    }else{
      result <- data.frame(
        ris = sub("\\s{0,}-\\s{0,}|^\\s+|\\s+$", "", substr(a$text, 1, n)),
        text = gsub("^\\s+|\\s+$", "", substr(a$text, n+1, nchar(a$text))),
        row_order = a$row,
        stringsAsFactors = FALSE
      )
    }
    return(result)
  })
  z_dframe <- do.call(rbind, z_list)
  z_dframe <- z_dframe[order(z_dframe$row), ]
  
  # clean up obvious errors
  z_dframe$ris <- gsub("[[:punct:]]", "", z_dframe$ris)
  z_dframe$text <- sub("^[[:punct:]]{0,1}\\s*", "", z_dframe$text)
  
  # replace tag information for delimiter == character | space
  if(delimiter == "character"){ # i.e. a single character repeated many times
    z_dframe$ris[which(
      unlist(lapply(
        strsplit(z, ""),
        function(a){
          length(unique(a)) == 1 & length(a > 6)
        }
      ))
    )] <- "ER"
  }
  if(delimiter == "space"){
    z_dframe$ris[which(z_dframe$ris == "" & z_dframe$text == "")] <- "ER"
    
    z_rollsum <- rollingsum(z_dframe$ris == "ER")
    if(any(z_rollsum > 1)){
      z_dframe <- z_dframe[which(z_rollsum <= 1), ]
    }
  }
  if(delimiter == "endrow"){
    # work out what most common starting tag is
    z_dframe$ref <- c(0, cumsum(z_dframe$ris == "ER")[
      seq_len(nrow(z_dframe)-1)]
    ) # split by reference
    
    start_tags <- unlist(lapply(
      split(z_dframe$ris, z_dframe$ref),
      function(a){a[which(a != "")[1]]}
    ))
    
    # fix bug where not all entries start with same tag
    start_tag_xtab <- xtabs(~ start_tags )
    end_rows <- which(z_dframe$ris == "ER")
    # previous behavior:
    if(max(xtabs(~ start_tags)) == length(which(z_dframe$ris == "ER"))){
      start_tag <- names(which.max(xtabs(~ start_tags)))
      row_df <- data.frame(
        start = which(z_dframe$ris == start_tag),
        end = end_rows
      )
      # new option:
    }else{
      row_df <- data.frame(
        start = c(1, end_rows[seq_len(length(end_rows) - 1)]),
        end = end_rows
      )
    }
    
    z_list <- apply(
      row_df,
      1,
      function(a){c(a[1]:a[2])}
    )
    z_list <- lapply(
      z_list,
      function(a, lookup){lookup[a, ]},
      lookup = z_dframe
    )
    z_dframe <- as.data.frame(
      do.call(rbind, z_list)
    )
  }
  
  # cleaning
  z_dframe$ref <- c(0, cumsum(z_dframe$ris == "ER")[
    seq_len(nrow(z_dframe)-1)]
  ) # split by reference
  z_dframe <- z_dframe[which(z_dframe$text != ""), ] # remove empty rows
  z_dframe <- z_dframe[which(z_dframe$ris != "ER"), ] # remove end rows
  z_dframe$text <- trimws(z_dframe$text)
  
  # fill missing tags
  z_split <- split(z_dframe, z_dframe$ref)
  z_split <- lapply(z_split, function(a){
    if(a$ris[1] == ""){
      a$ris[1] <- "ZZ"
    }
    accum_ris <- Reduce(c, a$ris, accumulate = TRUE)
    a$ris <- unlist(lapply(
      accum_ris,
      function(b){
        good_vals <- which(b != "")
        b[good_vals[length(good_vals)]]
      }))
    return(a)
  })
  z_dframe <- as.data.frame(
    do.call(rbind, z_split)
  )
  
  return(z_dframe)
}