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
                     CiteSource::synthesisr_code_lookup$ris_pubmed,
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
      rows <- which(CiteSource::synthesisr_code_lookup[, paste0("ris_", tag_naming)])
      code_lookup_thisfile <- CiteSource::synthesisr_code_lookup[
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
  x_split <- x_split[which(length_vals > 3)]
  
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
    lookup_df <- CiteSource::synthesisr_code_lookup[
      CiteSource::synthesisr_code_lookup[, paste0("ris_", tag_naming)],
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
  if(!any(c("bibliography", "data.frame") == class(x))) {
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
##                       Others                                 ##
##################################################################

#' Bibliographic code lookup for search results assembly
#'
#' A data frame that can be used to look up common
#' codes for different bibliographic fields across
#' databases and merge them to a common format.
#'
#' @format A data frame with 226 obs of 12 variables
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

