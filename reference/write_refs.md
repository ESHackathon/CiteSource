# Export data to a bibliographic format

This function exports data.frames containing bibliographic information
to either a .ris or .bib file.

## Usage

``` r
write_bib(x)

write_ris(x, tag_naming = "synthesisr")

write_refs(x, format = "ris", tag_naming = "synthesisr", file = FALSE)
```

## Arguments

- x:

  Either a data.frame containing bibliographic information or an object
  of class bibliography.

- tag_naming:

  what naming convention should be used to write RIS files? See details
  for options.

- format:

  What format should the data be exported as? Options are ris or bib.

- file:

  Either logical indicating whether a file should be written (defaulting
  to FALSE), or a character giving the name of the file to be written.

## Value

Returns a character vector containing bibliographic information in the
specified format if `file` is FALSE, or saves output to a file if TRUE.

## Functions

- `write_bib()`: Format a bib file for export

- `write_ris()`: Format a ris file for export
