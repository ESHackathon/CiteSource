# Parse bibliographic text in a variety of formats

Text in standard formats - such as imported via
[`readLines`](https://rdrr.io/r/base/readLines.html) - can be parsed
using a variety of standard formats. Use
[`detect_parser`](https://eshackathon.github.io/CiteSource/reference/detect_.md)
to determine which is the most appropriate parser for your situation.

## Usage

``` r
parse_pubmed(x)

parse_ris(x, tag_naming = "best_guess")

parse_bibtex(x)

parse_csv(x)

parse_tsv(x)
```

## Arguments

- x:

  A character vector containing bibliographic information in ris format.

- tag_naming:

  What format are ris tags in? Defaults to "best_guess" See
  [`synthesisr_read_refs`](https://eshackathon.github.io/CiteSource/reference/synthesisr_read_refs.md)
  for a list of accepted arguments.

## Value

Returns an object of class `bibliography` (ris, bib, or pubmed formats)
or `data.frame` (csv or tsv).
