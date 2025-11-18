# Detect file formatting information

Bibliographic data can be stored in a number of different file types,
meaning that detecting consistent attributes of those files is necessary
if they are to be parsed accurately. These functions attempt to identify
some of those key file attributes. Specifically, `detect_parser`
determines which
[`parse_`](http://www.eshackathon.org/CiteSource/reference/parse_.md)
function to use; `detect_delimiter` and `detect_lookup` identify
different attributes of RIS files; and `detect_year` attempts to fill
gaps in publication years from other information stored in a
`data.frame`.

## Usage

``` r
detect_parser(x)

detect_delimiter(x)

detect_lookup(tags)

detect_year(df)
```

## Arguments

- x:

  A character vector containing bibliographic data

- tags:

  A character vector containing RIS tags.

- df:

  a data.frame containing bibliographic data

## Value

`detect_parser` and `detect_delimiter` return a length-1 character;
`detect_year` returns a character vector listing estimated publication
years; and `detect_lookup` returns a `data.frame`.
