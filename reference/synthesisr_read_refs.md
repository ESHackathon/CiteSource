# Import bibliographic search results

Imports common bibliographic reference formats (i.e. .bib, .ris, or
.txt).

## Usage

``` r
synthesisr_read_refs(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE,
  select_fields = NULL
)

read_ref(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE,
  select_fields = NULL
)
```

## Arguments

- filename:

  A path to a filename or vector of filenames containing search results
  to import.

- tag_naming:

  Either a length-1 character stating how should ris tags be replaced
  (see details for a list of options), or an object inheriting from
  class `data.frame` containing user-defined replacement tags.

- return_df:

  If TRUE (default), returns a data.frame; if FALSE, returns a list.

- verbose:

  If TRUE, prints status updates (defaults to FALSE).

- select_fields:

  Character vector of fields to be retained. If NULL, all fields from
  the RIS file are returned

## Value

Returns a data.frame or list of assembled search results.

## Details

The default for argument `tag_naming` is `"best_guess"`, which estimates
what database has been used for ris tag replacement, then fills any gaps
with generic tags. Any tags missing from the database (i.e.
`code_lookup`) are passed unchanged. Other options are to use tags from
Web of Science (`"wos"`), Scopus (`"scopus"`), Ovid (`"ovid"`) or
Academic Search Premier (`"asp"`). If a `data.frame` is given, then it
must contain two columns: `"code"` listing the original tags in the
source document, and `"field"` listing the replacement column/tag names.
The `data.frame` may optionally include a third column named `"order"`,
which specifies the order of columns in the resulting `data.frame`;
otherwise this will be taken as the row order. Finally, passing `"none"`
to `replace_tags` suppresses tag replacement.

## Functions

- `read_ref()`: Import a single file
