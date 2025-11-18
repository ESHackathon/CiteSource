# Export data frame to RIS file

This function saves a data frame as a RIS file with specified columns
mapped to RIS fields. Note that *existing files are overwritten without
warning.*

## Usage

``` r
export_ris(
  citations,
  filename = "citations.ris",
  source_field = "DB",
  label_field = "C7",
  string_field = "C8"
)
```

## Arguments

- citations:

  Dataframe to be exported to RIS file

- filename:

  Name (and path) of file, should end in .ris

- source_field:

  Field in `citations` representing the source. Default is "DB".

- label_field:

  Field in `citations` representing the label. Default is "C7".

- string_field:

  Field in `citations` representing additional string information.
  Default is "C8".

## Examples

``` r
if (interactive()) {
  # Load example data from the package
  examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
  examplecitations <- readRDS(examplecitations_path)
  dedup_results <- dedup_citations(examplecitations, merge_citations = TRUE)
  export_ris(
   dedup_results$unique, 
    "cite_sources.ris", 
   user_mapping = list(
    "DB" = "cite_source_include", 
    "C7" = "cite_label_include"
  )
  )
}
```
