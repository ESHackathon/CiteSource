# Assessing Usefulness of Databases for Evidence Synthesis

## About this vignette

In the process of developing search strategies for evidence synthesis,
it is standard practice to test different versions of a search against a
set of already known relevant studies — benchmark studies. In this way,
the right balance between precision and sensitivity can be achieved
prior to screening.

Until now, this within-database testing has been the primary method of
pre-screening search validation. With CiteSource, we can test search
strategies across databases to assess the usefulness of certain
databases before finalizing our database set. This vignette provides a
workflow for testing a search strategy across multiple databases and
against a set of benchmark studies.

In this example, we are running a search about loneliness and gambling
addiction. We developed a search strategy for PsycInfo, our main
database, and want to see if searching Web of Science and PubMed adds
useful records and helps us find more of our benchmark studies.

## Installation and setup

``` r

#install.packages("CiteSource")
library(CiteSource)
```

## Import files from multiple sources

Here we import three database searches and a set of benchmark studies.
The benchmark file is assigned `cite_source = NA` since it does not
represent a database search, and `cite_label = "benchmark"` to identify
it as the reference set.

``` r

citation_files <- list.files(path = "valid_data", pattern = "\\.ris", full.names = TRUE)
citation_files
#> [1] "valid_data/benchmark.ris"   "valid_data/psycinfo_64.ris"
#> [3] "valid_data/pubmed_46.ris"   "valid_data/WoS_79.ris"

citations <- read_citations(citation_files,
                            cite_sources = c(NA, "psycinfo", "pubmed", "wos"),
                            cite_labels  = c("benchmark", "search", "search", "search"),
                            tag_naming   = "best_guess")
#> Note: the following cite_label value(s) are not in the standard vocabulary (search / screened / final): benchmark. Phase-analysis functions expect these exact labels.
#> Import completed - with the following details:
#>              file cite_source cite_string cite_label citations
#> 1   benchmark.ris        <NA>        <NA>  benchmark        13
#> 2 psycinfo_64.ris    psycinfo        <NA>     search        64
#> 3   pubmed_46.ris      pubmed        <NA>     search        46
#> 4      WoS_79.ris         wos        <NA>     search        79
```

## Deduplication and source information

CiteSource merges duplicate records while preserving the `cite_source`
and `cite_label` metadata fields, so the origin of each record is
retained through deduplication.

``` r

unique_citations <- dedup_citations(citations)
n_unique         <- count_unique(unique_citations)
source_comparison <- compare_sources(unique_citations, comp_type = "sources")
```

## Plot heatmap to compare source overlap

### Heatmap by number of records

A heatmap shows the total number of records from each database and the
count of overlapping records for each pair. Web of Science yielded the
highest number of records on gambling addiction and loneliness; PubMed
the least.

``` r

plot_source_overlap_heatmap(source_comparison)
```

![](citesource_vignette_db-pre-screen_validation_files/figure-html/unnamed-chunk-4-1.png)

### Heatmap by percentage of records

The percentage heatmap shows what share of each row’s records were also
found in each column. Here, 55% of Web of Science records were also
found in PsycInfo, while 44% of PsycInfo records were found in Web of
Science.

``` r

plot_source_overlap_heatmap(source_comparison, plot_type = "percentages")
```

![](citesource_vignette_db-pre-screen_validation_files/figure-html/unnamed-chunk-5-1.png)

## Plot an upset plot to compare source overlap

An upset plot provides more detail about shared and unique records
across all source combinations. Web of Science had the most unique
records not found in any other database (n=29); PubMed had only four
unique records. Twenty-four records were found in every database.

``` r

plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
```

![](citesource_vignette_db-pre-screen_validation_files/figure-html/unnamed-chunk-6-1.png)

## Bar plots of unique and shared records

[`plot_contributions()`](https://eshackathon.github.io/CiteSource/reference/plot_contributions.md)
visualizes unique and shared record counts by source, and can include
the benchmark label to show how each database contributed to the
benchmark set.

``` r

plot_contributions(n_unique, center = TRUE)
```

![](citesource_vignette_db-pre-screen_validation_files/figure-html/unnamed-chunk-7-1.png)

## Analyzing unique contributions

To examine which records are exclusive to each database, filter
`n_unique` for `unique == TRUE` and rejoin with `unique_citations` to
recover full bibliographic data.

``` r

unique_psycinfo <- n_unique |>
  dplyr::filter(cite_source == "psycinfo", unique == TRUE) |>
  dplyr::inner_join(unique_citations, by = "duplicate_id")

unique_pubmed <- n_unique |>
  dplyr::filter(cite_source == "pubmed", unique == TRUE) |>
  dplyr::inner_join(unique_citations, by = "duplicate_id")

unique_wos <- n_unique |>
  dplyr::filter(cite_source == "wos", unique == TRUE) |>
  dplyr::inner_join(unique_citations, by = "duplicate_id")

# To export for manual review:
# export_csv(unique_pubmed, "pubmed_unique.csv")
```

### Record-level table

Filtering `unique_citations` to only the benchmark records and passing
to
[`record_level_table()`](https://eshackathon.github.io/CiteSource/reference/record_level_table.md)
shows which databases contained each benchmark study.

``` r

unique_citations |>
  dplyr::filter(stringr::str_detect(cite_label, "benchmark")) |>
  record_level_table(return = "DT")
```

### Search summary table

[`citation_summary_table()`](https://eshackathon.github.io/CiteSource/reference/citation_summary_table.md)
calculates sensitivity and precision scores for each database against
the benchmark set, providing a concise overview of each source’s
performance before screening begins.

``` r

citation_summary_table(unique_citations, screening_label = "benchmark")
```

[TABLE]

## Exporting for further analysis

CiteSource can export deduplicated results as CSV, RIS, or BibTeX files,
and reimport them to resume analysis later.

``` r

#export_csv(unique_citations, filename = "unique-by-source.csv", separate = "cite_source")
#export_ris(unique_citations, filename = "unique_citations.ris", source_field = "DB", label_field = "N1")
#export_bib(unique_citations, filename = "unique_citations.bib", include = c("sources", "labels", "strings"))
#reimport_csv("unique-by-source.csv")
```

## In summary

CiteSource can evaluate the usefulness of different databases against a
set of benchmark studies before screening begins. In this example, both
PsycInfo and Web of Science made unique contributions to the benchmark
set and had a significant proportion of unique records. PubMed did not
contribute any unique benchmark records and mostly overlapped with the
other two databases — providing evidence that it may not be an effective
addition for this topic.
