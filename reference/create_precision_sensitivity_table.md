# Count and Precision/Sensitivity Table

This function generates a formatted table that displays the precision
and sensitivity (recall) metrics for each citation source, along with
distinct records and phase-specific counts such as "screened" and
"final".

## Usage

``` r
create_precision_sensitivity_table(data)
```

## Arguments

- data:

  A data frame containing phase-specific counts and calculated metrics
  for each citation source. It must include columns such as `Source`,
  `Distinct_Records`, `final`, `Precision`, `Recall`, and optionally
  `screened`.

## Value

A `gt` table object summarizing the precision and sensitivity metrics
for each citation source, with relevant footnotes and labels.

## Details

The function first checks whether all values in the `screened` column
are zero. If so, the column is removed from the table. The table is then
generated using the `gt` package, with labeled columns and footnotes
explaining the metrics.

## Examples

``` r
sample_data <- data.frame(
  Source = c("Source1", "Source2", "Total"),
  Distinct_Records = c(100, 150, 250),
  final = c(80, 120, 200),
  Precision = c(80.0, 80.0, 80.0),
  Recall = c(40.0, 60.0, 100.0),
  screened = c(90, 140, 230)
)
create_precision_sensitivity_table(sample_data)


  


Record Counts & Precision/Sensitivity
```
