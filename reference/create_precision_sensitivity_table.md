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
# Example usage with a sample dataset
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

Distinct Records¹

Final Included²

Precision³

Sensitivity/Recall⁴

Screened Included⁵

Source1

100

80

80

40

90

Source2

150

120

80

60

140

Total

⁶ 250

⁷ 200

⁸ 80

100

⁹ 230

¹ Number of records after internal source deduplication.

² Number of citations included after full text screening.

³ Number of final included citations / Number of distinct records.

⁴ Number of final included citations / Total number of final included
citations.

⁵ Number of citations included after title/abstract screening.

⁶ Total citations discovered (after internal and cross-source
deduplication).

⁷ Total citations included after full text screening.

⁸ Overall Precision = Number of final included citations / Total
distinct records.

⁹ Total citations included after Ti/Ab Screening.
