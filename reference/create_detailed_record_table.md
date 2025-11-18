# Create a Detailed Record Table

This function generates a formatted summary table using the `gt`
package, which displays detailed counts for each citation source. The
table includes columns for the number of records imported, distinct
records, unique records, non-unique records, and various contribution
percentages. Data from the function calculate_detailed_records is
pre-formatted for this table.

## Usage

``` r
create_detailed_record_table(data)
```

## Arguments

- data:

  A data frame containing the detailed counts for each citation source.
  The data frame must include the following columns:

  - `Source`: The name of the citation source.

  - `Records Imported`: The total number of records imported from the
    source.

  - `Distinct Records`: The number of distinct records after
    deduplication within the source.

  - `Unique Records`: The number of records unique to that source.

  - `Non-unique Records`: The number of records found in at least one
    other source.

  - `Source Contribution %`: The percentage contribution of each source
    to the total distinct records.

  - `Source Unique Contribution %`: The percentage contribution of each
    source to the total unique records.

  - `Source Unique %`: The percentage of records from each source that
    were unique.

## Value

A `gt` table object summarizing the detailed record counts for each
citation source.

## Details

The function checks for the presence of all required columns in the
input data frame. If any required columns are missing, the function
stops and returns an error message specifying the missing columns. This
ensures that the input data is correctly formatted before attempting to
generate the table.

The generated table includes a header and footnotes that provide
additional context for each column, explaining the meaning of the data
presented.

## Examples

``` r
# Example usage with a sample dataset
sample_data <- data.frame(
  Source = c("Source1", "Source2", "Source3", "Total"),
  `Records Imported` = c(100, 150, 250, 500),
  `Distinct Records` = c(90, 140, 230, 460),
  `Unique Records` = c(50, 70, 120, 240),
  `Non-unique Records` = c(40, 70, 110, 220),
  `Source Contribution %` = c("39.1%", "60.9%", "100%", "100%"),
  `Source Unique Contribution %` = c("41.7%", "58.3%", "100%", "100%"),
  `Source Unique %` = c("55.6%", "50%", "52.2%", "52.2%"),
  check.names = FALSE
)

# Create the detailed record table
create_detailed_record_table(sample_data)


  


Record Summary
```

Records Imported¹

Distinct Records²

Unique Records³

Non-unique Records⁴

Source Contribution %⁵

Source Unique Contribution %⁶

Source Unique %⁷

Source1

100

90

50

40

39.1%

41.7%

55.6%

Source2

150

140

70

70

60.9%

58.3%

50%

Source3

250

230

120

110

100%

100%

52.2%

Total

500

⁸ 460

240

220

100%

100%

52.2%

¹ Number of raw records imported from each database.

² Number of records after internal source deduplication.

³ Number of records not found in another source.

⁴ Number of records found in at least one other source.

⁵ Percent distinct records contributed to the total number of distinct
records.

⁶ Percent of unique records contributed to the total unique records.

⁷ Percentage of records that were unique from each source.

⁸ Total citations discovered (after internal and cross-source
deduplication).
