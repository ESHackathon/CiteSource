# precision_sensitivity_table

This function creates a gt table from a given data, and removes the
'screened' column and its associated footnotes if all its values are
zero.

## Usage

``` r
precision_sensitivity_table(data)
```

## Arguments

- data:

  A data.frame. The dataset to build the table from. It should contain
  the columns 'screened', 'final', 'Precision', 'Recall'.

## Value

A gt object representing the table.
