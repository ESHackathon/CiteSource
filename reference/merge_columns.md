# Bind two or more data frames with different columns

Takes two or more data.frames with different column names or different
column orders and binds them to a single data.frame.

## Usage

``` r
merge_columns(x, y)
```

## Arguments

- x:

  Either a data.frame or a list of data.frames.

- y:

  A data.frame, optional if x is a list.

## Value

Returns a single data.frame with all the input data frames merged.
