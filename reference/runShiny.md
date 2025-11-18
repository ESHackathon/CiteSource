# A wrapper function to run Shiny Apps from `CiteSource`.

Running this function will launch the CiteSource shiny app

## Usage

``` r
runShiny(app = "CiteSource", offer_install = interactive())
```

## Arguments

- app:

  Defaults to CiteSource - possibly other apps will be included in the
  future

- offer_install:

  Should user be prompted to install required packages if they are
  missing?

## Value

CiteSource shiny app

## Examples

``` r
if (interactive()) {
  # To run the CiteSource Shiny app:
  runShiny()
}
```
