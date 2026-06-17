# Record-level table

Creates a per-record table that shows which sources (and/or
labels/strings) each item was found in.

## Usage

``` r
record_level_table(
  citations,
  include = "sources",
  include_empty = TRUE,
  return = c("tibble", "DT"),
  indicator_presence = NULL,
  indicator_absence = NULL
)
```

## Arguments

- citations:

  A deduplicated tibble as returned by
  [`dedup_citations()`](https://eshackathon.github.io/CiteSource/reference/dedup_citations.md).

- include:

  Which metadata should be included in the table? Defaults to 'sources',
  can be replaced or expanded with 'labels' and/or 'strings'

- include_empty:

  Should records with empty metadata (e.g., no information on 'sources')
  be included in the table? Defaults to FALSE.

- return:

  Either a `tibble` that can be exported, e.g. as a csv, or a DataTable
  (`DT`) that allows for interactive exploration. Note that the
  DataTable allows users to download a .csv file; in that file, presence
  and absence is always indicated as TRUE and FALSE to prevent issues
  with character encodings.

- indicator_presence:

  How should it be indicated that a value is present in a
  source/label/string? Defaults to TRUE in tibbles and a tickmark in DT
  tables

- indicator_absence:

  How should it be indicated that a value is *not* present in a
  source/label/string? Defaults to FALSE in tibbles and a cross in DT
  tables

## Value

A tibble or DataTable containing the per-record table that shows which
sources (and/or labels/strings) each item was found in.

## Examples

``` r
# Load example data from the package
examplecitations_path <- system.file("extdata", "examplecitations.rds", package = "CiteSource")
examplecitations <- readRDS(examplecitations_path)

# Deduplicate citations and compare sources
unique_citations <- dedup_citations(examplecitations)
#> formatting data...
#> Warning: Search contains missing values for the record_id column. A record_id will be created using row numbers
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> 165 citations loaded...
#> 67 duplicate citations removed...
#> 98 unique citations remaining!

unique_citations |> 
dplyr::filter(stringr::str_detect(cite_label, "final"))  |> 
record_level_table(return = "DT")

{"x":{"filter":"none","vertical":false,"extensions":["Buttons"],"caption":"<caption style=\"caption-side: bottom; text-align: left;\">Click on the &amp;oplus; to view the full reference<\/caption>","data":[["&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;","&oplus;"],["Beckmann et al. (2021)","Bu et al. (2023)","Graham et al. (2016)","Johnson et al. (2019)","Keeler & Kristovich (2012)","Keith et al. (2021)","P. Singh et al. (2022)","Son et al. (2022)","Stanganelli & Soravia (2012)","Tsoka et al. (2021)","Turner et al. (2022)","C. Wang et al. (2019)","Zhou et al. (2020)","Zoran et al. (2016)","Zoran et al. (2019)"],["Beckmann, S. K., Hiete, M. & Beck, C. (2021). Threshold temperatures for subjective heat stress in urban apartments—Analysing nocturnal bedroom temperatures during a heat wave in Germany. <i>Climate Risk Management<\/i><i>, 32<\/i>. <a href=\"https://doi.org/10.1016/j.crm.2021.100286\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.crm.2021.100286<\/a>","Bu, F., Yan, D., Tan, G., Sun, H. & An, J. (2023). Acceleration algorithms for long-wavelength radiation integral in the annual simulation of radiative cooling in buildings. <i>Renewable Energy<\/i><i>, 202<\/i>. <a href=\"https://doi.org/10.1016/j.renene.2022.11.091\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.renene.2022.11.091<\/a>","Graham, D. A., Vanos, J. K., Kenny, N. & Brown, R. D. (2016). The relationship between neighbourhood tree canopy cover and heat-related ambulance calls during extreme heat events in Toronto, Canada. <i>Urban Forestry & Urban Greening<\/i><i>, 20<\/i>. <a href=\"https://doi.org/10.1016/j.ufug.2016.08.005\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.ufug.2016.08.005<\/a>","Johnson, J. C., Urcuyo, J., Moen, C. & Stevens, D. R. (2019). Urban heat island conditions experienced by the Western black widow spider (Latrodectus hesperus): Extreme heat slows development but results in behavioral accommodations. <i>Plos One<\/i><i>, 14<\/i>(9). <a href=\"https://doi.org/10.1371/journal.pone.0220153\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1371/journal.pone.0220153<\/a>","Keeler, J. M. & Kristovich, D. A. R. (2012). Observations of Urban Heat Island Influence on Lake-Breeze Frontal Movement. <i>Journal Of Applied Meteorology And Climatology<\/i><i>, 51<\/i>(4). <a href=\"https://doi.org/10.1175/jamc-d-11-0166.1\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1175/jamc-d-11-0166.1<\/a>","Keith, L., Iroz-Elardo, N., Austof, E., Sami, I. & Arora, M. (2021). Extreme heat at outdoor COVID-19 vaccination sites. <i>The Journal Of Climate Change And Health<\/i><i>, 4<\/i>. <a href=\"https://doi.org/10.1016/j.joclim.2021.100043\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.joclim.2021.100043<\/a>","Singh, P., Chaudhuri, A. S., Verma, P., Singh, V. K. & Meena, S. R. (2022). Earth observation data sets in monitoring of urbanization and urban heat island of Delhi, India. <i>Geomatics, Natural Hazards And Risk<\/i><i>, 13<\/i>(1). <a href=\"https://doi.org/10.1080/19475705.2022.2097452\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1080/19475705.2022.2097452<\/a>","Son, J., Eum, J. & Kim, S. (2022). Wind corridor planning and management strategies using cold air characteristics: The application in Korean cities. <i>Sustainable Cities And Society<\/i><i>, 77<\/i>. <a href=\"https://doi.org/10.1016/j.scs.2021.103512\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.scs.2021.103512<\/a>","Stanganelli, M. & Soravia, M. (2012). Connections between Urban Structure and Urban Heat Island Generation: An Analysis trough Remote Sensing and GIS. <i>Computational Science And Its Applications – Iccsa 2012<\/i>. <a href=\"https://doi.org/10.1007/978-3-642-31075-1_45\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1007/978-3-642-31075-1_45<\/a>","Tsoka, S., Velikou, K., Tolika, K. & Tsikaloudaki, A. (2021). Evaluating the Combined Effect of Climate Change and Urban Microclimate on Buildings' Heating and Cooling Energy Demand in a Mediterranean City. <i>Energies<\/i><i>, 14<\/i>(18). <a href=\"https://doi.org/10.3390/en14185799\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.3390/en14185799<\/a>","Turner, V. K., French, E. M., Dialesandro, J., Middel, A., Hondula, D. M., Weiss, G. B. & Abdellati, H. (2022). How are cities planning for heat? Analysis of United States municipal plans. <i>Environmental Research Letters<\/i><i>, 17<\/i>(6). <a href=\"https://doi.org/10.1088/1748-9326/ac73a9\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1088/1748-9326/ac73a9<\/a>","Wang, C., Wang, Z. & Yang, J. (2019). Urban water capacity: Irrigation for heat mitigation. <i>Computers Environment And Urban Systems<\/i><i>, 78<\/i>. <a href=\"https://doi.org/10.1016/j.compenvurbsys.2019.101397\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.compenvurbsys.2019.101397<\/a>","Zhou, X., Carmeliet, J., Sulzer, M. & Derome, D. (2020). Energy-efficient mitigation measures for improving indoor thermal comfort during heat waves. <i>Applied Energy<\/i><i>, 278<\/i>. <a href=\"https://doi.org/10.1016/j.apenergy.2020.115620\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.apenergy.2020.115620<\/a>","Zoran, M. A., Savastru, R. S., Savastru, D. M. & Dida, A. I. (2016). Impacts of urban growth and heat waves events on the urban heat island in Bucharest city. <i>Remote Sensing Technologies And Applications In Urban Environments<\/i><i>, 10008<\/i>. <a href=\"https://doi.org/10.1117/12.2241360\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1117/12.2241360<\/a>","Zoran, M. A., Savastru, R. S., Savastru, D. M., Tautan, M. N. & Baschir, L. A. (2019). Geospatial and in-situ information for assessment of urban climate. <i>Seventh International Conference On Remote Sensing And Geoinformation Of The Environment (Rscy2019)<\/i><i>, 11174<\/i>. <a href=\"https://doi.org/10.1117/12.2532253\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1117/12.2532253<\/a>"],["&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#10004;","&#x2717;","&#x2717;","&#x2717;","&#10004;","&#x2717;","&#10004;","&#10004;","&#10004;","&#10004;"],["&#10004;","&#10004;","&#x2717;","&#10004;","&#10004;","&#x2717;","&#10004;","&#10004;","&#10004;","&#x2717;","&#10004;","&#x2717;","&#x2717;","&#x2717;","&#x2717;"],["&#x2717;","&#x2717;","&#10004;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;","&#x2717;"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Citation<\/th>\n      <th>.html_reference<\/th>\n      <th>WoS<\/th>\n      <th>DIM<\/th>\n      <th>LENS<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"lengthMenu":[[10,25,50,100,-1],["10","25","50","100","All"]],"columnDefs":[{"visible":false,"searchable":false,"targets":2},{"orderable":false,"className":"details-control","targets":0},{"width":"2em","targets":0},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Citation","targets":1},{"name":".html_reference","targets":2},{"name":"WoS","targets":3},{"name":"DIM","targets":4},{"name":"LENS","targets":5}],"dom":"lBfrtip","buttons":["print",{"extend":"csv","filename":"CiteSource_record_summary","text":"Download csv","exportOptions":{"columns":[1,3,4,5],"modifier":{"page":"all"}}}],"order":[],"autoWidth":false,"orderClasses":false},"callback":"function(table) {\n\n            var refCol = 2;\n            table.column(0).nodes().to$().css({cursor: 'pointer'});\n            table.column(1).nodes().to$().css({cursor: 'pointer'});\n            var format = function(d) {\n              return '<div style=\"background-color:#eee; padding: .5em;\">' +\n                      d[refCol];\n            };\n            var toggleRow = function(tr) {\n              var row = table.row(tr);\n              var icon = $(tr).find('td').eq(0);\n              if (row.child.isShown()) {\n                row.child.hide();\n                icon.html('&oplus;');\n              } else {\n                row.child(format(row.data())).show();\n                icon.html('&CircleMinus;');\n              }\n            };\n            table.on('click', 'td.details-control', function() {\n              toggleRow($(this).closest('tr'));\n            });\n            table.on('click', 'tbody td', function() {\n              var idx = table.cell(this).index();\n              if (idx && idx.column === 1) toggleRow($(this).closest('tr'));\n            });\n}","selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":["callback"],"jsHooks":[]}
```
