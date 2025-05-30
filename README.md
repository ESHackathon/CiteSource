# CiteSource
<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" style="float:right; height:200px; padding: 10px;">


<!-- badges: start -->
[![R-CMD-check](https://github.com/ESHackathon/CiteSource/workflows/R-CMD-Check/badge.svg)](https://github.com/ESHackathon/CiteSource/actions)
[![Status](https://img.shields.io/badge/Status-Work%20in%20Progress-orange)](https://github.com/ESHackathon/CiteSource)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## About the Package

CiteSource was developed to provide researchers the ability to examine the utility and efficacy of literature resources and search methodologies. The idea behind CiteSource is simply allowing users to deduplicate citation records, while maintaining customizable metadata about the citation.

**Development**

Development of this project began as part of the Evidence Synthesis Hackathon and as part of Evidence Synthesis & Meta-Analysis in R Conference - ESMARConf 2022. to learn more about this awesome conference and hackathon please visit @ https://esmarconf.org/

**License**

CiteSource was created under [the General Public License (>=v3)](https://www.gnu.org/licenses/gpl-3.0.html). 

**Shiny Web Application**

Whether you know R or not, we want you to be able to use CiteSource! Check out our [CiteSource Shiny App!](https://litrev.shinyapps.io/CiteSource/) 

## Features
**Customizable Metadata Tags**

Users can provide customizable metadata in three fields, cite_source, cite_string, and cite_label. Metadata can include anything from a resource name (e.g. Web of Science, LENS.org, PubMed), a method (database search, handsearching, citation snowballing), a variation used within a method (WoS string #1, Wos string #2, WoS string #3), a research phase (search, Ti/Ab screening, Full-text Screening), or a unique group of citations (benchmarking articles, articles from a previous review, articles with a specific author affiliation). 

**Record Merging**

The CiteSource deduplication process is better described as a record merging process due to the fact that the customizable metadata from duplicate records is maintained through the creation of a single, primary record. Beyond the merging of customizable metadata, the primary record is created by using the most complete metadata available between duplicate records (currently DOI and Abstract fields).The ASySD package, developed by Kaitlyn Hair, serves as the backbone of this process.

**Table and Plot Visualizations**

Once records are deduplicated, users are able to easily create plots and tables to answer specific questions or to simply explore the data in an effort to develop new hypotheses. Examples of analysis may include how many unique records a specific source contributed or how traditional methods of searching fare against a new AI discovery tool in finding relevant articles. Users may want to understand the overlap in records between two different search strings or evaluate the impact of including Google Scholar in a review. Before searching, a user may even develop a targeted search to better understand the topical coverage across databases that they intend to search, and once the search has been developed, how a particular source, string, or method performed in discovering benchmarking articles. 

**Exporting and Re-importing Data**

Once records have been processed, users are able to export data in .csv, .ris, and .bib formats. Furthermore, users are able to reimport .csv and .ris files in order to recreate plots and tables.

## Getting Started
**Installation**

Install CiteSource in R with remotes::install_github("ESHackathon/CiteSource")

**Vignettes**

Vignettes covering various use cases can be found on the [CiteSource web page](https://www.eshackathon.org/CiteSource/). 

## Feedback

Be sure to check out [our discussion page](https://github.com/ESHackathon/CiteSource/discussions) to engage with us or to learn more about the various use cases for CiteSource. You can provide comments/suggestions or suggest a vignette for a specific use case. 

