# CiteSource
<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" style="float:right; height:200px;">


<!-- badges: start -->
[![R-CMD-check](https://github.com/ESHackathon/CiteSource/workflows/R-CMD-check/badge.svg)](https://github.com/ESHackathon/CiteSource/actions)
<!-- badges: end -->

**About CiteSource**
CiteSource was developed in order to provide researchers the ability to examine the utility and efficacy of literature resources and search methodologies. The idea behind CiteSource is simply allowing users to deduplicate citation records, while maintaining customizable metadata about the citation.

Customizable metadata can include anything from a resource name (ex.Web of Science, LENS.org, PubMed), a method (database search, handsearching, citation snowballing), a variation used within a method (WoS string #1, Wos string #2, WoS string #3), a research phase (search, Ti/Ab screening, Full-text Screening), or a unique group of citations (benchmarking articles, articles from a previous review, articles with specific a affiliation). 

The CiteSource deduplication process is better described as record merging process due to the fact that the customizable metadata from duplicate records is maintained through the creation of a single, primary record. Beyond the merging of customizable metadata, the primary record is created by using the most complete metadata available between duplicate records (currently DOI and Abstract fields).

Once records are deduplicated, users are able to easily create plots and tables in order to answer specific questions or to simply explore the data to an effort to develop new hypotheses. Examples of analysis may include how many unique records a specific source contributed or how the traditional method of developing a search string and applying it across databases fared against a new AI discovery tool in discovering relevant articles. Users may want to understand the overlap in records between two different search strings or evaluate the impact of including Google Scholar in a review. Before searching, a user may even develop a targeted search to better understand the topical coverage across databases they intend to search, and once the search has been developed, how particular source, string, or method performed in discovering benchmarking articles. 

**Vignettes**
Vignettes covering various use cases can be found on the CiteSource web page https://www.eshackathon.org/CiteSource/. 

**Feedback**
If you would like to develop a vignette or would like to provide comments/suggestions, reach out to us on the CiteSource discussion page https://github.com/ESHackathon/CiteSource/discussions.

**Development and license**
Development of this project began as part of the Evidence Synthesis Hackathon and as part of Evidence Synthesis & Meta-Analysis in R Conference - ESMARConf 2022. to learn more about this awesome conference and hackathon please visit @ https://esmarconf.org/

CiteSource was created under the General Public License (>=v3) find more information on GPL @ https://www.gnu.org/licenses/gpl-3.0.html


**Use Case Overview**

**Source & Method Analysis**

When teams are selecting databases for inclusion in a review it can be extremely difficult to determine the best resources and determine the ROI in terms of the time it takes to apply searches. This is especially true in environmental research where research is often cross-disciplinary. By tracking where/how each citation was found, the evidence synthesis community could in turn track the efficacy of various databases and identify the most relevant resources as it relates to their research topic. This idea can be extended to search string comparison as well as strategy and methodology comparison and hedge validation.

-Database/Platform/Index
-Methodology
-Search string/strategy
-Hedge validation

Examples:

Databases/Database: GreenFile vs. CAB Direct vs. Aquatic Sciences and Fisheries Abstracts (ASFA) vs. Water Resources Abstracts
Platform/Indexes: Web of Science- Science Citation Index Expanded (YR-YR) vs. Core Collection vs. "ALL Databases" (YR-YR)
Search Engine/Database: Google scholar vs. Web of Science
Methodology/Methodology: Hand searching vs. citation chasing vs. naive string
String/strategy: ASFA string 1 vs. ASFA string 2 vs. ASFA string 3

**Stage/Topic Analysis**

Once the title and absract screening has been complete OR once the final papers the final literature has been selected, users can analyze the contributions of each Source/Method to better understand its impact on the review. By using the "Source" data along with the "Tag" data, users can analyze the number of overlapping/unique records from each source or method.

-Review Phase (change in corpus over time/stages)
-Topic based (tags added in screening phase)

Examples: 

Databases/Database & (Stage Analysis): GreenFile (Search, Screen, Final) Aquatic Sciences and Fisheries Abstracts (Search, Screen, Final)
Databases/Database & (Topic Analysis): GreenFile (life history, genetics, stock dynamics) Aquatic Sciences and Fisheries Abstracts (life history, genetics, stock dynamics)

**Metadata Enhancement**

Beyond the ability to analyze and track an idividual citations' Source/Methods. The functions behind CiteSource provides users with the ability to create a single record that uses prefered metadata in selecte metadata fields. Selection is based on metadata attributes (filled or empty, length). This function alone will help to resolve many hours of metadata entry and ensure that each record is as complete as possible. At some point in the future users may be able to select Source data as prefered over these rules.

Basic logic for metadata selection (Filled/Empty + Length)

**IF** metadata from one record’s DOI field contains text and the others’ do not **THEN** the metadata from the record with text will be used for the merge record

**IF** metadata from one record’s ABSTRACT field contains more text and the other records with ABSTRACT text **THEN** the metadata from the record with more text will be used for the merge record

**Library Collection Development**

As the community continues to apply this analysis to resources, this information would be extremely valuable to researchers and librarians outside of teh Evidence Synthesis community. From the library's perspective this tool could have a substantial impact on collection development decisions and resource recommendations. 

Examples:

Database Coverage: Compare coverage of databases on specific research topics - What percent of results are duplicated in current subscriptions? What percent are unique? 

