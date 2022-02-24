# CiteSource
<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" width="200px" align="right">

CiteSource is an R package and Shiny app that is currently in development. Work on this project began as part of the Evidence Synthesis Hackathon and as part of ESMARConf 2022.

CiteSource provides users with the ability to deduplicate references while maintaining customizable metadata. Instead of the traditional deduplication method where records are removed and only one record is selected to be retained, CiteSource retains eac duplicate record while merging metadata into a single master record. This master record maintains user-customized metadata in two fields, "Source" and "Tag". In the merging process, select metadata fields are also automatically compared (currently DOI & Abstract) and the most complete metadata is used in the master record. 


**USE CASE TYPES & EXAMPLES**

**Source/Method Analysis**

When teams are selecting databases for inclusion in a review it can be extremely difficult to determine the best resources and determine the ROI in terms of the time it takes to apply searches. This is especially true in environmental research where research is often cross-disciplinary. By tracking where/how each citation was found, the evidence synthesis community could in turn track the efficacy of various databases and identify the most relevant resources as it relates to their research topic. This idea can be extended to search string comparison as well as strategy and methodology comparison.

-Database/Platform/Index

-Methodology

-Search string/strategy

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

**Metadata Enhancement + Time Savings**

Beyond the ability to analyze and track an idividual citations' Source/Methods. The functions behind CiteSource provides users with the ability to create a single record that uses prefered metadata in selecte metadata fields. Selection is based on metadata attributes (filled or empty, length). This function alone will help to resolve many hours of metadata entry and ensure that each record is as complete as possible. At some point in the future users may be able to select Source data as prefered over these rules.

Basic logic for metadata selection (Filled/Empty + Length)

**IF** metadata from one record’s DOI field contains text and the others’ do not **THEN** the metadata from the record with text will be used for the merge record

**IF** metadata from one record’s ABSTRACT field contains more text and the other records with ABSTRACT text **THEN** the metadata from the record with more text will be used for the merge record

**Library Collection Development**

As the community continues to apply this analysis to resources, this information would be extremely valuable to researchers and librarians outside of teh Evidence Synthesis community. From the library's perspective this tool could have a substantial impact on collection development decisions and resource recommendations. 

Examples:

Database Coverage: Compare coverage of databases on specific research topics - What percent of results are duplicated in current subscriptions? What percent are unique? 

