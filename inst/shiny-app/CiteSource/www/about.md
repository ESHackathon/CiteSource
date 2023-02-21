<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" width="200" align="right"/>

CiteSource was developed in order to provide researchers the ability to examine the utility and efficacy of literature resources and search methodologies. The idea behind CiteSource is simply allowing users to deduplicate citation records, while maintaining customizable metadata about the citation.

Customizable metadata can include anything from a resource name (ex.Web of Science, LENS.org, PubMed), a method (database search, handsearching, citation chasing/ snowballing), a variation used within a method (WoS string #1, Wos string #2, WoS string #3), a research phase (search, TI/AB screening, Full-text Screening), or a unique group of citations (benchmarking articles, articles from a previous review, articles with a specific author affiliation).

The CiteSource deduplication process is better described as record merging process due to the fact that the customizable metadata from duplicate records is maintained through the creation of a single, primary record. Beyond the merging of customizable metadata, the primary record is created by using the most complete metadata available between duplicate records (currently DOI and Abstract fields).

Once records are deduplicated, users are able to easily create plots and tables in order to answer specific questions or to simply explore the data to an effort to develop new hypotheses. Examples of analysis may include how many unique records a specific source contributed or how traditional methods of searching fare against a new AI discovery tool in discovering relevant articles. Users may want to understand the overlap in records between two different search strings or evaluate the impact of including Google Scholar in a review. Before searching, a user may even develop a targeted search to better understand the topical coverage across databases that they intend to search, and once the search has been developed, how particular source, string, or method performed in discovering benchmarking articles.

**Source/Method Analysis**  
Analyze the number and percent of overlapping & unique citations between multiple .ris files 

- Database/Platform/Index
- Methodology 
- Search string/strategy

Examples:

- Databases/Database: GreenFile vs. CAB Direct vs. Aquatic Sciences and Fisheries Abstracts (ASFA) vs. Water Resources Abstracts
- Platform/Indexes: Web of Science- Science Citation Index Expanded (YR-YR) vs. Core Collection vs. "ALL Databases" (YR-YR)
- Search Engine/Database: Google scholar vs. Web of Science 
- Methodology/Methodology: Hand searching vs. citation chasing vs. naive string
- String/strategy: ASFA string 1 vs. ASFA string 2 vs. ASFA string 3

*Visual example placeholder*
![percent](https://user-images.githubusercontent.com/89118428/155384690-c697d5da-f7d5-4c16-981f-13b7e8c222f8.jpg)
![percent](https://user-images.githubusercontent.com/89118428/155399396-f7f8d65c-e6c0-4c30-b40e-eda14cb17c84.png)
![overlap](https://user-images.githubusercontent.com/89118428/155399532-000230e1-6582-4c4c-8f1b-5a027fd542e3.png)


**Stage/Topic Analysis**
Analyze the number and percent of overlapping & unique citations between multiple .ris files to see changes over review stages (initial search results, post-screening, final included sources) OR to drill down and understand overlapping/unique content as it relates to topics or variables (for instance if citations are tagged during screening for a systematic map, the user can better understand how each databases contributed to the literature base - methodology, geography, etc.)

- Review Phase (change in corpus over time/stages)
- Topic based

Examples:
Databases/Database (Stage Analysis):

GreenFile (Search, Screen, Final) 
Aquatic Sciences and Fisheries Abstracts (Search, Screen, Final)

*Visual example placeholder*
![chrome-capture (4)](https://user-images.githubusercontent.com/89118428/155384374-b70a75eb-ab15-406f-86dd-a4cb13ca2177.jpg)





**Metadata Enhancement**
CiteSource provides you with the ability to create a single record and include prefered metadata for selected fields based on metadata attributes (filled or empty, length). At some point in the future users may be able to select Source data as prefered over these rules.

Basic logic for metadata selection (Filled/Empty + Length)

**IF** metadata from one record’s DOI field contains text and the others’ do not 
**THEN** the metadata from the record with text will be used for the merge record

**IF** metadata from one record’s ABSTRACT field contains more text and the other records with ABSTRACT text
**THEN** the metadata from the record with more text will be used for the merge record
