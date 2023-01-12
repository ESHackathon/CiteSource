<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" width="200" align="right"/>
CiteSource provides users with the ability to deduplicate references while maintaining customizable metadata. Instead of the traditional deduplication method where records are removed and only one duplicate record is selected to be retained, duplicate records are instead merged into a single record. This single record maintains user-customized metadata in two fields, Source and Tag. In the process of merging records, select metadata fields are also automatically compared (currently DOI & Abstract) and the field from record with the most complete metadata is used to create the merged record. Below are a few examples of general use cases, followed by specific examples.


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
