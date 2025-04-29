## CiteSource User Guide

<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" width="200" style="float: right; margin-left: 20px; margin-bottom: 10px;"/>

> CiteSource has a number of applications. This guide walks users through the step-wise process of uploading, deduplicating and analyzing data > within the shiny application. For step by step instructions for running CiteSource in R, [check out our vignettes](https://www.eshackathon.org/CiteSource/articles/)
---

### Using CiteSource: Step-by-Step

<details>
  <summary><strong>Step 1: File Upload, Labeling, & Re-importing</strong></summary>

> **Standard Upload:**
>
> * Navigate to the 'File upload' tab.
> * Use the 'Set Label for Uploaded File(s)' dropdown to select the appropriate stage for the file(s) you are about to upload (e.g., `search`, `screened`, or `final`). This label helps organize records, especially for phase analysis and some summary tables.
> * Click the file input area ('Browse...') to select one or more citation files from your computer. Supported formats are `.ris`, `.bib`, and `.txt`.
> * The label you selected will be applied to all citation records within the file(s) uploaded in that specific action.
>
> **Re-importing Previously Processed Data:**
>
> * If you have previously exported data from CiteSource as a `.ris` or `.csv` file (these exported files contain special `cite_` columns), you can re-upload this file directly.
> * On the 'File upload' tab, use the 'OR: Re-upload an .ris or .csv exported from CiteSource' file input.
> * This bypasses the initial upload processing and deduplication steps (Steps 3 & 4), allowing you to proceed directly to the 'Visualise' and 'Tables' tabs with your previously processed data.
>
> * **NOTE**: Raw citation exports from some platforms (e.g.OVID) may be incompatible due to abnormal .ris field use or structuring. If you are having issues, please be sure to try importing them using citation software (e.g. Zotero, EndNote) and exporting them before uploading to CiteSource.*

</details>

<details>
  <summary><strong>Step 2: Review Uploads & Edit</strong></summary>

> * After uploading citations, a summary table appears in the main panel showing each file, its detected record count, and the assigned source name, label, and string.
> * To correct the auto-assigned source name, or to change the label or string for *all* records from a specific file after upload, you can double-click the corresponding cell in the table and type the new value.

</details>

<details>
  <summary><strong>Step 3: Automated Deduplication</strong></summary>

> * Navigate to the 'Deduplicate' tab and ensure you are on the 'Automated deduplication' sub-tab.
> * Click the 'Find duplicates' button.
> * CiteSource will process all the records you've uploaded. It compares metadata fields (like DOI, title, authors, journal, year, volume, pages) to identify potential duplicates both *within* the same source file (internal deduplication) and *across* different source files (external deduplication).
> * A pop-up message will summarize the results, indicating the number of unique records found and if any potential duplicates require manual review.

</details>

<details>
  <summary><strong>Step 4: Manual Deduplication (If Needed)</strong></summary>

> * If the summary message from Step 3 indicates potential duplicates need review, or if you want to manually inspect potential matches, go to the 'Manual deduplication' sub-tab.
> * Pairs of records identified as potential duplicates are displayed. Each row represents a pair, showing selected metadata side-by-side (e.g., Title 1 vs. Title 2).
> * Use the 'Choose columns' filter dropdown (filter icon) above the table to select which metadata fields (e.g., author, year, abstract) you want to see for comparison.
> * Carefully review each pair. If you determine a pair represents the *same* underlying citation, click on that row to select it.
> * After selecting all rows that are true duplicates, click the 'Remove additional duplicates' button (this button only appears after you select at least one row). This merges the selected pairs, keeping only one unique record with combined metadata.
> * If you finish reviewing or decide no manual merging is needed, click 'Go to visualisations'.

</details>

<details>
  <summary><strong>Step 5: Visualise Overlap</strong></summary>

> * Navigate to the 'Visualise' tab.
> * Use the sidebar controls to tailor the analysis:
>     * **Choose comparison type:** Select whether you want to compare overlap based on 'sources' (original files/databases), 'labels' (e.g., search vs screened), or 'strings' (if used).
>     * **Filter data:** Select specific sources, labels, or strings to include in the visualizations.
> * Explore the generated plots:
>     * **Heatmap:** This matrix shows pairwise overlap. Each cell represents the number of citations shared between two groups (the groups depend on your chosen comparison type). Darker cells indicate higher overlap. Hover over cells to see exact counts. It helps quickly identify pairs with significant commonality.
>     * **Upset Plot:** This plot visualizes intersections among multiple groups simultaneously. The large bottom bar chart shows the number of citations unique to specific combinations of groups (e.g., found only in Source A, or found in both Source A and B but not C). The smaller top bar chart shows the total number of unique citations in each individual group. It's excellent for understanding complex overlap patterns involving more than two groups.
>     * **Phase Analysis Plot:** This plot is most useful when comparing by 'labels' representing stages (e.g., `search`, `screened`, `final`). It shows the total number of records at each stage, broken down into those that are unique (first identified at that stage) versus those that were already found in a previous stage (duplicates relative to earlier stages). It helps visualize the yield and deduplication effectiveness across a review workflow.
> * Use the 'Download' buttons above each plot to save them as image files.

</details>

<details>
  <summary><strong>Step 6: Summary Tables & Record Review</strong></summary>

> * Navigate to the 'Tables' tab.
> * Use the sidebar filters (Sources, Labels, Strings) to select the subset of data you want summarized.
> * Generate specific summary tables by clicking the corresponding 'Generate...' button:
>     * **Initial Records Table:** Provides a high-level count based on the earliest phase (typically records labeled `search`). Shows the total uploaded records for that phase. This table distinguishes between the number of uploaded records andduplicates found *within* the each source file. 
>     * **Detailed Record Table:** Breaks down the citation counts by individual source/method (within your selected filters). For each set of records, it shows how many citations were unique to that set and how many were also found in other sets. This helps identify which sources/methods contributed the most unique records and which have a high level of overlap.
>     * **Precision/Sensitivity Table:** Calculates performance metrics, requiring data labeled as `final` to be present and selected. It compares each source, method, or search string against this 'final' set. 'Precision' tells you what proportion of records retrieved by a source were actually relevant ('final' records). 'Sensitivity' (or Recall) tells you what proportion of all relevant ('final') records were found by that specific source. Useful for evaluating search strategy performance.
>     * **Review individual records:** Click 'Generate the table' on the "Review individual records" sub-tab to view the detailed, deduplicated citation list. This table may take a while to load if you have a large number of records. 
>
> **Using the Interactive Record Table:**
>
>     * **Expand/Collapse Row:** Click the `⊕` symbol in a row to view the full APA reference. Click `⊖` to hide it again.
>     * **Sort by Single Column:** Click any column header (like 'Citation' or a source name) to sort the table by that column's values. Click the header again to reverse the sort order.
>     * **Sort by Multiple Columns:** Click the primary column header you want to sort by. Then, hold down the **Shift** key on your keyboard and click a second column header. You can repeat this for more sorting levels.
>     * **Filter/Search:** Type into the search box located at the top-right of the table to dynamically filter records based on any information displayed.
>     * **Download Data:** Click the 'Download CSV' button (located above the table, next to 'Print') to save the data currently shown in the table (including applied filters) as a CSV file.

</details>

<details>
  <summary><strong>Step 7: Export Results</strong></summary>

> * Navigate to the 'Export' tab.
> * This tab becomes active after you have run the deduplication process (Step 3).
> * Click the button corresponding to your desired file format: 'Download csv', 'Download RIS', or 'Download BibTex'.
> * The custom metadata is embedded directly into fields within the export files (e.g., using C1, C2, C7, C8, DB fields in `.ris` format)
> * This will save the final dataset of unique citations (after both automated and any manual deduplication). 
> * **Note:** Only `.csv` and `.ris` files can be re-imported later.

</details>

---