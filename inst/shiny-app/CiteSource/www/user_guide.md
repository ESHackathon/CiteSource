## CiteSource User Guide

<img src="CS.gif" width="200" style="float: right; margin-left: 20px; margin-bottom: 10px;"/>

> CiteSource has a number of applications. This guide walks users through the step-wise process of uploading, deduplicating and analyzing data within the shiny application. For step by step instructions for running CiteSource in R, [check out our vignettes](https://eshackathon.github.io/CiteSource/articles/).
---

<a id="step-1"></a>

### Step 1 — Upload Citation Files

Navigate to the **File upload** tab. Click **Browse** to select one or more citation files (`.ris`, `.bib`, or `.txt`).

After uploading, a metadata form appears with one row per file. Set three fields for each:

| Field | What it does | Examples |
|-------|-------------|---------|
| **Source** | Tags where the citations came from | `Web of Science`, `Scopus`, `Citation Searching` |
| **Label** | Tags the screening stage | `search`, `screened`, `final` |
| **String** | Optional — tags string or method variations | `String_1`, `String_2_proximity` |

CiteSource auto-suggests a source name from the filename — edit it to something meaningful. Label defaults to `search`, which is correct for most initial uploads. The String field can be left blank if you aren't tracking string variations.

You can upload files in multiple batches. Each new upload adds rows to the form without clearing prior entries. Review the new rows and update their metadata before moving on.

<details>
<summary><strong>Re-importing previously processed CiteSource data</strong></summary>

> If you have a `.ris` or `.csv` previously exported from CiteSource, use the **"Re-upload an .ris or .csv exported from CiteSource"** input below the main upload area. These files contain embedded `cite_source`, `cite_label`, and `cite_string` columns. Re-importing skips deduplication entirely and takes you directly to Visualise and Tables with your prior results.

</details>

<details>
<summary><strong>Compatibility note</strong></summary>

> Some platforms (notably OVID) produce `.ris` files with non-standard field structures. If an upload produces unexpected results, try importing the file into citation software (Zotero, EndNote) first and re-exporting before uploading to CiteSource.

</details>

---

<a id="step-2"></a>

### Step 2 — Automated Deduplication

Navigate to the **Deduplicate** tab and click **Find duplicates**.

CiteSource compares metadata fields (DOI, title, authors, journal, year, volume, pages) to identify duplicates both *within* each source file (internal deduplication → *distinct* records) and *across* all uploaded files (external deduplication → *unique* records). When duplicates are merged, all `cite_source`, `cite_label`, and `cite_string` tags from every copy are preserved on the single merged record.

Once complete, a summary card shows:

- Total records uploaded, duplicates removed, and unique citations remaining
- A per-source record count breakdown
- Whether any pairs were flagged for manual review

---

<a id="step-3"></a>

### Step 3 — Manual Deduplication (If Needed)

If the summary card flags pairs for review, switch to the **Manual deduplication** sub-tab.

The default **Card View** shows each potential pair side-by-side with color-coded field comparisons: green fields match, amber fields differ, red fields are missing from one record. A similarity badge shows the overall match score. Check the box on pairs that are true duplicates, then click **Remove Selected Duplicates**. When you're done — or if no review is needed — click **Go to Visualisations**.

<details>
<summary><strong>Switching to Table View</strong></summary>

> Open the **Options & Filters** accordion to switch to Table View, which lists all pairs in a sortable table. Useful for quickly scanning a large number of flagged pairs. Click rows to select duplicates, then click **Remove additional duplicates**.

</details>

<details>
<summary><strong>Card navigation options</strong></summary>

> The Options & Filters accordion also lets you filter cards by minimum similarity score and sort by highest or lowest similarity first. Use this to prioritize high-confidence pairs or to focus on borderline cases.

</details>

---

<a id="step-4"></a>

### Step 4 — Visualise Overlap

Navigate to the **Visualise** tab. Use the sidebar to configure:

- **Comparison type** — compare by `sources`, `labels`, or `strings`
- **Filters** — limit which sources, labels, or strings appear in the plots

These filter selections sync automatically with the Tables tab, so you don't need to set them twice.

Three plots are available:

**Heatmap** — a matrix of pairwise overlap between all groups. Darker cells indicate higher overlap; hover for exact counts. Best for spotting pairs of sources with high redundancy.

**Upset Plot** — visualizes intersections across all groups simultaneously. Vertical bars show record counts for each intersection pattern; horizontal bars show totals per group. More informative than a Venn diagram when comparing more than three groups.

**Phase Analysis Plot** — most useful when comparing by labels (`search` → `screened` → `final`). Shows how many records at each stage were newly unique versus carried forward from a prior stage. Visualizes yield and deduplication effectiveness across your review workflow.

Each plot has a **Download** button to save it as a PNG.

---

<a id="step-5"></a>

### Step 5 — Summary Tables

Navigate to the **Tables** tab. Use the sidebar filters to select the subset of data to analyze, then click **Generate** for the table you need:

**Initial Records Table** — high-level counts for the earliest phase. Shows total uploaded records and how many were internal duplicates within each source file — the difference between raw download counts and distinct records.

**Record Summary Table** — breaks down unique versus overlapping contributions by source or method. Shows which sources contributed the most records found nowhere else.

**Precision/Sensitivity Table** — requires records labeled `final`. For each source, calculates *precision* (proportion of its records that were ultimately included) and *sensitivity* (proportion of all included records that it found). Useful for evaluating and reporting search strategy performance.

**Record Level Table** — the full deduplicated citation list with per-record provenance metadata. Click ⊕ to expand a row for the full APA reference.

<details>
<summary><strong>Working with the Record Level Table</strong></summary>

> - **Sort** — click any column header; hold Shift and click a second header to sort by multiple columns
> - **Filter** — type in the search box (top right) to filter across all displayed columns dynamically
> - **Download** — the CSV button above the table saves the currently filtered view

</details>

---

<a id="step-6"></a>

### Step 6 — Export Results

Navigate to the **Export** tab. Three sections are available:

**Citations** — download the full deduplicated dataset as `.csv`, `.ris`, or `.bib`. Provenance metadata (`cite_source`, `cite_label`, `cite_string`) is embedded in standard bibliographic fields (`.ris` uses C1, C2, C7, C8, DB). Only `.csv` and `.ris` can be re-imported into CiteSource later.

**Plots** — download any of the three visualizations as PNG files. Content reflects your current filter selections on the Visualise tab.

**Tables** — download the Detailed Record Table as CSV. Content reflects your current filter selections on the Tables tab.

---
