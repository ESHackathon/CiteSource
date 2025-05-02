## About CiteSource

<img src="CS.gif" width="200" style="float: right; margin-left: 20px; margin-bottom: 10px;"/>


CiteSource is an R package and accompanying Shiny web application designed to support evidence data-driven decision-making during search strategy development. CiteSource also allows users to analyze and report on the impact of information sources and search methods.

CiteSource was developed as part of the [Evidence Synthesis Hackathon](https://www.eshackathon.org/) initiative.

---

### Key Features:

<details>
  <summary><strong>Flexible Metadata for Provenance Tracking:</strong></summary>

> * A core strength of CiteSource is its ability to assign and retain custom metadata to track the *provenance* of each citation – precisely where and how it was found. Users can tag records using three key fields:
>     * `cite_source`: Identify the origin database ('Web of Science', 'Scopus'), platform ('Lens.org'), or the specific search method used ('Citation Searching', 'String_1').
>     * `cite_label`: Track citations through screening phases using standardized terms: `search` (for initial results, benchmarks), `screened` (for records passing title/abstract review), and `final` (for records included in the synthesis after full-text review).
>     * `cite_string`: Add further detail, such as variations in search string syntax tested ('String_1a_truncation'), specific supplementary methods ('Handsearching_JournalX'), or other custom categories relevant to analysis.
> * This detailed tagging enables rigorous analysis of the performance and contribution of each component of your overall search strategy.

</details>

<details>
  <summary><strong>Advanced Deduplication & Intelligent Merging:</strong></summary>

> * CiteSource employs the [`ASySD` (Automated Systematic Search Deduplicator) R package](https://github.com/camaradesuk/ASySD) to perform robust identification and merging of duplicate records.
> * It conducts both *internal deduplication* (identifying duplicates within a single uploaded file/source, resulting in *distinct* records) and *external deduplication* (identifying duplicates across all uploaded files/sources, resulting in the set of *unique* citations).
> * The process uses *intelligent merging*: custom metadata tags (`source`, `label`, `string`) from all identified duplicates are combined onto the primary record, preserving the full discovery history.
> * The most complete bibliographic data (prioritizing DOI, Abstract) across duplicates is retained in the primary record.
> * An optional *manual review* stage presents potential duplicates that fall below the automatic matching threshold, allowing users to confirm or reject merges for maximum accuracy.

</details>

<details>
  <summary><strong>Data-Driven Analysis & Visualization:</strong></summary>

> * Once deduplication is complete, CiteSource offers a suite of analysis and visualization tools designed specifically to speed up the *iterative process* of developing, testing, and validating search strategies:
>     * Visualize Overlap: Use interactive **Heatmaps** (pairwise overlap) and **Upset Plots** (multi-set intersections) to understand shared and unique records across sources, labels, or strings.
>     * Track Phase Progression: Employ the **Phase Analysis plot** (bar chart) to see the flow of unique and duplicate records through screening stages (`search` -> `screened` -> `final`).
>     * Generate Summary Tables: Access quantitative insights via automated tables detailing:
>         * Initial Record counts (showing the impact of internal deduplication).
>         * Record Summaries (detailing unique/overlapping records contributed by each source/method).
>         * Precision/Sensitivity calculations (evaluating source/method performance against the `final` included set).
>         * A detailed, interactive **Record Level Table** for quickly examining and linking to citations .

</details>

<details>
  <summary><strong>Enhanced Reporting & Transparent Export:</strong></summary>

> * CiteSource facilitates *transparent reporting* of search methods and results, aligning with guidelines like PRISMA.
> * Export your final, deduplicated dataset in standard bibliographic formats (`.csv`, `.ris`, `.bib`).
> * The custom metadata is embedded directly into standard fields within the export files (e.g., using C1, C2, C7, C8, DB fields in `.ris` format), providing a clear and reproducible audit trail for your methodology.

</details>

---

### Why use CiteSource for Evidence Synthesis?

CiteSource is built for anyone involved in evidence synthesis. It helps you:

* **Evaluate and optimize** information source selection based on unique record contributions.
* **Refine and optimize** search strings by quickly testing variations.
* **Analyze and report** the added value of different search methods, including supplementary searching techniques.
* **Perform benchmark testing** to ensure key articles are captured by your strategy.
* **Increase transparency and effectiveness** of your search strategy and processess through built-in tables for reporting.
* **Save time** during the iterative search development.

### What Other Applications does CiteSource Serve ?

* Training in evidence synthesis search methods - MLIS classroom use for skill/knowledge development.
* Methods research & development - large-scale methods testing, quick/live updates to analysis.
* Library collection development - analyzing coverage of new databases compared to current subscriptions.

---

*CiteSource is available both as this interactive Shiny application and as a full R package with detailed vignettes. For more information on the R package, visit the [CiteSource Website](https://www.eshackathon.org/CiteSource/).*