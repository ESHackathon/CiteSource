## CiteSource Use Cases: Overview

<img src="https://user-images.githubusercontent.com/89118428/155393065-780381a0-ff77-45d3-b2ee-40332ef72064.png" width="200" style="float: right; margin-left: 20px; margin-bottom: 10px;"/>

CiteSource provides a suite of tools to support data-driven decision-making throughout the evidence synthesis process. Its applications generally fall into two main categories: **Optimizing Search Strategies** (typically during protocol development and iterative searching) and **Analyzing Search Impact** (often after screening is complete, for reporting and methodological insights). Additional applications extend to training and resource management.

---

### I. Optimizing Search Strategies

CiteSource enables researchers to move beyond reliance on experience or potentially outdated guidance by providing empirical data specific to their project *during* the search development phase.

<details>
  <summary><strong>Information Source/Method Selection & Optimization</strong></summary>

> Choosing the most effective and efficient set of databases, platforms, or indexes (e.g., Web of Science, Scopus, ASFA, Dimensions, OATD) can be challenging, especially for interdisciplinary topics where overlap and unique contributions are unknown. CiteSource addresses this by allowing users to empirically compare potential sources *before* committing significant time. After uploading initial search results and tagging them using the `cite_source` field (e.g., `Web of Science`, `Scopus`), deduplication the overlapping and unique records across sources and methods. This analysis enables informed, data-driven decisions about which sources and methods provide the best return on investment and helps optimize the selection, potentially reducing redundancy. Key CiteSource features used include:
> * Tagging records with `cite_source` metadata.
> * Robust internal and external deduplication (using `ASySD`).
> * Visualization of overlap using interactive **Heatmaps** and **Upset Plots**.
> * Quick analysis of individual citations using the interactive **Record Level Table**

</details>

<details>
  <summary><strong>Search String Development & Optimization</strong></summary>

> Developing effective search strings is an iterative process involving testing terms, syntax variations, Boolean logic, proximity operators, field codes, etc. Comparing the impact of these subtle changes across potentially multiple databases is time-consuming. CiteSource assists by streamlining the analysis of string effectiveness. Users can upload results from different string variations, tag them using `cite_source` and `cite_string` (e.g., `String_1`, `String_2_proximity`), and visualize the impact on retrieval after deduplication. This allows for rapid assessment of how changes affect results, speeding up refinement for an optimal balance of sensitivity and precision and helping identify errors in logic or syntax. CiteSource facilitates this via:
> * Tagging result sets with `cite_source` and `cite_string` metadata.
> * Deduplication to compare results accurately.
> * Visualization of unique/overlapping records retrieved by different strings using **Upset Plots**.
> * Quick examination of individual citations using the interactive **Record Level Table**.

</details>

<details>
  <summary><strong>Benchmark Testing</strong></summary>

> Ensuring a search strategy retrieves known, key relevant articles (benchmark or reference articles) is crucial for assessing sensitivity. CiteSource facilitates this by comparing search results against a predefined benchmark set. After uploading search results and the benchmark set (tagging each appropriately using `cite_source`, `cite_string`, and `cite_label`), deduplication allows for direct comparison. This provides a quantitative assessment of how well different strings or sources capture the benchmark articles, highlighting potential weaknesses or indexing gaps. While benchmark sets should be used cautiously due to potential bias, this process aids refinement and is useful for updates or developing standardized protocols. Key functionalities include:
> * Tagging benchmark sets and search results distinctly using `cite_source` and `cite_label`.
> * Deduplication to identify matches between search results and the benchmark set.
> * Using **Upset Plots** to visualize captured vs. missed benchmark articles across different strings/sources.
> * Investigating specific missed articles using the interactive **Record Level Table**.

</details>

<details>
  <summary><strong>Efficient Iterative Testing (Overarching Benefit)</strong></summary>

> The entire process of testing variations in sources, strings, and methods is inherently iterative. CiteSource significantly compresses this cycle by providing rapid analysis and visualization (**Heatmaps**, **Upset Plots**, **Summary Tables**) immediately after deduplication. This drastic reduction in the time needed to evaluate the impact of each iteration saves researcher time and allows for more thorough testing and validation, leading to a more optimized and well-documented strategy.

</details>

---

### II. Analyzing Search Impact (Post-Screening / Reporting)

After screening is complete, CiteSource analyzes the *actual* contribution of different search components to the final set of included studies and enhances reporting.

<details>
  <summary><strong>Analyzing Information Source & Search Method Contribution</strong></summary>

> Understanding which sources or methods were most effective in identifying the studies ultimately included in the synthesis is crucial for methodological reflection and reporting. CiteSource enables this analysis by tracking records through screening phases. By tagging records with `cite_source`/`cite_string` and progressively updating the `cite_label` (`search` -> `screened` -> `final`), users can quantify the "true impact" or ROI of each component. This identifies high-yield sources/methods versus those contributing mostly irrelevant records, providing valuable data for reporting and future strategy refinement. Analysis tools include:
> * Tracking records using `cite_source`, `cite_string`, and `cite_label` tags.
> * Visualizing the flow through screening stages with the **Bar Chart (Phase Analysis Plot)**.
> * Quantifying performance using the **Precision/Sensitivity Table** (calculating precision and recall against the `final` set).
> * Examining contributions at each stage using the **Record Summary Table**.

</details>

<details>
  <summary><strong>Enhanced Reporting & Transparency</strong></summary>

> Reporting guidelines like PRISMA require transparent and detailed documentation of the search process. CiteSource directly supports this by generating clear outputs and ensuring provenance is maintained. The plots and tables offer visual and quantitative summaries of the search process, outcomes, and source/method contributions. Furthermore, exporting the final dataset embeds the custom metadata tags (`cite_source`, `cite_label`, `cite_string`) into standard bibliographic fields (e.g., C1, C2, C7, C8, DB in `.ris` format), providing a clear, reproducible audit trail. This enhances transparency and allows reviewers/readers to scrutinize the methodology effectively. Key outputs for reporting include:
> * Ready-to-use **plots** (**Heatmaps**, **Upset Plots**, **Bar Charts**).
> * Summary **tables** (**Initial Record**, **Record Summary**, **Precision/Sensitivity**, **Record Level**).
> * Exported datasets (`.csv`, `.ris`, `.bib`) with embedded provenance metadata.

</details>

---

### III. Broader Applications

Beyond individual reviews, CiteSource has wider utility:

<details>
  <summary><strong>Training & Education</strong></summary>

> CiteSource serves as an effective training tool for evidence synthesis methods. Its interactive visualizations provide a hands-on way for students and early-career researchers to understand abstract concepts like database overlap, string variation impacts, and benchmark testing. Instructors can use it to demonstrate best practices in real-time, building practical skills and competence in systematic searching.

</details>

<details>
  <summary><strong>Library Collection Development</strong></summary>

> Librarians can leverage CiteSource to support collection development decisions. By analyzing search results from institutional researchers or targeted test searches, they can gain empirical data on database coverage and overlap for specific research topics. This helps justify subscription costs, compare existing resources with potential new ones, and make effective recommendations based on demonstrated value and uniqueness.

</details>

<details>
  <summary><strong>Methodological Research</strong></summary>

> CiteSource facilitates methodological research on searching itself. When researchers use the tool and report their quantitative findings on source/method performance (e.g., precision/sensitivity, unique contributions), they contribute valuable empirical data to the wider evidence synthesis community. Aggregating such findings across studies can inform the development and refinement of evidence-based search guidelines and best practices, potentially supporting "Studies Within A Review" (SWAR) focused on search methodology.

</details>

---