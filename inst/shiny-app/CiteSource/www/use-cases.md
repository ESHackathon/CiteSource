## CiteSource Use Cases

<img src="CS.gif" width="200" style="float: right; margin-left: 20px; margin-bottom: 10px;"/>

CiteSource supports two broad phases of evidence synthesis work: **optimizing search strategies** during development, and **analyzing search impact** after screening. It also has useful applications in training and library management.

---

### I. Optimizing Search Strategies

These use cases apply during the search development phase — before you've committed to a final strategy. The common thread is using empirical data from your own search results rather than relying on general guidance or experience alone.

#### Source & Method Selection

Which databases are actually worth searching for your topic? CiteSource answers this empirically. Upload pilot searches from candidate databases, tag each with `cite_source`, and run deduplication. The overlap plots show which sources retrieve largely the same records (redundant) versus which contribute meaningfully unique citations. This turns source selection from a judgment call into a data-driven decision.

**Key tools:** Heatmap, Upset Plot, Record Summary Table

#### Search String Development

Testing variations in search strings — different terms, proximity operators, Boolean logic, field codes — is inherently iterative. CiteSource compresses that cycle. Tag results from each variation using `cite_source` and `cite_string`, deduplicate, and visualize what each version adds or removes. You can assess the impact of a single modification in minutes rather than days.

**Key tools:** Upset Plot, Record Level Table

#### Validation Against a Reference Set

Upload a known-relevant set of articles alongside your search results, tagging the reference set with a distinct `cite_source` name (e.g., `"benchmark"` or `"reference_set"`) and the `search` label — the same label used for your database results. After deduplication, the overlap between your results and the reference set tells you quantitatively how well your strategy is performing — and which strings or sources are missing key articles.

**Key tools:** Upset Plot, Record Level Table

<details>
<summary><strong>A note on benchmark sets</strong></summary>

> Benchmark sets should be used carefully. If your benchmark was assembled from the same databases you're searching, sensitivity estimates will be optimistic. They work best when the benchmark comes from an independent source — a previously published review, expert consultation, or citation chaining — rather than from the search you're evaluating.

</details>

#### Efficient Iterative Testing

The value of CiteSource compounds across multiple iterations. Because each round of analysis takes minutes rather than hours, you can test more variations, validate more thoroughly, and document every decision with quantitative support. This is especially valuable when developing a protocol for a multi-year systematic review or when building a reusable search template.

---

### II. Analyzing Search Impact (Post-Screening)

These use cases apply after screening is complete, when you want to understand and report on what your search actually achieved.

#### Source & Method Contribution Analysis

Which databases or methods found the studies that actually mattered? By tagging records with `cite_source`/`cite_string` and progressively updating `cite_label` as records move through screening (`search` → `screened` → `final`), CiteSource quantifies the contribution of each search component. This distinguishes high-yield sources from those that retrieved large volumes of irrelevant records — valuable for reporting, justifying methodology, and informing future searches on similar topics.

**Key tools:** Phase Analysis Plot, Precision/Sensitivity Table, Record Summary Table

#### Enhanced Reporting & Transparency

CiteSource directly supports PRISMA-aligned reporting. Its plots and tables are ready to drop into publications or protocols. Exported files embed `cite_source`, `cite_label`, and `cite_string` directly into standard bibliographic fields (`.ris` uses C1, C2, C7, C8, DB), creating a reproducible audit trail that reviewers and readers can verify.

**Key tools:** All plots and tables; Export tab (Citations, Plots, Tables sections)

---

### III. Broader Applications

#### Training & Education

CiteSource is an effective classroom tool for MLIS programs and evidence synthesis courses. Its visualizations make abstract concepts — database overlap, string sensitivity, benchmark recall — tangible and interactive. Instructors can demonstrate best practices live, and students can experiment with their own searches to develop practical skills.

#### Library Collection Development

Librarians can use CiteSource to generate empirical, topic-specific data on database coverage and overlap, informing subscription decisions and recommendations. Comparing results from an existing subscription against a candidate new resource gives a concrete estimate of added value that goes beyond publisher-supplied marketing.

#### Methodological Research

When CiteSource users publish their quantitative findings on source and method performance, they contribute empirical data to the wider evidence synthesis community. Aggregating such findings across studies can inform evidence-based search guidelines — a form of "Studies Within A Review" (SWAR) focused on search methodology.

---
