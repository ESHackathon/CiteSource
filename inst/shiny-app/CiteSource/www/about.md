## About CiteSource

<img src="CS.gif" width="200" style="float: right; margin-left: 20px; margin-bottom: 10px;"/>

CiteSource is an R package and Shiny web application for data-driven search strategy development and evidence synthesis reporting. It helps you understand where your citations come from, how your sources overlap, and which search methods contributed most to your final results.

CiteSource was developed as part of the [Evidence Synthesis Hackathon](https://www.eshackathon.org/) initiative.

---

### What CiteSource Does

**Tracks provenance.** Every citation carries metadata telling you exactly where and how it was found — which database (`cite_source`), which screening phase (`cite_label`), and which search string variation (`cite_string`). This information travels with each record through deduplication and into your exports.

**Deduplicates intelligently.** Built on the [ASySD](https://github.com/camaradesuk/ASySD) engine, CiteSource deduplicates both within and across sources. When duplicates are merged, their metadata tags are combined — so a record found in three databases is tagged with all three sources, not just one.

**Visualizes overlap.** Interactive heatmaps and Upset Plots show exactly which records are shared between sources, labels, or search strings. A phase analysis plot tracks records through screening stages.

**Generates summary tables.** Automated tables quantify source contributions, calculate precision and sensitivity against your final included set, and provide a record-level view for individual citation review.

**Supports transparent reporting.** Exports in `.csv`, `.ris`, and `.bib` embed provenance metadata directly into standard bibliographic fields, providing a reproducible audit trail aligned with PRISMA and similar guidelines.

---

### Why Use CiteSource?

CiteSource is built for anyone designing, testing, or reporting on a literature search. It helps you:

- **Choose databases wisely** — empirically compare source overlap and unique contributions before committing to your final set
- **Refine search strings** — quickly test syntax variations and visualize what changes
- **Validate against a reference set** — upload a known-relevant article set as an additional search source to verify your strategy captures key papers
- **Report more transparently** — generate ready-to-use plots and tables documenting your search process
- **Save time** — compress the iterative search development cycle

---

### Other Applications

- **Training** — hands-on classroom tool for MLIS programs and evidence synthesis courses
- **Methods research** — large-scale comparison of search approaches across projects
- **Library collection development** — empirical data on database coverage and overlap to inform subscription decisions

---

### How to Cite

Riley, T., Young, S., Paxton, A., Wallrich, L., Hair, K., & Grainger, M. (2026). CiteSource: An R package for data-driven search strategy development and enhanced evidence synthesis reporting. *Research Synthesis Methods*. https://doi.org/10.1017/rsm.2026.10084

---

*CiteSource is also available as a full R package with detailed vignettes. Visit the [CiteSource website](https://www.eshackathon.org/CiteSource/) for more.*
