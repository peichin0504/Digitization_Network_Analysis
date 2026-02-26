# Search Technologies and the Reallocation of Attention
### Replication and Extension of Nagaraj and Reimers (2021)

**Pei-Chin Lu**

---

## Overview

This project replicates and extends Nagaraj and Reimers (2021), which examines how digitization affects the demand for physical library borrowing. The replication covers Table 5, columns (1)–(2) of the original paper using Harvard Widener Library checkout records from 2003–2011. The extension constructs a pre-digitization author–topic network from Open Library metadata and tests whether the substitution effect of digitization is heterogeneous along network centrality—the degree to which an author was already discoverable within the pre-existing intellectual landscape.

---

## Repository Structure

```
Digitization_Network_Analysis/
├── data_raw/
│   └── loans_merged.dta          # Raw loans file (individual checkout events)
├── data_clean/
│   └── master.dta                # Cleaned book×year panel (792,054 observations)
├── output/
│   ├── figure/
│   │   ├── key_authors_topics_top50.png   # Bipartite author–topic network
│   │   ├── author_network_top30.png       # Projected author–author network
│   │   └── event_study.png               # Sun–Abraham event-study + centrality plots
│   ├── original_data/
│   │   └── open_library_data.rds         # Cached API results (~40,000 books)
│   ├── table_5.tex                        # Replication table (LaTeX)
│   └── table_5_robust.tex                 # Sun–Abraham robustness table (LaTeX)
└── scripts/
    └── R code.R                           # Full analysis pipeline
```

> **Reproducibility note.** Please open the RStudio Project file inside the `Digitization_Network_Analysis` folder before running the script. All paths are defined relative to the project root via `here()` and require no manual adjustment.

---

## Data

**Source.** The raw loans file (`loans_merged.dta`) records individual checkout events for 88,006 books at Harvard's Widener Library over 2003–2011. Each observation corresponds to a single loan event, with book identifiers and shelf location attached.

**Panel construction.** The raw file is aggregated to a balanced book×year panel by counting loan events per cell and imputing zeros where no checkout occurred in a given year. The resulting dataset contains 792,054 observations (88,006 books × 9 years) and is saved as `data_clean/master.dta`. The indicator `post_scanned` is set to 1 from each book's first digitization year onward, spanning the staggered treatment window of 2005–2009. Books never digitized receive `post_scanned = 0` throughout.

**Network data.** Subject metadata are retrieved from the Open Library API via OCLC identifiers (approximately 40,000 books). To avoid redundant API calls, results are cached locally as `output/original_data/open_library_data.rds`. **The cached file is included in this repository**; if it is present, the script loads it directly and skips the API retrieval step entirely.

---

## Analysis Pipeline

All four parts of the analysis are contained in a single script: `scripts/R code.R`.

### Part 1 — Panel Construction

The raw loans file is aggregated to a balanced book×year panel. Location and digitization year are merged from the raw data by book identifier, and `post_scanned` is constructed as an indicator equal to 1 from each book's first scan year onward.

### Part 2 — TWFE Main Effect (Replication)

The baseline specification is estimated via two-way fixed effects:

$$Y_{it} = \beta \cdot \text{PostScanned}_{it} + \gamma_i + \delta_{t \times \ell} + \varepsilon_{it}$$

where $\gamma_i$ denotes book fixed effects, $\delta_{t \times \ell}$ denotes year-by-location fixed effects, and standard errors are clustered at the book level. Two dependent variables are considered: $\log(\text{Loans}+1)$ and an indicator for any loan. Results are exported to `output/table_5.tex`.

Estimates closely match the original paper. The log-OLS specification yields a coefficient of −0.046 (paper: −0.051) and the LPM yields −0.055 (paper: −0.061), both significant at the 1% level.

### Part 3 — Sun and Abraham (2021) Robustness Check

Staggered digitization across 2005–2009 raises the concern that TWFE assigns negative implicit weights to some cohort-period cells when treatment effects are heterogeneous. The baseline is re-estimated using `sunab()` in `fixest`, which aggregates cohort-specific ATTs without negative weighting. Results are exported to `output/table_5_robust.tex`.

The resulting ATT is small and positive in both specifications, in contrast to the negative TWFE estimates. Two explanations are consistent with the data: genuine negative-weighting contamination under TWFE, and compositional changes arising from the reduction in sample size from 792,054 to 339,453 observations, as the Sun–Abraham estimator retains only treated cohorts and their clean controls. Given this ambiguity, the two sets of estimates are treated as complementary rather than definitive.

### Part 4 — Network Construction and Heterogeneity Analysis

**Network construction.** Subject metadata retrieved from the Open Library API are used to build an author–topic bipartite graph in `igraph`, connecting each author node to the subject category nodes associated with their books. Author and topic nodes are distinguished by a name suffix (`_author`, `_topic`) to prevent merging nodes that share a name. The bipartite graph is then projected onto authors alone using `bipartite_projection()`. Each author's degree centrality in the projected network—the number of distinct co-subject authors—measures the number of independent intellectual paths through which a patron could encounter that author in the pre-digitization catalog environment. The resulting centrality scores are merged into the panel at the book level; books with no recoverable author information are assigned a score of zero.

**Heterogeneity analysis.** Authors are split at the median degree centrality (among those with a positive score) into high- and low-centrality groups. A TWFE specification interacting `post_scanned` with centrality group is estimated, and a Sun–Abraham event-study is run on the full sample to inspect pre-trends. Both panels are exported together as `output/figure/event_study.png`.

The interaction between `post_scanned` and scaled degree centrality is negative and statistically significant ($\hat{\beta} = -0.000189$, $p < 0.01$). High-centrality authors are associated with a larger post-digitization borrowing decline (−5.4%) than their low-centrality counterparts (−4.4%).

**Pre-trends caveat.** The Sun–Abraham event-study reveals negative and trending pre-treatment coefficients, indicating that digitized books were already on declining borrowing trajectories prior to scanning—a violation of the parallel trends assumption. The most likely explanation is selection into digitization: Harvard may have prioritized scanning books with declining physical demand. All findings from Part 4 are therefore interpreted as descriptive patterns consistent with the proposed mechanism rather than as causally identified effects.

---

## Requirements

**R packages** (installed automatically if missing):

`haven`, `dplyr`, `tidyr`, `fixest`, `tidyverse`, `httr`, `jsonlite`, `stringr`, `igraph`, `plm`, `marginaleffects`, `ggplot2`, `ggraph`, `tidygraph`, `here`

---

## References

Merton, R. K. (1968). "The Matthew Effect in Science." *Science* 159(3810): 56–63.

Nagaraj, A. and I. Reimers (2021). "Digitization and the Demand for Physical Works." *Working Paper*. https://doi.org/10.1257/pol.20210702

Salganik, M. J., P. S. Dodds, and D. J. Watts (2006). "Experimental Study of Inequality and Unpredictability in an Artificial Cultural Market." *Science* 311(5762): 854–856.

Sun, L. and S. Abraham (2021). "Estimating Dynamic Treatment Effects in Event Studies with Heterogeneous Treatment Effects." *Journal of Econometrics* 225(2): 175–199.
