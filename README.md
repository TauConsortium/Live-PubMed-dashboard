# Publication Dashboard

**Live app:** https://sophiaqiu.shinyapps.io/neuropublications/

## Developed by

- Sophia Qiu — `xianglang@ucsb.edu`
- Juliana Acosta-Uribe — `acostauribe@ucsb.edu`

## Overview

An interactive R Shiny dashboard that pulls publications for a curated roster of neurodegeneration researchers directly from PubMed, then classifies each paper by research type, study focus, model system, and data-sharing practices. Classification runs on a keyword engine by default, with an optional Claude pass for higher accuracy. Everything is fetched live, so no local data files are required.

> **Note:** Results are queried live from PubMed (a very large database) so a search can take a while to complete especially when many scientists are selected or full-text data-availability lookups are enabled. Please allow some time after starting a search.

## Features

- **Live PubMed search** for 73 pre-configured scientists (name, affiliation, and author-name aliases) via the NCBI E-utilities API, scoped to 2020-2026 and biased toward neurodegeneration topics.
- **Data-availability extraction** from PubMed XML, PubMed Central full text, and DOI landing pages, categorized as Public Repository, Controlled Access, Mixed, Upon Request, or Not disclosed, with repository names and accession IDs / contact emails parsed out where present.
- **Automatic classification** of each paper into data type (Whole Genome Sequencing, Exome Sequencing, RNA-seq, Imaging, Proteomics, Computational code, Phenotypic data), study-focus tags (Tau, TDP-43, alpha-Synuclein, Amyloid-beta, and more), and model system (Human, Mouse, In-vitro, In-silico, Multi-model).
- **Optional Claude classification** (Claude Haiku) for research type and data availability when an Anthropic API key is provided, falls back to the keyword classifier otherwise.
- **Interactive dashboard** with filters (year, data type, focus, model, data sharing), an expandable results table, and Plotly summary charts.
- **CSV export** of the filtered set or the full result set.

## Requirements

R with the following packages:

```r
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "shinyjs",
                   "httr", "jsonlite", "xml2", "DT", "dplyr", "plotly",
                   "shinycssloaders"))
```

## Running

From the app directory:

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.

## API keys (optional)

Both are entered in the app's **Settings** tab and are optional.

- **NCBI API key** raises the PubMed request rate from 3 to 10 requests/second, making fetches noticeably faster. Create one from your NCBI account settings.
- **Claude API key** enables AI classification of research type and data availability via the Anthropic API. Create one in the [Anthropic Console](https://console.anthropic.com/).

## Notes

- Data-availability lookups against PubMed Central only work for open-access articles, papers without an availability statement may show as "Not disclosed."
- Fetch time scales with the number of scientists and the max-publications-per-scientist setting, especially when full-text data-availability lookups are enabled.
