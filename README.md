# AGE in Immunocompromised Hosts

## Objectives

- Describe the epidemiology of AGE cases in enrolled immunocompromised (IC) hosts.

## Requirements

### Software

Install [R](https://www.r-project.org) and [RStudio](https://posit.co/download/rstudio-desktop) (recommended).

The following R packages are required. Install them once from the R console:

```r
install.packages(c("haven", "data.table", "gtsummary", "ggplot2", "officer", "flextable"))
```

### Data

Obtain the latest dataset from CDC via the following password-protected link:

<https://upmchs-my.sharepoint.com/:u:/r/personal/snyderjn4_upmc_edu/Documents/Norovirus%20in%20IC%20Patients/Datasets/ShareFile%20Download%20Apr%202026/pitt_20260413.sas7bdat?csf=1&web=1&e=oouQB4>

Save the file as `pitt_20260413.sas7bdat` in the same folder as the scripts.

## Reproducing the Analysis

Run the scripts in the following order:

**Step 1 — Generate tables**

Open and run `prelim.R`. This script:

- Reads and filters the SAS dataset to the study period (October 2024 onward)
- Derives all analytic variables (demographics, outcomes, virology)
- Produces two `gtsummary` table objects in the R environment:
  - `tab.demo` — Table 1: demographic and baseline characteristics
  - `tab.outcomes` — Table 2: clinical outcomes and laboratory results

**Step 2 — Export to Word**

Open and run `export.R`. This script:

- Sources `prelim.R` automatically
- Exports both tables to a single Word document (`prelim_immunocomp_tables.docx`), one table per page

The output file will be saved in the working directory. To check or set the working directory in RStudio, use `getwd()` and `setwd()`.

## Notes

- Patients with unknown immunocompromised status (N=91) are excluded from the presentation tables but retained in the full dataset for downstream use.
- Season data for 2025-26 is partial through January 31, 2026.
- Laboratory virology results use RT-PCR preferentially; clinical test results are used only when RT-PCR is unavailable. Inconclusive results are treated as missing.
- Code was written with assistance from Claude (Anthropic); all analytical decisions were made by the author.

## Answers from CDC

`bf_astro` / `bf_sapo` are from BIOFIRE assays (currently no sites are testing with this assay)

Mary to ask Seattle about IC Status types "Unknown"...

Claire to present summary of IC enrollment at Annual Meeting (will share prior to 5/12)