# AGE in Immunocompromised Hosts

## Objectives

- Describe the epidemiology of AGE cases in enrolled immunocompromised (IC) hosts.

## Requirements

### Directory Structure

./
│   .gitignore
│   AGE immunocompromised.Rproj
│   README.md
│   ...
├───archive
│   ...
└───data
        Pitt - Immunocomp - NVSN AGE_HC documentation.docx
        pitt_20260413.sas7bdat
        
### Data

Files in the `data/` sub-folder are NOT stored on GitHub, by design.

Obtain the file `pitt_20260413.sas7bdat` and store it according to the specified directory structure.

### Software

Install [R](https://www.r-project.org) and [RStudio](https://posit.co/download/rstudio-desktop) (recommended).

The following R packages are required. Install them once from the R console:

```r
install.packages(c("haven", "data.table", "gtsummary", "ggplot2", "officer", "flextable", "patchwork", "ggpattern"))
```

## Reproducing the Analysis (May 2026)

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

