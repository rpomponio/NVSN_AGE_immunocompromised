# AGE in Immunocompromised Hosts

## Objectives

- Describe the epidemiology of AGE cases in enrolled immunocompromised (IC) hosts

## Requirements

In order to run the code in this repository, one must install `R` which is freely available. It is recommended to use [R studio](https://posit.co/download/rstudio-desktop) for ease of coding.

Additionally the following packages will be required:

- `haven` (for reading SAS dataset)
- `data.table`
- `gtsummary`
- `ggplot2`

Finally, one needs to obtain a copy of the latest dataset from CDC. As of April 2026, that file was named `pitt_20260413.sas7bdat`. Copy this file to the main folder in this repository.

## Overview

|**Characteristic** | **Not immunocompromised**  N = 4,289 | **Immunocompromised**  N = 198 | **Status Unknown**  N = 91 |
|:------------------|:------------------------------------:|:------------------------------:|:--------------------------:|
|AGE Season         |                                      |                                |                            |
|2023-24            |              269 (6.3%)              |            5 (2.5%)            |           0 (0%)           |
|2024-25            |             3,002 (70%)              |           128 (65%)            |          75 (82%)          |
|2025-26            |             1,018 (24%)              |            65 (33%)            |          16 (18%)          |
|Site               |                                      |                                |                            |
|Vanderbilt         |             1,331 (31%)              |           15 (7.6%)            |           0 (0%)           |
|Rochester          |              234 (5.5%)              |            4 (2.0%)            |          1 (1.1%)          |
|Cincinnati         |              315 (7.3%)              |            2 (1.0%)            |          1 (1.1%)          |
|Seattle            |              470 (11%)               |           12 (6.1%)            |          70 (77%)          |
|Houston            |              997 (23%)               |           126 (64%)            |          15 (16%)          |
|Kansas City        |              470 (11%)               |            29 (15%)            |          4 (4.4%)          |
|Pittsburgh         |              472 (11%)               |           10 (5.1%)            |           0 (0%)           |
|Age, months        |             29 (11, 76)              |          69 (32, 132)          |        57 (17, 107)        |
|Insurance type     |                                      |                                |                            |
|Public             |             2,837 (66%)              |           112 (57%)            |          47 (52%)          |
|Private            |             1,101 (26%)              |            65 (33%)            |          33 (36%)          |
|Both               |              104 (2.4%)              |            9 (4.5%)            |          6 (6.6%)          |
|No insurance       |              233 (5.5%)              |           12 (6.1%)            |          5 (5.5%)          |
|Unknown            |                  14                  |               0                |             0              |
|Final status       |                                      |                                |                            |
|Admitted           |             2,184 (51%)              |           187 (94%)            |          45 (49%)          |
|ED-only            |             2,105 (49%)              |           11 (5.6%)            |          46 (51%)          |
|Norovirus (lab)    |                                      |                                |                            |
|Inconclusive       |               3 (0.1%)               |             0 (0%)             |           0 (0%)           |
|Negative           |             2,327 (79%)              |           126 (85%)            |          50 (78%)          |
|Positive           |              607 (21%)               |            23 (15%)            |          14 (22%)          |
|Unknown            |                1,352                 |               49               |             27             |
|Rotavirus (lab)    |                                      |                                |                            |
|Inconclusive       |               4 (0.1%)               |             0 (0%)             |           0 (0%)           |
|Negative           |             2,684 (91%)              |           144 (97%)            |          57 (89%)          |
|Positive           |              249 (8.5%)              |            5 (3.4%)            |          7 (11%)           |
|Unknown            |                1,352                 |               49               |             27             |
