# AGE in Immunocompromised Hosts

## Objectives

- Describe the epidemiology of AGE cases in enrolled immunocompromised (IC) hosts.

## Requirements

In order to run the code in this repository, one must install `R` which is freely available. It is recommended to use [R studio](https://posit.co/download/rstudio-desktop) for ease of coding.

Additionally the following packages will be required:

- `haven` (for reading SAS dataset)
- `data.table`
- `gtsummary`
- `ggplot2`

Finally, one needs to obtain a copy of the latest dataset from CDC. Copy this file from the following password-protected link:

<https://upmchs-my.sharepoint.com/:u:/r/personal/snyderjn4_upmc_edu/Documents/Norovirus%20in%20IC%20Patients/Datasets/ShareFile%20Download%20Apr%202026/pitt_20260413.sas7bdat?csf=1&web=1&e=oouQB4>

The file should be `pitt_20260413.sas7bdat` and stored in the same folder as the code for this repository.

## Answers from CDC

`bf_astro` / `bf_sapo` are from BIOFIRE assays--currently no sites are testing this way

Mary to ask Seattle about IC Status types "Unknown"...

Claire to present summary of IC enrollment at Annual Meeting (will share prior to 5/12)

## Overview

|**Characteristic**             | **Not immunocompromised**  N = 4,020 | **Immunocompromised**  N = 193 | **Status Unknown**  N = 91 |
|:------------------------------|:------------------------------------:|:------------------------------:|:--------------------------:|
|Baseline Characteristics       |                                      |                                |                            |
|AGE Season                     |                                      |                                |                            |
|2024-25                        |             3,002 (75%)              |           128 (66%)            |          75 (82%)          |
|2025-26                        |             1,018 (25%)              |            65 (34%)            |          16 (18%)          |
|Site                           |                                      |                                |                            |
|Vanderbilt                     |             1,260 (31%)              |           15 (7.8%)            |           0 (0%)           |
|Rochester                      |              227 (5.6%)              |            4 (2.1%)            |          1 (1.1%)          |
|Cincinnati                     |              301 (7.5%)              |            2 (1.0%)            |          1 (1.1%)          |
|Seattle                        |              408 (10%)               |           12 (6.2%)            |          70 (77%)          |
|Houston                        |              930 (23%)               |           124 (64%)            |          15 (16%)          |
|Kansas City                    |              442 (11%)               |            26 (13%)            |          4 (4.4%)          |
|Pittsburgh                     |              452 (11%)               |           10 (5.2%)            |           0 (0%)           |
|Age, months                    |             29 (11, 77)              |          69 (31, 132)          |        57 (17, 107)        |
|Insurance type                 |                                      |                                |                            |
|Public                         |             2,655 (66%)              |           110 (57%)            |          47 (52%)          |
|Private                        |             1,034 (26%)              |            64 (33%)            |          33 (36%)          |
|Both                           |              100 (2.5%)              |            9 (4.7%)            |          6 (6.6%)          |
|No insurance                   |              217 (5.4%)              |           10 (5.2%)            |          5 (5.5%)          |
|Unknown                        |                  14                  |               0                |             0              |
|Self-reported Symptoms         |                                      |                                |                            |
|Fever                          |             2,188 (54%)              |           120 (62%)            |          38 (42%)          |
|Diarrhea                       |             2,185 (54%)              |           113 (59%)            |          43 (47%)          |
|Vomiting                       |             3,570 (89%)              |           151 (78%)            |          85 (93%)          |
|Outcomes                       |                                      |                                |                            |
|Final hospitalization status   |                                      |                                |                            |
|Admitted                       |             2,061 (51%)              |           183 (95%)            |          45 (49%)          |
|ED-only                        |             1,959 (49%)              |           10 (5.2%)            |          46 (51%)          |
|Inpatient length of stay, days |               2 (1, 3)               |            3 (2, 9)            |          2 (1, 3)          |
|Unknown                        |                1,914                 |               9                |             46             |
|Lab Results                    |                                      |                                |                            |
|RT-PCR Availability            |                                      |                                |                            |
|Not Tested                     |             1,265 (31%)              |            47 (24%)            |          27 (30%)          |
|Tested                         |             2,755 (69%)              |           146 (76%)            |          64 (70%)          |
|Norovirus                      |                                      |                                |                            |
|Inconclusive                   |               3 (0.1%)               |             0 (0%)             |           0 (0%)           |
|Negative                       |             2,185 (79%)              |           123 (84%)            |          50 (78%)          |
|Positive                       |              567 (21%)               |            23 (16%)            |          14 (22%)          |
|Unknown                        |                1,265                 |               47               |             27             |
|Rotavirus                      |                                      |                                |                            |
|Inconclusive                   |               3 (0.1%)               |             0 (0%)             |           0 (0%)           |
|Negative                       |             2,512 (91%)              |           141 (97%)            |          57 (89%)          |
|Positive                       |              240 (8.7%)              |            5 (3.4%)            |          7 (11%)           |
|Unknown                        |                1,265                 |               47               |             27             |
