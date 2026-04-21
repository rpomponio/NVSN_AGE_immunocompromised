################################################### -
## Title: Prelim analysis for AGE Immunocompromised
## Author: Ray Pomponio
## Email: pomponiord@upmc.edu
## Project: AGE Immunocompromised
## Date Created: 2026-04-20
################################################### -

# load packages
library(haven)
library(data.table)
library(gtsummary)
library(ggplot2)
theme_set(theme_classic(base_size=14))

# read dataset in SAS format, convert to friendly R object (data.table)
sas <- read_sas("pitt_20260413.sas7bdat")
dat <- data.table(sas)

# check integrity of critical variables
any(is.na(dat$caseid))
range(dat$scrdate)
table(dat$c_ageyear)
table(dat$studysite)
table(dat$provider, dat$c_carelevel)
table(dat$immcomp, exclude=NULL)
summary(dat$c_agemonths)

# begin building Table 1--baseline characteristics stratify'g by IC status
dat[, c_season:=factor(c_ageyear, 19:21, c("2023-24", "2024-25", "2025-26"))]
dat[, c_studysite:=factor(studysite, c(1:6, 8), c("Vanderbilt", "Rochester",
                                                  "Cincinnati", "Seattle",
                                                  "Houston", "Kansas City",
                                                  "Pittsburgh"))]
dat[, c_immcomp:=factor(immcomp, c(0:1, 8), c("Not immunocompromised",
                                              "Immunocompromised",
                                              "Status Unknown"))]
dat[, c_admitted:=factor(c_carelevel<=2, c(TRUE, FALSE), c("Admitted", "ED-only"))]
dat[, c_insurech:=factor(c_insurech, 0:3, c("Public", "Private", "Both",
                                            "No insurance"))]

# technically part of Table 2--laboratory confirmed detections
dat[!is.na(rtpcr_norogi) & !is.na(rtpcr_norogii), c_norovirus:=fcase(
  rtpcr_norogi==1, "Positive",
  rtpcr_norogii==1, "Positive",
  rtpcr_norogi==0 & rtpcr_norogii==0, "Negative",
  default="Inconclusive")]
dat[!is.na(rtpcr_result), c_rotavirus:=fcase(
  rtpcr_result==1, "Positive",
  rtpcr_result==0, "Negative",
  default="Inconclusive")]

# mockup of Table 1 (inlcuding unknown IC status)
tab1 <- tbl_summary(
  dat,
  by=c_immcomp,
  label=list(c_season="AGE Season", c_studysite="Site", c_agemonths="Age, months",
             c_insurech="Insurance type", c_admitted="Final status",
             c_norovirus="Norovirus", c_rotavirus="Rotavirus"),
  include=c(c_season, c_studysite, c_agemonths, c_insurech, c_admitted,
            c_norovirus, c_rotavirus))

# adding footnotes to table
tab1 <- tab1 |>
  modify_footnote_header(
    footnote='Column percentages shown, exclusive of missing values ("Unkown")',
    columns=all_stat_cols()) |>
  modify_footnote_body(
    footnote="Partial season data for 2025-26, up to/including Jan 31, 2026",
    columns="label",
    rows=variable=="c_season" & row_type=="label") |>
  modify_footnote_body(
    footnote="Results from RT-PCR testing after stool sample collection",
    columns="label",
    rows=variable %in% c("c_norovirus", "c_rotavirus") & row_type=="label")

# copy/paste table to README.md
as_kable(tab1, format="pipe") |> writeClipboard()

# next add self-reported symptoms