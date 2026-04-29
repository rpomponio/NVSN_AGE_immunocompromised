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
dat <- data.table(sas)[scrdate>=as.Date("2024-10-01")]

# check integrity of critical variables
any(is.na(dat$caseid))
range(dat$scrdate)
table(dat$c_ageyear)
table(dat$studysite)
table(dat$provider, dat$c_carelevel)
table(dat$immcomp, exclude=NULL)
summary(dat$c_agemonths)

# begin building Table 1--baseline characteristics stratifying by IC status
dat[, bl_season:=factor(c_ageyear, c(20, 21), c("2024-25", "2025-26"))]
dat[, bl_studysite:=factor(studysite, c(1:6, 8), c("Vanderbilt", "Rochester",
                                                  "Cincinnati", "Seattle",
                                                  "Houston", "Kansas City",
                                                  "Pittsburgh"))]
dat[, bl_agemonths:=as.numeric(c_agemonths)]
dat[, bl_immcomp:=factor(immcomp, c(0:1, 8), c("Not immunocompromised",
                                              "Immunocompromised",
                                              "Status Unknown"))]
dat[, bl_insurance:=factor(c_insurech, 0:3, c("Public", "Private", "Both",
                                            "No insurance"))]
dat[!is.na(fever), sr_feveryes:=as.integer(fever==1)]
dat[!is.na(diarrh), sr_diarrhea:=as.integer(diarrh==1)]
dat[!is.na(vomit), sr_vomiting:=as.integer(vomit==1)]

# technically part of Table 2--laboratory confirmed detections + admission status
dat[, lab_stoolresult:=fcase(
  !is.na(rtpcr_norotdat), "Tested",
  !is.na(rtpcr_rotatdat), "Tested",
  default="Not Tested")]
dat[!is.na(rtpcr_norogi) & !is.na(rtpcr_norogii), lab_norovirus:=fcase(
  rtpcr_norogi==1, "Positive",
  rtpcr_norogii==1, "Positive",
  rtpcr_norogi==0 & rtpcr_norogii==0, "Negative",
  default="Inconclusive")]
dat[!is.na(rtpcr_result), lab_rotavirus:=fcase(
  rtpcr_result==1, "Positive",
  rtpcr_result==0, "Negative",
  default="Inconclusive")]
dat[, out_admitted:=factor(c_carelevel<=2, c(TRUE, FALSE), c("Admitted", "ED-only"))]
dat[, out_inptlos:=as.integer(c_los)]

# mockup of Table 1 (inlcuding unknown IC status)
tab1 <- tbl_summary(
  dat,
  by=bl_immcomp,
  label=list(bl_season="AGE Season", bl_studysite="Site",
             bl_agemonths="Age, months",
             bl_insurance="Insurance type", sr_feveryes="Fever",
             sr_diarrhea="Diarrhea", sr_vomiting="Vomiting",
             out_admitted="Final hospitalization status",
             out_inptlos="Inpatient length of stay, days",
             lab_stoolresult="RT-PCR Availability",
             lab_norovirus="Norovirus", lab_rotavirus="Rotavirus"),
  include=c(bl_season, bl_studysite, bl_agemonths, bl_insurance,
            sr_feveryes, sr_diarrhea, sr_vomiting, out_admitted, out_inptlos,
            lab_stoolresult, lab_norovirus, lab_rotavirus))

# add formatting to table
tab1 <- tab1 |>
  add_variable_group_header(
    header="Baseline Characteristics",
    variables=starts_with("bl_")) |>
  add_variable_group_header(
    header="Self-reported Symptoms",
    variables=starts_with("sr_")) |>
  add_variable_group_header(
    header="Outcomes",
    variables=starts_with("out_")) |>
  add_variable_group_header(
    header="Lab Results",
    variables=starts_with("lab_")) |>
  modify_indent(
    columns="label",
    rows=row_type=="level",
    indent=8L) |>
  modify_indent(
    columns="label",
    rows=row_type=="missing",
    indent=8L) |>
  modify_footnote_header(
    footnote='Column percentages shown, exclusive of missing values ("Unkown")',
    columns=all_stat_cols()) |>
  modify_footnote_body(
    footnote="Partial season data for 2025-26, up to/including Jan 31, 2026",
    columns="label",
    rows=variable=="bl_season" & row_type=="label") |>
  modify_footnote_body(
    footnote="Norovirus, Rotavirus results from RT-PCR testing (when available)",
    columns="label",
    rows=variable %in% c("lab_norovirus", "lab_rotavirus") & row_type=="label")

# copy/paste table to README.md
as_kable(tab1, format="pipe") |> writeClipboard()

# experiment with "suspected etiology" as cause of illness
dat[, suspected_eti:=fcase(
  lab_norovirus=="Positive", "Viral",
  lab_rotavirus=="Positive", "Viral",
  sr_feveryes==1 & lab_stoolresult=="Tested", "Non-viral")]
table(dat$bl_immcomp, dat$suspected_eti)
tbl_summary(
  dat,
  by=bl_immcomp,
  include=suspected_eti,
  label=list(suspected_eti="Suspected Etiology"))
