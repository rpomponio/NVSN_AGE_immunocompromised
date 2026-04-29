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

# read dataset in SAS format, convert to R-friendly object (data.table)
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

# technically part of Table 2--stool test
dat[clinicalspec %in% c(0, 1), clin_stoolresult:=fcase(
  clinicalrota %in% c(0, 1), "Tested",
  clinicalnoro %in% c(0, 1), "Tested",
  clinicalbact %in% c(0, 1), "Tested",
  default="Not Available")]
dat[!is.na(specimencol), lab_stoolresult:=fcase(
  specimentest==1, "Tested",
  specimencol==1, "Not Tested",
  default="Not Collected")]
dat[clin_stoolresult=="Tested" | lab_stoolresult=="Tested", any_norovirus:=fcase(
  rtpcr_norogi==1, "Positive",
  rtpcr_norogii==1, "Positive",
  clinicalnoro==1, "Positive",
  rtpcr_norogi==0 & rtpcr_norogii==0, "Negative",
  default="Inconclusive or Clinically Neg.")]
dat[clin_stoolresult=="Tested" | lab_stoolresult=="Tested", any_rotavirus:=fcase(
  rtpcr_result==1, "Positive",
  clinicalrota==1, "Positive",
  rtpcr_result==0, "Negative",
  default="Inconclusive or Clinically Neg.")]
dat[clin_stoolresult=="Tested", clin_bacterial:=fcase(
  clinicalbact==1, "Positive",
  clinicalbact==0, "Negative")]
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
             clin_stoolresult="Clinical Testing Availability",
             any_norovirus="Norovirus", any_rotavirus="Rotavirus",
             clin_bacterial="Bacterial Pathogens"),
  include=c(bl_season, bl_studysite, bl_agemonths, bl_insurance,
            sr_feveryes, sr_diarrhea, sr_vomiting, out_admitted, out_inptlos,
            lab_stoolresult, clin_stoolresult, any_norovirus, any_rotavirus,
            clin_bacterial))

# add formatting to table
tab1 <- tab1 |>
  add_variable_group_header(
    header="**Baseline Characteristics**",
    variables=starts_with("bl_")) |>
  add_variable_group_header(
    header="**Self-reported Symptoms**",
    variables=starts_with("sr_")) |>
  add_variable_group_header(
    header="**Outcomes**",
    variables=starts_with("out_")) |>
  add_variable_group_header(
    header="**Lab Results**",
    variables=starts_with("lab_") | starts_with("clin_") | starts_with("any_")) |>
  modify_indent(
    columns="label",
    rows=row_type=="level",
    indent=8L) |>
  modify_indent(
    columns="label",
    rows=row_type=="missing",
    indent=8L) |>
  modify_footnote_header(
    footnote='Column percentages shown, exclusive of missing ("Unkown") values',
    columns=all_stat_cols()) |>
  modify_footnote_body(
    footnote="Season data for 2025-26 is partial thru Jan 31, 2026",
    columns="label",
    rows=variable=="bl_season" & row_type=="label") |>
  modify_footnote_body(
    footnote="Results from RT-PCR testing (if avail.) otherwise clinical testing",
    columns="label",
    rows=variable %in% c("any_norovirus", "any_rotavirus") & row_type=="label") |>
  modify_footnote_body(
    footnote="Result of clinicial testing, unspecified bacterial pathogen(s)",
    columns="label",
    rows=variable=="clin_bacterial" & row_type=="label")

# copy/paste table to README.md
as_kable(tab1, format="pipe") |> writeClipboard()

# NEXT: investigate rotavirus vaccine coverage?

# IF TIME: separate table of immunocomp/unknown characteristics, BY SITE