################################################### -
## Title: Preliminary analysis, immunocompromised vs. not
## Author: Ray Pomponio
## Email: pomponiord@upmc.edu
## Project: AGE Immunocompromised
## Date Created: 2026-04-20
## Note: Code written with assistance from Claude (Anthropic);
##       all analytical decisions made by the author
################################################### -

library(haven)
library(data.table)
library(gtsummary)
library(ggplot2)
theme_set(theme_classic(base_size=14))

# ── Data ingest ───────────────────────────────────────────────────────────────

# read dataset in SAS format, convert to data.table and filter to study period
sas <- read_sas("pitt_20260413.sas7bdat")
dat <- data.table(sas)[scrdate >= as.Date("2024-09-01")]

# ── Integrity checks ──────────────────────────────────────────────────────────

any(is.na(dat$caseid))
range(dat$scrdate)
table(dat$c_ageyear)
table(dat$studysite)
table(dat$provider, dat$c_carelevel)
table(dat$immcomp, exclude=NULL)
summary(dat$c_agemonths)

# ── Derived variables (prefix: d_) ────────────────────────────────────────────

# stratification variable
dat[, d_immcomp:=factor(immcomp, c(0, 1, 8),
                        c("Not immunocompromised",
                          "Immunocompromised",
                          "Status Unknown"))]

# demographics
dat[, d_sex:=factor(sexch, 1:2, c("Male", "Female"))]
dat[, d_agemonths:=as.numeric(c_agemonths)]
dat[, d_age_under2:=as.integer(c_agemonths < 24)]           # derived from age
dat[, d_diapers:=factor(diapers, 0:1, c("No", "Yes"))]      # 8 -> NA
dat[, d_breastf:=factor(breastf, 0:1, c("No", "Yes"))]      # 8 -> NA; only asked if <5y
dat[, d_race:=factor(c_race_intb, 1:4,
                     c("White", "Black", "Other", "Hispanic"))]  # 8 -> NA
dat[, d_daycare:=factor(daycare, 0:1, c("No", "Yes"))]      # 8 -> NA
dat[, d_insurance:=factor(c_insurech, 0:3,
                          c("Public", "Private", "Both", "No insurance"))]
dat[, d_rotavax:=factor(c_anydose, 0:1,
                        c("No valid dose", "Any valid dose"))]
dat[, d_exposure:=factor(agecontact_12, 0:1, c("No", "Yes"))]  # 8 -> NA
dat[, d_season:=factor(c_ageyear, c(20, 21), c("2024-25", "2025-26"))]
dat[, d_studysite:=factor(studysite, c(1:6, 8),
                          c("Vanderbilt", "Rochester", "Cincinnati",
                            "Seattle", "Houston", "Kansas City",
                            "Pittsburgh"))]

# outcomes
dat[!is.na(fever),  d_fever:=as.integer(fever==1)]
dat[!is.na(diarrh), d_diarrhea:=as.integer(diarrh==1)]
dat[!is.na(vomit),  d_vomiting:=as.integer(vomit==1)]

dat[, d_admitted:=factor(c_carelevel<=2, c(TRUE, FALSE),
                         c("Admitted", "ED-only"))]
dat[, d_los:=as.integer(c_los)]
dat[, d_icu:=factor(hicu, 0:1, c("No", "Yes"))]             # 8 -> NA
dat[, d_ivfluids:=factor(irtherapydur, 0:1, c("No", "Yes"))]  # 8 -> NA

# virology: lab result takes precedence; clinical used only if lab unavailable;
# inconclusive results recoded to NA
dat[clinicalspec %in% c(0, 1), d_clin_stool:=fcase(
  clinicalrota %in% c(0, 1), "Tested",
  clinicalnoro %in% c(0, 1), "Tested",
  clinicalbact %in% c(0, 1), "Tested",
  default="Not Available")]
dat[!is.na(specimencol), d_lab_stool:=fcase(
  specimentest==1, "Tested",
  specimencol==1,  "Not Tested",
  default="Not Collected")]

dat[, d_tested:=(d_clin_stool=="Tested" | d_lab_stool=="Tested")]

# simplify stool testing variables to binary Tested/Not Tested for tables
dat[, d_clin_stool:=fcase(d_clin_stool=="Tested", "Tested", default="Not Tested")]
dat[, d_lab_stool:=fcase(d_lab_stool=="Tested",   "Tested", default="Not Tested")]

dat[d_tested==TRUE, d_norovirus:=fcase(
  rtpcr_norogi==1,                    "Positive",  # lab positive GI
  rtpcr_norogii==1,                   "Positive",  # lab positive GII
  rtpcr_norogi==0 & rtpcr_norogii==0, "Negative",  # lab negative
  clinicalnoro==1,                    "Positive",  # clinical positive (no lab)
  clinicalnoro==0,                    "Negative")] # clinical negative (no lab)
# all other cases (inconclusive, untestable) -> NA

dat[d_tested==TRUE, d_rotavirus:=fcase(
  rtpcr_result==1, "Positive",   # lab positive
  rtpcr_result==0, "Negative",   # lab negative
  clinicalrota==1, "Positive",   # clinical positive (no lab)
  clinicalrota==0, "Negative")]  # clinical negative (no lab)
# all other cases -> NA

dat[d_clin_stool=="Tested", d_bacterial:=fcase(
  clinicalbact==1, "Positive",
  clinicalbact==0, "Negative")]

# ── Presentation subset ───────────────────────────────────────────────────────

# exclude unknown IC status for presentation tables; retain in dat for downstream use
# droplevels prevents "Status Unknown" from appearing as an empty column
prelim <- droplevels(dat[d_immcomp != "Status Unknown"])
stopifnot(nrow(prelim)==nrow(dat) - 91L)

# tested-only subset for virology (explicit, not relying on NA drop)
prelim.tested <- prelim[d_tested==TRUE]

# ── Shared helpers ─────────────────────────────────────────────────────────────

# applied to each sub-table before stacking; requires p.value column to be present
fmt.stars <- function(tbl) {
  tbl |>
    bold_p() |>
    modify_table_body(
      ~dplyr::mutate(.x,
                     label=ifelse(
                       row_type=="label" & variable %in% .x$variable[!is.na(.x$p.value) & .x$p.value < 0.001],
                       paste0(label, " ***"),
                       ifelse(
                         row_type=="label" & variable %in% .x$variable[!is.na(.x$p.value) & .x$p.value < 0.01],
                         paste0(label, " **"),
                         ifelse(
                           row_type=="label" & variable %in% .x$variable[!is.na(.x$p.value) & .x$p.value < 0.05],
                           paste0(label, " *"),
                           label)))))
}

# applied to the final table after stacking; handles footnotes and indentation
fmt <- function(tbl) {
  tbl |>
    modify_footnote_header(
      footnote='Column percentages shown, exclusive of missing ("Unknown") values',
      columns=all_stat_cols()) |>
    modify_footnote_header(
      footnote="N=91 patients with unknown immunocompromised status excluded from this comparison",
      columns=starts_with("stat_")) |>
    modify_indent(columns="label", rows=row_type=="level",   indent=8L) |>
    modify_indent(columns="label", rows=row_type=="missing", indent=8L)
}

# ── TABLE 1: Demographics ─────────────────────────────────────────────────────

DEMO.VARS <- c("d_sex", "d_agemonths", "d_age_under2", "d_diapers", "d_breastf",
               "d_race", "d_daycare", "d_insurance", "d_rotavax", "d_exposure",
               "d_studysite")
DEMO.LABELS <- list(
  d_sex        = "Sex",
  d_agemonths  = "Age, months",
  d_age_under2 = "Age < 2 years",
  d_diapers    = "Still wearing diapers",
  d_breastf    = "Breastfed",
  d_race       = "Race/Ethnicity",
  d_daycare    = "Daycare or school attendance",
  d_insurance  = "Insurance type",
  d_rotavax    = "Received rotavirus vaccine",
  d_exposure   = "Exposure to someone with diarrhea/vomiting",
  d_studysite  = "Site")

tab.demo <- tbl_summary(
  prelim,
  by=d_immcomp,
  label=DEMO.LABELS,
  type=list(
    d_age_under2 ~ "dichotomous",
    d_diapers    ~ "categorical",
    d_breastf    ~ "categorical",
    d_daycare    ~ "categorical",
    d_exposure   ~ "categorical"),
  include=all_of(DEMO.VARS)) |>
  add_p(test=list(
    d_studysite       ~ "chisq.test",
    d_diapers         ~ "fisher.test",
    d_breastf         ~ "fisher.test",
    d_daycare         ~ "fisher.test",
    d_exposure        ~ "fisher.test",
    all_continuous()  ~ "kruskal.test",
    all_dichotomous() ~ "fisher.test")) |>
  modify_footnote_body(
    footnote="Only asked for children < 5 years of age; missing for older children",
    columns="label",
    rows=variable=="d_breastf" & row_type=="label") |>
  modify_footnote_body(
    footnote="Seasonal data from subjects enrolled September 1, 2024 thru January 31, 2026.",
    columns="label",
    rows=variable=="d_studysite" & row_type=="label") |>
  modify_footnote_header(
    footnote="Chi-square test used for Site; Fisher's exact test for all other categorical variables; Kruskal-Wallis test for continuous variables",
    columns="p.value") |>
  fmt.stars() |>
  fmt()

tab.demo

# ── TABLE 2: Outcomes ─────────────────────────────────────────────────────────

# symptoms, hospitalization, and stool testing availability (all patients)
OUT.ALL.VARS <- c("d_fever", "d_diarrhea", "d_vomiting", "d_admitted",
                  "d_clin_stool", "d_lab_stool")
OUT.ALL.LABELS <- list(
  d_fever      = "Fever",
  d_diarrhea   = "Diarrhea",
  d_vomiting   = "Vomiting",
  d_admitted   = "Hospitalization status",
  d_clin_stool = "Any clinical stool testing",
  d_lab_stool  = "Any laboratory stool testing")

tab.out.all <- tbl_summary(
  prelim,
  by=d_immcomp,
  label=OUT.ALL.LABELS,
  type=list(
    d_fever    ~ "dichotomous",
    d_diarrhea ~ "dichotomous",
    d_vomiting ~ "dichotomous"),
  include=all_of(OUT.ALL.VARS)) |>
  add_p(test=list(
    all_categorical() ~ "fisher.test")) |>
  fmt.stars()

# admitted patients only: LOS, ICU, IV fluids
OUT.ADM.VARS <- c("d_los", "d_icu", "d_ivfluids")
OUT.ADM.LABELS <- list(
  d_los      = "Length of stay, days",
  d_icu      = "ICU admission",
  d_ivfluids = "Received IV fluids")

tab.out.adm <- tbl_summary(
  prelim[d_admitted=="Admitted"],
  by=d_immcomp,
  label=OUT.ADM.LABELS,
  type=list(
    d_icu      ~ "dichotomous",
    d_ivfluids ~ "dichotomous"),
  include=all_of(OUT.ADM.VARS)) |>
  add_p(test=list(
    all_continuous()  ~ "kruskal.test",
    all_categorical() ~ "fisher.test")) |>
  fmt.stars()

# laboratory results (tested patients only)
VIRO.VARS <- c("d_norovirus", "d_rotavirus", "d_bacterial")
VIRO.LABELS <- list(
  d_norovirus = "Norovirus",
  d_rotavirus = "Rotavirus",
  d_bacterial = "Bacterial Pathogens")

tab.out.viro <- tbl_summary(
  prelim.tested,
  by=d_immcomp,
  label=VIRO.LABELS,
  include=all_of(VIRO.VARS)) |>
  add_p(test=list(
    all_categorical() ~ "fisher.test")) |>
  modify_footnote_body(
    footnote=paste("Among tested patients only (N=", nrow(prelim.tested), ").",
                   "Lab result used preferentially; clinical result used if lab unavailable.",
                   "Inconclusive results treated as missing."),
    columns="label",
    rows=variable=="d_norovirus" & row_type=="label") |>
  modify_footnote_body(
    footnote="Result of clinical testing, unspecified bacterial pathogen(s)",
    columns="label",
    rows=variable=="d_bacterial" & row_type=="label") |>
  fmt.stars()

# stack all three outcome sections
tab.outcomes <- tbl_stack(
  list(tab.out.all, tab.out.adm, tab.out.viro),
  group_header=c("Symptoms & Hospitalization",
                 "Admitted patients only",
                 "Laboratory Results (tested patients only)")) |>
  modify_footnote_header(
    footnote="Fisher's exact test for categorical variables; Kruskal-Wallis test for continuous variables",
    columns="p.value") |>
  fmt()

tab.outcomes