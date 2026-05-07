################################################### -
## Title: Figures for preliminary analysis, immunocompromised vs. not
## Author: Ray Pomponio
## Email: pomponiord@upmc.edu
## Project: AGE Immunocompromised
## Date Created: 2026-04-20
## Note: Code written with assistance from Claude (Anthropic);
##       all analytical decisions made by the author
################################################### -

library(ggplot2)
library(ggpattern)
library(data.table)
library(scales)
theme_set(theme_classic(base_size=14))

# run analysis script to produce prelim and prelim.tested
source("prelim.R")

# ── Shared aesthetics ─────────────────────────────────────────────────────────

# group colors: blue = not immunocompromised, orange = immunocompromised
GROUP.COLORS <- c("Not immunocompromised"="#4E84C4",
                  "Immunocompromised"="#E07B39")
GROUP.LABELS <- c("Not immunocompromised"="Not IC",
                  "Immunocompromised"="IC")

# shared theme additions applied to every figure
theme.fig <- function() {
  theme(
    legend.position="bottom",
    legend.title=element_blank(),
    plot.title=element_text(face="bold", size=13),
    plot.subtitle=element_text(size=11, color="grey40"),
    axis.title=element_text(size=11),
    strip.text=element_text(face="bold"))
}

# ── Fig 1: Admission rate + ICU rate (combined) ───────────────────────────────

# admission rate: all patients
fig1.adm <- prelim[, .(
  outcome="Hospitalization",
  pct=mean(d_admitted=="Admitted", na.rm=TRUE) * 100,
  n.num=sum(d_admitted=="Admitted", na.rm=TRUE),
  n.den=.N
), by=d_immcomp]

# ICU rate: admitted patients only
fig1.icu <- prelim[d_admitted=="Admitted" & !is.na(d_icu), .(
  outcome="ICU Admission",
  pct=mean(d_icu=="Yes") * 100,
  n.num=sum(d_icu=="Yes"),
  n.den=.N
), by=d_immcomp]

fig1.dat <- rbindlist(list(fig1.adm, fig1.icu))
fig1.dat[, outcome:=factor(outcome, c("Hospitalization", "ICU Admission"))]
fig1.dat[, label:=paste0(round(pct, 0), "%\n(", n.num, "/", n.den, ")")]
fig1.dat[, pattern:=ifelse(outcome=="ICU Admission", "crosshatch", "none")]

fig1 <- ggplot(fig1.dat,
               aes(x=outcome, y=pct, fill=d_immcomp, pattern=pattern,
                   group=d_immcomp)) +
  geom_bar_pattern(
    stat="identity",
    position=position_dodge(width=0.6),
    width=0.55,
    colour="grey30",           # bar border color
    pattern_colour="grey20",   # crosshatch line color — dark on light fill
    pattern_fill=NA,           # no fill between lines
    pattern_density=0.08,      # controls line thickness
    pattern_spacing=0.015,     # controls spacing between lines
    pattern_angle=45) +        # 45+135 degree lines = crosshatch
  geom_text(aes(label=label),
            position=position_dodge(width=0.6),
            vjust=-0.3, size=3.8, fontface="bold") +
  scale_fill_manual(values=GROUP.COLORS, labels=GROUP.LABELS) +
  scale_pattern_identity() +
  scale_y_continuous(limits=c(0, 115), expand=c(0, 0),
                     labels=function(x) paste0(x, "%")) +
  labs(title="Hospitalization and ICU admission rates",
       subtitle="Solid bars = hospitalization (all patients); crosshatch = ICU (admitted patients only)",
       x=NULL, y="Rate (%)") +
  theme.fig() +
  guides(fill=guide_legend(override.aes=list(pattern="none")))

# ── Fig 2: Age distribution ───────────────────────────────────────────────────

fig2 <- ggplot(prelim, aes(x=d_immcomp, y=d_agemonths, fill=d_immcomp)) +
  geom_violin(alpha=0.4, trim=TRUE, show.legend=FALSE) +
  geom_boxplot(width=0.15, outlier.shape=NA, show.legend=FALSE) +
  scale_fill_manual(values=GROUP.COLORS) +
  scale_x_discrete(labels=GROUP.LABELS) +
  scale_y_continuous(labels=function(x) paste0(x, " mo")) +
  labs(title="Age distribution",
       subtitle="Median age: 29 mo (not IC) vs. 69 mo (IC)",
       x=NULL, y="Age, months") +
  theme.fig()

# ── Fig 3: Length of stay (admitted only) ─────────────────────────────────────

# pseudo-log transformation handles zeros natively; sigma controls the linear
# region around zero (default sigma=1 works well for integer day counts)
fig3 <- ggplot(prelim[d_admitted=="Admitted"],
               aes(x=d_immcomp, y=d_los, fill=d_immcomp)) +
  geom_violin(alpha=0.4, trim=TRUE, adjust=2.5, show.legend=FALSE) +
  geom_boxplot(width=0.15, outlier.shape=NA, show.legend=FALSE) +
  scale_fill_manual(values=GROUP.COLORS) +
  scale_x_discrete(labels=GROUP.LABELS) +
  scale_y_continuous(
    trans=pseudo_log_trans(sigma=1, base=10),
    breaks=c(0, 10, 50, 100, 150),
    labels=as.character(c(0, 10, 50, 100, 150))) +
  labs(title="Length of stay",
       subtitle="Admitted patients only; pseudo-log scale",
       x=NULL, y="Days (pseudo-log scale)") +
  theme.fig()

# ── Fig 4: Site enrollment (stacked horizontal bars) ─────────────────────────

fig4.dat <- prelim[, .(n=.N), by=.(d_immcomp, d_studysite)]
fig4.dat[, pct:=n / sum(n) * 100, by=d_immcomp]
fig4.dat[, d_studysite:=factor(d_studysite, levels=levels(prelim$d_studysite))]
fig4.dat[, label:=ifelse(pct >= 10,
                         paste0(as.character(d_studysite), "\n",
                                round(pct, 0), "%"), "")]

fig4 <- ggplot(fig4.dat,
               aes(x=pct, y=d_immcomp, fill=d_studysite, label=label)) +
  geom_col(color="white", linewidth=0.4) +
  geom_text(position=position_stack(vjust=0.5),
            size=3, lineheight=0.85, fontface="bold", color="white") +
  scale_y_discrete(labels=GROUP.LABELS) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, 101),
                     labels=function(x) paste0(x, "%")) +
  scale_fill_manual(values=c(
    "Vanderbilt"  = "#4E84C4",
    "Rochester"   = "#6CB4A0",
    "Cincinnati"  = "#A8C86A",
    "Seattle"     = "#E07B39",
    "Houston"     = "#C45E8A",
    "Kansas City" = "#8B6BB1",
    "Pittsburgh"  = "#B0956A")) +
  labs(title="Site enrollment",
       subtitle="% of enrolled patients within each immunocompromised group",
       x=NULL, y=NULL) +
  theme.fig() +
  theme(panel.grid.major.x=element_line(color="grey90"),
        axis.text.y=element_text(size=11))

# ── Fig 5: Virology positivity ────────────────────────────────────────────────

fig5.noro <- prelim.tested[!is.na(d_norovirus), .(
  pathogen="Norovirus",
  pct.pos=mean(d_norovirus=="Positive") * 100
), by=d_immcomp]

fig5.rota <- prelim.tested[!is.na(d_rotavirus), .(
  pathogen="Rotavirus",
  pct.pos=mean(d_rotavirus=="Positive") * 100
), by=d_immcomp]

fig5.bact <- prelim.tested[!is.na(d_bacterial), .(
  pathogen="Bacterial",
  pct.pos=mean(d_bacterial=="Positive") * 100
), by=d_immcomp]

fig5.dat <- rbindlist(list(fig5.noro, fig5.rota, fig5.bact))
fig5.dat[, pathogen:=factor(pathogen, c("Norovirus", "Rotavirus", "Bacterial"))]
fig5.dat[, label:=paste0(round(pct.pos, 0), "%")]

fig5 <- ggplot(fig5.dat, aes(x=d_immcomp, y=pct.pos, fill=d_immcomp)) +
  geom_col(width=0.5, show.legend=FALSE) +
  geom_text(aes(label=label), vjust=-0.4, size=4, fontface="bold") +
  scale_fill_manual(values=GROUP.COLORS) +
  scale_x_discrete(labels=GROUP.LABELS) +
  scale_y_continuous(labels=function(x) paste0(x, "%"),
                     expand=c(0, 0), limits=c(0, 60)) +
  facet_wrap(~pathogen) +
  labs(title="Pathogen positivity rates",
       subtitle="Among tested patients only; bacterial = clinical testing only",
       x=NULL, y="% Positive") +
  theme.fig()

# ── Save figures ──────────────────────────────────────────────────────────────

OUT.WIDTH  <- 7   # inches
OUT.HEIGHT <- 5   # inches
OUT.DPI    <- 300

# individual PNGs
ggsave("fig1_admission_icu_rate.png", fig1, width=OUT.WIDTH,       height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig2_age_distribution.png",   fig2, width=OUT.WIDTH,       height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig3_los.png",                fig3, width=OUT.WIDTH,       height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig4_site_enrollment.png",    fig4, width=OUT.WIDTH,       height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig5_virology.png",           fig5, width=OUT.WIDTH * 1.2, height=OUT.HEIGHT, dpi=OUT.DPI)

# single PDF with all figures, one per page
pdf("Immunocomp - Prelim Figures - Generated by Ray.pdf", width=OUT.WIDTH, height=OUT.HEIGHT)
print(fig1)
print(fig2)
print(fig3)
print(fig4)
print(fig5)
dev.off()

cat("Saved: 5 PNG files and Immunocomp - Prelim Figures - Generated by Ray.pdf\n")