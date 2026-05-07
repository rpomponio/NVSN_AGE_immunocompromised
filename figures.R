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
library(data.table)
library(patchwork)
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

# ── Fig 1: Admission rate ─────────────────────────────────────────────────────

# compute admission rate per group
fig1.dat <- prelim[, .(
  n.admitted=sum(d_admitted=="Admitted", na.rm=TRUE),
  n.total=.N
), by=d_immcomp]
fig1.dat[, pct:=n.admitted / n.total * 100]
fig1.dat[, label:=paste0(round(pct, 0), "%\n(", n.admitted, "/", n.total, ")")]

fig1 <- ggplot(fig1.dat, aes(x=d_immcomp, y=pct, fill=d_immcomp)) +
  geom_col(width=0.5, show.legend=FALSE) +
  geom_text(aes(label=label), vjust=-0.4, size=4.5, fontface="bold") +
  scale_fill_manual(values=GROUP.COLORS) +
  scale_x_discrete(labels=GROUP.LABELS) +
  scale_y_continuous(limits=c(0, 110), expand=c(0, 0),
                     labels=function(x) paste0(x, "%")) +
  labs(title="Hospitalization rate",
       subtitle="Admitted vs. ED-only by immunocompromised status",
       x=NULL, y="% Admitted") +
  theme.fig()

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

# ── Fig 3: Severity cascade (admitted only) ───────────────────────────────────

# LOS: median per group
fig3a.dat <- prelim[d_admitted=="Admitted", .(
  median.los=median(d_los, na.rm=TRUE),
  q1=quantile(d_los, 0.25, na.rm=TRUE),
  q3=quantile(d_los, 0.75, na.rm=TRUE)
), by=d_immcomp]

fig3a <- ggplot(fig3a.dat, aes(x=d_immcomp, y=median.los,
                               ymin=q1, ymax=q3, color=d_immcomp)) +
  geom_pointrange(size=1.1, show.legend=FALSE) +
  scale_color_manual(values=GROUP.COLORS) +
  scale_x_discrete(labels=GROUP.LABELS) +
  labs(title="Length of stay",
       subtitle="Median (IQR), admitted patients",
       x=NULL, y="Days") +
  theme.fig()

# ICU admission rate per group
fig3b.dat <- prelim[d_admitted=="Admitted" & !is.na(d_icu), .(
  pct.icu=mean(d_icu=="Yes") * 100,
  n=.N
), by=d_immcomp]
fig3b.dat[, label:=paste0(round(pct.icu, 0), "%")]

fig3b <- ggplot(fig3b.dat, aes(x=d_immcomp, y=pct.icu, fill=d_immcomp)) +
  geom_col(width=0.5, show.legend=FALSE) +
  geom_text(aes(label=label), vjust=-0.4, size=4.5, fontface="bold") +
  scale_fill_manual(values=GROUP.COLORS) +
  scale_x_discrete(labels=GROUP.LABELS) +
  scale_y_continuous(limits=c(0, 30), expand=c(0, 0),
                     labels=function(x) paste0(x, "%")) +
  labs(title="ICU admission",
       subtitle="% admitted to ICU",
       x=NULL, y="% ICU") +
  theme.fig()

# combine LOS and ICU side by side using patchwork
fig3 <- fig3a + fig3b +
  plot_annotation(
    title="Among admitted patients",
    theme=theme(plot.title=element_text(face="bold", size=14)))

# ── Fig 4: Site enrollment ────────────────────────────────────────────────────

fig4.dat <- prelim[, .(n=.N), by=.(d_immcomp, d_studysite)]
fig4.dat[, pct:=n / sum(n) * 100, by=d_immcomp]

fig4 <- ggplot(fig4.dat, aes(x=d_studysite, y=pct, fill=d_immcomp)) +
  geom_col(position="dodge", width=0.6) +
  scale_fill_manual(values=GROUP.COLORS, labels=GROUP.LABELS) +
  scale_y_continuous(labels=function(x) paste0(x, "%"),
                     expand=c(0, 0), limits=c(0, 75)) +
  labs(title="Site enrollment",
       subtitle="Column % within each immunocompromised group",
       x=NULL, y="% of group") +
  theme.fig() +
  theme(axis.text.x=element_text(angle=35, hjust=1))

# ── Fig 5: Virology positivity ────────────────────────────────────────────────

# reshape virology results to long format for faceting
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
ggsave("fig1_admission_rate.png",    fig1, width=OUT.WIDTH, height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig2_age_distribution.png",  fig2, width=OUT.WIDTH, height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig3_severity_cascade.png",  fig3, width=OUT.WIDTH * 1.4, height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig4_site_enrollment.png",   fig4, width=OUT.WIDTH, height=OUT.HEIGHT, dpi=OUT.DPI)
ggsave("fig5_virology.png",          fig5, width=OUT.WIDTH * 1.2, height=OUT.HEIGHT, dpi=OUT.DPI)

# single PDF with all figures, one per page
pdf("Immunocomp - Prelim Figures - Generated by Ray.pdf", width=OUT.WIDTH, height=OUT.HEIGHT)
print(fig1)
print(fig2)
print(fig3)
print(fig4)
print(fig5)
dev.off()

cat("Saved: 5 PNG files and prelim_immunocomp_figures.pdf\n")