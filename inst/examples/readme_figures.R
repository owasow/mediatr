# Regenerate the README sample diagrams in man/figures/.
# Run from the package root:  Rscript inst/examples/readme_figures.R
# Requires: pdflatex, pdftoppm (poppler), and the mediation + lavaan packages.

library(mediatr)

stopifnot(file.exists("DESCRIPTION"))
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
out_dir <- normalizePath("man/figures", mustWork = TRUE)

compile_png <- function(tikz_code, name, dpi = 300) {
  td <- tempfile(name)
  dir.create(td)
  tex <- file.path(td, paste0(name, ".tex"))
  writeLines(c(
    "\\documentclass[border=10pt]{standalone}",
    "\\usepackage{tikz}",
    "\\usetikzlibrary{arrows.meta,positioning,calc}",
    "\\usepackage{xcolor}",
    "\\begin{document}",
    tikz_code,
    "\\end{document}"
  ), tex)
  owd <- setwd(td)
  on.exit(setwd(owd), add = TRUE)
  system2("pdflatex", c("-interaction=nonstopmode", basename(tex)), stdout = NULL)
  pdf <- paste0(name, ".pdf")
  if (!file.exists(pdf)) stop("pdflatex failed for ", name)
  system2("pdftoppm", c("-png", "-r", dpi, "-singlefile", pdf, name))
  file.copy(paste0(name, ".png"), file.path(out_dir, paste0(name, ".png")),
            overwrite = TRUE)
  message("wrote man/figures/", name, ".png")
}

## --- Single mediator: the README's own jobs example --------------------
library(mediation)

set.seed(2026)
data(jobs)
model_m <- lm(job_seek ~ treat, data = jobs)
model_y <- lm(depress2 ~ treat + job_seek, data = jobs)
med_out <- mediate(model_m, model_y, treat = "treat", mediator = "job_seek",
                   sims = 1000)

single_dat <- med_data_prep_df(
  med_out, model_m,
  lab_x = "Job Training",
  lab_y = "Depression",
  lab_m = "Job Search\nSelf-Efficacy",
  mode  = "article"
)
compile_png(med_diagram_acme_tikz(single_dat), "README-single-acme")

## --- Dual mediator: simulated two-channel example ----------------------
library(lavaan)

set.seed(2026)
n <- 1500
sim <- data.frame(x = rbinom(n, 1, 0.5))
sim$m1 <- 0.50 * sim$x + rnorm(n)               # strong a1
sim$m2 <- 0.25 * sim$x + rnorm(n)               # weaker a2
sim$y  <- 0.40 * sim$m1 + 0.00 * sim$m2 -       # b2 null (dotted path)
          0.20 * sim$x + rnorm(n)

# The := labels below are the names sem_dual_med_data_prep_df() expects by
# default (see ?sem_dual_med_data_prep_df; remappable via path_labels=).
dual_spec <- '
    m1 ~ a1*x
    m2 ~ a2*x
    y  ~ b1*m1 + b2*m2 + c*x

    indirect_anger := a1 * b1
    indirect_fear := a2 * b2
    total_indirect := indirect_anger + indirect_fear
    total := c + total_indirect
'
fit <- sem(dual_spec, data = sim, se = "bootstrap", bootstrap = 500)

dual_dat <- sem_dual_med_data_prep_df(
  fit,
  lab_x  = "Treatment",
  lab_y  = "Outcome",
  lab_m1 = "Mediator 1",
  lab_m2 = "Mediator 2",
  mode   = "article"
)

# Classic two-color layout
compile_png(sem_dual_med_diagram_tikz(dual_dat, show_paths = TRUE),
            "README-dual-color")

# Monochrome, arrow width proportional to |coefficient| (v0.3.0)
compile_png(sem_dual_med_diagram_tikz(dual_dat, show_paths = TRUE,
                                      bw = TRUE, weight_by = "coefficient"),
            "README-dual-weighted-bw")
