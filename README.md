# mediatr

Publication-ready mediation analysis diagrams and tables for LaTeX documents.

## Installation

```r
# Install from GitHub
devtools::install_github("owasow/mediatr")
```

## Overview

`mediatr` extracts coefficients from mediation analysis output and generates:

- **TikZ diagrams** for LaTeX articles and Beamer slides
- **Formatted tables** via kable for RMarkdown/Quarto

Supports two backends:

- **`mediation` package**: Single mediator models via `mediate()`
- **`lavaan` package**: Dual mediator and serial mediation SEM models

## Usage: Single Mediator (mediation package)

```r
library(mediatr)
library(mediation)

# Fit models
data(jobs)
model_m <- lm(job_seek ~ treat, data = jobs)
model_y <- lm(depress2 ~ treat + job_seek, data = jobs)

# Run mediation analysis
med_out <- mediate(model_m, model_y,
                   treat = "treat",
                   mediator = "job_seek",
                   sims = 1000)

# Create table
med_table(med_out, caption = "Mediation Results")

# Create TikZ diagram
diagram_data <- med_data_prep_df(
  med_out,
  model_m,
  lab_x = "Job Training",
  lab_y = "Depression",
  lab_m = "Job Search\nSelf-Efficacy",
  mode = "article"
)

# Basic diagram
tikz_code <- med_diagram_tikz(diagram_data)

# Diagram with curved ACME arrow (recommended)
tikz_code <- med_diagram_acme_tikz(diagram_data)

# Write to file for LaTeX \input{}
writeLines(tikz_code, "mediation_diagram.tex")
```

## Inline Output with cat()

In Sweave (.Rnw) or knitr documents, you can output diagrams directly inline using `cat()` instead of writing to a separate file:

```r
# In a Sweave/knitr chunk with results="asis"
cat(med_diagram_acme_tikz(diagram_data, scale = 0.5))
```

This approach:

- Avoids creating intermediate .tex files
- Keeps diagram code directly in your document flow
- Works well when diagrams are generated dynamically

Example chunk header for .Rnw files:
```
<<diagram, results="asis", echo=FALSE>>=
cat(med_diagram_acme_tikz(diagram_data, scale = 0.5))
@
```

For R Markdown with PDF output, use a raw LaTeX block or `results='asis'` chunk.

## Diagram Modes

Two preset modes optimize output for different contexts:

- **`mode = "article"`**: Compact sizing, no CIs in diagram (cleaner for journal figures)
- **`mode = "slide"`**: Larger sizing with CIs, suitable for Beamer presentations

## Usage: Dual Mediator SEM (lavaan package)

For models with two parallel mediators (X → M1 + M2 → Y):

```r
library(mediatr)
library(lavaan)

# Specify dual mediator model
model_spec <- '
    # Mediator models
    m1 ~ a1*x + covariates
    m2 ~ a2*x + covariates

    # Outcome model
    y ~ b1*m1 + b2*m2 + c*x + covariates

    # Indirect effects (use these exact labels)
    indirect_anger := a1 * b1
    indirect_fear := a2 * b2
    total_indirect := indirect_anger + indirect_fear
    total := c + total_indirect
'

# Fit with bootstrap CIs
fit <- sem(model_spec, data = mydata, se = "bootstrap", bootstrap = 500)

# Create table
sem_dual_med_table(fit, caption = "Dual Mediator Results")

# Create diagram
diagram_data <- sem_dual_med_data_prep_df(
    fit,
    lab_x = "Treatment",
    lab_y = "Outcome",
    lab_m1 = "Mediator 1",
    lab_m2 = "Mediator 2",
    mode = "article"
)

tikz_code <- sem_dual_med_diagram_tikz(diagram_data, show_paths = TRUE)
cat(tikz_code)
```

## Usage: Serial Mediation SEM (lavaan package)

For serial mediation chains (X → M1 → M2 → Y):

```r
# Specify serial mediation model
model_spec <- '
    # Serial chain
    m1 ~ a1*x + covariates
    m2 ~ a2*m1 + d1*x + covariates
    y  ~ b1*m1 + b2*m2 + c*x + covariates

    # Indirect effects
    ind_serial := a1 * a2 * b2    # X -> M1 -> M2 -> Y
    ind_teach  := a1 * b1         # X -> M1 -> Y
    ind_enrb   := d1 * b2         # X -> M2 -> Y
    total_indirect := ind_serial + ind_teach + ind_enrb
    total := c + total_indirect
'

fit <- sem(model_spec, data = mydata, se = "bootstrap", bootstrap = 500)

# Create diagram
diagram_data <- sem_serial_med_data_prep_df(
    fit,
    lab_x = "Treatment",
    lab_y = "Outcome",
    lab_m1 = "Mediator 1",
    lab_m2 = "Mediator 2"
)

tikz_code <- sem_serial_med_diagram_tikz(diagram_data)
cat(tikz_code)
```

## LaTeX Requirements

Add to your preamble:

```latex
\usepackage{tikz}
\usetikzlibrary{arrows.meta}
\usepackage{xcolor}
```

## Examples

Minimal working LaTeX examples are included in the package:

```r
# Find examples folder
system.file("examples", package = "mediatr")
```

- `mediatr_example_article.tex` — article/paper template
- `mediatr_example_beamer.tex` — Beamer slides template

Both show the required preamble and how to `\input{}` generated diagrams.

## Functions

### Single Mediator (mediation package)

| Function | Description |
|----------|-------------|
| `med_tidy()` | Extract mediation results to tidy data frame |
| `med_table()` | Create formatted kable table |
| `med_data_prep_df()` | Prepare data for diagrams |
| `med_diagram_tikz()` | Basic TikZ diagram |
| `med_diagram_acme_tikz()` | TikZ diagram with curved ACME arrow |

### Dual Mediator SEM (lavaan)

| Function | Description |
|----------|-------------|
| `sem_dual_med_tidy()` | Extract SEM results to tidy data frame |
| `sem_dual_med_table()` | Create formatted kable table |
| `sem_dual_med_data_prep_df()` | Prepare data for dual mediator diagrams |
| `sem_dual_med_diagram_tikz()` | TikZ diagram with parallel mediators |
| `sem_dual_med_diagram_compact_tikz()` | Compact layout variant |

### Serial Mediation SEM (lavaan)

| Function | Description |
|----------|-------------|
| `sem_serial_med_data_prep_df()` | Prepare data for serial mediation diagrams |
| `sem_serial_med_diagram_tikz()` | TikZ diagram for X → M1 → M2 → Y chains |

### Utilities

| Function | Description |
|----------|-------------|
| `format2()` | Format numbers to fixed decimals |
| `starify()` | Convert p-values to significance stars |
| `latexify()` | Escape special characters for LaTeX |

## License

MIT
