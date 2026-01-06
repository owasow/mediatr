# mediatr

Publication-ready mediation analysis diagrams and tables for LaTeX documents.

## Installation

```r
# Install from GitHub
devtools::install_github("owasow/mediatr")
```

## Overview

`mediatr` extracts coefficients from `mediation` package output and generates:

- **TikZ diagrams** for LaTeX articles and Beamer slides
- **Formatted tables** via kable for RMarkdown/Quarto

## Usage

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

| Function | Description |
|----------|-------------|
| `med_tidy()` | Extract mediation results to tidy data frame |
| `med_table()` | Create formatted kable table |
| `med_data_prep_df()` | Prepare data for diagrams |
| `med_diagram_tikz()` | Basic TikZ diagram |
| `med_diagram_acme_tikz()` | TikZ diagram with curved ACME arrow |
| `format2()` | Format numbers to fixed decimals |
| `starify()` | Convert p-values to significance stars |

## License

MIT
