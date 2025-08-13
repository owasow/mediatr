# mediatr

A personal R package for creating publication-ready mediation analysis diagrams.

## Installation

```r
# Install from local directory
devtools::install("path/to/mediatr")

# Load the package
library(mediatr)
```

## Quick Start

```r
library(mediatr)
library(mediation)

# Run your mediation analysis
med_results <- mediate(model.m, model.y, treat = "x", mediator = "m")

# Create a diagram
create_mediation_diagram(
  med_model = med_results,
  x_on_m_model = model.m,
  x_label = "Treatment",
  m_label = "Mediator",
  y_label = "Outcome",
  output_type = "graphviz"
)

# Create a table
med_table(med_results, caption = "Mediation Analysis Results")
```

## Available Functions

- `med_table()` - Create formatted mediation results table
- `create_mediation_diagram()` - Create diagrams (wrapper function)
- `med_prepare_diagram_data()` - Prepare data for diagrams
- `med_diagram_graphviz()` - Create GraphViz/DiagrammeR diagram
- `med_diagram_plotmat()` - Create plotmat diagram
- `med_diagram_tikz()` - Create TikZ diagram for LaTeX

