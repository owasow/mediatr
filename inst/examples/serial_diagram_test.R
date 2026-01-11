#' ---
#' title: "Serial Mediation Diagram Test"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' header-includes:
#'   - \usepackage{tikz}
#'   - \usetikzlibrary{arrows.meta}
#'   - \usepackage{xcolor}
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)

#+ load-functions, message=FALSE
library(dplyr)
library(glue)
library(stringr)

# Source all package R files
pkg_dir <- here::here("R")
for (f in list.files(pkg_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

#' # Test Data
#'
#' Pre-built data frame - edit coefficients here to test different scenarios.

#+ test-data
test_data <- data.frame(
  lab_x = "Black\\\\Funding",
  lab_m1 = "Rosenwald\\\\Teachers",
  lab_m2 = "Black\\\\Enrollment",
  lab_y = "Protest\\\\Participation",
  coef_a1 = "0.40$^{***}$",
  coef_a2 = "-0.04$^{**}$",
  coef_d1 = "0.24$^{***}$",
  coef_b1 = "0.17$^{***}$",
  coef_b2 = "-0.09$^{*}$",
  coef_c = "-0.07$^{*}$",
  coef_ind_serial = "0.00",
  coef_ind_m1 = "0.07$^{***}$",
  coef_ind_m2 = "-0.02$^{*}$",
  coef_total_ind = "0.05$^{*}$",
  coef_total = "-0.02",
  stringsAsFactors = FALSE
)

#' # Serial Mediation Diagram

#+ diagram, results='asis', echo=FALSE
cat("\\begin{figure}[htbp]\n\\centering\n")
cat(sem_serial_med_diagram_tikz(test_data, scale = 0.38, diag_label = "Model 2: Serial"))
cat("\n\\caption{Serial mediation: Funding $\\rightarrow$ Teachers $\\rightarrow$ Enrollment $\\rightarrow$ Protest}\n\\end{figure}\n")
