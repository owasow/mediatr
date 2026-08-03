#' @import dplyr
#' @import glue
#' @import stringr
#' @importFrom stats coef
NULL

#' Format numeric values to fixed decimal places
#'
#' @param x Numeric value to format
#' @param digits Number of decimal places (default: 2)
#' @return Formatted character string
#' @export
#' @examples
#' format2(3.14159, digits = 2)
#' # "3.14"
format2 <- function(x, digits = 2) {
  formatC(x, format = 'f', digits = digits) %>%
    as.character()
}


#' Convert p-values to significance stars
#'
#' @param x Numeric p-value
#' @param stars Number of significance tiers: 3 for standard (*, **, ***) or 1 for single threshold
#' @param alpha Significance threshold for single-tier mode (default: 0.05)
#' @return Character string with significance stars
#' @export
#' @examples
#' starify(0.001)
#' # "***"
#' starify(0.03)
#' # "*"
#' starify(0.10)
#' # ""
starify <- function(x, stars = 3, alpha = 0.05) {
  if (stars == 3) {
    dplyr::case_when(
      x <  0.001              ~ "***",
      x >= 0.001  & x < 0.01  ~ "**",
      x >= 0.01   & x < 0.05  ~ "*",
      x >= 0.05               ~ ""
    )
  } else {
    dplyr::case_when(
      x <= alpha ~ "*",
      TRUE       ~ ""
    )
  }
}


#' Build a per-arrow TikZ style function for weighted diagrams
#'
#' Internal factory shared by all *_diagram_tikz() functions. Returns a
#' function that maps one formatted coefficient string (as produced by the
#' *_data_prep_df() functions, i.e. "0.42$^{***}$"-style) to a TikZ option
#' fragment such as ", line width=1.2pt" or ", line width=0.3pt, densely
#' dotted".
#'
#' Modes: "none" returns "" for every path (legacy output, byte-identical).
#' "significance" takes the width from the path's own star tier
#' (tier_widths = pt widths for 0/1/2/3 stars). "coefficient" scales width
#' with the |estimate| parsed from the leading number of the coefficient
#' string (coef_widths = min/max pt; |estimate| >= ref saturates; ref
#' defaults to the largest |estimate| among styled_coefs, or pass coef_ref
#' to share a scale across panels). In both weighted modes paths that are
#' not statistically significant (zero stars) render densely dotted so the
#' topology stays visible; strings without star markup are left unstyled.
#'
#' @noRd
.tier_style_factory <- function(weight_by,
                                styled_coefs,
                                tier_widths = c(0.3, 0.45, 0.8, 1.25),
                                coef_widths = c(0.3, 1.25),
                                coef_ref = NULL) {

  .star_tier <- function(coef_str) {
    if (is.na(coef_str)) return(NA_integer_)
    m <- regmatches(coef_str, gregexpr("\\$\\^\\{(\\**)\\}\\$", coef_str))[[1]]
    if (!length(m)) return(NA_integer_)
    max(nchar(gsub("[^*]", "", m)))
  }
  .coef_est <- function(coef_str) {
    if (is.na(coef_str)) return(NA_real_)
    m <- regmatches(coef_str, regexpr("^\\s*-?[0-9]+\\.?[0-9]*", coef_str))
    if (!length(m)) return(NA_real_)
    as.numeric(m)
  }

  if (weight_by == "coefficient") {
    stopifnot(length(coef_widths) == 2, coef_widths[1] <= coef_widths[2])
    ests <- vapply(styled_coefs, .coef_est, numeric(1))
    ref  <- if (!is.null(coef_ref)) coef_ref else suppressWarnings(max(abs(ests), na.rm = TRUE))
    if (!is.finite(ref) || ref <= 0) ref <- 1  # degenerate: all zero/unparseable -> min widths
  }

  function(coef_str) {
    if (weight_by == "none") return("")
    n <- .star_tier(coef_str)
    if (is.na(n)) return("")
    if (weight_by == "coefficient") {
      est <- .coef_est(coef_str)
      if (is.na(est)) return("")
      w <- coef_widths[1] + diff(coef_widths) * min(abs(est) / ref, 1)
      sty <- sprintf(", line width=%.3gpt", w)
    } else {
      sty <- sprintf(", line width=%.3gpt", tier_widths[n + 1])
    }
    if (n == 0) sty <- paste0(sty, ", densely dotted")
    sty
  }
}


#' Encode text for LaTeX output
#'
#' Escapes special LaTeX characters and converts newlines to \\\\
#'
#' @param x Character string to encode
#' @return LaTeX-safe character string
#' @export
#' @examples
#' latexify("100% sure")
#' # "100\\% sure"
latexify <- function(x) {
  if (!requireNamespace("textutils", quietly = TRUE)) {
    stop("Package 'textutils' is required. Install with: install.packages('textutils')")
  }

  tex_out <- x %>%
    textutils::TeXencode() %>%
    stringr::str_replace_all("\\n", "\\\\\\\\")

  tex_out
}
