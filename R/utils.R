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
