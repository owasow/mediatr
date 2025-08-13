# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Format numeric values to specified decimal places
#' @param x Numeric value to format
#' @param digits Number of decimal places (default: 2)
#' @return Formatted character string
format2 <- function(x, digits = 2) {
    formatC(x, format = 'f', digits = digits) %>% 
        as.character()
}

#' Convert p-values to significance stars
#' @param x Numeric p-value
#' @param cutpoint Significance threshold (default: 0.05)
#' @return Character string with significance stars
starify <- function(x, cutpoint = 0.05) {
    case_when(
        x < 0.001  ~ "***",
        x < 0.01   ~ "**",
        x < 0.05   ~ "*",
        TRUE       ~ ""
    )
}
