# ============================================================================
# TABLE OUTPUT FUNCTION
# ============================================================================

#' Create formatted table of mediation analysis results
#' @param med_object Output from mediation::mediate()
#' @param decimals Number of decimal places for estimates
#' @param p_accuracy Accuracy for p-value display
#' @param caption Table caption
#' @param label LaTeX label for cross-referencing
#' @param exp Logical: exponentiate coefficients?
#' @return Kable-formatted table
med_table <- function(med_object, 
                      decimals = 3, 
                      p_accuracy = 0.001, 
                      caption = "", 
                      label = "", 
                      exp = FALSE) {
    
    # Helper function for padding strings
    pad <- function(s, w) {
        stringr::str_pad(string = s, width = w, side = "left", pad = " ")
    }
    
    # Extract sample size and tidy results
    nobs <- summary(med_object)$nobs
    mediate_tidy_object <- med_tidy(med_object, exp = exp)
    
    # Set label if using knitr
    if (exists("opts_current")) {
        opts_current$set(label = label)
    }
    
    # Format and create table
    mediate_tidy_object %>% 
        mutate(
            term = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
            estimate = round(estimate, decimals),
            ci_lower = format2(ci_lower, digits = decimals),
            ci_upper = format2(ci_upper, digits = decimals),
            w = max(c(nchar(ci_lower), nchar(ci_upper))),
            ci_lower = paste0("(", pad(ci_lower, w), ", "),
            ci_upper = paste0(pad(ci_upper, w), ")"),
            p_value = scales::pvalue(p_value, accuracy = p_accuracy) %>%
                str_replace("<", "$<$")
        ) %>% 
        select(term, estimate, ci_lower, ci_upper, p_value) %>% 
        rename(
            ` ` = term,
            `Estimate` = estimate,
            `CI Lower` = ci_lower,
            `Upper` = ci_upper,
            `p-value` = p_value
        ) %>% 
        kable(
            align = c('l', rep('r', 4)),
            escape = FALSE,
            caption = paste0(
                caption, 
                " Causal mediation analysis with quasi-Bayesian confidence intervals. ",
                "Sample size: ", nobs, "."
            )
        )
}
