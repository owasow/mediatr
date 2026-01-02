#' Extract and tidy mediation analysis results
#'
#' Converts output from \code{mediation::mediate()} into a tidy data frame
#' with estimates, confidence intervals, and p-values for ACME, ADE,
#' Total Effect, and Proportion Mediated.
#'
#' @param med_object Output from \code{mediation::mediate()}
#' @param exp Logical: exponentiate coefficients? Useful for logistic or Cox models (default: FALSE)
#' @return Data frame with columns: estimate, ci_lower, ci_upper, p_value
#' @export
#' @examples
#' \dontrun{
#' library(mediation)
#' data(jobs)
#' m <- lm(job_seek ~ treat, data = jobs)
#' y <- lm(depress2 ~ treat + job_seek, data = jobs)
#' med <- mediate(m, y, treat = "treat", mediator = "job_seek", sims = 100)
#' med_tidy(med)
#' }
med_tidy <- function(med_object, exp = FALSE) {
  sout <- summary(med_object)

  df <- data.frame(
    estimate = c(sout[["d0"]], sout[["z0"]], sout[["tau.coef"]], sout[["n.avg"]]),
    ci_lower = c(sout[["d0.ci"]][["2.5%"]], sout[["z0.ci"]][["2.5%"]],
                 sout[["tau.ci"]][["2.5%"]], sout[["n.avg.ci"]][["2.5%"]]),
    ci_upper = c(sout[["d0.ci"]][["97.5%"]], sout[["z0.ci"]][["97.5%"]],
                 sout[["tau.ci"]][["97.5%"]], sout[["n.avg.ci"]][["97.5%"]]),
    p_value  = c(sout[["d0.p"]], sout[["z0.p"]], sout[["tau.p"]], sout[["n.avg.p"]])
  )

  if (exp) {
    df$estimate <- exp(df$estimate)
    df$ci_lower <- exp(df$ci_lower)
    df$ci_upper <- exp(df$ci_upper)
  }

  return(df)
}


#' Create formatted kable table of mediation results
#'
#' Generates a publication-ready table from mediation analysis output,
#' suitable for inclusion in RMarkdown/Quarto documents.
#'
#' @param med_object Output from \code{mediation::mediate()}
#' @param decimals Number of decimal places for estimates (default: 3)
#' @param p_accuracy Accuracy for p-value display (default: 0.001)
#' @param caption Table caption text
#' @param label LaTeX label for cross-referencing
#' @param exp Logical: exponentiate coefficients? (default: FALSE)
#' @return A kable object
#' @export
#' @examples
#' \dontrun{
#' library(mediation)
#' data(jobs)
#' m <- lm(job_seek ~ treat, data = jobs)
#' y <- lm(depress2 ~ treat + job_seek, data = jobs)
#' med <- mediate(m, y, treat = "treat", mediator = "job_seek", sims = 100)
#' med_table(med, caption = "Job Training Mediation Analysis")
#' }
med_table <- function(med_object,
                      decimals = 3,
                      p_accuracy = 0.001,
                      caption = "",
                      label = "",
                      exp = FALSE) {

  pad <- function(s, w) {
    stringr::str_pad(string = s, width = w, side = "left", pad = " ")
  }

  nobs <- summary(med_object)$nobs
  mediate_tidy_object <- med_tidy(med_object, exp = exp)

  # Set knitr label if available

  if (exists("opts_current", where = asNamespace("knitr"))) {
    knitr::opts_current$set(label = label)
  }

  mediate_tidy_object %>%
    dplyr::mutate(
      term     = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
      estimate = round(.data$estimate, decimals),
      ci_lower = format2(.data$ci_lower, digits = decimals),
      ci_upper = format2(.data$ci_upper, digits = decimals),
      w        = max(c(nchar(.data$ci_lower), nchar(.data$ci_upper))),
      ci_lower = paste0("(", pad(.data$ci_lower, .data$w), ", "),
      ci_upper = paste0(pad(.data$ci_upper, .data$w), ")"),
      p_value  = scales::pvalue(.data$p_value, accuracy = p_accuracy) %>%
        purrr::when(
          knitr::is_latex_output() ~ stringr::str_replace(., "<", "$<$"),
          TRUE ~ .
        )
    ) %>%
    dplyr::select("term", "estimate", "ci_lower", "ci_upper", "p_value") %>%
    dplyr::rename(
      ` `        = "term",
      `Estimate` = "estimate",
      `CI Lower` = "ci_lower",
      `Upper`    = "ci_upper",
      `p-value`  = "p_value"
    ) %>%
    knitr::kable(
      align   = c('l', rep('r', 4)),
      escape  = FALSE,
      caption = paste0(caption, " Causal mediation analysis with quasi-Bayesian confidence intervals. Sample size of ", nobs, ".")
    )
}
