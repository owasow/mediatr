# ============================================================================
# DATA EXTRACTION FUNCTIONS
# ============================================================================

#' Extract and tidy mediation analysis results
#' @param med_object Output from mediation::mediate()
#' @param exp Logical: exponentiate coefficients? (for logistic/Cox models)
#' @return Data frame with estimates, CIs, and p-values
med_tidy <- function(med_object, exp = FALSE) {
    # Extract summary statistics from mediation object
    sout <- summary(med_object)

    # Create tidy data frame with key statistics
    df <- data.frame(
        estimate = c(
            sout[["d0"]],        # ACME (Average Causal Mediation Effect)
            sout[["z0"]],        # ADE (Average Direct Effect)
            sout[["tau.coef"]],  # Total Effect
            sout[["n.avg"]]      # Proportion Mediated
        ),
        ci_lower = c(
            sout[["d0.ci"]][["2.5%"]],
            sout[["z0.ci"]][["2.5%"]],
            sout[["tau.ci"]][["2.5%"]],
            sout[["n.avg.ci"]][["2.5%"]]
        ),
        ci_upper = c(
            sout[["d0.ci"]][["97.5%"]],
            sout[["z0.ci"]][["97.5%"]],
            sout[["tau.ci"]][["97.5%"]],
            sout[["n.avg.ci"]][["97.5%"]]
        ),
        p_value = c(
            sout[["d0.p"]],
            sout[["z0.p"]],
            sout[["tau.p"]],
            sout[["n.avg.p"]]
        )
    )

    # Exponentiate coefficients if requested (for odds ratios, hazard ratios)
    if (exp) {
        df$estimate <- exp(df$estimate)
        df$ci_lower <- exp(df$ci_lower)
        df$ci_upper <- exp(df$ci_upper)
    }

    return(df)
}


# ============================================================================
# COEFFICIENT EXTRACTION FOR DIAGRAMS
# ============================================================================

#' Extract formatted coefficients for diagram labels
#' @param mediation_out Output from mediation::mediate()
#' @param model_x_on_m Linear model of mediator ~ treatment
#' @param ci Include confidence intervals in output?
#' @param decimals Number of decimal places
#' @param exp Exponentiate coefficients?
#' @return Data frame with formatted coefficient strings
med_extract_coefs <- function(mediation_out,
                              model_x_on_m,
                              ci = TRUE,
                              decimals = 2,
                              exp = FALSE) {

  # Extract mediation results
  med_df <- med_tidy(mediation_out, exp = exp)

  if (ci) {
    # Format with confidence intervals
    med_ci <- glue_data(
      med_df,
      "(<<format2(ci_lower, decimals)>>, <<format2(ci_upper, decimals)>>)",
      .open = "<<",
      .close = ">>"
    )

    # Format ACME (indirect effect through mediator)
    acme <- glue(
      format2(med_df$estimate[1], digits = decimals),
      "$^{", starify(med_df$p_value[1]), "}$",
      " \\\\ \\textcolor{gray}{\\small{\\,\\,\\,", med_ci[1], "}}"
    )

    # Format ADE (direct effect)
    ade <- glue(
      format2(med_df$estimate[2], digits = decimals),
      "$^{", starify(med_df$p_value[2]), "}$",
      " \\\\ \\textcolor{gray}{\\small{", med_ci[2], "}}"
    )

    # Format total effect
    tot <- glue(
      format2(med_df$estimate[3], digits = decimals),
      "$^{", starify(med_df$p_value[3]), "}$",
      " \\\\ \\textcolor{gray}{\\small{", med_ci[3], "}}"
    )

    # Extract X->M path coefficient
    m1_df <- broom::tidy(model_x_on_m, conf.int = TRUE, conf.level = .95)
    m1_ci <- paste0(
      "(",
      format2(m1_df$conf.low[2], decimals),
      ", ",
      format2(m1_df$conf.high[2], decimals),
      ")"
    )

    if (exp) {
      m1_df$estimate[2] <- exp(m1_df$estimate[2])
    }

    x_on_m <- glue(
      format2(m1_df$estimate[2], digits = decimals),
      "$^{", starify(m1_df$p.value[2]), "}$",
      " \\\\ \\textcolor{gray}{\\small{", m1_ci, "\\,\\,\\,}}"
    )

  } else {
    # Format without confidence intervals (cleaner for presentations)
    acme <- glue(
      format2(med_df$estimate[1], digits = decimals),
      "$^{", starify(med_df$p_value[1]), "}$"
    )

    ade <- glue(
      format2(med_df$estimate[2], digits = decimals),
      "$^{", starify(med_df$p_value[2]), "}$"
    )

    tot <- glue(
      format2(med_df$estimate[3], digits = decimals),
      "$^{", starify(med_df$p_value[3]), "}$"
    )

    m1_df <- broom::tidy(model_x_on_m)
    if (exp) {
      m1_df$estimate[2] <- exp(m1_df$estimate[2])
    }

    x_on_m <- glue(
      format2(m1_df$estimate[2], digits = decimals),
      "$^{", starify(m1_df$p.value[2]), "}$"
    )
  }

  return(data.frame(
    acme = as.character(acme),
    ade = as.character(ade),
    x_on_m = as.character(x_on_m),
    tot = as.character(tot),
    stringsAsFactors = FALSE
  ))
}
