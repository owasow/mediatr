#' Extract formatted coefficients for diagram labels
#'
#' Extracts coefficients from mediation analysis and formats them with
#' optional confidence intervals and significance stars for use in TikZ diagrams.
#'
#' @param mediation_out Output from \code{mediation::mediate()}
#' @param model_x_on_m Linear model of mediator ~ treatment (the "a" path)
#' @param ci Logical: include confidence intervals? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param exp Logical: exponentiate coefficients? (default: FALSE)
#' @param stars Number of significance tiers: 3 for (*, **, ***) or 1 for single threshold
#' @param alpha Significance threshold for single-tier mode (default: 0.05)
#' @param ci_size LaTeX size command for CI text (default: "")
#' @return Data frame with formatted coefficient strings: acme, ade, x_on_m, tot, m_on_y
#' @export
med_data_prep_coefs <- function(mediation_out,
                                model_x_on_m,
                                ci = TRUE,
                                decimals = 2,
                                exp = FALSE,
                                stars = 3,
                                alpha = 0.05,
                                ci_size = "") {

  med_df <- med_tidy(mediation_out, exp = exp)

  if (ci) {
    med_ci <- glue::glue_data(
      med_df,
      "(<<format2(ci_lower, decimals)>>, <<format2(ci_upper, decimals)>>)",
      .open = "<<", .close = ">>"
    )

    acme <- med_df$estimate[1] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(med_df$p_value[1], stars = stars, alpha = alpha),
                 "}$ \\\\ \\textcolor{gray}{<<ci_size>>{\\,\\,\\,",
                 med_ci[1], "}}",
                 .open = "<<", .close = ">>"
      )

    ade <- med_df$estimate[2] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(med_df$p_value[2], stars = stars, alpha = alpha),
                 "}$ \\\\ \\textcolor{gray}{<<ci_size>>{",
                 med_ci[2], "}}",
                 .open = "<<", .close = ">>"
      )

    tot <- med_df$estimate[3] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(med_df$p_value[3], stars = stars, alpha = alpha),
                 "}$ \\\\ \\textcolor{gray}{<<ci_size>>{",
                 med_ci[3], "}}",
                 .open = "<<", .close = ">>"
      )

    m1_df <- broom::tidy(model_x_on_m, conf.int = TRUE, conf.level = .95)
    m1_ci <- paste0("(", format2(m1_df$conf.low[2], decimals), ", ",
                    format2(m1_df$conf.high[2], decimals), ")")

    if (exp) { m1_df$estimate[2] <- exp(m1_df$estimate[2]) }

    x_on_m <- m1_df$estimate[2] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(m1_df$p.value[2], stars = stars, alpha = alpha),
                 "}$ \\\\ \\textcolor{gray}{<<ci_size>>{",
                 m1_ci, "\\,\\,\\,}}",
                 .open = "<<", .close = ">>"
      )

    # Extract M->Y coefficient from the mediation object's outcome model
    y_model <- mediation_out$model.y
    y1_df <- broom::tidy(y_model, conf.int = TRUE, conf.level = .95)
    med_idx <- 3  # Mediator is typically 3rd row (after intercept and treatment)
    y1_ci <- paste0("(", format2(y1_df$conf.low[med_idx], decimals), ", ",
                    format2(y1_df$conf.high[med_idx], decimals), ")")

    if (exp) { y1_df$estimate[med_idx] <- exp(y1_df$estimate[med_idx]) }

    m_on_y <- y1_df$estimate[med_idx] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(y1_df$p.value[med_idx], stars = stars, alpha = alpha),
                 "}$ \\\\ \\textcolor{gray}{<<ci_size>>{",
                 y1_ci, "\\,\\,\\,}}",
                 .open = "<<", .close = ">>"
      )

  } else {
    # Without confidence intervals
    acme <- med_df$estimate[1] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(med_df$p_value[1], stars = stars, alpha = alpha),
                 "}$", .open = "<<", .close = ">>"
      )

    ade <- med_df$estimate[2] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(med_df$p_value[2], stars = stars, alpha = alpha),
                 "}$", .open = "<<", .close = ">>"
      )

    tot <- med_df$estimate[3] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(med_df$p_value[3], stars = stars, alpha = alpha),
                 "}$", .open = "<<", .close = ">>"
      )

    m1_df <- broom::tidy(model_x_on_m, conf.int = TRUE, conf.level = .95)
    if (exp) { m1_df$estimate[2] <- exp(m1_df$estimate[2]) }

    x_on_m <- m1_df$estimate[2] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(m1_df$p.value[2], stars = stars, alpha = alpha),
                 "}$", .open = "<<", .close = ">>"
      )

    y_model <- mediation_out$model.y
    y1_df <- broom::tidy(y_model, conf.int = TRUE, conf.level = .95)
    med_idx <- 3
    if (exp) { y1_df$estimate[med_idx] <- exp(y1_df$estimate[med_idx]) }

    m_on_y <- y1_df$estimate[med_idx] %>%
      format2(digits = decimals) %>%
      glue::glue(., "$^{",
                 starify(y1_df$p.value[med_idx], stars = stars, alpha = alpha),
                 "}$", .open = "<<", .close = ">>"
      )
  }

  data.frame(acme = as.character(acme),
             ade = as.character(ade),
             x_on_m = as.character(x_on_m),
             tot = as.character(tot),
             m_on_y = as.character(m_on_y),
             stringsAsFactors = FALSE)
}


#' Prepare data frame for mediation diagrams
#'
#' Combines coefficient extraction with variable labels to create a data frame
#' ready for diagram generation functions.
#'
#' @param med_out Output from \code{mediation::mediate()}
#' @param mod_x_on_m Linear model of mediator ~ treatment
#' @param lab_x Label for treatment/independent variable (X)
#' @param lab_y Label for outcome/dependent variable (Y)
#' @param lab_m Label for mediator variable (M)
#' @param mode Output mode: "article" (compact, no CIs) or "slide" (spacious, with CIs)
#' @param ci Logical: include confidence intervals? If NULL, uses mode default
#' @param decimals Number of decimal places (default: 2)
#' @param exp Logical: exponentiate coefficients? (default: FALSE)
#' @param stars Number of significance tiers (default: 3)
#' @param alpha Significance threshold for single-tier mode (default: 0.05)
#' @return Data frame with labels and formatted coefficients for diagram functions
#' @export
#' @examples
#' \dontrun{
#' # Prepare data for article (compact) format
#' df <- med_data_prep_df(med, m_model,
#'                        lab_x = "Treatment",
#'                        lab_y = "Outcome",
#'                        lab_m = "Mediator",
#'                        mode = "article")
#' }
med_data_prep_df <- function(med_out,
                             mod_x_on_m,
                             lab_x,
                             lab_y,
                             lab_m,
                             mode = "article",
                             ci = NULL,
                             decimals = 2,
                             exp = FALSE,
                             stars = 3,
                             alpha = 0.05) {

  # Set mode-specific defaults
  if (mode == "article") {
    ci_default <- FALSE
    ci_size <- ""
  } else {
    ci_default <- TRUE
    ci_size <- "\\small"
  }

  if (is.null(ci)) {
    ci <- ci_default
  }

  coefs_text <- med_data_prep_coefs(
    mediation_out = med_out,
    model_x_on_m  = mod_x_on_m,
    ci            = ci,
    decimals      = decimals,
    exp           = exp,
    stars         = stars,
    alpha         = alpha,
    ci_size       = ci_size
  )

  data.frame(
    lab_x    = lab_x,
    lab_m    = lab_m,
    lab_y    = lab_y,
    coef_xm  = coefs_text$x_on_m,
    coef_xmy = coefs_text$acme,
    coef_my  = coefs_text$m_on_y,
    coef_xy  = coefs_text$ade,
    coef_tot = coefs_text$tot,
    stringsAsFactors = FALSE
  )
}


#' Create basic TikZ mediation diagram
#'
#' Generates TikZ code for a mediation path diagram with X, M, Y nodes
#' and labeled arrows showing coefficients. Includes both direct (ADE)
#' and total effect paths.
#'
#' @param data Data frame from \code{med_data_prep_df()}
#' @param mode Output mode: "article" or "slide" (sets default sizes)
#' @param scale Overall diagram scale (default: mode-dependent)
#' @param box_width Width of node boxes (default: mode-dependent)
#' @param box_height Height of node boxes (default: mode-dependent)
#' @param arrow_size Size of arrow heads (default: mode-dependent)
#' @param text_size LaTeX size command (default: mode-dependent)
#' @param diag_label Optional label in top-left corner
#' @param ashift X-shift for left arrow label
#' @param bshift X-shift for right arrow label
#' @param cshift Y-shift for bottom arrow label
#' @param caption Figure caption (currently commented out in output)
#' @param label LaTeX label (currently commented out in output)
#' @return Character string containing TikZ code
#' @export
med_diagram_tikz <- function(data,
                             mode = "article",
                             scale = NULL,
                             box_width = NULL,
                             box_height = NULL,
                             arrow_size = NULL,
                             text_size = NULL,
                             diag_label = "",
                             ashift = "-5pt",
                             bshift = "+5pt",
                             cshift = "-5pt",
                             caption = "",
                             label = "") {

  # Set defaults based on mode
  if (mode == "article") {
    data$scale      <- if (is.null(scale)) 0.8 else scale
    data$box_width  <- if (is.null(box_width)) ".75in" else box_width
    data$box_height <- if (is.null(box_height)) ".4in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.5mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\scriptsize" else text_size
  } else if (mode == "slide") {
    data$scale      <- if (is.null(scale)) 0.8 else scale
    data$box_width  <- if (is.null(box_width)) "1.0in" else box_width
    data$box_height <- if (is.null(box_height)) ".5in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "2mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\small" else text_size
  } else {
    data$scale      <- if (is.null(scale)) 0.8 else scale
    data$box_width  <- if (is.null(box_width)) ".75in" else box_width
    data$box_height <- if (is.null(box_height)) ".4in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.5mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\scriptsize" else text_size
  }

  data$diag_label <- diag_label
  data$caption    <- caption
  data$label      <- label
  data$ashift     <- ashift
  data$bshift     <- bshift
  data$cshift     <- cshift

  glue::glue_data(data,
"%\\begin{figure}
%\\begin{center}
\\begin{tikzpicture}[scale=<<scale>>, >=stealth, font=\\sffamily]
<<text_size>>
\\tikzset{mynode/.style={draw, text centered, text width = <<box_width>>, minimum height = <<box_height>>, align=center} }
\\tikzset{>={Latex[width=<<arrow_size>>,length=<<arrow_size>>]}}
\\node[mynode] (x) at (1,0) {<<lab_x>>};
\\node[mynode] (y) at (7,0) {<<lab_y>>};
\\node[mynode] (m) at (4,3) {<<lab_m>>};
\\path[->] (x) edge node[above] {<<coef_xy>>} (y);
\\path[->] (x) edge node[left,  xshift=<<ashift>>] {<<coef_xm>>} (m);
\\path[->] (m) edge node[right, xshift=<<bshift>>] {<<coef_my>>} (y);
\\path[->] (x) edge node[below, yshift=<<cshift>>] {Total: <<coef_tot>>} (y);
\\node at (0, 3.5) {\\scriptsize <<diag_label>>};
\\end{tikzpicture}
%\\caption{<<caption>> \\label{<<label>>}}
%\\end{center}
%\\end{figure}
",
                  .open = "<<", .close = ">>"
  )
}


#' Create TikZ mediation diagram with curved ACME arrow
#'
#' Generates TikZ code for a mediation path diagram that includes a curved
#' arrow above showing the ACME (indirect effect). This is the recommended
#' diagram style for showing the complete mediation decomposition.
#'
#' @param data Data frame from \code{med_data_prep_df()}
#' @param mode Output mode: "article" or "slide" (sets default sizes)
#' @param scale Overall diagram scale
#' @param box_width Width of node boxes
#' @param box_height Height of node boxes
#' @param arrow_size Size of arrow heads
#' @param text_size LaTeX size command
#' @param diag_label Optional label in top-left corner
#' @param ashift X-shift for left arrow label
#' @param bshift X-shift for right arrow label
#' @return Character string containing TikZ code
#' @export
#' @examples
#' \dontrun{
#' df <- med_data_prep_df(med, m_model, "Treatment", "Outcome", "Mediator")
#' tikz_code <- med_diagram_acme_tikz(df, mode = "article")
#' cat(tikz_code)
#' }
med_diagram_acme_tikz <- function(data,
                                  mode = "article",
                                  scale = NULL,
                                  box_width = NULL,
                                  box_height = NULL,
                                  arrow_size = NULL,
                                  text_size = NULL,
                                  diag_label = "",
                                  ashift = NULL,
                                  bshift = NULL) {

  # Encode labels for LaTeX
  data$lab_x <- latexify(data$lab_x)
  data$lab_y <- latexify(data$lab_y)
  data$lab_m <- latexify(data$lab_m)

  # Set defaults based on mode
  if (mode == "article") {
    data$scale      <- if (is.null(scale)) 0.4 else scale
    data$box_width  <- if (is.null(box_width)) ".75in" else box_width
    data$box_height <- if (is.null(box_height)) ".4in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.5mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\scriptsize" else text_size
    data$ashift     <- if (is.null(ashift)) "-5pt" else ashift
    data$bshift     <- if (is.null(bshift)) "5pt" else bshift
    fig_begin <- "%\\begin{figure}\n%\\begin{center}"
    fig_end   <- "%\\end{center}\n%\\end{figure}"
  } else if (mode == "slide") {
    data$scale      <- if (is.null(scale)) 0.8 else scale
    data$box_width  <- if (is.null(box_width)) "1.25in" else box_width
    data$box_height <- if (is.null(box_height)) ".85in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "2mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\normalsize" else text_size
    data$ashift     <- if (is.null(ashift)) "-5pt" else ashift
    data$bshift     <- if (is.null(bshift)) "10pt" else bshift
    fig_begin <- "\\begin{figure}\n\\begin{center}"
    fig_end   <- "\\end{center}\n\\end{figure}"
  } else {
    data$scale      <- if (is.null(scale)) 0.8 else scale
    data$box_width  <- if (is.null(box_width)) "1.25in" else box_width
    data$box_height <- if (is.null(box_height)) ".85in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "2mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\normalsize" else text_size
    data$ashift     <- if (is.null(ashift)) "-5pt" else ashift
    data$bshift     <- if (is.null(bshift)) "10pt" else bshift
    fig_begin <- "%\\begin{figure}\n%\\begin{center}"
    fig_end   <- "%\\end{center}\n%\\end{figure}"
  }

  data$diag_label <- diag_label

  paste0(fig_begin, "\n",
         glue::glue_data(data,
"\\begin{tikzpicture}[scale=<<scale>>, >=stealth, font=\\sffamily]
<<text_size>>
\\tikzset{mynode/.style={draw, text centered, text width = <<box_width>>, minimum height = <<box_height>>, align=center} }
\\tikzset{>={Latex[width=<<arrow_size>>,length=<<arrow_size>>]}}
\\node[mynode] (x) at (0,0)  {<<lab_x>>};
\\node[mynode] (y) at (12,0) {<<lab_y>>};
\\node[mynode] (m) at (6,6)  {<<lab_m>>};
\\path[->] (x) edge node[above, align=center, yshift=1pt] {ADE: <<coef_xy>> } (y);
\\path[->] (x) edge node[left, align=center, xshift=<<ashift>>] {<<coef_xm>>} (m);
\\path[->] (m) edge node[right, align=center, xshift=<<bshift>>] {<<coef_my>>} (y);
\\path[->] (x) edge node[below, align=center, yshift=-5pt] {Total: <<coef_tot>>} (y);
\\draw[->] (x.north east) to[out=25, in=155, looseness=1.05] node[midway, above, align=center, yshift=1pt] {ACME: <<coef_xmy>>} (y.north west);
\\node at (-1.5, 6.5) {\\scriptsize <<diag_label>>};
\\end{tikzpicture}",
                         .open = "<<", .close = ">>"
         ), "\n", fig_end)
}
