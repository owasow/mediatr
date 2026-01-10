#' Extract and tidy SEM dual mediator results
#'
#' Converts output from a lavaan SEM model with two mediators into a tidy
#' data frame with path coefficients, indirect effects, confidence intervals,
#' and p-values.
#'
#' @param sem_fit A fitted lavaan model object
#' @param path_labels Named vector mapping path labels to display names.
#'   Default assumes labels: a1 (X->M1), a2 (X->M2), b1 (M1->Y), b2 (M2->Y),
#'   c (direct X->Y), indirect_anger, indirect_fear, total_indirect, total.
#' @param conf.level Confidence level for intervals (default: 0.95)
#' @return Data frame with columns: path, estimate, ci_lower, ci_upper, se, p_value
#' @export
#' @examples
#' \dontrun{
#' library(lavaan)
#' sem_model <- '
#'   anger ~ a1*ice_contact
#'   fear ~ a2*ice_contact
#'   vote ~ b1*anger + b2*fear + c*ice_contact
#'   indirect_anger := a1 * b1
#'   indirect_fear := a2 * b2
#'   total_indirect := indirect_anger + indirect_fear
#'   total := total_indirect + c
#' '
#' fit <- sem(sem_model, data = mydata)
#' sem_dual_med_tidy(fit)
#' }
sem_dual_med_tidy <- function(sem_fit,
                               path_labels = NULL,
                               conf.level = 0.95) {

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required. Install with: install.packages('lavaan')")
  }

  # Extract parameter estimates with CIs
  params <- lavaan::parameterEstimates(sem_fit, ci = TRUE, level = conf.level)

  # Default path labels
  if (is.null(path_labels)) {
    path_labels <- c(
      a1 = "X -> M1 (a1)",
      a2 = "X -> M2 (a2)",
      b1 = "M1 -> Y (b1)",
      b2 = "M2 -> Y (b2)",
      c = "X -> Y direct (c)",
      indirect_anger = "Indirect via M1",
      indirect_fear = "Indirect via M2",
      total_indirect = "Total Indirect",
      total = "Total Effect"
    )
  }

  # Filter to labeled paths and defined effects
  labeled_params <- params %>%
    dplyr::filter(.data$label != "" | .data$op == ":=") %>%
    dplyr::mutate(
      path = dplyr::case_when(
        .data$label %in% names(path_labels) ~ path_labels[.data$label],
        TRUE ~ .data$label
      )
    ) %>%
    dplyr::select(
      path = "path",
      estimate = "est",
      se = "se",
      ci_lower = "ci.lower",
      ci_upper = "ci.upper",
      p_value = "pvalue"
    )

  return(labeled_params)
}


#' Create publication-ready table from SEM dual mediator results
#'
#' Generates a formatted kable table showing path coefficients and indirect
#' effects from a dual mediator SEM, suitable for RMarkdown/Quarto documents.
#'
#' @param sem_fit A fitted lavaan model object
#' @param decimals Number of decimal places for estimates (default: 3)
#' @param p_accuracy Accuracy for p-value display (default: 0.001)
#' @param caption Table caption text
#' @param label LaTeX label for cross-referencing
#' @param show_paths Logical: show individual path coefficients? (default: TRUE)
#' @param show_effects Logical: show indirect/total effects? (default: TRUE)
#' @param conf.level Confidence level for intervals (default: 0.95)
#' @return A kable object
#' @export
#' @examples
#' \dontrun{
#' library(lavaan)
#' fit <- sem(sem_model, data = mydata)
#' sem_dual_med_table(fit, caption = "Dual Mediator Analysis: Anger and Fear")
#' }
sem_dual_med_table <- function(sem_fit,
                                decimals = 3,
                                p_accuracy = 0.001,
                                caption = "",
                                label = "",
                                show_paths = TRUE,
                                show_effects = TRUE,
                                conf.level = 0.95) {

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required. Install with: install.packages('lavaan')")
  }

  pad <- function(s, w) {
    stringr::str_pad(string = s, width = w, side = "left", pad = " ")
  }

  # Get tidy output
  tidy_df <- sem_dual_med_tidy(sem_fit, conf.level = conf.level)

  # Get sample size from lavaan
  nobs <- lavaan::nobs(sem_fit)

  # Separate paths and effects
  path_rows <- c("X -> M1 (a1)", "X -> M2 (a2)", "M1 -> Y (b1)", "M2 -> Y (b2)", "X -> Y direct (c)")
  effect_rows <- c("Indirect via M1", "Indirect via M2", "Total Indirect", "Total Effect")

  # Filter based on options
  if (show_paths && show_effects) {
    # Keep all
  } else if (show_paths && !show_effects) {
    tidy_df <- tidy_df %>% dplyr::filter(.data$path %in% path_rows)
  } else if (!show_paths && show_effects) {
    tidy_df <- tidy_df %>% dplyr::filter(.data$path %in% effect_rows)
  }

  # Set knitr label if available
  if (exists("opts_current", where = asNamespace("knitr"))) {
    knitr::opts_current$set(label = label)
  }

  # Format the table
  tidy_df %>%
    dplyr::mutate(
      estimate = round(.data$estimate, decimals),
      ci_lower = format2(.data$ci_lower, digits = decimals),
      ci_upper = format2(.data$ci_upper, digits = decimals),
      w        = max(c(nchar(.data$ci_lower), nchar(.data$ci_upper))),
      ci_lower = paste0("(", pad(.data$ci_lower, .data$w), ", "),
      ci_upper = paste0(pad(.data$ci_upper, .data$w), ")"),
      p_value  = scales::pvalue(.data$p_value, accuracy = p_accuracy) %>%
        stringr::str_replace("<", "< ")
    ) %>%
    dplyr::select("path", "estimate", "ci_lower", "ci_upper", "p_value") %>%
    dplyr::rename(
      `Path`     = "path",
      `Estimate` = "estimate",
      `CI Lower` = "ci_lower",
      `Upper`    = "ci_upper",
      `p-value`  = "p_value"
    ) %>%
    knitr::kable(
      align   = c('l', rep('r', 4)),
      escape  = FALSE,
      caption = paste0(caption,
                       if (nchar(caption) > 0) " " else "",
                       "SEM dual mediator model (N = ", nobs, ").")
    )
}


#' Prepare data frame for SEM dual mediator diagrams
#'
#' Extracts path coefficients from a lavaan SEM model and formats them with
#' significance stars for use in TikZ diagram generation.
#'
#' @param sem_fit A fitted lavaan model object
#' @param lab_x Label for treatment/independent variable (X)
#' @param lab_y Label for outcome/dependent variable (Y)
#' @param lab_m1 Label for first mediator (M1, e.g., "Anger")
#' @param lab_m2 Label for second mediator (M2, e.g., "Fear")
#' @param mode Output mode: "article" (compact) or "slide" (spacious)
#' @param ci Logical: include confidence intervals? If NULL, uses mode default
#' @param decimals Number of decimal places (default: 2)
#' @param stars Number of significance tiers (default: 3)
#' @param alpha Significance threshold for single-tier mode (default: 0.05)
#' @param path_labels Named vector mapping lavaan labels to paths.
#'   Default: c(a1="a1", a2="a2", b1="b1", b2="b2", c="c",
#'              indirect_anger="indirect_anger", indirect_fear="indirect_fear")
#' @return Data frame with labels and formatted coefficients for diagram functions
#' @export
#' @examples
#' \dontrun{
#' df <- sem_dual_med_data_prep_df(fit,
#'         lab_x = "ICE Contact",
#'         lab_y = "Validated Vote",
#'         lab_m1 = "Anger",
#'         lab_m2 = "Fear",
#'         mode = "article")
#' }
sem_dual_med_data_prep_df <- function(sem_fit,
                                       lab_x,
                                       lab_y,
                                       lab_m1,
                                       lab_m2,
                                       mode = "article",
                                       ci = NULL,
                                       decimals = 2,
                                       stars = 3,
                                       alpha = 0.05,
                                       path_labels = NULL) {

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required. Install with: install.packages('lavaan')")
  }

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

  # Default path labels (what to look for in lavaan output)
  if (is.null(path_labels)) {
    path_labels <- c(
      a1 = "a1", a2 = "a2", b1 = "b1", b2 = "b2", c = "c",
      indirect_m1 = "indirect_anger",
      indirect_m2 = "indirect_fear",
      total_indirect = "total_indirect",
      total = "total"
    )
  }

  # Extract parameter estimates
  params <- lavaan::parameterEstimates(sem_fit, ci = TRUE, level = 0.95)

  # Helper to extract and format a coefficient
  format_coef <- function(label_to_find, params, decimals, stars, alpha, ci, ci_size) {
    row <- params %>% dplyr::filter(.data$label == label_to_find)

    if (nrow(row) == 0) {
      return(NA_character_)
    }

    est <- row$est[1]
    pval <- row$pvalue[1]
    ci_lo <- row$ci.lower[1]
    ci_hi <- row$ci.upper[1]

    # Format values before glue (to avoid delimiter issues)
    est_fmt <- format2(est, decimals)
    star_str <- starify(pval, stars = stars, alpha = alpha)

    if (ci) {
      ci_str <- paste0("(", format2(ci_lo, decimals), ", ", format2(ci_hi, decimals), ")")
      glue::glue("<<est_fmt>>$^{<<star_str>>}$ \\\\ \\textcolor{{gray}}{{<<ci_size>>{{<<ci_str>>}}}}",
                 .open = "<<", .close = ">>")
    } else {
      glue::glue("<<est_fmt>>$^{<<star_str>>}$",
                 .open = "<<", .close = ">>")
    }
  }

  # Extract all coefficients
  coef_a1 <- format_coef(path_labels["a1"], params, decimals, stars, alpha, ci, ci_size)
  coef_a2 <- format_coef(path_labels["a2"], params, decimals, stars, alpha, ci, ci_size)
  coef_b1 <- format_coef(path_labels["b1"], params, decimals, stars, alpha, ci, ci_size)
  coef_b2 <- format_coef(path_labels["b2"], params, decimals, stars, alpha, ci, ci_size)
  coef_c <- format_coef(path_labels["c"], params, decimals, stars, alpha, ci, ci_size)
  coef_ind_m1 <- format_coef(path_labels["indirect_m1"], params, decimals, stars, alpha, ci, ci_size)
  coef_ind_m2 <- format_coef(path_labels["indirect_m2"], params, decimals, stars, alpha, ci, ci_size)
  coef_total_ind <- format_coef(path_labels["total_indirect"], params, decimals, stars, alpha, ci, ci_size)
  coef_total <- format_coef(path_labels["total"], params, decimals, stars, alpha, ci, ci_size)

  data.frame(
    lab_x = lab_x,
    lab_m1 = lab_m1,
    lab_m2 = lab_m2,
    lab_y = lab_y,
    coef_a1 = as.character(coef_a1),
    coef_a2 = as.character(coef_a2),
    coef_b1 = as.character(coef_b1),
    coef_b2 = as.character(coef_b2),
    coef_c = as.character(coef_c),
    coef_ind_m1 = as.character(coef_ind_m1),
    coef_ind_m2 = as.character(coef_ind_m2),
    coef_total_ind = as.character(coef_total_ind),
    coef_total = as.character(coef_total),
    stringsAsFactors = FALSE
  )
}


#' Create TikZ diagram for SEM dual mediator model
#'
#' Generates TikZ code for a path diagram with two mediators (M1 and M2),
#' showing all paths and indirect effects. The diagram uses a diamond layout
#' with X on the left, Y on the right, and M1/M2 above.
#'
#' @param data Data frame from \code{sem_dual_med_data_prep_df()}
#' @param mode Output mode: "article" or "slide" (sets default sizes)
#' @param scale Overall diagram scale
#' @param box_width Width of node boxes
#' @param box_height Height of node boxes
#' @param arrow_size Size of arrow heads
#' @param text_size LaTeX size command
#' @param diag_label Optional label in top-left corner
#' @param show_indirect Logical: show curved indirect effect arrows? (default: TRUE)
#' @param m1_color Color for M1 path (default: "black")
#' @param m2_color Color for M2 path (default: "black")
#' @return Character string containing TikZ code
#' @export
#' @examples
#' \dontrun{
#' df <- sem_dual_med_data_prep_df(fit, "ICE Contact", "Vote", "Anger", "Fear")
#' tikz_code <- sem_dual_med_diagram_tikz(df, mode = "article")
#' cat(tikz_code)
#' }
sem_dual_med_diagram_tikz <- function(data,
                                       mode = "article",
                                       scale = NULL,
                                       box_width = NULL,
                                       box_height = NULL,
                                       arrow_size = NULL,
                                       text_size = NULL,
                                       diag_label = "",
                                       show_indirect = TRUE,
                                       m1_color = "black",
                                       m2_color = "black") {

  # Encode labels for LaTeX
  data$lab_x <- latexify(data$lab_x)
  data$lab_y <- latexify(data$lab_y)
  data$lab_m1 <- latexify(data$lab_m1)
  data$lab_m2 <- latexify(data$lab_m2)

  # Set defaults based on mode
  if (mode == "article") {
    data$scale      <- if (is.null(scale)) 0.4 else scale
    data$box_width  <- if (is.null(box_width)) ".85in" else box_width
    data$box_height <- if (is.null(box_height)) ".4in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.5mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\scriptsize" else text_size
  } else if (mode == "slide") {
    data$scale      <- if (is.null(scale)) 0.7 else scale
    data$box_width  <- if (is.null(box_width)) "1.1in" else box_width
    data$box_height <- if (is.null(box_height)) ".6in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "2mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\small" else text_size
  } else {
    data$scale      <- if (is.null(scale)) 0.5 else scale
    data$box_width  <- if (is.null(box_width)) ".85in" else box_width
    data$box_height <- if (is.null(box_height)) ".4in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.5mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\scriptsize" else text_size
  }

  data$diag_label <- diag_label
  data$m1_color <- m1_color
  data$m2_color <- m2_color

  # Build indirect effect arrows if requested
  if (show_indirect) {
    indirect_arrows <- glue::glue_data(data,
"% Indirect effect arrows (curved, above)
\\draw[->, <<m1_color>>, dashed] (x.north) to[out=60, in=180] node[midway, above left, align=center] {\\textcolor{<<m1_color>>}{ACME$_1$: <<coef_ind_m1>>}} (m1.west);
\\draw[->, <<m1_color>>, dashed] (m1.east) to[out=0, in=120] (y.north);
\\draw[->, <<m2_color>>, dotted] (x.south) to[out=-60, in=180] node[midway, below left, align=center] {\\textcolor{<<m2_color>>}{ACME$_2$: <<coef_ind_m2>>}} (m2.west);
\\draw[->, <<m2_color>>, dotted] (m2.east) to[out=0, in=-120] (y.south);",
                                       .open = "<<", .close = ">>"
    )
  } else {
    indirect_arrows <- ""
  }

  data$indirect_arrows <- indirect_arrows

  # Main diagram
  glue::glue_data(data,
"\\begin{tikzpicture}[scale=<<scale>>, >=stealth, font=\\sffamily]
<<text_size>>
\\tikzset{mynode/.style={draw, text centered, text width = <<box_width>>, minimum height = <<box_height>>, align=center} }
\\tikzset{>={Latex[width=<<arrow_size>>,length=<<arrow_size>>]}}
% Nodes: Diamond layout with M1 top, M2 bottom
\\node[mynode] (x)  at (0, 0)   {<<lab_x>>};
\\node[mynode] (y)  at (14, 0)  {<<lab_y>>};
\\node[mynode] (m1) at (7, 4)   {<<lab_m1>>};
\\node[mynode] (m2) at (7, -4)  {<<lab_m2>>};
% Direct paths
\\path[->, <<m1_color>>] (x) edge node[above left, align=center, xshift=-3pt] {$a_1$: <<coef_a1>>} (m1);
\\path[->, <<m1_color>>] (m1) edge node[above right, align=center, xshift=3pt] {$b_1$: <<coef_b1>>} (y);
\\path[->, <<m2_color>>] (x) edge node[below left, align=center, xshift=-3pt] {$a_2$: <<coef_a2>>} (m2);
\\path[->, <<m2_color>>] (m2) edge node[below right, align=center, xshift=3pt] {$b_2$: <<coef_b2>>} (y);
% Direct effect X -> Y
\\path[->] (x) edge node[above, align=center, yshift=2pt] {$c'$: <<coef_c>>} (y);
\\path[->] (x) edge node[below, align=center, yshift=-2pt] {Total: <<coef_total>>} (y);
% Label
\\node at (-2, 5) {\\scriptsize <<diag_label>>};
\\end{tikzpicture}",
                  .open = "<<", .close = ">>"
  )
}


#' Create compact TikZ diagram for SEM dual mediator model
#'
#' Generates a more compact TikZ diagram with both mediators arranged
#' horizontally above the X-Y path. Shows indirect effects as curved arrows.
#' This layout is better suited for narrow column widths.
#'
#' @param data Data frame from \code{sem_dual_med_data_prep_df()}
#' @param mode Output mode: "article" or "slide"
#' @param scale Overall diagram scale
#' @param box_width Width of node boxes
#' @param box_height Height of node boxes
#' @param arrow_size Size of arrow heads
#' @param text_size LaTeX size command
#' @param diag_label Optional label in top-left corner
#' @param m1_color Color for M1 pathway (default: "blue!70!black")
#' @param m2_color Color for M2 pathway (default: "red!70!black")
#' @return Character string containing TikZ code
#' @export
sem_dual_med_diagram_compact_tikz <- function(data,
                                               mode = "article",
                                               scale = NULL,
                                               box_width = NULL,
                                               box_height = NULL,
                                               arrow_size = NULL,
                                               text_size = NULL,
                                               diag_label = "",
                                               m1_color = "blue!70!black",
                                               m2_color = "red!70!black") {

  # Encode labels for LaTeX
  data$lab_x <- latexify(data$lab_x)
  data$lab_y <- latexify(data$lab_y)
  data$lab_m1 <- latexify(data$lab_m1)
  data$lab_m2 <- latexify(data$lab_m2)

  # Set defaults based on mode
  if (mode == "article") {
    data$scale      <- if (is.null(scale)) 0.35 else scale
    data$box_width  <- if (is.null(box_width)) ".75in" else box_width
    data$box_height <- if (is.null(box_height)) ".35in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.2mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\tiny" else text_size
  } else {
    data$scale      <- if (is.null(scale)) 0.6 else scale
    data$box_width  <- if (is.null(box_width)) "1.0in" else box_width
    data$box_height <- if (is.null(box_height)) ".5in" else box_height
    data$arrow_size <- if (is.null(arrow_size)) "1.8mm" else arrow_size
    data$text_size  <- if (is.null(text_size)) "\\scriptsize" else text_size
  }

  data$diag_label <- diag_label
  data$m1_color <- m1_color
  data$m2_color <- m2_color

  glue::glue_data(data,
"\\begin{tikzpicture}[scale=<<scale>>, >=stealth, font=\\sffamily]
<<text_size>>
\\tikzset{mynode/.style={draw, text centered, text width = <<box_width>>, minimum height = <<box_height>>, align=center} }
\\tikzset{>={Latex[width=<<arrow_size>>,length=<<arrow_size>>]}}
% Nodes: Horizontal layout with M1, M2 above
\\node[mynode] (x)  at (0, 0)   {<<lab_x>>};
\\node[mynode] (y)  at (12, 0)  {<<lab_y>>};
\\node[mynode, fill=<<m1_color>>!10] (m1) at (4, 5)   {<<lab_m1>>};
\\node[mynode, fill=<<m2_color>>!10] (m2) at (8, 5)   {<<lab_m2>>};
% M1 pathway (solid, colored)
\\path[->, <<m1_color>>, thick] (x) edge node[left, align=center] {<<coef_a1>>} (m1);
\\path[->, <<m1_color>>, thick] (m1) edge node[left, align=center] {<<coef_b1>>} (y);
% M2 pathway (dashed, colored)
\\path[->, <<m2_color>>, thick, dashed] (x) edge node[right, align=center, xshift=2pt] {<<coef_a2>>} (m2);
\\path[->, <<m2_color>>, thick, dashed] (m2) edge node[right, align=center, xshift=2pt] {<<coef_b2>>} (y);
% Mediator correlation (if applicable)
\\path[<->, gray, dotted] (m1) edge (m2);
% Direct effect X -> Y
\\path[->] (x) edge node[above, align=center] {Direct: <<coef_c>>} (y);
% Indirect effect annotations
\\node[<<m1_color>>, align=center] at (2, 7) {ACME$_1$: <<coef_ind_m1>>};
\\node[<<m2_color>>, align=center] at (10, 7) {ACME$_2$: <<coef_ind_m2>>};
\\node[align=center] at (6, -1.5) {Total Indirect: <<coef_total_ind>>};
% Label
\\node at (-1.5, 7) {\\scriptsize <<diag_label>>};
\\end{tikzpicture}",
                  .open = "<<", .close = ">>"
  )
}
