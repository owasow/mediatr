# Save this as R/mediatr_functions.R in your package directory

#' @import dplyr
#' @import glue
#' @import stringr
#' @import scales
#' @import broom
#' @importFrom stats lm
#' @importFrom utils capture.output
#' @importFrom grDevices gray
NULL

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Format numeric values to specified decimal places
#'
#' @param x Numeric value to format
#' @param digits Number of decimal places (default: 2)
#' @return Formatted character string
#' @export
format2 <- function(x, digits = 2) {
  formatC(x, format = 'f', digits = digits) %>%
    as.character()
}

#' Convert p-values to significance stars
#'
#' @param x Numeric p-value
#' @param cutpoint Significance threshold (default: 0.05)
#' @return Character string with significance stars
#' @export
starify <- function(x, cutpoint = 0.05) {
  dplyr::case_when(
    x < 0.001  ~ "***",
    x < 0.01   ~ "**",
    x < 0.05   ~ "*",
    TRUE       ~ ""
  )
}

# ============================================================================
# DATA EXTRACTION FUNCTIONS
# ============================================================================

#' Extract and tidy mediation analysis results
#'
#' @param med_object Output from mediation::mediate()
#' @param exp Logical: exponentiate coefficients? (for logistic/Cox models)
#' @return Data frame with estimates, CIs, and p-values
#' @export
med_tidy <- function(med_object, exp = FALSE) {
  sout <- summary(med_object)

  df <- data.frame(
    estimate = c(
      sout[["d0"]],
      sout[["z0"]],
      sout[["tau.coef"]],
      sout[["n.avg"]]
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

  if (exp) {
    df$estimate <- exp(df$estimate)
    df$ci_lower <- exp(df$ci_lower)
    df$ci_upper <- exp(df$ci_upper)
  }

  return(df)
}

# ============================================================================
# TABLE OUTPUT FUNCTION
# ============================================================================

#' Create formatted table of mediation analysis results
#'
#' @param med_object Output from mediation::mediate()
#' @param decimals Number of decimal places for estimates
#' @param p_accuracy Accuracy for p-value display
#' @param caption Table caption
#' @param label LaTeX label for cross-referencing
#' @param exp Logical: exponentiate coefficients?
#' @return Kable-formatted table
#' @export
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

  if (exists("opts_current")) {
    opts_current$set(label = label)
  }

  mediate_tidy_object %>%
    dplyr::mutate(
      term = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
      estimate = round(estimate, decimals),
      ci_lower = format2(ci_lower, digits = decimals),
      ci_upper = format2(ci_upper, digits = decimals),
      w = max(c(nchar(ci_lower), nchar(ci_upper))),
      ci_lower = paste0("(", pad(ci_lower, w), ", "),
      ci_upper = paste0(pad(ci_upper, w), ")"),
      p_value = scales::pvalue(p_value, accuracy = p_accuracy) %>%
        stringr::str_replace("<", "$<$")
    ) %>%
    dplyr::select(term, estimate, ci_lower, ci_upper, p_value) %>%
    dplyr::rename(
      ` ` = term,
      `Estimate` = estimate,
      `CI Lower` = ci_lower,
      `Upper` = ci_upper,
      `p-value` = p_value
    ) %>%
    knitr::kable(
      align = c('l', rep('r', 4)),
      escape = FALSE,
      caption = paste0(
        caption,
        " Causal mediation analysis with quasi-Bayesian confidence intervals. ",
        "Sample size: ", nobs, "."
      )
    )
}

# ============================================================================
# COEFFICIENT EXTRACTION FOR DIAGRAMS
# ============================================================================

#' Extract formatted coefficients for diagram labels
#'
#' @param mediation_out Output from mediation::mediate()
#' @param model_x_on_m Linear model of mediator ~ treatment
#' @param ci Include confidence intervals in output?
#' @param decimals Number of decimal places
#' @param exp Exponentiate coefficients?
#' @return Data frame with formatted coefficient strings
#' @export
med_extract_coefs <- function(mediation_out,
                              model_x_on_m,
                              ci = TRUE,
                              decimals = 2,
                              exp = FALSE) {

  med_df <- med_tidy(mediation_out, exp = exp)

  if (ci) {
    # Create CI strings
    ci_lower_str <- format2(med_df$ci_lower, decimals)
    ci_upper_str <- format2(med_df$ci_upper, decimals)

    med_ci <- paste0("(", ci_lower_str, ", ", ci_upper_str, ")")

    # Format ACME with CI - using paste instead of glue to avoid gray() issue
    acme <- paste0(
      format2(med_df$estimate[1], digits = decimals),
      "$^{", starify(med_df$p_value[1]), "}$",
      " \\\\ \\textcolor{gray}{\\small{\\,\\,\\,", med_ci[1], "}}"
    )

    # Format ADE with CI
    ade <- paste0(
      format2(med_df$estimate[2], digits = decimals),
      "$^{", starify(med_df$p_value[2]), "}$",
      " \\\\ \\textcolor{gray}{\\small{", med_ci[2], "}}"
    )

    # Format total effect with CI
    tot <- paste0(
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

    x_on_m <- paste0(
      format2(m1_df$estimate[2], digits = decimals),
      "$^{", starify(m1_df$p.value[2]), "}$",
      " \\\\ \\textcolor{gray}{\\small{", m1_ci, "\\,\\,\\,}}"
    )

  } else {
    # Simple format without CI
    acme <- paste0(
      format2(med_df$estimate[1], digits = decimals),
      starify(med_df$p_value[1])
    )

    ade <- paste0(
      format2(med_df$estimate[2], digits = decimals),
      starify(med_df$p_value[2])
    )

    tot <- paste0(
      format2(med_df$estimate[3], digits = decimals),
      starify(med_df$p_value[3])
    )

    m1_df <- broom::tidy(model_x_on_m)
    if (exp) {
      m1_df$estimate[2] <- exp(m1_df$estimate[2])
    }

    x_on_m <- paste0(
      format2(m1_df$estimate[2], digits = decimals),
      starify(m1_df$p.value[2])
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

# ============================================================================
# DIAGRAM PREPARATION FUNCTION
# ============================================================================

#' Prepare data frame for diagram creation
#'
#' @param med_out Mediation analysis output
#' @param mod_x_on_m Model for X->M path
#' @param lab_x Label for treatment variable
#' @param lab_y Label for outcome variable
#' @param lab_m Label for mediator variable
#' @param ci Include confidence intervals?
#' @param total Include total effect label?
#' @param decimals Number of decimal places
#' @param exp Exponentiate coefficients?
#' @return Data frame ready for diagram functions
#' @export
med_prepare_diagram_data <- function(med_out,
                                     mod_x_on_m,
                                     lab_x,
                                     lab_y,
                                     lab_m,
                                     ci = TRUE,
                                     total = TRUE,
                                     decimals = 2,
                                     exp = FALSE) {

  coefs_text <- med_extract_coefs(
    mediation_out = med_out,
    model_x_on_m = mod_x_on_m,
    ci = ci,
    decimals = decimals,
    exp = exp
  )

  med_df <- data.frame(
    lab_x = lab_x,
    lab_m = lab_m,
    lab_y = lab_y,
    coef_xm = coefs_text$x_on_m,
    coef_my = coefs_text$acme,
    coef_xy = coefs_text$ade,
    coef_tot = coefs_text$tot,
    stringsAsFactors = FALSE
  )

  if (total) {
    med_df$coef_tot <- paste0("Total: ", med_df$coef_tot)
  }

  return(med_df)
}

# ============================================================================
# DIAGRAMMER (GRAPHVIZ) OUTPUT
# ============================================================================

#' Create mediation diagram using DiagrammeR/GraphViz
#'
#' @param data Data frame from med_prepare_diagram_data()
#' @param height Node height in inches
#' @param width Node width in inches
#' @param graph_label Overall diagram label
#' @param node_text_size Font size for node labels
#' @param edge_text_size Font size for edge labels
#' @param color Node and edge color
#' @param ranksep Vertical spacing between ranks
#' @param minlen Minimum edge length
#' @return GraphViz diagram specification
#' @export
med_diagram_graphviz <- function(data,
                                 height = 0.75,
                                 width = 2,
                                 graph_label = "Mediation Diagram",
                                 node_text_size = 12,
                                 edge_text_size = 12,
                                 color = "black",
                                 ranksep = 0.2,
                                 minlen = 3) {

  data$height <- height
  data$width <- width
  data$color <- color
  data$ranksep <- ranksep
  data$minlen <- minlen
  data$node_text_size <- node_text_size
  data$edge_text_size <- edge_text_size

  diagram_spec <- glue::glue_data(
    data,
    "digraph mediation {
      fontname = Helvetica
      label = '<<graph_label>>'
      graph [ranksep = <<ranksep>>]

      node [
        fontname = Helvetica,
        shape = rectangle,
        fixedsize = TRUE,
        width = <<width>>,
        height = <<height>>,
        fontsize = <<node_text_size>>,
        color = <<color>>
      ]

      mm [label = '<<lab_m>>']
      xx [label = '<<lab_x>>']
      yy [label = '<<lab_y>>']

      edge [
        minlen = <<minlen>>,
        fontname = Helvetica,
        fontsize = <<edge_text_size>>,
        color = <<color>>
      ]

      mm -> yy [label = '<<coef_my>>'];
      xx -> mm [label = '<<coef_xm>>'];
      xx -> yy [label = '<<coef_xy>>'];

      { rank = same; mm }
      { rank = same; xx; yy }
    }",
    .open = "<<",
    .close = ">>"
  )

  return(diagram_spec)
}

# ============================================================================
# DIAGRAM PACKAGE OUTPUT
# ============================================================================

#' Create mediation diagram using diagram package
#'
#' @param data Data frame from med_prepare_diagram_data()
#' @param box_size Size of boxes
#' @param box_prop Box proportions
#' @param arrow_size Size of arrows
#' @param text_size Size of text
#' @param ... Additional arguments (ignored)
#' @return Plot object
#' @export
med_diagram_plotmat <- function(data,
                                box_size = 0.15,
                                box_prop = 0.5,
                                arrow_size = 1.5,
                                text_size = 1,
                                ...) {  # Accept and ignore extra arguments

  if (!requireNamespace("diagram", quietly = TRUE)) {
    stop("Package 'diagram' is needed. Please install it.", call. = FALSE)
  }

  # Simple approach: matrix with the coefficient values as edge labels
  # Matrix shows flow from rows to columns
  # Order: Row/Col 1 = Mediator, 2 = Treatment, 3 = Outcome
  M <- matrix(
    nrow = 3,
    ncol = 3,
    byrow = TRUE,
    data = c(
      0, 0, as.character(data$coef_my),     # Mediator -> Outcome
      as.character(data$coef_xm), 0, as.character(data$coef_xy),  # Treatment -> Mediator & Outcome
      0, 0, 0                                # Outcome (no outgoing)
    )
  )

  # Create the plot
  diagram::plotmat(
    M,
    pos = c(1, 2),  # Layout: 1 node top row, 2 nodes bottom row
    name = c(data$lab_m, data$lab_x, data$lab_y),  # Labels for nodes
    box.type = "rect",
    box.size = box_size,
    box.prop = box_prop,
    curve = 0,
    cex.txt = text_size,
    arr.width = arrow_size,
    arr.length = 0.3,
    shadow.size = 0,
    main = ""
  )
}

# ============================================================================
# TIKZ OUTPUT (FOR LATEX)
# ============================================================================

#' Create mediation diagram in TikZ format for LaTeX
#'
#' @param data Data frame from med_prepare_diagram_data()
#' @param scale Overall diagram scale
#' @param box_width Width of node boxes
#' @param box_height Height of node boxes
#' @param caption Figure caption
#' @param label LaTeX label for cross-referencing
#' @param include_figure Include figure environment?
#' @param ... Additional arguments (ignored)
#' @return TikZ code as string
#' @export
med_diagram_tikz <- function(data,
                             scale = 0.8,
                             box_width = "0.75in",
                             box_height = "0.4in",
                             caption = "",
                             label = "",
                             include_figure = TRUE,
                             ...) {  # Accept and ignore extra arguments

  data$scale <- scale
  data$box_width <- box_width
  data$box_height <- box_height
  data$caption <- caption
  data$label <- label

  tikz_code <- glue::glue_data(
    data,
    "\\begin{tikzpicture}[scale=<<scale>>, >=stealth, font=\\sffamily]
      \\scriptsize
      \\tikzset{
        mynode/.style={
          draw,
          text centered,
          text width = <<box_width>>,
          minimum height = <<box_height>>,
          align=center
        }
      }
      \\tikzset{>={Latex[width=1.5mm,length=1.5mm]}}

      \\node[mynode] (x) at (1,0) {<<lab_x>>};
      \\node[mynode] (y) at (7,0) {<<lab_y>>};
      \\node[mynode] (m) at (4,3) {<<lab_m>>};

      \\path[->] (x) edge node[below] {<<coef_xy>>} (y);
      \\path[->] (x) edge node[left]  {<<coef_xm>>} (m);
      \\path[->] (m) edge node[right] {<<coef_my>>} (y);
    \\end{tikzpicture}",
    .open = "<<",
    .close = ">>"
  )

  if (include_figure) {
    tikz_code <- glue::glue(
      "\\begin{figure}[htbp]
        \\centering
        <<tikz_code>>
        \\caption{<<caption>>}
        \\label{<<label>>}
      \\end{figure}",
      .open = "<<",
      .close = ">>",
      tikz_code = tikz_code,
      caption = caption,
      label = label
    )
  }

  return(as.character(tikz_code))
}

# ============================================================================
# WRAPPER FUNCTION FOR EASY USE
# ============================================================================

#' Create mediation diagram with any output format
#'
#' @param med_model Output from mediation::mediate()
#' @param x_on_m_model Linear model of mediator ~ treatment
#' @param x_label Label for treatment variable
#' @param m_label Label for mediator variable
#' @param y_label Label for outcome variable
#' @param output_type One of "graphviz", "plotmat", "tikz", or "all"
#' @param ci Include confidence intervals?
#' @param total Include total effect?
#' @param decimals Number of decimal places
#' @param exp Exponentiate coefficients?
#' @param ... Additional arguments passed to specific diagram functions
#' @return Diagram in specified format(s)
#' @export
create_mediation_diagram <- function(med_model,
                                     x_on_m_model,
                                     x_label = "Treatment",
                                     m_label = "Mediator",
                                     y_label = "Outcome",
                                     output_type = "graphviz",
                                     ci = TRUE,
                                     total = TRUE,
                                     decimals = 2,
                                     exp = FALSE,
                                     ...) {

  # Prepare data for all diagram types
  diagram_data <- med_prepare_diagram_data(
    med_out = med_model,
    mod_x_on_m = x_on_m_model,
    lab_x = x_label,
    lab_m = m_label,
    lab_y = y_label,
    ci = ci,
    total = total,
    decimals = decimals,
    exp = exp
  )

  # Get additional arguments for specific functions
  dots <- list(...)

  # Generate requested output type(s)
  output <- list()

  if (output_type %in% c("graphviz", "all")) {
    # Extract graphviz-specific arguments
    gv_args <- dots[names(dots) %in% c("height", "width", "graph_label",
                                       "node_text_size", "edge_text_size",
                                       "color", "ranksep", "minlen")]
    gv_spec <- do.call(med_diagram_graphviz, c(list(data = diagram_data), gv_args))

    if (requireNamespace("DiagrammeR", quietly = TRUE)) {
      output$graphviz <- DiagrammeR::grViz(gv_spec)
    } else {
      output$graphviz <- gv_spec
      warning("DiagrammeR package not installed. Returning GraphViz specification as text.")
    }
  }

  if (output_type %in% c("plotmat", "all")) {
    # Extract plotmat-specific arguments
    pm_args <- dots[names(dots) %in% c("box_size", "box_prop", "arrow_size", "text_size")]
    output$plotmat <- do.call(med_diagram_plotmat, c(list(data = diagram_data), pm_args))
  }

  if (output_type %in% c("tikz", "all")) {
    # Extract tikz-specific arguments
    tikz_args <- dots[names(dots) %in% c("scale", "box_width", "box_height",
                                         "caption", "label", "include_figure")]
    output$tikz <- do.call(med_diagram_tikz, c(list(data = diagram_data), tikz_args))
  }

  # Return single output or list
  if (length(output) == 1) {
    return(output[[1]])
  } else {
    return(output)
  }
}
