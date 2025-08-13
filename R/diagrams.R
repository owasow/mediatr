# ============================================================================
# DIAGRAM PREPARATION FUNCTION
# ============================================================================


#' Prepare data frame for diagram creation
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
med_prepare_diagram_data <- function(med_out,
                                     mod_x_on_m,
                                     lab_x,
                                     lab_y,
                                     lab_m,
                                     ci = TRUE,
                                     total = TRUE,
                                     decimals = 2,
                                     exp = FALSE) {

    # Extract formatted coefficients
    coefs_text <- med_extract_coefs(
        mediation_out = med_out,
        model_x_on_m = mod_x_on_m,
        ci = ci,
        decimals = decimals,
        exp = exp
    )

    # Create data frame with all diagram elements
    med_df <- data.frame(
        lab_x = lab_x,
        lab_m = lab_m,
        lab_y = lab_y,
        coef_xm = coefs_text$x_on_m,   # X -> M path
        coef_my = coefs_text$acme,     # M -> Y path (indirect effect)
        coef_xy = coefs_text$ade,      # X -> Y path (direct effect)
        coef_tot = coefs_text$tot,     # Total effect
        stringsAsFactors = FALSE
    )

    # Optionally add "Total: " prefix
    if (total) {
        med_df$coef_tot <- paste0("Total: ", med_df$coef_tot)
    }

    return(med_df)
}

# ============================================================================
# DIAGRAMMER (GRAPHVIZ) OUTPUT
# ============================================================================

#' Create mediation diagram using DiagrammeR/GraphViz
#' @param data Data frame from med_prepare_diagram_data()
#' @param height Node height in inches
#' @param width Node width in inches
#' @param graph_label Overall diagram label
#' @param node_text_size Font size for node labels
#' @param edge_text_size Font size for edge labels
#' @param color Node and edge color
#' @param ranksep Vertical spacing between ranks
#' @param minlen Minimum edge length
#' @return GraphViz diagram specification (use with DiagrammeR::grViz())
med_diagram_graphviz <- function(data,
                                 height = 0.75,
                                 width = 2,
                                 graph_label = "Mediation Diagram",
                                 node_text_size = 12,
                                 edge_text_size = 12,
                                 color = "black",
                                 ranksep = 0.2,
                                 minlen = 3) {

    # Add formatting parameters to data
    data$height <- height
    data$width <- width
    data$color <- color
    data$ranksep <- ranksep
    data$minlen <- minlen
    data$node_text_size <- node_text_size
    data$edge_text_size <- edge_text_size

    # Generate GraphViz specification using glue templating
    diagram_spec <- glue::glue_data(
        data,
        "digraph mediation {
      fontname = Helvetica
      label = '<<graph_label>>'
      graph [ranksep = <<ranksep>>]

      # Node definitions
      node [
        fontname = Helvetica,
        shape = rectangle,
        fixedsize = TRUE,
        width = <<width>>,
        height = <<height>>,
        fontsize = <<node_text_size>>,
        color = <<color>>
      ]

      mm [label = '<<lab_m>>']  # Mediator
      xx [label = '<<lab_x>>']  # Treatment
      yy [label = '<<lab_y>>']  # Outcome

      # Edge definitions with path coefficients
      edge [
        minlen = <<minlen>>,
        fontname = Helvetica,
        fontsize = <<edge_text_size>>,
        color = <<color>>
      ]

      mm -> yy [label = '<<coef_my>>'];  # Indirect effect
      xx -> mm [label = '<<coef_xm>>'];  # X to M
      xx -> yy [label = '<<coef_xy>>'];  # Direct effect

      # Layout constraints
      { rank = same; mm }           # Mediator on separate level
      { rank = same; xx; yy }       # Treatment and outcome on same level
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
#' @param data Data frame from med_prepare_diagram_data()
#' @param box_size Size of boxes
#' @param box_prop Box proportions
#' @param arrow_size Size of arrows
#' @param text_size Size of text
#' @return Plot object (displays immediately)
med_diagram_plotmat <- function(data,
                                box_size = 0.15,
                                box_prop = 0.5,
                                arrow_size = 1.5,
                                text_size = 1) {

  if (!requireNamespace("diagram", quietly = TRUE)) {
    stop("Package 'diagram' is needed. Please install it.", call. = FALSE)
  }

    # Create adjacency matrix for paths
    # Matrix represents: X->M, M->Y, X->Y
    M <- matrix(
        nrow = 3,
        ncol = 3,
        byrow = TRUE,
        data = c(
            0, data$coef_xm, 0,    # X row: X->M path
            0, 0, 0,               # M row: no outgoing (handled separately)
            data$coef_my, data$coef_xy, 0  # Y row: M->Y and X->Y paths
        )
    )

    # Create the plot
    plotmat(
        M,
        pos = c(1, 2),  # 1 node in first row (M), 2 in second (X, Y)
        name = c(data$lab_m, data$lab_x, data$lab_y),
        box.type = "rect",
        box.size = box_size,
        box.prop = box_prop,
        curve = 0,
        arr.width = arrow_size,
        cex.txt = text_size
    )
}

# ============================================================================
# TIKZ OUTPUT (FOR LATEX)
# ============================================================================

#' Create mediation diagram in TikZ format for LaTeX
#' @param data Data frame from med_prepare_diagram_data()
#' @param scale Overall diagram scale
#' @param box_width Width of node boxes
#' @param box_height Height of node boxes
#' @param caption Figure caption
#' @param label LaTeX label for cross-referencing
#' @param include_figure Include figure environment?
#' @return TikZ code as string
med_diagram_tikz <- function(data,
                             scale = 0.8,
                             box_width = "0.75in",
                             box_height = "0.4in",
                             caption = "",
                             label = "",
                             include_figure = TRUE) {

    # Add parameters to data for templating
    data$scale <- scale
    data$box_width <- box_width
    data$box_height <- box_height
    data$caption <- caption
    data$label <- label

    # Generate TikZ code
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

      % Nodes
      \\node[mynode] (x) at (1,0) {<<lab_x>>};
      \\node[mynode] (y) at (7,0) {<<lab_y>>};
      \\node[mynode] (m) at (4,3) {<<lab_m>>};

      % Paths
      \\path[->] (x) edge node[below] {<<coef_xy>>} (y);  % Direct effect
      \\path[->] (x) edge node[left]  {<<coef_xm>>} (m);  % X to M
      \\path[->] (m) edge node[right] {<<coef_my>>} (y);  % M to Y (indirect)

      % Optional total effect below
      \\path[->] (x) edge[bend right=30] node[below] {<<coef_tot>>} (y);
    \\end{tikzpicture}",
        .open = "<<",
        .close = ">>"
    )

    # Optionally wrap in figure environment
    if (include_figure) {
        tikz_code <- glue(
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
