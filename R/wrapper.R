# ============================================================================
# WRAPPER FUNCTION FOR EASY USE
# ============================================================================

#' Create mediation diagram with any output format
#' @param med_model Output from mediation::mediate()
#' @param x_on_m_model Linear model of mediator ~ treatment
#' @param x_label Label for treatment variable
#' @param m_label Label for mediator variable
#' @param y_label Label for outcome variable
#' @param output_type One of "graphviz", "plotmat", "tikz", or "all"
#' @param ... Additional arguments passed to specific diagram functions
#' @return Diagram in specified format(s)
create_mediation_diagram <- function(med_model,
                                     x_on_m_model,
                                     x_label = "Treatment",
                                     m_label = "Mediator",
                                     y_label = "Outcome",
                                     output_type = "plotmat",
                                     ...) {

    # Prepare data for all diagram types
    diagram_data <- med_prepare_diagram_data(
        med_out = med_model,
        mod_x_on_m = x_on_m_model,
        lab_x = x_label,
        lab_m = m_label,
        lab_y = y_label,
        ...
    )

    # Generate requested output type(s)
    output <- list()

    if (output_type %in% c("graphviz", "all")) {
        gv_spec <- med_diagram_graphviz(diagram_data, ...)
        output$graphviz <- DiagrammeR::grViz(gv_spec)
    }

    if (output_type %in% c("plotmat", "all")) {
        output$plotmat <- med_diagram_plotmat(diagram_data, ...)
    }

    if (output_type %in% c("tikz", "all")) {
        output$tikz <- med_diagram_tikz(diagram_data, ...)
    }

    # Return single output or list
    if (length(output) == 1) {
        return(output[[1]])
    } else {
        return(output)
    }
}
