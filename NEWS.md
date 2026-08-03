# mediatr 0.4.0

* The `bw` and `weight_by` display options introduced in 0.3.0 for
  `sem_dual_med_diagram_tikz()` now cover every diagram function. The
  star-tier and coefficient-magnitude machinery moved to a shared internal
  helper (`.tier_style_factory()`), so all functions use identical mapping
  rules and accept the same `tier_widths` / `coef_widths` / `coef_ref`
  arguments. Defaults reproduce each function's legacy output byte-for-byte.
  - `sem_dual_med_diagram_compact_tikz()`: gains `bw` and `weight_by`. In
    weighted modes the legacy dashed-M2-pathway convention is dropped: dash
    type then encodes evidence (nonsignificant paths densely dotted), not
    pathway identity.
  - `sem_serial_med_diagram_tikz()`: gains `bw` (all three pathway colors)
    and `weight_by` across all six drawn paths.
  - `med_diagram_tikz()` and `med_diagram_acme_tikz()`: gain `weight_by`
    (these diagrams are black and white by design, so no `bw` flag). The
    curved ACME arrow is weighted by its own magnitude.
* First testthat suite: weighting behavior and legacy-output stability for
  all five diagram functions.
* Citation infrastructure: `inst/CITATION`, `CITATION.cff`, and URL /
  BugReports fields in DESCRIPTION.
* README gains a sample-diagram gallery (`man/figures/`, regenerable via
  `inst/examples/readme_figures.R`).

# mediatr 0.3.0

* `sem_dual_med_diagram_tikz()` gains two display options, developed on the
  emotions-paper deployment (2026-08-02):
  - `bw = TRUE`: monochrome diagram (both pathway colors black), so line
    weight is the only visual channel.
  - `weight_by = "significance"`: each arrow's line width encodes its own
    significance tier, read from the stars in its formatted coefficient
    (heavy p < .001, medium p < .01, light p < .05; nonsignificant paths
    render densely dotted so the topology stays visible). `tier_widths`
    customizes the ladder. Default `"none"` reproduces legacy output
    byte-for-byte.
  - `weight_by = "coefficient"`: line width scales continuously with each
    path's absolute standardized estimate (parsed from the leading number of
    its formatted coefficient), capped at `coef_ref` (default: this diagram's
    max |estimate|; pass a shared value for cross-panel comparability), with
    `coef_widths` setting the min/max pt. Width encodes magnitude; dash type
    still encodes evidence (n.s. paths densely dotted).
  - `legend = FALSE`: suppress the in-diagram significance legend (for
    multi-panel figures where the caption carries the star key once).
  Recommended pairing: `bw = TRUE` with `weight_by = "coefficient"` (width =
  effect size, dotted = null) or `weight_by = "significance"` (width =
  evidence tier); monochrome arrows make weight the only channel.
