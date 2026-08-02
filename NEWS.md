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
  Recommended pairing: `bw = TRUE` with `weight_by = "coefficient"` (width =
  effect size, dotted = null) or `weight_by = "significance"` (width =
  evidence tier); monochrome arrows make weight the only channel.
