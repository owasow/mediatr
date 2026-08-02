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
  Recommended pairing: `bw = TRUE, weight_by = "significance"` (the APSR
  agenda-seeding Figure 9 idiom: monochrome arrows, weight as the channel).
