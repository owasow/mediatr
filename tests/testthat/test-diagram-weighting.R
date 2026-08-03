# bw / weight_by across all diagram functions (shared .tier_style_factory).
# Fixtures are hand-built prep-style data frames so no lavaan/mediation fits
# are needed; coefficient strings use the same "$^{***}$" markup the
# *_data_prep_* functions emit (zero stars = "$^{}$").

dual_dat <- data.frame(
  lab_x = "Treatment", lab_y = "Outcome", lab_m1 = "Mediator 1", lab_m2 = "Mediator 2",
  coef_a1 = "0.48$^{***}$", coef_b1 = "0.42$^{***}$",
  coef_a2 = "0.22$^{***}$", coef_b2 = "-0.03$^{}$",
  coef_ind_m1 = "0.20$^{***}$", coef_ind_m2 = "-0.01$^{}$",
  coef_c = "-0.15$^{**}$", coef_total = "0.04$^{}$", coef_total_ind = "0.19$^{***}$",
  stringsAsFactors = FALSE)

serial_dat <- data.frame(
  lab_x = "Treatment", lab_y = "Outcome", lab_m1 = "Mediator 1", lab_m2 = "Mediator 2",
  coef_a1 = "0.48$^{***}$", coef_a2 = "0.38$^{***}$", coef_d1 = "0.13$^{*}$",
  coef_b1 = "0.33$^{***}$", coef_b2 = "-0.02$^{}$", coef_c = "-0.11$^{*}$",
  coef_ind_serial = "0.06$^{***}$", coef_ind_m1 = "0.16$^{***}$",
  coef_ind_m2 = "0.04$^{*}$", coef_total_ind = "0.26$^{***}$", coef_total = "0.15$^{**}$",
  stringsAsFactors = FALSE)

single_dat <- data.frame(
  lab_x = "Job Training", lab_y = "Depression", lab_m = "Self-Efficacy",
  coef_xm = "0.07$^{}$", coef_xmy = "-0.02$^{}$", coef_my = "-0.23$^{***}$",
  coef_xy = "-0.05$^{}$", coef_tot = "-0.06$^{}$",
  stringsAsFactors = FALSE)

test_that(".tier_style_factory maps tiers, magnitudes, and null paths", {
  f_none <- mediatr:::.tier_style_factory("none", "0.5$^{***}$")
  expect_identical(f_none("0.5$^{***}$"), "")

  f_sig <- mediatr:::.tier_style_factory("significance", "0.5$^{***}$")
  expect_identical(f_sig("0.5$^{***}$"), ", line width=1.25pt")
  expect_identical(f_sig("0.5$^{*}$"),   ", line width=0.45pt")
  expect_identical(f_sig("0.5$^{}$"),    ", line width=0.3pt, densely dotted")
  expect_identical(f_sig("no markup"),   "")
  expect_identical(f_sig(NA_character_), "")

  coefs <- c("0.50$^{***}$", "0.25$^{**}$", "0.00$^{}$")
  f_coef <- mediatr:::.tier_style_factory("coefficient", coefs)
  expect_identical(f_coef("0.50$^{***}$"), ", line width=1.25pt")  # = ref
  expect_identical(f_coef("0.25$^{**}$"),  ", line width=0.775pt") # halfway
  expect_match(f_coef("0.00$^{}$"), "densely dotted")

  # shared coef_ref: same coefficient, larger ref -> thinner arrow
  f_shared <- mediatr:::.tier_style_factory("coefficient", coefs, coef_ref = 1)
  expect_identical(f_shared("0.50$^{***}$"), ", line width=0.775pt")

  # |est| beyond ref saturates at max width
  expect_identical(f_shared("2.00$^{***}$"), ", line width=1.25pt")

  # degenerate: no parseable magnitudes -> min widths, no error
  f_degen <- mediatr:::.tier_style_factory("coefficient", c(NA, "text"))
  expect_identical(f_degen("0.00$^{***}$"), ", line width=0.3pt")
})

test_that("legacy defaults emit no weighting styles (all functions)", {
  for (out in list(
    sem_dual_med_diagram_tikz(dual_dat, show_paths = TRUE),
    sem_dual_med_diagram_compact_tikz(dual_dat),
    sem_serial_med_diagram_tikz(serial_dat),
    med_diagram_tikz(single_dat),
    med_diagram_acme_tikz(single_dat)
  )) {
    out <- as.character(out)
    expect_false(grepl("line width=", out))
    expect_false(grepl("densely dotted", out))
  }
})

test_that("compact dual: bw, weighting, and the dashed-M2 convention", {
  legacy <- as.character(sem_dual_med_diagram_compact_tikz(dual_dat))
  expect_match(legacy, ", thick, dashed", fixed = TRUE)

  w <- as.character(sem_dual_med_diagram_compact_tikz(
    dual_dat, bw = TRUE, weight_by = "coefficient"))
  expect_false(grepl("blue", w))
  expect_false(grepl("red!70", w))
  expect_match(w, "line width=")
  # dash now reserved for evidence: legacy pathway dashes gone, n.s. dotted
  expect_false(grepl(", thick, dashed", w, fixed = TRUE))
  expect_match(w, "densely dotted")
})

test_that("serial: bw kills all three colors; both weight modes style arrows", {
  w <- as.character(sem_serial_med_diagram_tikz(
    serial_dat, bw = TRUE, weight_by = "coefficient"))
  expect_false(grepl("blue|red!70|purple", w))
  expect_match(w, "line width=")
  expect_match(w, "densely dotted")  # b2 is n.s.

  s <- as.character(sem_serial_med_diagram_tikz(serial_dat, weight_by = "significance"))
  expect_match(s, "line width=1.25pt")  # *** paths at top tier
})

test_that("single-mediator diagrams weight all arrows incl. curved ACME", {
  w <- as.character(med_diagram_acme_tikz(single_dat, weight_by = "coefficient"))
  expect_match(w, "line width=1.25pt")   # b-path 0.23 is this diagram's ref
  expect_match(w, "densely dotted")      # a-path, ADE, Total, ACME all n.s.
  expect_match(w, "\\\\draw\\[->, line width=")  # the curved ACME arrow styled

  b <- as.character(med_diagram_tikz(single_dat, weight_by = "significance"))
  expect_match(b, "line width=")
})

test_that("weight_by validates its argument", {
  expect_error(sem_serial_med_diagram_tikz(serial_dat, weight_by = "magnitude"))
  expect_error(med_diagram_tikz(single_dat, weight_by = "magnitude"))
})
