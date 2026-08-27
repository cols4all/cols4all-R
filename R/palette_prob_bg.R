# Mark discrimination probability model -- Szafir (2018), "Modeling Color
# Difference for Visualization Design", IEEE TVCG 24(1). Per CIELAB axis,
# detection rate scales linearly with color difference and inversely with
# mark size:
#   p = m_x(s) * dx,   m_x(s) = c_x + k_x / s
# s = mark size (deg visual angle -- line thickness for "line", point
# diameter for "point"), dx = |difference| along L*/a*/b*, p = proportion of
# viewers expected to detect it. Axes are combined via the paper's own
# Euclidean generalization (Eq. 4), clipped to [0, 1]. Coefficients:
#   "line"  -- Experiment 3 (line graphs), Eqs. 14-16, fit for s in 0.05-0.35
#   "point" -- Experiment 1 (scatterplots), Eqs. 5-7, fit for s in 0.25-2.0
# (Coefficients cross-checked in Aug 2026 against the paper's own supplemental
# regression data at cmci.colorado.edu/visualab/VisColors, which reproduces
# the same c/k values via a straight `slope ~ 1/size` OLS fit.)
#
# CAVEATS (keep these when reusing this model, both marks):
# 1. White background only -- Szafir's stimuli were on plain white; this is
#    a white-background baseline and says nothing about gray/black.
# 2. Validated range differs per mark (see above). Outside it the model
#    extrapolates; it's linear per axis with no saturation, so raw
#    predictions can exceed 1 (clipped here, a kink rather than the smooth
#    asymptote a real psychometric function would have) -- and for "point",
#    c_x + k_x/s actually goes *negative* below roughly s = 0.1-0.2 deg
#    (k is more negative, relative to c, than for "line"), which is not a
#    meaningful extrapolation at all, just a sign flip from being far
#    outside the fitted domain. Point sizes well under ~0.25 deg should not
#    be trusted.
# 3. Cross-axis combination (Euclidean, above) was proposed by the paper for
#    real multi-axis color pairs but not directly tested that way -- axis
#    was a between-participants factor in the original experiments.
# 4. No mark-vs-background term: this models two thin/small marks compared
#    to each other against a field of gray distractors, not a mark against a
#    large background field, so lum_factor()'s bdiff logic isn't reused
#    here. palette_prob_bg()'s lum_adjust = TRUE multiplies in lum_factor()
#    anyway, as an untested hypothesis (that penalty was fit on distances,
#    not probabilities) -- off by default, not a trusted correction.
# 5. Points are less discriminable than lines of "equal" size: the paper
#    found colors are generally more discriminable on elongated marks
#    (lines, bars) than on points of the same visual-angle size -- don't
#    expect a 0.2 deg point and a 0.2 deg line to give similar probabilities
#    for the same color pair.
.mark_coefs = list(
	line = list(
		L = c(c = 0.0742, k = -0.0023),
		A = c(c = 0.0623, k = -0.0015),
		B = c(c = 0.0425, k = -0.0009)
	),
	point = list(
		L = c(c = 0.0937, k = -0.0085),
		A = c(c = 0.0775, k = -0.0121),
		B = c(c = 0.0611, k = -0.0096)
	)
)

.mark_discrim_slope = function(s, axis, mark = "line") {
	co = .mark_coefs[[mark]][[axis]]
	unname(co["c"] + co["k"] / s)
}

#' Line-mark discrimination probability for a pair of colors
#'
#' Predicts the proportion of viewers who would detect the color difference
#' between two line marks, as a function of line thickness, using the model
#' fit by Szafir (2018) for line graphs (see Details).
#'
#' Model: `p = m_x(s) * dx`, `m_x(s) = c_x + k_x / s`, per CIELAB axis (L*,
#' a*, b*), combined across axes as `sqrt(sum((dx * m_x(s))^2))` and clipped
#' to `[0, 1]`. `s` is line thickness in degrees of visual angle, `dx` is the
#' absolute difference between the two colors along that axis.
#'
#' **Caveats:**
#' - **White background only.** Szafir's stimuli were rendered on plain
#'   white; this is a white-background baseline and says nothing about gray
#'   or black backgrounds.
#' - **Validated range.** The regressions were fit using six color-difference
#'   steps per axis, all below the detection asymptote, and thickness was
#'   tested from 0.05 to 0.35 degrees. Outside that range the model is
#'   extrapolating: it is linear in `dx` per axis, so nothing stops a raw
#'   prediction from exceeding 1 (clipped here, which produces a kink rather
#'   than the smooth saturation a real psychometric function would have).
#' - **Cross-axis combination is untested.** Szafir's experiment held "which
#'   axis differs" as a between-participants factor, so no participant
#'   judged a pair differing on more than one axis at once. The Euclidean
#'   combination above is the paper's own proposed generalization for real
#'   (multi-axis) color pairs, but it was not directly tested that way.
#'
#' @param hex1,hex2 two hex color strings, e.g. `"#3B4CC0"` and `"#B40426"`
#' @param thickness numeric vector of line thicknesses in degrees of visual
#'   angle. Szafir's tested range was 0.05 to 0.35 degrees; the default spans
#'   a bit wider for a smooth curve, but see the validated-range caveat above
#'   for values outside that range.
#' @return data.frame with columns `thickness_deg` and `p_discriminable`
#' @references Szafir, D.A. (2018). Modeling Color Difference for
#'   Visualization Design. IEEE Transactions on Visualization and Computer
#'   Graphics, 24(1), Experiment 3.
#' @seealso [point_discrim_prob()], [palette_prob_bg()], [visual_angle_to_px()]
#' @export
line_discrim_prob = function(hex1, hex2, thickness = seq(0.05, 0.5, by = 0.01)) {
	lab1 = colorspace::coords(as(colorspace::hex2RGB(hex1), "LAB"))[1, c("L", "A", "B")]
	lab2 = colorspace::coords(as(colorspace::hex2RGB(hex2), "LAB"))[1, c("L", "A", "B")]
	d = lab1 - lab2

	p = vapply(thickness, function(s) {
		sqrt(sum(vapply(c("L", "A", "B"), function(axis) {
			(d[[axis]] * .mark_discrim_slope(s, axis, "line"))^2
		}, numeric(1))))
	}, numeric(1))

	data.frame(thickness_deg = thickness, p_discriminable = pmin(p, 1))
}

#' Point-mark discrimination probability for a pair of colors
#'
#' The point-mark analogue of [line_discrim_prob()]: predicts the proportion
#' of viewers who would detect the color difference between two circular
#' scatterplot-style point marks, as a function of point diameter, using the
#' model fit by Szafir (2018) for scatterplots (Experiment One; see Details).
#'
#' Model: `p = m_x(s) * dx`, `m_x(s) = c_x + k_x / s`, per CIELAB axis (L*,
#' a*, b*), combined across axes as `sqrt(sum((dx * m_x(s))^2))` and clipped
#' to `[0, 1]` -- identical form to [line_discrim_prob()], but with its own
#' coefficients fit on point marks, and `s` is point *diameter* rather than
#' line thickness.
#'
#' **Caveats** (see [line_discrim_prob()] for the shared ones -- white
#' background only, cross-axis combination untested):
#' - **Validated range.** Fit using six diameter steps from 0.25 to 2.0
#'   degrees. This is a much higher floor than the line model's 0.05-0.35
#'   degrees -- **do not** feed sizes below roughly 0.25 degrees into this
#'   function. Below about 0.1-0.2 degrees (depending on axis), `c_x + k_x/s`
#'   goes negative, which is not a meaningful extrapolation, just a sign
#'   flip from being far outside the fitted domain.
#' - **Points are less discriminable than lines of the same size.** Szafir's
#'   own comparison found colors generally more discriminable on elongated
#'   marks (lines, bars) than on points -- don't expect a point and a line of
#'   the same visual-angle size to give similar probabilities for the same
#'   color pair.
#'
#' @param hex1,hex2 two hex color strings, e.g. `"#3B4CC0"` and `"#B40426"`
#' @param size numeric vector of point diameters in degrees of visual angle.
#'   Szafir's tested range was 0.25 to 2.0 degrees; see the validated-range
#'   caveat above for values outside that range.
#' @return data.frame with columns `size_deg` and `p_discriminable`
#' @references Szafir, D.A. (2018). Modeling Color Difference for
#'   Visualization Design. IEEE Transactions on Visualization and Computer
#'   Graphics, 24(1), Experiment One.
#' @seealso [line_discrim_prob()], [palette_prob_bg()]
#' @export
point_discrim_prob = function(hex1, hex2, size = seq(0.25, 2, by = 0.05)) {
	lab1 = colorspace::coords(as(colorspace::hex2RGB(hex1), "LAB"))[1, c("L", "A", "B")]
	lab2 = colorspace::coords(as(colorspace::hex2RGB(hex2), "LAB"))[1, c("L", "A", "B")]
	d = lab1 - lab2

	p = vapply(size, function(s) {
		sqrt(sum(vapply(c("L", "A", "B"), function(axis) {
			(d[[axis]] * .mark_discrim_slope(s, axis, "point"))^2
		}, numeric(1))))
	}, numeric(1))

	data.frame(size_deg = size, p_discriminable = pmin(p, 1))
}

#' Convert a line thickness from degrees of visual angle to pixels
#'
#' The defaults (30 inch viewing distance, 96 dpi) match Szafir (2018)'s own
#' assumed viewing conditions. Override both to match an actual lab setup
#' (measured pixel pitch and viewing distance), rather than trusting a
#' reported dpi, which is frequently wrong for actual displays.
#'
#' The standard chord formula for the linear size subtending a visual angle
#' `deg` at distance `d` is `2 * d * tan(deg/2)` -- this is what `convention
#' = "standard"` computes. Szafir (2018)'s own in-text px-equivalents (e.g.
#' "0.25 deg (6px)", "2.0 deg (50px)" for points; "0.05 deg (1px)", "0.35 deg
#' (9px)" for lines) are consistently exactly *half* of that formula's result
#' at their own stated 30in/96dpi assumptions -- across both experiments, at
#' four independently checked (angle, px) pairs, which is why this is treated
#' as their own conversion convention rather than a one-off rounding
#' difference. `convention = "szafir"` (the default, and the package default
#' via `c4a_options("angle_convention")`) reproduces that, so preview sizes
#' in [palette_prob_bg()] / `c4a_gui()` visually match the paper's own
#' figures; `convention = "standard"` gives the textbook-correct value, which
#' is what you want once calibrating against a real measured display.
#'
#' @param deg line thickness in degrees of visual angle
#' @param viewing_distance_in viewing distance, in inches
#' @param dpi display resolution, in dots (pixels) per inch
#' @param convention `"szafir"` or `"standard"` -- see Details. Defaults to
#'   the `angle_convention` package option (see [c4a_options()]).
#' @return line thickness in pixels
#' @seealso [px_to_visual_angle()], [line_discrim_prob()], [c4a_options()]
#' @export
visual_angle_to_px = function(deg, viewing_distance_in = 30, dpi = 96,
							   convention = .C4A$angle_convention) {
	convention = match.arg(convention, c("szafir", "standard"))
	factor = if (convention == "szafir") 1 else 2
	rad = deg * pi / 180
	inches = factor * viewing_distance_in * tan(rad / 2)
	inches * dpi
}

#' Convert a line thickness from pixels to degrees of visual angle
#'
#' Inverse of [visual_angle_to_px()]. See its documentation for the default
#' viewing conditions, the `convention` argument, and why the conditions
#' should be overridden for a real setup.
#'
#' @param px line thickness in pixels
#' @param viewing_distance_in viewing distance, in inches
#' @param dpi display resolution, in dots (pixels) per inch
#' @param convention `"szafir"` or `"standard"` -- see [visual_angle_to_px()].
#'   Defaults to the `angle_convention` package option (see [c4a_options()]).
#' @return line thickness in degrees of visual angle
#' @seealso [visual_angle_to_px()], [line_discrim_prob()], [c4a_options()]
#' @export
px_to_visual_angle = function(px, viewing_distance_in = 30, dpi = 96,
							   convention = .C4A$angle_convention) {
	convention = match.arg(convention, c("szafir", "standard"))
	factor = if (convention == "szafir") 1 else 2
	inches = px / dpi
	2 * atan(inches / (factor * viewing_distance_in)) * 180 / pi
}

#' Pairwise mark-discrimination probability matrix for a palette
#'
#' The probability analogue of `palette_dist_bg()`'s `"min-lum"` method: for
#' every pair of colors in `x`, the probability that two `size`-sized marks
#' (line marks, or point/scatterplot marks) drawn in those colors would be
#' told apart, using the Szafir (2018) mark-discrimination models (see
#' [line_discrim_prob()] / [point_discrim_prob()]) instead of a
#' `colorblindcheck::palette_dist()` distance.
#'
#' Only `"min-lum"`'s weakest-link-plus-lightness-penalty idea is mirrored
#' here, since it is the only `palette_dist_bg()` method with a coherent
#' probability interpretation: `"bg-norm"` and `"bg-norm-cr-spread"` are
#' distance-space transforms (squared-distance ratios, log-contrast spread)
#' that don't map onto a `[0, 1]`-bounded probability, so there is no
#' `method` argument here.
#'
#' Unlike `palette_dist_bg()`, this function does **not** compute a
#' mark-vs-background discriminability term (the `bdiff` piece of
#' `"min-lum"`). Szafir's models were fit on two thin/small marks compared
#' against each other on a field of gray distractors, not a mark against a
#' large uniform background field, so reusing that ΔE2000-based `bdiff`
#' logic here is not well founded. `lum_adjust = TRUE` is offered as an
#' exploratory alternative instead (see below) -- if a background-visibility
#' floor is wanted for this model, it needs its own justification, not a
#' straight port from the distance version.
#'
#' **Caveats**, in addition to those on [line_discrim_prob()] /
#' [point_discrim_prob()] (note the two marks have different validated size
#' ranges -- 0.05-0.35 degrees for `"line"`, 0.25-2.0 degrees for `"point"`):
#' - **`lum_adjust = TRUE` is exploratory, not validated.** It multiplies the
#'   Szafir-derived probability by `lum_factor()`, which was fit on ΔE-based
#'   *distance* data, not probabilities. There is no empirical basis yet for
#'   combining the two this way -- treat it as a hypothesis to test, not a
#'   trustworthy correction. It defaults to `FALSE`.
#'
#' @param x vector of hex colors (the palette)
#' @param bgcol background color as a hex string. Only used when
#'   `lum_adjust = TRUE`.
#' @param cvd type of color vision deficiency to simulate before computing
#'   probabilities, or `NULL` for none. See `colorblindcheck::palette_dist()`
#'   for supported values.
#' @param severity severity of the CVD simulation, between 0 and 1
#' @param thickness mark size in degrees of visual angle -- line thickness
#'   when `mark = "line"`, point diameter when `mark = "point"` -- passed to
#'   the underlying [line_discrim_prob()] / [point_discrim_prob()] model. See
#'   their documentation for the (different) validated ranges.
#' @param mark which Szafir (2018) model to use: `"line"` (line graphs) or
#'   `"point"` (scatterplots). See [line_discrim_prob()] and
#'   [point_discrim_prob()].
#' @param metric accepted for signature parity with `palette_dist_bg()`, but
#'   unused: this model works directly on CIELAB L*/a*/b*, not on a
#'   `colorblindcheck::palette_dist()` distance.
#' @param lum_k,lum_floor passed to `lum_factor()` when `lum_adjust = TRUE`;
#'   see that function's documentation for their meaning.
#' @param lum_adjust should the probability be multiplied by `lum_factor()`?
#'   Exploratory and unvalidated -- see Details.
#' @return n x n matrix of discrimination probabilities as percentages
#'   (0-100), with `NA` on the diagonal
#' @seealso [line_discrim_prob()], [point_discrim_prob()]
#' @export
palette_prob_bg = function(x, bgcol = "#FFFFFF", cvd = NULL, severity = 1,
							thickness = 0.05, mark = c("line", "point"), metric = 2000,
							lum_k = 100, lum_floor = 0.05,
							lum_adjust = FALSE) {
	mark = match.arg(mark)
	x_use = if (!is.null(cvd)) cols4all:::sim_cvd(x, cvd = cvd, severity = severity) else x

	lab = colorspace::coords(as(colorspace::hex2RGB(x_use), "LAB"))[, c("L", "A", "B"), drop = FALSE]

	dL = outer(lab[, "L"], lab[, "L"], `-`)
	dA = outer(lab[, "A"], lab[, "A"], `-`)
	dB = outer(lab[, "B"], lab[, "B"], `-`)

	p = sqrt((dL * .mark_discrim_slope(thickness, "L", mark))^2 +
			 (dA * .mark_discrim_slope(thickness, "A", mark))^2 +
			 (dB * .mark_discrim_slope(thickness, "B", mark))^2)
	p = pmin(p, 1)

	if (lum_adjust) {
		# un-simulated x on purpose, same rationale as lum_factor()'s own use
		# in palette_dist_bg: a display-luminance relationship, independent
		# of CVD simulation (which preserves luminance).
		p = p * lum_factor(x, bgcol, k = lum_k, floor = lum_floor)
	}

	diag(p) = NA
	dimnames(p) = list(x, x)
	p * 100
}
