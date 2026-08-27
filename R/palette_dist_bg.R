# Background-lightness penalty (CIELAB L*, colorspace). Returns n x n matrix in
# (floor, 1]: 1 when the background lightness sits at a pair's midpoint (ideal /
# straddle), falling off as both colors move to one side of the background.
lum_factor = function(x, bgcol, k = 100, floor = 0.05) {
	L  = colorspace::coords(as(colorspace::hex2RGB(x),     "LAB"))[, "L"]
	Lb = colorspace::coords(as(colorspace::hex2RGB(bgcol), "LAB"))[1, "L"]
	mid = outer(L, L, function(a, b) (a + b) / 2)
	f   = 1 - abs(mid - Lb) / k
	f[] = pmax(floor, pmin(1, f))
	f
}

# palette_dist_bg -- ΔE2000 base (via colorblindcheck), three methods:
#   "bg-norm"           m^2 / b_mean                       (original)
#   "bg-norm-cr-spread" bg-norm * log(rho + 1)             (original)
#   "min-lum"           min(m_ij, b_i, b_j) * lum_factor   (recommended)
# In "min-lum", b is the visibility floor (weaker color's distance to bg) and the
# lightness penalty captures dark-on-light suppression. lum_k / lum_floor tune the
# penalty (fit to user-study data). lum_factor uses the un-simulated colors on
# purpose: it is a display-luminance relationship, and CVD simulation preserves
# luminance, so it is independent of `cvd`.
palette_dist_bg = function(x, bgcol = "#FFFFFF", cvd = NULL, severity = 1,
						   metric = 2000, lum_k = 100, lum_floor = 0.05,
						   method = c("bg-norm", "bg-norm-cr-spread", "min-lum")) {
	method = match.arg(method)
	if (is.null(bgcol)) {
		return(colorblindcheck::palette_dist(x, cvd = cvd, severity = severity, metric = metric))
	}
	n  = length(x)
	x2 = c(x, bgcol)
	m2 = colorblindcheck::palette_dist(x2, cvd = cvd, severity = severity, metric = metric)
	bdiff = m2[1:n, n + 1]
	m     = m2[1:n, 1:n]

	if (method == "min-lum") {
		base = pmin(m, outer(bdiff, bdiff, pmin))          # min(m_ij, b_i, b_j)
		out  = base * lum_factor(x, bgcol, k = lum_k, floor = lum_floor) #
		diag(out) = NA
		return(out)
	}

	# --- original bg-norm family -------------------------------------------
	b    = matrix((rep(bdiff, times = n) + rep(bdiff, each = n)) / 2, nrow = n)
	base = (m^2) / b
	if (method == "bg-norm") {
		base
	} else if (method == "bg-norm-cr-spread") {
		cvd_type  = if (is.null(cvd)) "none" else cvd
		x_sim     = cols4all:::sim_cvd(x,     cvd = cvd_type, severity = severity)
		bgcol_sim = cols4all:::sim_cvd(bgcol, cvd = cvd_type, severity = severity)
		r   = sapply(x_sim, function(ci) colorspace::contrast_ratio(ci, bgcol_sim))
		rho = outer(r, r, FUN = function(a, b) pmax(a, b) / pmin(a, b))
		base * log(rho + 1)
	}
}
