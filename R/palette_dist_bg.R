palette_dist_bg = function(x, bgcol = "#FFFFFF", cvd = NULL, severity = 1,
						   metric = 2000,
						   method = c("bg-norm", "bg-norm-cr-spread")) {
	method = match.arg(method)

	if (is.null(bgcol)) {
		return(colorblindcheck::palette_dist(x, cvd = cvd, severity = severity, metric = metric))
	}

	n  = length(x)
	x2 = c(x, bgcol)
	m2 = colorblindcheck::palette_dist(x2, cvd = cvd, severity = severity, metric = metric)

	bdiff = m2[1:n, n+1]
	b     = matrix((rep(bdiff, times = n) + rep(bdiff, each = n)) / 2, nrow = n)
	m     = m2[1:n, 1:n]

	base  = (m^2) / b

	if (method == "bg-norm") {
		base

	} else if (method == "bg-norm-cr-spread") {
		cvd_type  = if (is.null(cvd)) "none" else cvd

		x_sim     = cols4all:::sim_cvd(x,      cvd = cvd_type, severity = severity)
		bgcol_sim = cols4all:::sim_cvd(bgcol,  cvd = cvd_type, severity = severity)

		r   = sapply(x_sim, function(ci) colorspace::contrast_ratio(ci, bgcol_sim))
		rho = outer(r, r, FUN = function(a, b) pmax(a, b) / pmin(a, b))

		base * log(rho + 1)
	}
}
