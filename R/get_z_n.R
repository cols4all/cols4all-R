get_z_n = function(z, n = NA, m = NA, filters = character(0), range = NA, colorsort = "orig", include_tritan = TRUE) {
	# if (!is.null(n)) {

	if ("nmax" %in% filters) {
		n.only = TRUE
		filters = setdiff(filters, "nmax")
	} else {
		n.only = FALSE
	}

	if (is.na(n) && is.na(m)) {
		sel = TRUE
	} else if (is.na(m)) {
		if (n.only) {
			sel = n == z$nmax
		} else {
			sel = n <= z$nmax & n >= z$nmin
		}
	} else {
		if (n.only) {
			sel = n == z$nmax & m <= z$mmax & m >= z$mmin
		} else {
			sel = n <= z$nmax & n >= z$nmin & m <= z$mmax & m >= z$mmin
		}
	}

	z2 = z[sel, ]

	if (nrow(z2) == 0) return(NULL)
	z2$palette = do.call(mapply, c(list(FUN = get_pal_n, MoreArgs = list(n = n, m = m, range = range, colorsort = colorsort), SIMPLIFY = FALSE), as.list(z2)))
	z2$n = n
	z2$m = m


	if (length(filters)) {
		z3 = show_attach_scores(z2, include_tritan = include_tritan)
		z2 = z2[pass_filters(z3, filters), ]
	 }

	z2
}

# Labels of the table filters (also used in c4a_gui)
.filter_labels = c(nmax = "Only n = nmax",
				   cbf = "Colorblind-friendly",
				   fair = "Fair",
				   crW = "Good contrast ratio with white",
				   crB = "Good contrast ratio with black",
				   naming = "Nameability")

# Which palettes pass all filters? z3: palettes with scores (show_attach_scores)
pass_filters = function(z3, filters) {
	filters = setdiff(filters, "nmax")
	if (!length(filters)) return(rep(TRUE, nrow(z3)))
	sels = lapply(filters, function(f) {
		switch(f,
			   cbf = z3$cbfriendly >= 1,
			   fair = z3$fair == "H",
			   naming = z3$nameable,
			   crW = !z3$contrastWT,
			   crB = !z3$contrastBK)
	})
	x = Reduce("&", sels)
	x[is.na(x)] = FALSE
	x
}

# Number of palettes in z (one type) with n (and m) colors that pass the filters.
# Only uses the precomputed scores, so it is fast.
count_pass = function(z, n, m = NA, filters = character(0), include_tritan = TRUE) {
	sel = if ("nmax" %in% filters) z$nmax == n else (z$nmin <= n & z$nmax >= n)
	if (!is.na(m)) sel = sel & z$mmin <= m & z$mmax >= m
	z2 = z[sel, , drop = FALSE]
	f = setdiff(filters, "nmax")
	if (!nrow(z2) || !length(f)) return(nrow(z2))
	z2$n = n
	z2$m = m
	sum(pass_filters(show_attach_scores(z2, include_tritan = include_tritan), f))
}

# Explain why no palettes are found, and what to change. Returns a list with a
# title and a character vector of suggestions.
explain_no_palettes = function(type, n, m = NA, series, filters = character(0), include_tritan = TRUE) {
	z = .C4A$z
	z = z[z$type == type, , drop = FALSE]
	isbiv = substr(type, 1, 3) == "biv"
	if (length(series) && series[1] == "all") series = unique(z$series)

	type_lab = .C4A$type_info$description[match(type, .C4A$type_info$type)]
	ncol_lab = function(n) if (isbiv) paste0(n, " x ", if (type == "bivs") n else m, " colors") else paste0(n, " colors")
	m_of = function(n) if (type == "bivs") n else m
	enum = function(x) if (length(x) <= 1) x else paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
	fmt_ranges = function(x) {
		grp = cumsum(c(1, diff(x) != 1))
		enum(unname(tapply(x, grp, function(y) if (length(y) == 1) as.character(y) else paste0(y[1], " to ", y[length(y)]))))
	}

	if (!length(series)) return(list(title = "No palette series selected.", details = "Select one or more palette series."))

	zs = z[z$series %in% series, , drop = FALSE]
	zo = z[!(z$series %in% series), , drop = FALSE]

	# candidate numbers of colors (as in the GUI slider)
	nmx = suppressWarnings(max(z$nmax[is.finite(z$nmax)]))
	if (!is.finite(nmx) || any(is.infinite(z$nmax))) nmx = max(15, if (is.finite(nmx)) nmx else 0)
	ns = seq(if (type %in% c("div", "bivd")) 3 else 2, nmx)

	k0 = count_pass(zs, n, m_of(n), include_tritan = include_tritan)
	details = character(0)

	if (k0 == 0) {
		title = paste0("The selected series contain no ", type_lab, " palettes with ", ncol_lab(n), ".")
		ok_n = ns[vapply(ns, function(ni) count_pass(zs, ni, m_of(ni), include_tritan = include_tritan) > 0, logical(1))]
		if (length(ok_n)) details = c(details, paste0("Choose another number of colors: available for ", fmt_ranges(ok_n), " colors."))
		other = unique(zo$series[zo$nmin <= n & zo$nmax >= n])
		if (length(other)) details = c(details, paste0("Or select other series that contain such palettes: ", enum(sort(other)), "."))
		return(list(title = title, details = details))
	}

	if (!length(filters) || count_pass(zs, n, m_of(n), filters, include_tritan) > 0) {
		return(list(title = "No palettes found. Please change the selection.", details = character(0)))
	}

	flab = unname(.filter_labels[filters])
	ftxt = paste0(if (length(filters) > 1) "the filters " else "the filter ", enum(paste0("\"", flab, "\"")))
	title = if (k0 == 1) {
		paste0("The only ", type_lab, " palette with ", ncol_lab(n), " does not pass ", ftxt, ".")
	} else {
		paste0("None of the ", k0, " ", type_lab, " palettes with ", ncol_lab(n), " pass ", ftxt, ".")
	}

	# which filters are the bottleneck?
	if (length(filters) > 1) {
		kf = vapply(filters, function(f) count_pass(zs, n, m_of(n), f, include_tritan), numeric(1))
		details = c(details, paste0("Palettes passing each filter separately: ", enum(paste0(flab, " ", kf, " of ", k0)), "."))
	}

	# other number of colors
	kn = vapply(ns, function(ni) count_pass(zs, ni, m_of(ni), filters, include_tritan), numeric(1))
	ok_n = ns[kn > 0]
	if (length(ok_n)) {
		nbest = ok_n[which.min(abs(ok_n - n))]
		details = c(details, paste0("Choose another number of colors: palettes that pass are available for ", fmt_ranges(ok_n), " colors",
									" (e.g. ", kn[ns == nbest], " palette", if (kn[ns == nbest] > 1) "s" else "", " with ", ncol_lab(nbest), ")."))
	} else {
		details = c(details, paste0("No palettes in the selected series pass ", if (length(filters) > 1) "these filters" else "this filter", " for any number of colors."))
	}

	# other series
	if (nrow(zo)) {
		ko = vapply(split(zo, zo$series), function(zi) count_pass(zi, n, m_of(n), filters, include_tritan), numeric(1))
		ko = ko[ko > 0]
		if (length(ko)) details = c(details, paste0("Or select other series with palettes that pass: ", enum(names(ko)), "."))
	}

	# tritan
	if ("cbf" %in% filters && include_tritan) {
		kt = count_pass(zs, n, m_of(n), filters, include_tritan = FALSE)
		if (kt > 0) details = c(details, paste0("Or exclude tritan (blue-yellow) color blindness from the colorblind-friendly filter: then ", kt, " palette", if (kt > 1) "s pass" else " passes", "."))
	}

	if (length(filters) > 1) details = c(details, "Or remove one of the filters.")
	list(title = title, details = details)
}
