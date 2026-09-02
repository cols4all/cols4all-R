show_attach_scores = function(z, include_tritan = TRUE) {
	type = z$type[1]
	if (!all(z$type == type)) stop("mixed palette types not allowed")


	k = nrow(z)

	s = .C4A$s

	# intersect(), not score_x100 directly: a sysdata.rda built before the
	# _dp (deutan+protan-only) columns existed won't have them in `s` yet,
	# and this rescaling must not break existing (include_tritan = TRUE)
	# behavior while that rebuild is pending.
	sx100 = intersect(.C4A$score_x100, dimnames(s)[[2]])
	s[,sx100,] = s[,sx100,] / 100

	if (!include_tritan && !"min_dist_dp" %in% dimnames(s)[[2]]) {
		warning("include_tritan = FALSE requires sysdata.rda to be rebuilt (missing '_dp' score columns); falling back to include_tritan = TRUE", call. = FALSE)
		include_tritan = TRUE
	}


	s2 = s[match(z$fullname, dimnames(s)[[1]]), , , drop = FALSE]
	s3 = do.call(rbind, lapply(1:k, function(i) {
		# maximum n to take scores from (cat: dim max, seq/div, the scores for the largest palettes)
		mmax = if (type == "cat") dim(s2)[3] else min(z$n[i], utils::tail(which(!is.na(s2[i, "min_dist", ])), 1))
		m = min(z$n[i], mmax)
		s2[i,,m]
	}))

	# approximation of min step for decreased range
	# if (!is.null(range)) {
	# 	rng = range[2] - range[1]
	# 	s3[, "min_step"] = round(s3[, "min_step"] * rng)
	# 	s3[, "max_step"] = round(s3[, "max_step"] * rng)
	# }

	z2 = cbind(z, as.data.frame(s3))

	z2$cbfriendly = get_friendlyness(z2, include_tritan = include_tritan)
	z2$cbfriendly[is.na(z2$cbfriendly)] = 0

	# Per-CVD badges (cat only, for now -- see check_cat_pal()'s
	# min_dist_none/_deutan/_protan/_tritan): unlike cbfriendly above, no
	# worst-case aggregation across CVDs, one classification per CVD state,
	# same CBF_th$cat/CBVF_th$cat/CBU_th$cat thresholds applied to each.
	# NA (not 0) for non-cat types, where these raw columns don't exist.
	classify_cat = function(x) ifelse(x >= .C4A$CBVF_th$cat["min_dist"], 2,
								ifelse(x >= .C4A$CBF_th$cat["min_dist"], 1,
								ifelse(x <= .C4A$CBU_th$cat["min_dist"], -1, 0)))
	# NA (rather than erroring on a length-0 assignment), not classify_cat():
	# a sysdata.rda predating these columns won't have min_dist_none/etc. in
	# .C4A$s at all yet, same staleness gap as the _dp columns above.
	z2$cbf_none = if (is.null(z2$min_dist_none)) NA else classify_cat(z2$min_dist_none)
	z2$cbf_deutan = if (is.null(z2$min_dist_deutan)) NA else classify_cat(z2$min_dist_deutan)
	z2$cbf_protan = if (is.null(z2$min_dist_protan)) NA else classify_cat(z2$min_dist_protan)
	z2$cbf_tritan = if (is.null(z2$min_dist_tritan)) NA else classify_cat(z2$min_dist_tritan)

	# Raw "overall" numbers (cat/bivc only) for the "Show scores" table --
	# distinct from cbfriendly above (a classification, gated by
	# include_tritan) and from get_friendlyness()'s internal aggregate
	# (used only for sort/filter): these are both variants shown side by
	# side, always, regardless of include_tritan.
	if (all(c("min_dist_deutan", "min_dist_protan", "min_dist_tritan") %in% names(z2))) {
		z2$min_dist_overall = pmin(z2$min_dist_deutan, z2$min_dist_protan, z2$min_dist_tritan)
		z2$min_dist_overall_dp = pmin(z2$min_dist_deutan, z2$min_dist_protan)
	}
	#ßz2$iscbf = (z2$cbfriendly == 1)
	#a = t(mapply(analyse_hcl, z2$palette, z2$type))
	#z2 = cbind(z2, a)

	z2$chroma = "M"

	z2$chroma[z2$Cmax >= .C4A$Cintense] = "H"
	z2$chroma[z2$Cmax < .C4A$Cpastel] = "L"


	#z2$highC = z2$Cmax >= .C4A$Cintense

	z2$Hspread = round(get_spread(z2$Hwidth, z2$n))


	if (type %in% c("cat", "bivc")) {
		z2$fairness = get_fairness(z2$Lrange, z2$Crange)
		z2$fair = ifelse(z2$fairness >= .C4A$LC_fair, "H", ifelse(z2$fairness < .C4A$LC_unfair, "L", "M"))
		# z2$fair = ifelse(z2$Crange < .C4A$CrangeFair & z2$Lrange < .C4A$LrangeFair, "H",
		# 					ifelse(z2$Crange > .C4A$CrangeUnfair | z2$Lrange > .C4A$LrangeUnfair, "L", "M"))
		# z2$fairRank = rank(c("H" = 2000000, "M" = 1000000, "L" = 0)[z2$fair] + (999000 - z2$Crange * 1000) + (999 - z2$Lrange))
	} else {
		z2$fairness = get_fairness(0, z2$Crange)
		z2$fair = ifelse(z2$fairness >= .C4A$LC_fair, "H", ifelse(z2$fairness < .C4A$LC_unfair, "L", "M"))
		# z2$fair = ifelse(z2$Crange < .C4A$CrangeFair, "H",
		# 			 ifelse(z2$Crange > .C4A$CrangeUnfair, "L", "M"))
		# z2$fairRank = rank(z2$Crange)
	}


	if (type == "div") {
		z2$hues = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
	} else if (type == "seq") {
		z2$hues = ifelse(z2$Hwidth < .C4A$HwidthSeqSingle, "SH", ifelse(z2$Hwidth < .C4A$HwidthSeqRainbow, "MH", "RH"))
	} else if (type %in% c("cat")) {
		z2$hues = ifelse(z2$Hspread > .C4A$Hspread, "RH", "NA")
	} else if (type %in% c("bivs", "bivd", "bivg")) {
		z2$hues = ifelse(z2$HwidthL >= .C4A$HwidthDivRainbow | z2$HwidthR >= .C4A$HwidthDivRainbow, "RH",
					 ifelse(z2$HwidthL < .C4A$HwidthDivSingle & z2$HwidthR < .C4A$HwidthDivSingle, "SH", "MH"))
		z2$HwidthLR = pmax(z2$HwidthL, z2$HwidthR)
	}
	z2$equiluminance = z2$CRmin < .C4A$contrastEL
	z2$contrastWT = z2$CRwt < .C4A$contrastTxt
	z2$contrastBK = z2$CRbk < .C4A$contrastTxt

	z2$float = z2$Blues >= .C4A$Blues

	z2$H[z2$Hwidth >= 180] = 360

	z2$nameable = as.logical(z2$nameability)



	z2
}

get_spread = function(Hwidth, n) {
	one_piece = (360 / n)

	mx = 360 - one_piece

	Hwidth / mx * 100

}



get_friendlyness = function(zn, include_tritan = TRUE) {
	if (!include_tritan) {
		# swap in the deutan+protan-only scores (tritan is ~400x rarer than
		# deutan+protan combined, ~1 in 10,000 vs ~4% of the population) --
		# the branching logic below is otherwise unchanged. Harmless no-op
		# for cat/bivc rows here (their min_dist_dp is NA/absent, see below).
		for (col in c("min_dist", "min_step", "inter_wing_dist", "tri_ineq")) {
			dp_col = paste0(col, "_dp")
			if (dp_col %in% names(zn)) zn[[col]] = zn[[dp_col]]
		}
	}

	# cat/bivc no longer store min_dist/min_dist_dp (see check_cat_pal()) --
	# they're fully represented by the 4 raw per-cvd columns instead, so
	# derive the worst-case aggregate here, overwriting whatever the swap
	# above left in zn$min_dist for these rows (NA, since they never had a
	# min_dist_dp to swap in). Other types are untouched: they still don't
	# have this per-cvd breakdown, so they keep using the swap above.
	is_cat_like = zn$type %in% c("cat", "bivc")
	if (any(is_cat_like) && all(c("min_dist_deutan", "min_dist_protan", "min_dist_tritan") %in% names(zn))) {
		agg = pmin(zn$min_dist_deutan, zn$min_dist_protan,
				   if (include_tritan) zn$min_dist_tritan else Inf)
		if (is.null(zn$min_dist)) zn$min_dist = NA_real_
		zn$min_dist[is_cat_like] = agg[is_cat_like]
	}

	with(zn, {
		ifelse(type == "cat", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBVF_th$cat["min_dist"], 2, ifelse(min_dist >= .C4A$CBF_th$cat["min_dist"], 1,
							  ifelse(min_dist <= .C4A$CBU_th$cat["min_dist"], -1, 0))),


		ifelse(type == "seq", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBF_th$seq["min_dist"] & tri_ineq >= .C4A$CBF_th$seq["tri_ineq"], 1,
							  ifelse(min_dist < .C4A$CBU_th$seq["min_dist"] | tri_ineq < .C4A$CBU_th$seq["tri_ineq"], -1, 0)),

		ifelse(type == "cyc", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBF_th$cyc["min_dist"] & tri_ineq >= .C4A$CBF_th$cyc["tri_ineq"], 1,
			   												 ifelse(min_dist <= .C4A$CBU_th$cyc["min_dist"] | tri_ineq < .C4A$CBU_th$cyc["tri_ineq"], -1, 0)),

		ifelse(type == "div", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$div["inter_wing_dist"] & min_step >= .C4A$CBF_th$div["min_step"] & tri_ineq >= .C4A$CBF_th$div["tri_ineq"], 1,
							  ifelse(inter_wing_dist < .C4A$CBU_th$div["inter_wing_dist"] | min_step < .C4A$CBU_th$div["min_step"] | tri_ineq < .C4A$CBU_th$div["tri_ineq"], -1, 0)),


		ifelse(type == "bivs", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$bivs["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivs["min_step"], 1,
							   ifelse(inter_wing_dist < .C4A$CBU_th$bivs["inter_wing_dist"] | min_step < .C4A$CBU_th$bivs["min_step"], -1, 0)),


	   ifelse(type == "bivc", (min_dist / 1000) + ifelse(min_dist >= .C4A$CBF_th$cat["min_dist"], 1,
	   							 ifelse(min_dist <= .C4A$CBU_th$cat["min_dist"], -1, 0)),


		ifelse(type == "bivd", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$bivd["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivd["min_step"], 1,
   							  ifelse(inter_wing_dist < .C4A$CBU_th$bivd["inter_wing_dist"] | min_step < .C4A$CBU_th$bivd["min_step"], -1, 0)),


	   ifelse(type == "bivg", (inter_wing_dist / 1000) + (min_step / 1e6) + ifelse(inter_wing_dist >= .C4A$CBF_th$bivg["inter_wing_dist"] & min_step >= .C4A$CBF_th$bivg["min_step"], 1,
							   ifelse(inter_wing_dist < .C4A$CBU_th$bivg["inter_wing_dist"] | min_step < .C4A$CBU_th$bivg["min_step"], -1, 0)), 0))))))))
	})
}
