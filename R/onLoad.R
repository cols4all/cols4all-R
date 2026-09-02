#' cols4all overview
#'
#' cols4all stands for: color palettes for all people, including those with color vision deficiency. Popular color palette series, such as ColorBrewer, have been organized by type and have been scored on several properties such as color-blind-friendliness and fairness (i.e. do colors stand out equally?). Own palettes can also be loaded and analysed. Besides the common palette types (categorical, sequential, and diverging) it also includes bivariate color palettes. ggplot2 scales are included.
#'
#' This page provides a brief overview of all package functions.
#'
#' @section Main functions:
#' \tabular{ll}{
#' \code{\link{c4a_gui}}\tab Dashboard for analyzing the palettes \cr
#' \code{\link{c4a}}\tab Get the colors from a palette (\code{\link{c4a_na}} for the associated color for missing values) \cr
#' \code{\link{c4a_plot}}\tab Plot a color palette \cr
#' }
#'
#' @section Palette names and properties:
#' \tabular{ll}{
#' \code{\link{c4a_palettes}}\tab Get available palette names \cr
#' \code{\link{c4a_series}}\tab Get available series names\cr
#' \code{\link{c4a_overview}}\tab Get an overview of palettes per series x type\cr
#' \code{\link{c4a_citation}}\tab Show how to cites palettes (with bibtex code) \cr
#' \code{\link{c4a_info}}\tab Get information from a palette, such as type and maximum number of colors) \cr
#' \code{\link{.P}}\tab Environment via which palette names can be browsed with auto-completion (using `$`) \cr

#' }
#'
#' @section Importing and exporting palettes:
#' \tabular{ll}{
#' \code{\link{c4a_data}}\tab Build color palette data \cr
#' \code{\link{c4a_load}}\tab Load color palette data \cr
#' \code{\link{c4a_sysdata_import}}\tab Import system data \cr
#' \code{\link{c4a_sysdata_export}}\tab Export system data \cr
#' }
#'
#' @concept color
#' @concept visualization
"_PACKAGE"



do_cellspec = function(lst) {
	do.call(kableExtra::cell_spec, lst)
}


.onLoad <- function(...) {
	assign("z", .z, envir = .C4A)
	assign("s", .s, envir = .C4A)
	assign("zbib", .zbib, envir = .C4A)
	assign("zdes", .zdes, envir = .C4A)
	assign("names_NL_model", names_NL_model, envir = .C4A)
	assign("names_NL_colors", names_NL_colors, envir = .C4A)
	name_data = rdata$name_data
	assign("name_data", name_data, envir = .C4A)


	attach_bib()

	with(.C4A,{
		defaults = c(cat = "cols4all.line7", seq = "kovesi.blue", div = "cols4all.pu_gn_div", cyc = "scico.roma_o", bivs = "cols4all.bu_br_bivs", bivc = "met_monet", bivd = "cols4all.pu_gn_bivd", bivg = "cols4all.br_bivg")

		# min_dist_none/_deutan/_protan/_tritan NOT included here: unlike
		# the DeltaE-based scores below (where *100-then-/100 preserves 2
		# decimal digits through integer storage), get_prob_matrix() already
		# returns a 0-100 percentage -- check_cat_pal() stores it directly
		# (no *100), so it needs no /100 recovery either. Rescaling it here
		# would double-divide it into a decimal fraction (e.g. 0.45 instead
		# of the whole percentage 45).
		score_x100 = c("min_dist", "min_step", "max_step", "inter_wing_dist", "tri_ineq", "CRmin", "CRwt", "CRbk", "Blues",
					   "min_dist_dp", "min_step_dp", "max_step_dp", "inter_wing_dist_dp", "tri_ineq_dp")

		# "_dp" (deutan+protan) variants: tritan is ~400x rarer than deutan+
		# protan combined (~1 in 10,000 vs ~4% of the population), so the
		# worst-case min() across all 3 CVDs (the non-_dp scores) is opt-out
		# rather than the only option -- see get_friendlyness()'s
		# include_tritan arg (used for the "cbfriendly" sort/filter, default
		# TRUE i.e. unchanged historical behavior). Same thresholds are
		# reused for both: dropping tritan can only raise a palette's score
		# (never lower it), never requires a separate cutoff.
		#
		# "_none"/"_deutan"/"_protan"/"_tritan" (cat only, for now): the raw
		# per-CVD value with no worst-case aggregation at all, one column per
		# CVD state, shown as 4 separate N/D/P/T badges in the GUI table
		# instead of collapsing to a single number -- see get_friendlyness()
		# is not involved here, that's still the single aggregate; these
		# feed a separate per-cvd classification in show_attach_scores().
		# Same CBF_th$cat/CBVF_th$cat/CBU_th$cat thresholds reused for all 4.

		# cat/bivc are scored via get_prob_matrix() (Szafir 2018, % viewers who
		# can discriminate a 0.05deg-thick line -- check_cat_pal()), so their
		# min_dist thresholds below are on a 0-100 percentage scale. All other
		# types (seq/cyc/div/bivs/bivd/bivg) still use get_dist_matrix() (the
		# old min-lum DeltaE metric) -- their thresholds are unchanged /
		# original DeltaE-scale values.

		#color-blind-friendly thresholds
		CBF_th = list(cat = c(min_dist = 90),
					  seq = c(min_dist = 5, tri_ineq = 2),
					  cyc = c(min_dist = 5, tri_ineq = 2),
					  div = c(inter_wing_dist = 10, min_step = 5, tri_ineq = 2),
					  bivs = c(inter_wing_dist = 7, min_step = 3),
					  bivc = c(min_dist = 90),
					  bivd = c(inter_wing_dist = 7, min_step = 3),
					  bivg = c(inter_wing_dist = 7, min_step = 3))

		#color-blind-very-friendly thresholds
		CBVF_th = list(cat = c(min_dist = 95))

		# unfriendly (rolling eyes)
		CBU_th = list(cat = c(min_dist = 50),
					  seq = c(min_dist = 2, tri_ineq = 0),
					  cyc = c(min_dist = 2, tri_ineq = 0),
					  div = c(inter_wing_dist = 4, min_step = 2, tri_ineq = 0),
					  bivs = c(inter_wing_dist = 3, min_step = 2),
					  bivc = c(min_dist = 50),
					  bivd = c(inter_wing_dist = 3, min_step = 2),
					  bivg = c(inter_wing_dist = 3, min_step = 2))

		Cgray = 10 # maximum chroma value to be considered as gray (used for Hwidth and c4a_add_series)

		LrangeFair = 30
		LrangeUnfair = 50
		CrangeFair = 50
		CrangeUnfair = 80

		Lrange_mid = 50
		Lrange_steep = 0.1
		Crange_mid = 80
		Crange_steep = 0.07

		LC_fair = 75
		LC_unfair = 25


		Blues = 3
		contrastEL = 1.2 # Equiluminance
		contrastTxt = 3

		Cintense = 100 # chroma of colors that are considered intense
		Cpastel = 70 # chroma of 'pastel' colors
		HwidthDivRainbow = 90 # a diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least HwidthDivRainbow
		HwidthDivSingle = 20 # a diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most HwidthDivSingle
		HwidthSeqRainbow = 180 # a sequential palette is labeled as 'rainbow hue' if Hwidth is at least HwidthSeqRainbow
		HwidthSeqSingle = 15 # a sequential palette is labeled as 'single hue' if Hwidth is at most HwidthSeqSingle

		Hspread = 90 # from which number between 0 and 100, is a palette labeled "Hue spread" (cat)

		sc = c("min_dist",
			   "nameability",
			   "min_step",
			   "max_step",
			   "inter_wing_dist",
			   "tri_ineq",
			   "min_dist_dp",
			   "min_step_dp",
			   "max_step_dp",
			   "inter_wing_dist_dp",
			   "tri_ineq_dp",
			   "min_dist_none",
			   "min_dist_deutan",
			   "min_dist_protan",
			   "min_dist_tritan")

		types = c("Categorical" = "cat",
				  "Sequential" = "seq",
				  "Diverging" = "div",
				  "Cyclic" = "cyc",
				  "Bivariate (sequential x sequential)" = "bivs",
				  "Bivariate (sequential x categorical)" = "bivc",
				  "Bivariate (sequential x diverging)" = "bivd",
				  "Bivariate (sequential x desaturated)" = "bivg")

		types1 = c("Categorical" = "cat",
				   "Sequential" = "seq",
				   "Diverging" = "div",
				   "Cyclic" = "cyc",
				   "Bivariate" = "biv")

		types2 = list(biv = c("Sequential x sequential" = "bivs",
							  "Sequential x categorical" = "bivc",
							  "Sequential x diverging" = "bivd",
							  "Sequential x desaturated" = "bivg"))

		type_info = data.frame(type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"),
							   description = c("categorical",
							   				"sequential",
							   				"diverging",
							   				"cyclic",
							   				"bivariate (sequential x sequential)", "bivariate (sequential x categorical)", "bivariate (sequential x diverging)", "bivariate (sequential x desaturated)"))

		ndef = c(cat = Inf, seq = 7, cyc = 9, div = 9, bivc = Inf, bivs = 3, bivd = 3, bivg  = 3) # Inf meaning maximum available colors
		mdef = c(cat = 1, seq = 1, cyc = 1, div = 1, bivc = 3, bivs = NA, bivd = 3, bivg  = 3) # NA meaning same as ndef

		# cat/bivc display range is on the get_prob_matrix() (% discriminable)
		# scale; all other types are back on the DeltaE scale (original ranges).
		# cat/bivc: min_dist isn't stored anymore (see check_cat_pal()) -- the
		# 4 raw per-CVD values plus both "overall" aggregate variants are
		# shown instead (all computed in show_attach_scores()).
		cb_ranges_cat_like = list(min_dist_none = c(0, 100), min_dist_deutan = c(0, 100), min_dist_protan = c(0, 100),
								  min_dist_tritan = c(0, 100), min_dist_overall = c(0, 100), min_dist_overall_dp = c(0, 100))
		CB_ranges = list(cat = cb_ranges_cat_like,
						 seq = list(min_dist = c(0, 20), tri_ineq = c(-50, 50)),
						 cyc = list(min_dist = c(0, 20), tri_ineq = c(-50, 50)),
						 div = list(inter_wing_dist = c(0, 20), min_step = c(0, 20), tri_ineq = c(-50, 50)),
						 bivs = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)),
						 bivc = cb_ranges_cat_like,
						 bivd = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)),
						 bivg = list(inter_wing_dist = c(0, 20), min_step = c(0, 20)))

		Ohter_ranges = list(C = c(0, 180, 5),
							L = c(0, 100, 5),
							H = c(0, 360, 5),
							Blues = c(1, 5, 0.1),
							contrastTH = c(1, 2, 0.1))


		rgb = c("Blues")

		# for score file
		hcl = c("Cmax", "H", "HL", "HR", "Lmid", "Hwidth", "HwidthL", "HwidthR", "Lrange", "Crange", "fairness", "CRmin", "CRwt", "CRbk")

		# for table (with derived variables) -- grouped by theme (Hue/Chroma/
		# Luminance/Contrast Ratio), not the original arbitrary order, so
		# plot_table() can draw one shared column header per contiguous
		# group ("fairness" doesn't belong to any of the 4 named groups, so
		# it's moved to the end rather than interrupting one of them). Just
		# display order -- .C4A$hcl (no "2", used for the score array schema
		# in series_add_get_scores.R / c4a_data.R) is untouched, unaffected.
		hcl2 = c("H", "HL", "HR", "Hwidth", "Hspread", "HwidthL", "HwidthR",
				 "Cmax", "Crange",
				 "Lmid", "Lrange",
				 "CRmin", "CRwt", "CRbk",
				 "fairness")

		sortRev = c("cbfriendly", "harmonyRank", "fairness", "Cmax", "min_dist", "tri_ineq", "min_dist_dp", "tri_ineq_dp", "nameability", "Lmid", "Hwidth", "Hspread", "HwidthL", "HwidthR", "nmax", "CRwt", "CRbk", "Blues",
					  "cbf_none", "cbf_deutan", "cbf_protan", "cbf_tritan",
					  "min_dist_none", "min_dist_deutan", "min_dist_protan", "min_dist_tritan", "min_dist_overall", "min_dist_overall_dp")

		# naming_fun = "naming_dist_centroid"
		# naming_colors = c(Green = "#859F68",
		# 				  Blue = "#5792A4",
		# 				  Purple = "#7E6A89",
		# 				  Pink = "#C7848F",
		# 				  Yellow = "#E7B352",
		# 				  Brown = "#8F5F49",
		# 				  Orange = "#D97447",
		# 				  Red = "#9D4149",
		# 				  White = "#D8CEBA",
		# 				  Gray = "#868782",
		# 				  Black = "#394245") #boynton
		# naming_softmax = list(a = 2, th = .1)
		# naming_fun_args = list(weights = c(Green = 1, Blue = 1, Purple = 1.1, Pink = 0.9,
		# 								   Yellow = 1, Brown = 1, Orange = 1, Red = 1.05,
		# 								   White = 0.7, Gray = 0.7, Black = 1.05))
		#

		naming_fun = "naming_sample_from_distribution"
		naming_fun_args = list(model = names_NL_model)
		naming_colors = names_NL_colors
		naming_softmax = list(a = 8, th = .1)

		labels = c(min_dist = "Minimum distance",
				   min_dist_none = "Separability (normal)",
				   min_dist_deutan = "Separability (deutan)",
				   min_dist_protan = "Separability (protan)",
				   min_dist_tritan = "Separability (tritan)",
				   min_dist_overall = "Separability (overall, incl. tritan)",
				   min_dist_overall_dp = "Separability (overall, excl. tritan)",
				   nameability = "Nameability",
				   min_step = "Minimum step",
				   max_step = "Maximum step",
				   inter_wing_dist = "Inter-wing-distance",
				   tri_ineq = "Triangle inequality",
				   min_dist_dp = "Minimum distance (excl. tritan)",
				   min_step_dp = "Minimum step (excl. tritan)",
				   max_step_dp = "Maximum step (excl. tritan)",
				   inter_wing_dist_dp = "Inter-wing-distance (excl. tritan)",
				   tri_ineq_dp = "Triangle inequality (excl. tritan)",
				   Crel = "Chroma (rel) max",
				   Cmax = "Chroma max",
				   H = "Hue middle",
				   HL = "Hue middle L",
				   HR = "Hue middle R",
				   Lmid = "Luminance mid",
				   Hwidth = "Hue width",
				   Hspread = "Hue spread",
				   HwidthL = "Hue width L",
				   HwidthR = "Hue width R",
				   Lrange = "Luminance range",
				   Crange = "Chroma range",
				   LCrange = "Lum/Chr range",
				   CRmin = "Contrast-Ratio minimum",
				   CRwt = "Contrast-Ratio white",
				   CRbk = "Contrast-Ratio black",
				   cbfriendly = "Colorblind-friendly",
				   cbf_none = "Separability (normal vision)",
				   cbf_deutan = "Separability (deutan)",
				   cbf_protan = "Separability (protan)",
				   cbf_tritan = "Separability (tritan)",
				   chroma = "Vivid",
				   fair = "Fair",
				   nameable = "Naming",
				   fairness = "Fairness",
				   hues = "Hues",
				   equiluminance = "Contrast (between)",
				   contrastWT = "Contrast (white)",
				   contrastBK = "Contrast (black)",
				   float = "3D Blues",
				   Blues = "Dominant blues",
				   nmax = "Max number")

		th = list(series = list("Series", tooltip = "Palette series. See last column for references"),
				  name = list("Name", tooltip = "Palette name"),
				  cbfriendly = list("CBF", tooltip = "Colorblind-friendly: is the palette suitable for colorblind people?"),
				  cbf_none = list("N", tooltip = "Normal color vision: baseline distinctness (not itself a colorblindness measure)"),
				  cbf_deutan = list("D", tooltip = "Deutan (red-green color blind, ~5% of men, ~0.4% of women)"),
				  cbf_protan = list("P", tooltip = "Protan (also red-green color blind, ~1% of men)"),
				  cbf_tritan = list("T", tooltip = "Tritan (blue-yellow color blind, ~1 in 10,000 people -- far rarer than deutan/protan)"),
				  # "Show scores" raw-number counterparts of cbf_none/etc above
				  # (badges vs. the underlying numbers) -- same short labels,
				  # since the group header (see plot_table()) makes clear
				  # which block a given "N"/"D"/"P"/"T" belongs to.
				  min_dist_none = list("N", tooltip = "Normal color vision: baseline separability (%), not itself a colorblindness measure"),
				  min_dist_deutan = list("D", tooltip = "Deutan (red-green color blind, ~5% of men, ~0.4% of women): separability (%)"),
				  min_dist_protan = list("P", tooltip = "Protan (also red-green color blind, ~1% of men): separability (%)"),
				  min_dist_tritan = list("T", tooltip = "Tritan (blue-yellow color blind, ~1 in 10,000 people -- far rarer than deutan/protan): separability (%)"),
				  min_dist_overall = list("All", tooltip = "Overall separability (%): worst case across deutan, protan, and tritan"),
				  min_dist_overall_dp = list("All\n(-T)", tooltip = "Overall separability (%) excluding tritan: worst case across deutan and protan only"),
				  chroma = list("Vivid", tooltip = "Are there any vivid (saturated) colors?"),
				  nmax = list("Max number", tooltip = "Maximum number of colors"),
				  fair = list("Fair", tooltip = "Do colors stand out about equally?"),
				  # Short labels below (no leading "Contrast"/"Hue"/"Chroma"/
				  # "Luminance") because plot_table() draws a shared group
				  # header naming the theme once -- repeating it in every
				  # individual column is what was causing multi-line wrap.
				  contrastWT = list("wt", tooltip = "Contrast ratio with white background"),
				  contrastBK = list("bk", tooltip = "Contrast ratio with black background"),
				  equiluminance = list("eq.", tooltip = "If colors are equiluminant (i.e. very low contrast) visual illusions may appear"),
				  CRmin = list("min", tooltip = "Contrast ratio minimum: the closest (least contrasting) pair of colors in the palette"),
				  CRwt = list("wt", tooltip = "Contrast ratio with white background"),
				  CRbk = list("bk", tooltip = "Contrast ratio with black background"),
				  nameable = list("Naming", tooltip = "Are the colors are easy to name? If so, they are also easy to remember (in development)"),
				  float = list("3D Blues", tooltip = "Is there a pure blue color that may cause a 3D illusion?"),
				  hues = list("Pattern", tooltip = "Are hues spread across the spectrum (rainbow), a few hues, or a single hue?"),
				  H = list("Mid", tooltip = "Hue middle"),
				  HL = list("Mid L", tooltip = "Hue middle, left wing"),
				  HR = list("Mid R", tooltip = "Hue middle, right wing"),
				  Hwidth = list("Width", tooltip = "Hue width"),
				  Hspread = list("Spread", tooltip = "Hue spread"),
				  HwidthL = list("Width L", tooltip = "Hue width, left wing"),
				  HwidthR = list("Width R", tooltip = "Hue width, right wing"),
				  Cmax = list("Max", tooltip = "Chroma maximum"),
				  Crange = list("Range", tooltip = "Chroma range"),
				  Lmid = list("Mid", tooltip = "Luminance mid"),
				  Lrange = list("Range", tooltip = "Luminance range"),
				  references = list("References", tooltip = "Click to copy the colors and references"))

		tc = list(cbfriendly = list('NA' = "",
									'0' = "",
									'2' = list("&#9786;&#9786;", extra_css="font-size: 80%;", tooltip = "Extra colorblind-friendly! Also for points and lines", escape = FALSE),
									'1' = list("&#9786;", extra_css="font-size: 80%;", tooltip = "Colorblind-friendly! Be careful with points and lines", escape = FALSE),
									'-1' = list("&#128064;", extra_css ="font-size: 60%;", tooltip = "Be careful! Some colors are hard to distinguish by color blind people (see tab 'Color Blind Friendliness'", escape = FALSE)),
					  # Single-glyph variant for the 4 per-CVD N/D/P/T
					  # columns (cbf_none/_deutan/_protan/_tritan): with 4
					  # independent columns per row, a 2-glyph icon (like
					  # cbfriendly's paired smileys/eyes above) makes a row's
					  # total count jump in twos (0/2/4/6/8), obscuring how
					  # many of the 4 are actually flagged. One glyph per
					  # column keeps that count direct (0-4). "Very
					  # separable" gets a distinct (bigger-smile) glyph
					  # rather than reusing "separable"'s, so the two
					  # remain visually distinguishable even singular.
					  cbf_cvd = list('NA' = "",
									'0' = "",
									'2' = list("&#9786;&#9786;", extra_css="font-size: 80%; letter-spacing: -0.15em;", tooltip = "Very separable! Also for points and lines", escape = FALSE),
									'1' = list("&#9786;", extra_css="font-size: 80%;", tooltip = "Separable. Be careful with points and lines", escape = FALSE),
									'-1' = list("&#128065;", extra_css ="font-size: 60%;", tooltip = "Be careful! Colors are hard to separate for this vision type (see tab 'Color Blind Friendliness')", escape = FALSE)),
				  chroma = list('NA' = "",
				  			  'H' = list("&#x1f576;", tooltip = "Vivid colors (high chroma) present: ideal for small important objects to stand out (e.g. markers on a map), but less suited for space filling visualizations because it may cause eye fatigue (see tab 'HCL Analysis')", escape = FALSE),
				  			  'M' = "",
				  			  'L' = list("&#10057;", tooltip = "All colors are pastel colors (low chroma): ideal for space filling visualizations, such as choropleths (see tab 'HCL Analysis')", escape = FALSE, extra_css = "font-size: 70%;")), #&#9729; &#10020;
				  hues = list(cat = list('NA' = "",
				  					   'RH' = list("&#127752;",
				  					   			tooltip = "Hues from the whole hue spectrum are used (see tab 'HCL Analysis')",
				  					   			escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;")),
				  			 seq = list('NA' = "",
				  						  'MH' = "",
				  						  'RH' = list("&#127752;",
				  						  			tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  						  			escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  						  'SH' = list("&#128396;",
				  						  			tooltip = "Single hue palette: good for quantitative analysis, but harder to distinguish colors",
				  						  			escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   bivg = list('NA' = "",
				  			   			'MH' = "",
				  			   			'RH' = list("&#127752;",
				  			   						tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   						escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   			'SH' = list("&#128396;",
				  			   						tooltip = "Single hue palette: good for quantitative analysis, but harder to distinguish colors",
				  			   						escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   div = list('NA' = "",
				  			   		   'MH' = "",
				  			   		   'RH' = list("&#127752;",
				  			   		   			tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   		   			escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   		   'SH' = list("&#x262F;",
				  			   		   			tooltip = "Each side has its own distinct hue: recommended!",
				  			   		   			escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   bivd = list('NA' = "",
				  			   			'MH' = "",
				  			   			'RH' = list("&#127752;",
				  			   						tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   						escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   			'SH' = list("&#x262F;",
				  			   						tooltip = "Each side has its own distinct hue: recommended!",
				  			   						escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;")),
				  			   bivs = list('NA' = "",
				  			   			'MH' = "",
				  			   			'RH' = list("&#127752;",
				  			   						tooltip = "Spectral (&#34;rainbow&#34;) palette: easy to distinguish colors, but less suitable for quantitative analysis",
				  			   						escape = FALSE, extra_css = "font-size: 150%; vertical-align: -0.1em; line-height: 0px;"),
				  			   			'SH' = list("&#x262F;",
				  			   						tooltip = "Each dimension has its own distinct hue: recommended!",
				  			   						escape = FALSE, extra_css = "font-size: 200%; vertical-align: -0.2em; line-height: 0px;"))
				  ),
				  fair = list(cat = list('NA' = "",
				  					   'M' = "",
				  					   'L' =  list("&#10799;",
				  					   			tooltip = "Unfair: colors are not equally vivid and/or bright. See tab 'HCL Analysis'", escape = FALSE,
				  					   			extra_css = "font-size: 100%; vertical-align: 0.1em; line-height: 0px;"),
				  					   'H' = list("&#9825;",
				  					   		   tooltip = "Fair: colors are equally vivid and bright. See tab 'HCL Analysis'", escape = FALSE,
				  					   		   extra_css = "font-size: 60%; vertical-align: 0em; line-height: 0px;")),
				  			x = list('NA' = "",
				  					 'M' = "",
				  					 'L' =  list("&#10799;",
				  					 			tooltip = "Unfair: colors are not equally vivid. See tab 'HCL Analysis'", escape = FALSE,
				  					 			extra_css = "font-size: 100%; vertical-align: 0.1em; line-height: 0px;"),
				  					 'H' = list("&#9825;",
				  					 		   tooltip = "Fair: colors are equally vivid. See tab 'HCL Analysis'", escape = FALSE,
				  					 		   extra_css = "font-size: 60%; vertical-align: 0em; line-height: 0px;"))),
				  nameable = list('NA' = "",
				  				  'FALSE' =  "",
				  				  'TRUE' = list("&#10023;",
				  				  			  tooltip = "Colors are easy to name, and therefore, easy to remember (in development)",
				  				  			  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px;")),
				  equiluminance = list('NA' = "",
				  				'FALSE' =  "",
				  				'TRUE' = list("&#43612;",
				  							  tooltip = "Very low contrast between some colors (equiluminance); borders needed (see tab 'Contrast')",
				  							  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px;")),
				  contrastWT = list('NA' = "",
				  				  'FALSE' =  list("&#127987;",
				  				  				tooltip = "Good contrast with white for text and lines (see tab 'Contrast')",
				  				  				escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px; margin-right: -10px;"),
				  				  'TRUE' = ""),
				  contrastBK = list('NA' = "",
				  				  'FALSE' =  list("&#127988;",
				  				  				tooltip = "Good contrast with black for text and lines (see tab 'Contrast')",
				  				  				escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px; margin-right: -10px;"),
				  				  'TRUE' = ""),
				  float = list('NA' = "",
				  			 'FALSE' = "",
				  			 'TRUE' = list("&#128313;",
				  			 			  tooltip = "This palette has got the blues; it contains a pure blue color which may cause a floating (3D) effect next to red colors (see tab '3D Blues')",
				  			 			  escape = FALSE, extra_css = "font-size: 130%; vertical-align: -0.1em; line-height: 0px; color: '#000000'"))
		)


		nmax = c(cat = 36, seq = 15, cyc = 15, div = 15, bivs = 7, bivc = 10, bivd = 7, bivg = 7)
		nmin = c(cat = 1, seq = 2, cyc = 3, div = 3, bivs = 2, bivc = 2, bivd = 3, bivg = 2)
		mdef = c(bivc = 5, bivd = 5, bivg = 5)
		matrix_breaks = list(CR = c(1, 1.2, 1.5, 2, 3, 4.5, 7), dist = c(0, 50, 75, 90, 99))
		matrix_pchs = list(CR = c(15, 17, 16, 1, 1, 2, 0), dist = c(15, 17, 16, 16, 1))
		matrix_sizes = list(CR = c(1, 0.6, 0.3, 0, 0.3, 0.6, 1), dist = c(1, 0.6, 0.6, 0.3, 0))
		matrix_interval_labels = list(CR = c("1.0 - 1.2", "1.2 - 1.5", "1.5 - 2.0", "", "3.0 - 4.5", "4.5 - 7.0", "7.0 +"), dist = c("< 50%", "50 - 75%", "75 - 90%", "90 - 99%"))
		matrix_breaks_digits = c(CR = 1, dist = 0)

		# Degrees-of-visual-angle <-> pixel conversion used by visual_angle_to_px()
		# / px_to_visual_angle() (and so the c4a_gui() line/point size previews).
		# "szafir" matches Szafir (2018)'s own in-text px-equivalents (e.g. "0.25
		# deg (6px)"), which are consistently half of the standard chord formula
		# (2 * distance * tan(angle/2)) at their own stated 30in/96dpi assumptions.
		# "standard" is that textbook formula, e.g. for calibrating against a real
		# measured display/viewing distance rather than reproducing the paper.
		angle_convention = "szafir"

		show_ggplot2_div_message = TRUE
	})
	fill_P()
}

.C4A <- new.env(FALSE, parent=globalenv())



fill_P = function() {
	rm(list = ls(envir = .P), envir = .P)
	z = .C4A$z[, c("name", "fullname", "series", "type")]
	if (is.null(z)) return(invisible(NULL))
	x = sort(unique(z$series))
	y1 = structure(lapply(x, function(xi) {
		zi = z[z$series == xi, ]
		structure(as.list(zi$fullname), names = zi$name)
	}), names = x)

	tps = unname(.C4A$types)

	y = structure(lapply(x, function(xi) {
		zi = z[z$series == xi, ]

		tpx = tps[tps %in% unique(zi$type)]

		structure(lapply(tpx, function(ti) {
			zii = zi[zi$type == ti, ]
			structure(as.list(zii$fullname), names = zii$name)
		}), names = tpx)
	}), names = x)

	list2env(y, envir = .P)
}

