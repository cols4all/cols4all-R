# general color tools
library(colorblindcheck)
library(colorspace)

# packages with colors
library(pals)
library(rcartocolor)
library(RColorBrewer)
library(viridisLite)
library(khroma) # library(scico)
library(ggthemes)
library(reticulate) # to get seaborn
library(Polychrome)
library(MetBrewer)
library(rvest)
library(xml2)
library(rcolors)

# required for building scripts
library(volcano3D)
library(fitdistrplus)

sessioninfo::session_info(pkgs = "attached")
# colorblindcheck * 1.0.2   2023-05-13 [1] CRAN (R 4.4.0)
# colorspace      * 2.1-1   2024-07-26 [1] CRAN (R 4.4.0)
# VP cols4all        * 0.7-2   2024-03-12 [?] CRAN (R 4.4.0) (on disk 0.7.1)
# ggthemes        * 5.1.0   2024-02-10 [1] CRAN (R 4.4.0)
# khroma          * 1.14.0  2024-08-26 [1] CRAN (R 4.4.1)
# MetBrewer       * 0.2.0   2022-03-21 [1] CRAN (R 4.4.0)
# pals            * 1.9     2024-07-16 [1] CRAN (R 4.4.0)
# Polychrome      * 1.5.1   2022-05-03 [1] CRAN (R 4.4.0)
# rcartocolor     * 2.1.1   2023-05-13 [1] CRAN (R 4.4.0)
# RColorBrewer    * 1.1-3   2022-04-03 [1] CRAN (R 4.4.0)
# reticulate      * 1.38.0  2024-06-19 [1] CRAN (R 4.4.0)
# shiny           * 1.8.1.1 2024-04-02 [1] CRAN (R 4.4.0)
# treemap         * 2.4-4   2023-05-25 [1] CRAN (R 4.4.0)
# viridisLite     * 0.4.2   2023-05-02 [1] CRAN (R 4.4.0)


#source("build/build_naming_model.R")

c4a_sysdata_remove(are.you.sure = TRUE)

###################################
### package grDevices: cat
###################################
local({
	pals = grDevices:::.palette_colors_hex

	p1 = pals[c("R3", "R4", "ggplot2", "Okabe-Ito")]
	names(p1) = c("R3", "R4", "ggplot2", "okabe")

	c4a_load(c4a_data(p1, types = "cat", series = "misc"))
})

###################################
### package grDevices: seq and div
###################################
local({
	seq = c("Grays", "Light Grays", "Blues 2", "Blues 3", "Purples 2",
			"Purples 3", "Reds 2", "Reds 3", "Greens 2", "Greens 3", "Purple-Blue",
			"Red-Purple", "Red-Blue", "Purple-Orange", "Purple-Yellow", "Blue-Yellow",
			"Green-Yellow", "Red-Yellow", "Heat", "Heat 2", "Dark Mint")

	div = c("Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown",
			"Green-Brown", "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta")

	terrain = c("Terrain", "Terrain 2")

	spals = lapply(seq, function(s) hcl.colors(11, s))
	names(spals) = seq

	dpals = lapply(div, function(s) hcl.colors(11, s))
	names(dpals) = div

	#names(dpals)[1] = "Blue-Red 1" #to prevent conflict with reversed "Red-Blue"

	tpals = lapply(terrain, function(s) hcl.colors(11, s))
	names(tpals) = c("terrain", "terrain2")

	type = c(rep("seq", length(spals)), rep("div", length(dpals)))

	c4a_load(c4a_data(c(spals, dpals), types = type, series = "hcl", space = "Lab"))
	c4a_load(c4a_data_as_is(tpals, types = "seq", series = "hcl", space = "Lab"))
})



###################################
### package RColorBrewer
###################################
local({
	inf = RColorBrewer::brewer.pal.info

	pals = lapply(1:nrow(inf), function(i) {
		if (inf$category[i] != "qual") {
			xs = lapply(3:inf$maxcolors[i], function(j) {
				RColorBrewer::brewer.pal(n = j, name = rownames(inf)[i])
			})
			x = unique(unlist(rev(xs)))

			index = lapply(xs, function(xi) {
				match(xi, x)
			})
			names(index) = 3:inf$maxcolors[i]
			attr(x, "index") = 	index
		} else {
			x = RColorBrewer::brewer.pal(n = inf$maxcolors[i], name = rownames(inf)[i])

		}
		x
	})
	names(pals) = rownames(inf)
	types = ifelse(inf$category == "qual", "cat", inf$category)

	c4a_load(c4a_data(pals, types = types, series = "brewer"))

	divc = list(paired_biv = pals$Paired)
	c4a_load(c4a_data(divc, types = "bivc", series = "brewer", biv.method = "bycol6"))
})





###################################
### Tol: from python script (https://personal.sron.nl/~pault/) 2022-02-10
###################################

local({
	# Method:
	# * copy indices from tol_colors.py
	# * replace [ by list( and ] by )
	# * run:
	#     l2 = lapply(l, function(i) {unlist(i)+1L})
	#     names(l2) = sapply(l2, length)
	#     dput(l2)
	rainbow_ids = list(`1` = 10, `2` = c(10, 26), `3` = c(10, 18, 26), `4` = c(10, 15, 18, 26), `5` = c(10, 14, 15, 18, 26), `6` = c(10, 14, 15, 17, 18, 26), `7` = c(9, 10, 14, 15, 17, 18, 26), `8` = c(9, 10, 14, 15, 17, 18, 23, 26), `9` = c(9, 10, 14, 15, 17, 18, 23, 26, 28), `10` = c(9, 10, 14, 15, 17, 18, 21, 24, 26, 28), `11` = c(9, 10, 12, 14, 15, 17, 18, 21, 24, 26, 28), `12` = c(3, 6, 9, 10, 12, 14, 15, 17, 18, 21, 24, 26), `13` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 21, 24, 26), `14` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26), `15` = c(3, 6, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `16` = c(3, 5, 7, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `17` = c(3, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28), `18` = c(3, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `19` = c(2, 4, 5, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `20` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28), `21` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28), `22` = c(2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29), `23` = c(1, 2, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29))
	# from https://personal.sron.nl/~pault/data/tol_colors.py
	p1 = list(bright = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB'),
		 contrast = c('#004488', '#DDAA33', '#BB5566'),
		 vibrant = c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB'),
		 muted = c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD'),
		 medium = c('#6699CC', '#004488', '#EECC66', '#994455', '#997700','#EE99AA'),
		 light = c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF','#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD'),
		 dark = c("#222255", "#225555", "#225522", "#666633", "#663333", "#555555"
		 ))

	p2 = list(rainbow = structure(c('#E8ECFB', '#D9CCE3', '#D1BBD7', '#CAACCB', '#BA8DB4',
										 '#AE76A3', '#AA6F9E', '#994F88', '#882E72', '#1965B0',
										 '#437DBF', '#5289C7', '#6195CF', '#7BAFDE', '#4EB265',
										 '#90C987', '#CAE0AB', '#F7F056', '#F7CB45', '#F6C141',
										 '#F4A736', '#F1932D', '#EE8026', '#E8601C', '#E65518',
										 '#DC050C', '#A5170E', '#72190E', '#42150A'),
								index = rainbow_ids))


	p3 = list(
		sunset = c('#364B9A', '#4A7BB7', '#6EA6CD', '#98CAE1', '#C2E4EF',
					   '#EAECCC', '#FEDA8B', '#FDB366', '#F67E4B', '#DD3D2D',
					   '#A50026'),
		nightfall = c("#125A56", "#00767B", "#238F9D", "#42A7C6", "#60BCE9",
					  "#9DCCEF", "#C6DBED", "#DEE6E7", "#ECEADA", "#F0E6B2", "#F9D576",
					  "#FFB954", "#FD9A44", "#F57634", "#E94C1F", "#D11807", "#A01813"
		),
		bu_rd = c('#2166AC', '#4393C3', '#92C5DE', '#D1E5F0', '#F7F7F7',
					  '#FDDBC7', '#F4A582', '#D6604D', '#B2182B'),
		pu_gn = c('#762A83', '#9970AB', '#C2A5CF', '#E7D4E8', '#F7F7F7',
					  '#D9F0D3', '#ACD39E', '#5AAE61', '#1B7837'),
		yl_or_br = c('#FFFFE5', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29',
						 '#EC7014', '#CC4C02', '#993404', '#662506'),
		wh_or_br = c('#FFFFFF', '#FFF7BC', '#FEE391', '#FEC44F', '#FB9A29',
						 '#EC7014', '#CC4C02', '#993404', '#662506'),
		iridescent = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF',
						   '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1',
						   '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD',
						   '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388',
						   '#805770', '#684957', '#46353A'),
		incandescent = c("#CEFFFF", "#C6F7D6", "#A2F49B", "#BBE453", "#D5CE04",
		  "#E7B503", "#F19903", "#F6790B", "#F94902", "#E40515", "#A80003"
		),
		rainbow_pu_rd = c('#6F4C9B', '#6059A9', '#5568B8', '#4E79C5', '#4D8AC6',
							  '#4E96BC', '#549EB3', '#59A5A9', '#60AB9E', '#69B190',
							  '#77B77D', '#8CBC68', '#A6BE54', '#BEBC48', '#D1B541',
							  '#DDAA3C', '#E49C39', '#E78C35', '#E67932', '#E4632D',
							  '#DF4828', '#DA2222'),
		rainbow_pu_br = c('#6F4C9B', '#6059A9', '#5568B8', '#4E79C5', '#4D8AC6',
							  '#4E96BC', '#549EB3', '#59A5A9', '#60AB9E', '#69B190',
							  '#77B77D', '#8CBC68', '#A6BE54', '#BEBC48', '#D1B541',
							  '#DDAA3C', '#E49C39', '#E78C35', '#E67932', '#E4632D',
							  '#DF4828', '#DA2222', '#B8221E', '#95211B', '#721E17',
							  '#521A13'),
		rainbow_wh_rd = c('#E8ECFB', '#DDD8EF', '#D1C1E1', '#C3A8D1', '#B58FC2',
							  '#A778B4', '#9B62A7', '#8C4E99', '#6F4C9B', '#6059A9',
							  '#5568B8', '#4E79C5', '#4D8AC6', '#4E96BC', '#549EB3',
							  '#59A5A9', '#60AB9E', '#69B190', '#77B77D', '#8CBC68',
							  '#A6BE54', '#BEBC48', '#D1B541', '#DDAA3C', '#E49C39',
							  '#E78C35', '#E67932', '#E4632D', '#DF4828', '#DA2222'),
		rainbow_wh_br = c('#E8ECFB', '#DDD8EF', '#D1C1E1', '#C3A8D1', '#B58FC2',
							  '#A778B4', '#9B62A7', '#8C4E99', '#6F4C9B', '#6059A9',
							  '#5568B8', '#4E79C5', '#4D8AC6', '#4E96BC', '#549EB3',
							  '#59A5A9', '#60AB9E', '#69B190', '#77B77D', '#8CBC68',
							  '#A6BE54', '#BEBC48', '#D1B541', '#DDAA3C', '#E49C39',
							  '#E78C35', '#E67932', '#E4632D', '#DF4828', '#DA2222',
							  '#B8221E', '#95211B', '#721E17', '#521A13'))

	p3_na = c("#FFFFFF", "#FFEE99", "#FFEE99", "#888888", "#888888", "#999999", "#888888", "#FFFFFF", "#FFFFFF", "#666666", "#666666")

	p3_types = ifelse(names(p3) %in% c("bu_rd", "pu_gn", "sunset", "nightfall"), "div", "seq")

	c4a_load(c4a_data(p1, types = "cat", series = "tol"))
	c4a_load(c4a_data(p2, types = "cat", series = "tol", xNA = "#777777", take.gray.for.NA = FALSE, remove.other.grays = FALSE, remove.blacks = FALSE))
	c4a_load(c4a_data(p3, types = p3_types, xNA = p3_na, series = "tol"))
})


###################################
### Just checking: tol via khroma
###################################

if (FALSE) {
local({
	library(khroma)
	tol_cat = c("bright", "high contrast", "vibrant", "muted", "medium contrast", "pale", "dark", "light")
	tol_cat2 = c("bright", "contrast", "vibrant", "muted", "medium", "pale", "dark", "light")
	tol_cat_n = c(7, 3, 7, 9, 6, 6, 6, 9)

	c1 = mapply(function(m, n) {
		unname(khroma::color(m)(n))
	}, tol_cat, tol_cat_n, SIMPLIFY = FALSE)
	names(c1) = tol_cat2

	p1 = lapply(c1, function(ci) {
		v = unclass(ci)
		attributes(v) = NULL
		v
	})
	na1 = lapply(c1, function(ci) {
		attr(ci, "missing")
	})

	rainbow = lapply(1:23, function(i) {
		x = khroma::color("discreterainbow")(i)
		v = unclass(x)
		attributes(v) = NULL
		v
	})

	rainbow2 = lapply(1:34, function(i) {
		x = khroma::color("smoothrainbow")(i)
		v = unclass(x)
		attributes(v) = NULL
		v
	})

	generate_indices = function(palette_to_n) {
		k = length(palette_to_n)
		ks = sapply(palette_to_n, length)
		pal = unique(unlist(rev(palette_to_n)))
		indices = structure(lapply(1:k, function(i) {
			match(palette_to_n[[i]], pal)
		}), names = as.character(ks))
		structure(pal, index = indices)

	}

	rainbowI = generate_indices(rainbow)
	rainbowI2 = generate_indices(rainbow2)

	div = list(sunset = khroma::colour("sunset")(11),
			   nightfall = khroma::colour("nightfall")(17),
			   bu_rd = khroma::colour("BuRd")(9),
			   pu_gn = khroma::colour("PRGn")(9))

	div_na = sapply(div, function(x) {
		attr(x, "missing")
	})

	div_lst = lapply(div, function(d) {
		v = unclass(d)
		attributes(v) = NULL
		v
	})

	seq = list(yl_or_br = khroma::colour("YlOrBr")(9),
			   iridescent = khroma::colour("iridescent")(23),
			   incandescent = khroma::colour("incandescent")(11)
	)

	seq_na = sapply(seq, function(x) {
		attr(x, "missing")
	})

	seq_lst = lapply(seq, function(d) {
		v = unclass(d)
		attributes(v) = NULL
		v
	})



	c4a_load(c4a_data(p1, types = "cat", series = "tol2", xNA = unlist(na1)))
	c4a_load(c4a_data(list(rainbow = rainbowI, rainbow_smooth = rainbowI2), types = c("cat", "seq"), series = "tol2", xNA = c("#777777", "#666666")))
	c4a_load(c4a_data(seq_lst, types = "seq", series = "tol2", xNA = seq_na))
	c4a_load(c4a_data(div_lst, types = "div", series = "tol2", xNA = div_na))
})
}

###################################
### matplotlib: package viridisLite
###################################
# in the original cols4all package <0.8
# now in seaborn and matplotlib
if (FALSE) {
	local({
		nms = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
		types = ifelse(nms == "cividis", "div", "seq")

		pals = lapply(nms, function(nm) {
			viridisLite::viridis(11, option = nm)
		})
		names(pals) = nms

		c4a_load(c4a_data(pals, types = types, series = "matplotlib"))
	})
}

###################################
### matplotlib: other palettes
###################################

local({
	library(reticulate)

	mpl = import("matplotlib")

	v = c("magma", "inferno", "plasma", "viridis", "cividis")

	sq1 = c('Greys', 'Purples', 'Blues', 'Greens', 'Oranges', 'Reds',
	'YlOrBr', 'YlOrRd', 'OrRd', 'PuRd', 'RdPu', 'BuPu',
	'GnBu', 'PuBu', 'YlGnBu', 'PuBuGn', 'BuGn', 'YlGn')

	sq2 = c('gray', 'bone',
		'pink', 'spring', 'summer', 'autumn', 'winter', 'cool',
		'Wistia', 'hot', 'afmhot', 'gist_heat', 'copper')
	# 'gist_gray' same as gray:
	#   b = mpl$colormaps$get_cmap("gist_gray")(seq(0,1,length.out = 5))
	#   a = mpl$colormaps$get_cmap("gray")(seq(0,1,length.out = 5))
	#   a-b
	# 'gist_yarg' just a reverse
	# 'binary' also identical

	# (almost?) identical to brewer
	dv = c('PiYG', 'PRGn', 'BrBG', 'PuOr', 'RdGy', 'RdBu', 'RdYlBu',
		   'RdYlGn', 'Spectral', 'coolwarm', 'bwr', 'seismic')

	cyc = c("twilight", "twilight_shifted", "hsv")

	# qualitative are equal to Brewer and Tableau (?)
	misc = c('ocean', 'gist_earth', 'terrain',
	'gist_stern', 'gnuplot', 'gnuplot2', 'CMRmap',
	'cubehelix', 'brg', 'gist_rainbow', 'rainbow', 'jet',
	'turbo', 'nipy_spectral', 'gist_ncar')
	# 'flag', 'prism' left out

	get_pals = function(nms) {
		pals = lapply(nms, function(nm) {
			x = mpl$colormaps$get_cmap(nm)(seq(0,1,length.out = 7))
			rgb(x[,1], x[,2], x[,3])
		})
		names(pals) = nms
		pals
	}

	pals_v = get_pals(v)
	pals_sq1 = get_pals(sq1)
	pals_sq2 = get_pals(sq2)
	pals_dv = get_pals(dv)
	pals_cyc = get_pals(cyc)
	pals_misc = get_pals(misc)

	c4a_load(c4a_data(pals_v, types = "seq", series = "matplotlib"))
	c4a_load(c4a_data(pals_sq1, types = "seq", series = "matplotlib"))
	c4a_load(c4a_data(pals_sq2, types = "seq", series = "matplotlib"))
	c4a_load(c4a_data(pals_dv, types = "div", series = "matplotlib"))
	c4a_load(c4a_data(pals_cyc, types = "cyc", series = "matplotlib"))
	c4a_load(c4a_data(pals_misc, types = "seq", series = "matplotlib"))
})





###################################
### package pals
###################################

local({
	syspals = pals:::syspals
	palsCat = "watlington"
	palsNew = "watlington"
	series = "misc"

	pals = syspals[palsCat]
	names(pals) = palsNew

	pals4 = syspals[substr(names(syspals), 1, 6) == "kovesi"] # & substr(names(syspals), 1, 13) != "kovesi.cyclic"]

	isdiv = substr(names(pals4), 1, 16) == "kovesi.diverging"
	iscyc = substr(names(pals4), 1, 13) == "kovesi.cyclic"

	pals4_type = ifelse(isdiv, "div", ifelse(iscyc, "cyc", "seq"))

	names(pals4) = substr(names(pals4), 8, nchar(names(pals4)))

	orig = c("linear_grey_10_95_c0",
			 "rainbow_bgyr_35_85_c72",
			 "rainbow_bgyrm_35_85_c69",
			 "linear_ternary_blue_0_44_c57",
			 "linear_ternary_green_0_46_c42",
			 "linear_ternary_red_0_50_c52",
			 "linear_kry_5_95_c72",
			 "linear_kryw_5_100_c64",
			 "linear_green_5_95_c69",
			 "linear_bmy_10_95_c71",
			 "linear_bmw_5_95_c86",
			 "linear_blue_95_50_c20",
			 "linear_blue_5_95_c73",
			 "linear_bgyw_15_100_c67",
			 "linear_bgy_10_95_c74",
			 "isoluminant_cgo_70_c39")

	new = c("grey",
			"rainbow_bu_gn_yl_rd",
			"rainbow_bu_gn_yl_rd_mg",
			"ternary_blue",
			"ternary_green",
			"ternary_red",
			"bk_rd_yl",
			"bk_rd_wh",
			"green",
			"bu_yl_mg",
			"bu_wh_mg",
			"blue",
			"blue_cyan",
			"bu_gn_yl",
			"bu_gn_yl_wh",
			"cy_or")

	orig_div = c("diverging_gwv_55_95_c39",
				 "diverging_bky_60_10_c30",
				 "diverging_bwr_40_95_c42",
				 "diverging_bwr_55_98_c37",
				 "diverging_linear_bjy_30_90_c45",
				 "diverging_bkr_55_10_c35",
				 "diverging_linear_bjr_30_55_c53",
				 "diverging_isoluminant_cjo_70_c25",
				 "diverging_rainbow_bgymr_45_85_c67",
				 "diverging_cwm_80_100_c22",
				 "diverging_isoluminant_cjm_75_c24",
				 "diverging_gwr_55_95_c38",
				 "diverging_gkr_60_10_c40")


	new_div = c("gn_wh_pu",
				"bu_bk_br",
				"bu_wh_rd",
				"bu_wh_rd2",
				"bu_gy_yl",
				"bu_bk_rd",
				"bu_gy_rd",
				"cy_gy_or",
				"rainbow",
				"cy_wh_mg",
				"cy_gy_mg",
				"gn_wh_rd",
				"gn_bk_rd")


	orig_cyc = c("cyclic_grey_15_85_c0", "cyclic_grey_15_85_c0_s25", "cyclic_mrybm_35_75_c68",
				 "cyclic_mrybm_35_75_c68_s25", "cyclic_mygbm_30_95_c78", "cyclic_mygbm_30_95_c78_s25",
				 "cyclic_wrwbw_40_90_c42", "cyclic_wrwbw_40_90_c42_s25")

	new_cyc = c("cyclic_grey", "cyclic_grey2", "cyclic_mg_rd_yl_bu_mg",
				"cyclic_bu_mg_rd_yl_bu", "cyclic_mg_yl_gn_bu_mg", "cyclic_bu_mg_yl_gn_bu",
				"cyclic_wh_rd_wh_bu_wh", "cyclic_bu_wh_rd_wh_bu")

	ids = match(orig_div, names(pals4))
	pals5 = pals4[ids]
	pals5_type = pals4_type[ids]
	names(pals5) = new_div


	#misc.watlington
	c4a_load(c4a_data(pals, types = "cat", series = series))

	# kovesi
	names(pals4)[match(orig, names(pals4))] = new
	names(pals4)[match(orig_div, names(pals4))] = new_div
	names(pals4)[match(orig_cyc, names(pals4))] = new_cyc

	# c4a_data_as_is to prevent black and whites to be removed
	c4a_load(c4a_data_as_is(pals4, types = pals4_type, series = "kovesi", format.palette.name = FALSE))

})



###################################
### package wesanderson
###################################


local({
	pals = wesanderson::wes_palettes

	names(pals)[c(18,19)] = c("isle_of_dogs1", "isle_of_dogs2")

	type = ifelse(names(pals) == "Zissou1", "div", "cat")

	c4a_load(c4a_data(pals, types = type, series = "wes"))

})



###################################
### package rcartocolor
###################################

local({
	cartoQual = cartocolors[cartocolors$Type == "qualitative",]

	# seems indexed, but only the 12th color (a gray) is added.
	# -> use that 12th as colNA

	pals = lapply(cartoQual$n12, function(p) p[1:11])
	colNAs = sapply(cartoQual$n12, function(p) p[12])

	names(pals) = cartoQual$Name

	cartoNum = cartocolors[cartocolors$Type %in% c("quantitative", "diverging"), c("Name", "Type", "n7")]
	pals2 = cartoNum$n7
	names(pals2) = cartoNum$Name
	type = ifelse(cartoNum$Type == "diverging", "div", "seq")

	#pals2rev = lapply(pals2, rev) # trick to reverse names
	#pals2rev["SunsetDark"] = rev(pals2rev["SunsetDark"]) # another reverse to undo

	cartoAgg = cartocolors[cartocolors$Type %in% c("aggregation"), c("Name", "Type", "n7")]
	pals3 = cartoAgg$n7
	names(pals3) = tolower(cartoAgg$Name)
	names(pals3)[2] = "ag_grn_yl"

	c4a_load(c4a_data(pals, xNA = colNAs, types = "cat", series = "carto"))
	c4a_load(c4a_data(pals2, types = type, series = "carto"))
	c4a_load(c4a_data(pals3, types = "seq", series = "carto", format.palette.name = FALSE))

})




###################################
### package colorspace
###################################

local({
	hclnames = c("Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold", "Harmonic", "Dynamic")
	pals = structure(lapply(hclnames, function(h) {
		pals = lapply(1:36, function(i) {
			qualitative_hcl(palette = h, n = i)
		})
		pal = unique(unlist(rev(pals)))
		indices = structure(lapply(1:36, function(i) {
			match(pals[[i]], pal)
		}), names = as.character(1:36))
		structure(pal, index = indices)
	}), names = hclnames)

	c4a_load(c4a_data(pals, types = "cat", series = "hcl"))

	hcl_cyc = c("pastel1", "dark2", "dark3", "set2", "set3", "dynamic")
	pals_cyc = structure(lapply(hcl_cyc, function(h){
		pal = c4a(paste0("hcl.", h))
		pal = c(pal, pal[1])
	}), names = paste0(hcl_cyc, "_cyc"))
	c4a_load(c4a_data(pals_cyc, types = "cyc", series = "hcl"))
})

###################################
### package scico
###################################

local({
	ids = seq(1,256, length.out=16)
	d = scico:::palettes

	d$.categorical = NULL

	div = c("broc","cork", "vik", "lisbon", "tofino", "berlin", "bam", "roma", "vanimo", "managua")
	mseq = c("oleron", "bukavu", "fes")
	cyc = c( "brocO", "corkO",  "vikO", "romaO", "bamO")

	sq = setdiff(names(d), c(div, mseq, cyc))

	pals = mapply(function(x, nm) {
		if (nm %in% mseq) {
			c(rampPal(rgb(x$r[128:1], x$g[128:1], x$b[128:1], maxColorValue = 1), 7),
			rampPal(rgb(x$r[256:129], x$g[256:129], x$b[256:129], maxColorValue = 1), 7))
		} else if (nm %in% cyc) {
			rampPal(rgb(c(x$r, x$r[1]), c(x$g, x$g[1]), c(x$b, x$b[1]), maxColorValue = 1), 15)
		} else {
			rampPal(rgb(x$r, x$g, x$b, maxColorValue = 1), 15)
		}
	}, d, names(d))

	pals_cat = lapply(scico:::palettes$.categorical, head, 10)
	names(pals_cat) = paste0(names(pals_cat), "_cat")

	pals_div = pals[div]
	pals_seq = pals[sq]
	pals_biv = pals[mseq]
	pals_biv = lapply(pals_biv, function(p) {
		matrix(p[c(1:7,8:14)], ncol = 2)
	})
	pals_biv[["fes"]] = pals_biv[["fes"]][,2:1]
	pals_cyc = pals[cyc]

	#names(pals_seq)[match(c("batlowK", "batlowW", "grayC"), names(pals_seq))] = c("k_batlow", "w_batlow", "c_gray") # reverse names (because palettes will be reversed)

	c4a_load(c4a_data_as_is(pals_cat, types = "cat", series = "scico"), overwrite = T)
	c4a_load(c4a_data(pals_div, types = "div", series = "scico"))
	c4a_load(c4a_data(pals_seq, types = "seq", series = "scico"))
	c4a_load(c4a_data(pals_biv, types = c("bivc", "bivc", "bivg"), series = "scico", biv.method = "bycol2"))
	c4a_load(c4a_data(pals_cyc, types = "cyc", series = "scico"))
})

###################################
### package crameri
###################################
# local({
# 	inf = khroma::info()[1:35, ]
#
# 	scms = khroma:::.schemes
#
# 	names = names(scms)
# 	types = unname(sapply(scms, "[[", "type"))
# 	pals = unname(lapply(scms, "[[", "colours"))
# 	nas = sapply(scms, "[[", missing)
# 	schemes = unname(lapply(scms, "[[", "scheme"))
#
# 	df = data.table::rbindlist(scms)
#
#
# 	cols = mapply(function(name, mx) {
# 		khroma:::.schemes[[name]] r(name, mx)
# 	}, inf$palette, inf$max, SIMPLIFY = FALSE, USE.NAMES = FALSE)
# })

###################################
### package ggthemes (tableau)
###################################

local({
	palettes = ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
	tab_cat = lapply(palettes, function(pal) {
		pal$value
	})
	names(tab_cat)[1:2] = c("10", "20")


	palettes2 = ggthemes_data[["tableau"]][["color-palettes"]][["ordered-sequential"]]
	tab_seq = lapply(palettes2, function(pal) {
		pal$value
	})

	# 7th color is off (magenta instead of gray) do to typo:-) #colorRampPalette(tab_seq[["Gray Warm"]][c(6,8)], space = "rgb")(3)
	tab_seq[["Gray Warm"]][7] = "#b0a7a4"
	palettes3 = ggthemes_data[["tableau"]][["color-palettes"]][["ordered-diverging"]]
	tab_div = lapply(palettes3, function(pal) {
		pal$value
	})

	c4a_load(c4a_data(tab_cat, types = "cat", series = "tableau"))
	c4a_load(c4a_data(tab_seq, types = "seq", series = "tableau"))
	c4a_load(c4a_data(tab_div, types = "div", series = "tableau"))

	divc = list(winter_biv = tab_cat$Winter)
	c4a_load(c4a_data(divc, types = "bivc", series = "tableau", biv.method = "bycol5"))

	divc2 = list('20_biv' = tab_cat$'20',
				classic20_biv = tab_cat$'Classic 20')
	c4a_load(c4a_data(divc2, types = "bivc", series = "tableau", biv.method = "bycol10"))

})

###################################
### seaborn
###################################


local({
	library(reticulate)

	sns = import("seaborn")
	mpc = import("matplotlib.colors")

	sb_cat_names = c("deep", "muted", "pastel", "bright", "dark", "colorblind")
	sb_cat = lapply(sb_cat_names, sns$color_palette, as_cmap = TRUE)
	names(sb_cat) = sb_cat_names

	sb_seq_names = c("rocket", "mako", "flare", "crest")
	sb_seq = lapply(sb_seq_names, function(nm) {
		pal = sns$color_palette(nm)
		m = as.data.frame(t(sapply(1:(length(pal)), function(i) pal[[i-1]])))
		names(m) = c("red", "green", "blue")
		do.call(rgb, as.list(m))
	})
	names(sb_seq) = sb_seq_names

	sb_div_names = c("vlag", "icefire")
	sb_div = lapply(sb_div_names, function(nm) {
		pal = sns$color_palette(nm)
		m = as.data.frame(t(sapply(1:(length(pal)), function(i) pal[[i-1]])))
		names(m) = c("red", "green", "blue")
		do.call(rgb, as.list(m))
	})
	names(sb_div) = sb_div_names

	c4a_load(c4a_data(sb_cat, types = "cat", series = "seaborn"))
	c4a_load(c4a_data(sb_seq, types = "seq", series = "seaborn"))
	c4a_load(c4a_data(sb_div, types = "div", series = "seaborn"))
})

local({
	p = list(kelly = Polychrome::kelly.colors(n = 22),
			 glasbey = Polychrome::glasbey.colors(n = 32),
			 alphabet2 = Polychrome::green.armytage.colors(n = 26),
			 palette36 = Polychrome::palette36.colors(n = 36),
			 alphabet = Polychrome::alphabet.colors(n = 26),
			 light24 = Polychrome::light.colors(n = 24),
			 dark24 = Polychrome::dark.colors(n = 24),
			 sky24 = Polychrome::sky.colors(n = 24),
			 wright25 = pals::cols25())
	c4a_load(c4a_data(p, types = "cat", series = "poly", remove.blacks = FALSE, take.gray.for.NA = FALSE, remove.other.grays = FALSE))
})






## meta package: 2000 palettes,
# library(paletteer)
# str(palettes_d)

# library(paletteer)
# cat_paletteer = local({
#
# 	cat_paletteer = do.call(c, palettes_d)
# })
# paletteer::palettes_c_names

## See also:

# https://github.com/EmilHvitfeldt/r-color-palettes



########################################################################################
######################################## BIVARIATE #####################################
########################################################################################
#c4a_palettes_remove(series = "c4a")
local({
	bu2 = c4a("-hcl.blues3", n = 5, range = c(0.3, 0.8))
	yl_rd = c4a("-hcl.red_yellow", n = 5, range = c(0.3, 0.8))
	pg = hcl.colors(11, "Purple-Green")
	bu = hcl.colors(9, "Blues 3")[7:3]
	gn = hcl.colors(9, "Greens 3")[7:3]
	pu = hcl.colors(9, "Purples 3")[7:3]

	# extract average c and l values for both wings
	mat = get_hcl_matrix(pg)
	cs = (mat[1:6, 2] + mat[11:6, 2]) / 2
	cs[6] = 0
	cs[5] = 25
	ls = (mat[1:6, 3] + mat[11:6, 3]) / 2

	ls2 = ls
	ls2[6] = 92


	# candidate hues
	hs = seq(0, 359, by = 0.2)

	# find hues for which colors have most chroma (the higher the better to distinguisch with grey)
	res = lapply(1:5, function(i) {
		sapply(hs, function(h) hcl(h=h,c=cs[i], l =ls[i]))
	})
	get_chroma = function(x) attr(as(colorspace::hex2RGB(x), "polarLAB"), "coords")[, "C"]
	max_chroma_cvd = function(x) {
		cr1 = deutan(x) |> get_chroma()
		cr2 = protan(x) |> get_chroma()
		cr3 = tritan(x) |> get_chroma()
		y = pmin(cr1, cr2, cr3)
		y = y / max(y)
	}
	res2 = lapply(res, max_chroma_cvd)
	res3 = rowSums(do.call(cbind, res2))
	plot(res3, pch=16)

	hue_br = hs[1:500][which.max(res3[1:500])]
	hue_gn = hs[500:800][which.max(res3[500:800])]
	hue_bu = hs[1000:1300][which.max(res3[1000:1300])]
	hue_pu = hs[1300:1800][which.max(res3[1300:1800])]

	bu_br_div = c(hcl(hue_bu, c = cs, l = ls2),
				  rev(hcl(hue_br, c = cs, l = ls2))[-1])

	pu_gn_div = c(hcl(hue_pu, c = cs, l = ls2),
				  rev(hcl(hue_gn, c = cs, l = ls2))[-1])

	bu_br_biv = c(hcl(hue_bu, c = cs, l = ls),
				  rev(hcl(hue_br, c = cs, l = ls))[-1])

	pu_gn_biv = c(hcl(hue_pu, c = cs, l = ls),
				  rev(hcl(hue_gn, c = cs, l = ls))[-1])
	# pu_gn_div |> specplot()
	# pg |> specplot()
	#pu_gn_div = pg # very similar but still a bit better

	pals_div = list(bu_br_div = bu_br_div, pu_gn_div = pu_gn_div)
	pals_bivs = list(bu_br_bivs = bu_br_biv[3:9], pu_gn_bivs = pu_gn_biv[3:9])
	pals_bivd = list(bu_br_bivd = bu_br_biv[2:10], pu_gn_bivd = pu_gn_biv[2:10])
	pals_bivg = list(bu_bivg = bu2, yl_rd_bivg = yl_rd, br_bivg = bu_br_biv[8:10], pu_bivg = pu_gn_biv[4:2], gn_bivg = pu_gn_biv[8:10])


	c4a_load(c4a_data(pals_div, types = "div", series = "cols4all", space = "rgb"))
	c4a_load(c4a_data(pals_bivs, types = "bivs", series = "cols4all", biv.method = "div2seqseq", space = "rgb"))
	c4a_load(c4a_data(pals_bivd, types = "bivd", series = "cols4all", biv.method = "div2catseq", space = "rgb"))
	c4a_load(c4a_data(pals_bivg, types = "bivg", series = "cols4all", biv.method = "seq2uncseq", space = "rgb"))

	pals2 = list(pinkgreen = pals::stevens.pinkgreen(n = 9),
				 bluered = pals::stevens.bluered(n = 9),
				 pinkblue = pals::stevens.pinkblue(n = 9),
				 greenblue = pals::stevens.greenblue(n = 9),
				 purplegold = pals::stevens.purplegold(n = 9))



	c4a_load(c4a_data(pals2, types = "bivs", series = "stevens", biv.method = "byrow"))

	pals3 = list(divseq = brewer.divseq(n = 9),
				 qualseq = brewer.qualseq(n = 9),
				 seqseq1 = brewer.seqseq1(n = 9),
				 seqseq2 = brewer.seqseq2(n = 9))
	c4a_load(c4a_data(pals3, types = c("bivd", "bivc", "bivs", "bivs"), series = "brewer", biv.method = "byrow"))


	pals4 = list(stepped = do.call(c, lapply(1:6, function(i) pals::stepped()[(i*4):(i*4-3)])),
				 stepped2 = do.call(c, lapply(1:5, function(i) pals::stepped2()[(i*4):(i*4-3)])),
				 stepped3 = do.call(c, lapply(1:5, function(i) pals::stepped3()[(i*4):(i*4-3)])))
	c4a_load(c4a_data(pals4[1], types = "bivc", series = "misc", biv.method = "bycol6"))
	c4a_load(c4a_data(pals4[2:3], types = "bivc", series = "misc", biv.method = "bycol5"))
})


local({
	pals = structure(lapply(MetBrewer::MetPalettes, "[[", 1), names = names(MetBrewer::MetPalettes))
	cbf = which(sapply(MetBrewer::MetPalettes, "[[", 3))
	seq = c("Greek", "Hokusai1", "Hokusai2", "Hokusai3", "Manet", "OKeeffe2", "Tam", "VanGogh3")
	div = c("Benedictus", "Cassatt1", "Cassatt2", "Demuth", "Hiroshige", "Homer1", "Homer2", "Ingres", "Isfahan1", "Johnson", "Morgenstern", "OKeeffe1", "Paquin", "Troy")

	bivc = "Monet"

	pals$Monet = pals$Monet[c(3:1, 4:6, 9:7)]

	types = ifelse(names(pals) %in% seq, "seq", ifelse(names(pals) %in% div, "div", ifelse(names(pals) %in% bivc, "bivc", "cat")))

	#pals_cbf = pals[cbf]
	#types_cbf = types[cbf]

	c4a_load(c4a_data(pals, types = types, series = "met", biv.method = "bycol3"))
})

local({
	library(NatParksPalettes)

	seq = c("Arches2", "CapitolReef", "Denali", "Glacier", "WindCave")
	div = c("Acadia", "Arches", "Olympic")


	pals = structure(lapply(NatParksPalettes::NatParksPalettes, "[[", 1), names = names(NatParksPalettes::NatParksPalettes))

	types = ifelse(names(pals) %in% seq, "seq", ifelse(names(pals) %in% div, "div", "cat"))

	c4a_load(c4a_data(pals, types = types, series = "parks"))

})



#### c4a cat
local({
	cols4all = list(area7 = c("#FF9D9A", "#77AADD", "#F1CE63", "#2CA02C", "#B07AA1", "#9EDAE5", "#CC6677"),
					area8 = c("#CC6677", "#AEC7E8", "#44BB99", "#B07AA1", "#BBCC33", "#FFAABB", "#B6992D", "#98DF8A"),
					area9 = c("#EE8866", "#88CCEE", "#2CA02C", "#B07AA1", "#F1CE63", "#FFAABB", "#6699CC", "#44BB99", "#CC6677"),
					area7d = c("#72190E", "#332288", "#225555", "#997700", "#437DBF", "#994F88", "#666633"),
					area8d = c("#663333", "#1F77B4", "#225555", "#994F88", "#997700", "#332288", "#666633", "#661100"),
					area9d = c("#72190E", "#1965B0", "#225555", "#994F88", "#997700", "#332288", "#666633", "#663333", "#437DBF"),
					line7 = c("#1F77B4", "#2CA02C", "#E73F74", "#6699CC", "#994F88", "#117733", "#D37295"),
					line8 = c("#DC050C", "#1F77B4", "#117733", "#994F88", "#999933", "#D37295", "#6699CC", "#E73F74"),
					line9 = c("#EE3377", "#1F77B4", "#117733", "#CF1C90", "#999933", "#994455", "#6699CC", "#D37295", "#DC050C"),
					friendly5 = c("#CC6677", "#F1CE63", "#117733", "#99DDFF", "#9467BD"),
					friendly7 = c("#E65518", "#F2B701",  "#009988", "#88CCEE", "#9467BD","#225522", "#882255"),
					friendly9 = c("#E73F74", "#F1CE63", "#99DDFF", "#9467BD", "#009988", "#882255", "#225522", "#4B4B8F", "#999933"),
					friendly11 = c("#E73F74", "#F1CE63", "#77AADD", "#9467BD", "#AAAA00", "#FF9D9A", "#99DDFF", "#B07AA1", "#225522", "#882255", "#4B4B8F"),
					friendly13 = c("#E73F74", "#F1CE63", "#77AADD", "#009988", "#9467BD", "#FF9D9A", "#99DDFF", "#AAAA00", "#225522", "#882255", "#997700", "#4B4B8F", "#8C564B"))
	c4a_load(c4a_data(cols4all, types = "cat", series = "cols4all"))
})

local({


	# scrape (date website 2024-07-12, scraped on 2024-09-25)
	url <-"https://learn.microsoft.com/en-us/power-bi/create-reports/desktop-report-themes"
	url2 = "https://learn.microsoft.com/en-us/power-bi/create-reports"

	webpage <- session(url)
	link.titles <- webpage %>% html_nodes("img")
	img.url <- link.titles[7:30] %>% html_attr("src")

	fn = basename(img.url)

	td = "build/bowerbi"
	unlink(td, force = TRUE, recursive = TRUE)
	dir.create(td)

	fls = mapply(function(a, b) {
		b2 = file.path(td, b)
		download.file(paste(url2, a, sep = "/"), destfile = b2)
		b2
	}, img.url, fn)

	pals = lapply(fls, function(f) {
		p = png::readPNG(f)
		rgb_codes = p[12, seq(15,230, length.out = 8), 1:3]
		do.call(rgb, unname(as.list(as.data.frame(rgb_codes))))
	})

	# manual
	nms = fn
	nms[1:19] = substr(nms[1:19], 28, nchar(nms[1:19]) - 4)
	nms[20:24] = paste0("accessible_", substr(nms[20:24], 18, nchar(nms[20:24]) - 4))
	nms = sub("-", "_", nms, fixed = TRUE)

	names(pals) = nms

	ts = c(rep("cat", 6), rep("div", 4), "seq", rep("cat", 13))

	c4a_load(c4a_data(pals, types = ts, series = "powerbi"), overwrite = T)

})

## Stevens seq/div palettes
local({
	stevens_seq = list(ocean_moana = c('#003d50','#003e51','#003f52','#003f53','#003f53','#014054','#014256','#014256','#014357','#014458','#01455a','#01445a','#02455b','#02465c','#02475d','#02485d','#02485f','#034a60','#034a60','#034b61','#034b62','#034c64','#034c64','#044e65','#044f66','#045068','#045068','#055169','#05526a','#05526b','#05536b','#06536d','#06556e','#06566f','#06566f','#075770','#075872','#075873','#075973','#085a74','#085a75','#095b77','#095b77','#095d78','#095d79','#0a5e7b','#0a5f7b','#0a607c','#0b617d','#0b627e','#0b627f','#0c6380','#0c6481','#0c6482','#0d6683','#0d6683','#0d6685','#0e6786','#0e6887','#0e6987','#0f6a89','#0f6b8a','#0f6b8b','#106c8c','#106d8c','#116e8d','#126e8e','#14708e','#15708e','#16718f','#16718f','#18728f','#19738f','#1a7490','#1b7690','#1c7790','#1d7790','#1d7791','#1e7891','#1f7991','#207a92','#217c92','#227c92','#227d92','#237e93','#237f93','#248093','#258094','#268294','#278394','#278395','#288495','#288495','#298595','#2a8796','#2b8896','#2b8896','#2c8a97','#2c8a97','#2d8a97','#2d8c97','#2e8c98','#2f8e98','#2f8e98','#309099','#319199','#319299','#319299','#32939a','#33939a','#33949a','#34959b','#35969c','#36979c','#37989c','#37989d','#389a9e','#399a9e','#3a9c9f','#3b9c9f','#3b9da0','#3c9da0','#3d9ea1','#3d9fa1','#3ea0a2','#3fa2a3','#40a3a3','#40a3a4','#41a4a4','#42a5a5','#43a5a5','#43a6a6','#44a8a7','#45a8a7','#46a9a7','#46a9a8','#47aaa8','#48aca9','#48acaa','#49aeaa','#4aafab','#4bafab','#4bb0ac','#4cb0ac','#4db2ad','#4db2ad','#4eb4ae','#4fb5af','#50b5af','#50b6b0','#51b7b0','#51b8b1','#52b8b1','#53b9b2','#54bbb3','#54bcb3','#55bdb4','#56bdb4','#56beb5','#57beb5','#58c0b6','#5ac0b6','#5dc1b7','#5ec2b7','#63c2b7','#65c3b8','#67c4b8','#6ac4b9','#6cc5b9','#6ec5b9','#72c7ba','#74c7ba','#76c7ba','#79c8bb','#7bc9bb','#7dc9bb','#80cbbc','#81cbbc','#84ccbd','#87ccbd','#87cdbd','#8acdbe','#8dcebe','#8ecebe','#91cfbf','#93d0bf','#94d0c0','#98d2c0','#99d2c0','#9bd3c1','#9ed4c1','#9fd4c1','#a1d5c2','#a4d5c2','#a4d6c3','#a7d6c3','#a9d7c4','#aad8c4','#add8c5','#afd9c6','#afd9c6','#b1dac8','#b3dbc9','#b4dcc9','#b5ddcb','#b7decc','#b8decc','#badfce','#bce0cf','#bde0d0','#bee0d1','#c0e2d2','#c1e2d3','#c2e3d4','#c4e3d5','#c6e4d6','#c6e5d7','#c8e6d8','#cae6da','#cae7da','#cce7db','#cee9dd','#cfe9dd','#d0eade','#d2ebe0','#d3ebe0','#d4ece1','#d6ece3','#d8ede4','#d9eee4','#dbeee6','#dcefe7','#ddf0e8','#dff1e9','#e1f2ea','#e1f2eb','#e3f3ec','#e5f4ed','#e6f4ee','#e7f5ef','#e9f6f0','#eaf6f1','#ebf7f2','#edf8f3','#eff8f5','#f0f9f5','#f2faf6','#f3faf8','#f4fbf8','#f6fcfa','#f8fcfb','#f9fdfc','#fafdfd','#fcfffe','#fdffff'),
		 florida_keys = c('#060910','#070a12','#080b14','#080d15','#090e17','#0a0f18','#0b101a','#0b111b','#0c121d','#0c131e','#0d1420','#0d1422','#0d1524','#0e1625','#0e1725','#0f1726','#101927','#101927','#111928','#131b29','#141b29','#141b2a','#151c2a','#161d2b','#171e2b','#181f2c','#191f2d','#1a202e','#1a212e','#1b222f','#1c222f','#1e2330','#1e2430','#1f2531','#202532','#212632','#212633','#222834','#232834','#242935','#252935','#262b36','#272c37','#282c38','#292d38','#292d38','#2a2f39','#2c303a','#2c303b','#2d303b','#2e313c','#2f323c','#30333d','#31343e','#31343e','#32353f','#333740','#353740','#36383f','#38383e','#3b393e','#3b3a3d','#3e3b3c','#403b3b','#403c3b','#423c3a','#453d39','#453e39','#473f38','#494037','#4a4036','#4c4036','#4e4235','#504234','#514333','#524433','#544431','#554531','#564530','#58472f','#5a472e','#5a482e','#5c492d','#5e4a2b','#604a2b','#604b2a','#624c29','#644d28','#654d27','#684d27','#684e28','#6b4e28','#6e4f28','#704f29','#724f29','#744f29','#765029','#79502a','#7b502a','#7c502a','#7e512b','#81512b','#83522b','#85522c','#87522c','#8a522d','#8c532d','#8e532d','#90532d','#93532e','#95532e','#96542e','#98542f','#9a542f','#9d542f','#a05430','#a15430','#a35530','#a55531','#a85531','#a95631','#ab5730','#ad5730','#ae5830','#af5930','#b05a2f','#b25a2f','#b45b2e','#b45c2e','#b65d2e','#b75e2d','#b85e2d','#b95f2d','#bb5f2c','#bd602c','#be612b','#c0622b','#c1632b','#c2642a','#c4642a','#c56529','#c66629','#c76729','#c96828','#ca6827','#cb6927','#cd6a26','#cf6b26','#d06b25','#d26c25','#d26d24','#d46e24','#d66e23','#d77022','#d97022','#d97121','#db7221','#dd7320','#de731f','#e0741f','#e1751e','#e2751d','#e4761c','#e5771b','#e6781b','#e8791a','#e97a19','#eb7a18','#ed7b16','#ee7c15','#ef7d14','#f17e13','#f27e12','#f47f10','#f5800e','#f6810d','#f8820c','#f9830a','#fb8307','#fc8505','#fd8503','#ff8602','#ff8701','#ff8a02','#ff8c02','#ff8c02','#ff8f03','#ff9103','#ff9303','#ff9404','#ff9604','#ff9805','#ff9805','#ff9a05','#ff9d06','#ff9e06','#ffa007','#ffa107','#ffa208','#ffa408','#ffa609','#ffa709','#ffa90a','#ffab0a','#ffad0b','#ffad0b','#ffaf0c','#ffb00c','#ffb30d','#ffb40d','#ffb60e','#ffb70f','#ffb80f','#ffba10','#ffbc10','#ffbd11','#ffbe11','#ffc112','#ffc212','#ffc413','#ffc513','#ffc714','#ffc714','#ffc915','#ffca15','#ffcc15','#ffce16','#ffcf16','#ffd117','#ffd217','#ffd318','#ffd518','#ffd619','#ffd719','#ffda1a','#ffda1a','#ffdc1b','#ffde1b','#ffde1c','#ffe01c','#ffe21d','#ffe41d','#ffe51e','#ffe61e','#ffe81f','#ffe81f','#ffeb20','#ffec20','#ffed21','#ffee21','#fff021','#fff222','#fff323','#fff523','#fff624','#fff724'),
		 carrots = c('#372442','#4e1831','#4f1933','#501933','#511a34','#511b36','#521c37','#531c37','#531d39','#541e3a','#551f3a','#561f3c','#56203d','#57213e','#58223f','#582241','#592341','#5a2342','#5b2444','#5c2545','#5c2546','#5d2647','#5e2749','#5e2849','#5f284a','#60294c','#602a4c','#612b4e','#622b4f','#632c50','#642c50','#662c4f','#692c4f','#6a2c4f','#6e2b4e','#6f2b4e','#712b4d','#732b4d','#742b4d','#772b4c','#7a2a4b','#7c2a4b','#7c2a4b','#7e2a4a','#81294a','#832949','#842949','#872949','#892848','#8a2848','#8d2848','#8f2747','#912747','#932646','#952646','#972545','#982545','#9b2444','#9d2444','#9f2344','#a02343','#a32243','#a42142','#a62142','#a92042','#ab1f41','#ad1e41','#ae1d40','#b01c40','#b21b3f','#b31b3f','#b7193e','#b8183e','#b91a3f','#b91d40','#ba1f41','#bb2042','#bb2244','#bc2344','#bc2646','#bd2747','#bd2948','#be2a49','#bf2b49','#bf2e4b','#c0304c','#c1304b','#c2324b','#c3344a','#c3354a','#c43649','#c53749','#c53948','#c63a48','#c73c47','#c83d47','#c93e46','#c93f46','#ca4045','#cb4144','#cc4344','#cd4443','#cd4643','#ce4642','#cf4742','#cf4941','#d14a40','#d14c3f','#d24c3f','#d34d3e','#d34f3e','#d44f3d','#d5513d','#d6523c','#d7533b','#d7543a','#d8563a','#d95739','#d95838','#da5838','#db5937','#dc5b36','#dd5c35','#dd5e34','#de5f33','#df5f33','#df6032','#e06131','#e16330','#e16432','#e16533','#e26734','#e26836','#e26937','#e26b39','#e26d3a','#e26f3c','#e26f3c','#e3713e','#e3723f','#e37342','#e37443','#e37745','#e37746','#e47847','#e47a48','#e47b4a','#e47c4b','#e47d4c','#e4804d','#e4814c','#e4824c','#e4834c','#e4864b','#e4874b','#e4874b','#e48a4a','#e48b4a','#e48d49','#e48d49','#e48e49','#e49048','#e49248','#e49348','#e49447','#e49547','#e49746','#e49946','#e49946','#e49b45','#e49c45','#e49e44','#e49f44','#e4a143','#e4a143','#e4a243','#e4a442','#e4a542','#e4a641','#e4a841','#e3aa40','#e3ab3f','#e3ac3f','#e3ae3e','#e3af3e','#e3b03d','#e3b13d','#e3b23c','#e3b43c','#e3b53b','#e2b73a','#e2b83a','#e2b939','#e2ba38','#e2bb39','#e2bd3c','#e1bf41','#e1c043','#e0c145','#e0c247','#dfc44c','#dfc54e','#dec650','#ddc754','#ddc857','#ddc959','#dccb5b','#dbcd5f','#dbce61','#dccf64','#ddcf65','#ded068','#ded068','#ded26a','#dfd36d','#e0d36f','#e0d571','#e1d573','#e2d776','#e3d778','#e3d87a','#e3d97b','#e4da7c','#e5da7e','#e5dc81','#e6dd83','#e7dd85','#e7df87','#e8e08a','#e8e08b','#e9e18d','#e9e28f','#eae290','#eae492','#ebe594','#ece596','#ece799','#ede79b','#ede89d','#eeea9f','#efeaa1','#efeba3','#efeca4','#f0eca5','#f0eea8','#f1efaa','#f1f0ac','#f2f1ae','#f3f1b0','#f3f2b2','#f4f4b5','#f4f5b7','#f5f5b9','#f5f7bb','#f6f7bc','#f6f8be'),
		 arid_elevation = c('#999188','#9a9188','#9a9288','#9b9289','#9b9289','#9b938a','#9c938a','#9c948a','#9d948b','#9d948b','#9d958b','#9e968c','#9e968c','#9f968c','#9f978c','#a0978d','#a0978d','#a0988d','#a1998e','#a1998e','#a2998e','#a29a8f','#a29a8f','#a39b90','#a39b90','#a49c90','#a49b90','#a49c90','#a69d91','#a69c91','#a79d92','#a79e92','#a79e92','#a89e93','#a89e93','#a99f93','#a99f94','#aaa094','#aaa094','#aaa094','#aba295','#aba295','#aca296','#aca296','#aca396','#ada397','#ada497','#aea497','#aea598','#aea598','#afa599','#afa699','#b0a699','#b1a699','#b1a799','#b2a89a','#b2a89a','#b3a89b','#b3a99b','#b3a99c','#b4a99c','#b4aa9c','#b5aa9d','#b5aa9d','#b6ab9d','#b6ac9e','#b6ab9e','#b7ac9e','#b7ac9e','#b8ad9f','#b8ad9f','#b9ae9f','#b9afa0','#b9aea0','#baafa1','#bbafa1','#bbb0a1','#bcb1a2','#bcb0a2','#bdb1a3','#bdb2a3','#beb2a3','#beb2a4','#beb2a4','#bfb4a4','#bfb4a4','#bfb4a5','#c0b4a5','#c0b5a6','#c1b6a6','#c1b6a6','#c2b7a7','#c2b6a7','#c3b7a7','#c4b8a8','#c4b7a8','#c5b8a9','#c5b9a9','#c5b9aa','#c6b9aa','#c6b9aa','#c7bbab','#c7bbab','#c8bbab','#c8bbac','#c8bcac','#c9bcad','#c9bdad','#cabdae','#cabdae','#cabeae','#ccbfaf','#ccbfaf','#ccbfaf','#cdc0b0','#cec0b0','#cec1b1','#cec0b1','#cfc2b2','#cfc2b2','#cfc2b2','#d0c2b3','#d1c3b3','#d2c3b4','#d2c3b4','#d2c4b5','#d3c4b5','#d3c4b5','#d4c5b7','#d4c6b7','#d4c6b7','#d6c6b9','#d6c6bb','#d6c7bb','#d6c7bc','#d6c8bd','#d7c9be','#d7c8be','#d7cac0','#d7cac0','#d8cbc1','#d8cac1','#d8cbc2','#d8ccc2','#d8cdc4','#d8ccc4','#d9cec5','#d9cdc5','#d9cec7','#d9cec7','#dacfc7','#dad0c8','#dad0c9','#dbd1c9','#dbd1c9','#dbd2cb','#dbd2cb','#dcd2cc','#dcd3cc','#dcd3cd','#dcd3cd','#ddd4cf','#ddd5cf','#ded5d0','#ded5d0','#ded6d0','#ded6d1','#ded7d2','#dfd7d2','#dfd8d3','#e0d9d4','#e0d8d4','#e0d9d5','#e0d9d5','#e1dad6','#e1dbd6','#e1dbd7','#e2dbd7','#e2dcd7','#e2dcd9','#e3ddd9','#e3ddda','#e3ddda','#e4dfdb','#e4dfdb','#e5dfdc','#e5e0dc','#e5e1dd','#e5e0dd','#e6e1de','#e6e1df','#e6e2df','#e7e3e0','#e7e2e0','#e8e4e1','#e8e4e1','#e8e4e2','#e8e5e2','#e9e5e3','#e9e5e3','#eae6e4','#eae7e4','#eae7e5','#ebe7e5','#ebe8e5','#ece8e7','#ece8e7','#eceae8','#ece9e8','#edeae9','#edebe9','#edebe9','#eeecea','#eeecea','#efeceb','#efeceb','#f0edec','#f0eeec','#f1eeed','#f1eeed','#f1f0ee','#f1efee','#f2f0ee','#f2f1ef','#f2f1f0','#f3f2f1','#f3f2f1','#f4f3f2','#f4f3f2','#f5f3f3','#f5f4f3','#f6f4f4','#f6f4f4','#f6f5f4','#f7f6f5','#f7f6f5','#f7f6f6','#f7f7f6','#f8f7f7','#f8f7f7','#f9f8f8','#f9f9f8','#faf9f9','#fafaf9','#fbfafa','#fbfafa','#fbfbfb','#fcfcfb','#fcfcfb','#fdfcfc','#fdfdfc','#fefdfd','#fefefd','#fefefe','#fffffe','#ffffff'),
		 blue_fluoride = c('#291b32', '#2a1b34', '#2b1b34', '#2d1c36', '#2f1c38', '#301c39', '#301d3a', '#321d3b', '#331d3d', '#351d3f', '#351e40', '#371e41', '#381e43', '#3a1e45', '#3b1f45', '#3c1f46', '#3e1f48', '#3f1f4a', '#401f4c', '#42204d', '#43204e', '#44204f', '#462051', '#472052', '#482054', '#4a2056', '#4a2157', '#4c2158', '#4e215a', '#4f215b', '#50215d', '#52215e', '#532160', '#552162', '#552263', '#562264', '#582265', '#592267', '#5b2268', '#5c226b', '#5e226c', '#5f226e', '#60226f', '#622271', '#632272', '#642274', '#662276', '#672277', '#692278', '#6a227a', '#6c227b', '#6e227d', '#6e237e', '#6f247f', '#702480', '#712581', '#722681', '#732683', '#742783', '#752884', '#762985', '#772987', '#792a87', '#792b88', '#7a2c89', '#7b2c8a', '#7c2d8a', '#7d2d8c', '#7e2e8d', '#7f2f8d', '#80308e', '#813190', '#823191', '#833292', '#843292', '#863393', '#863494', '#873595', '#893596', '#8a3697', '#8b3798', '#8b3899', '#8c389a', '#8e399b', '#8e3a9c', '#8f3b9c', '#8f3d9d', '#8f3e9e', '#903f9e', '#90419e', '#90439f', '#9044a0', '#9046a0', '#9047a1', '#9049a1', '#914aa2', '#914ca2', '#914ca3', '#914ea3', '#9150a4', '#9151a5', '#9153a5', '#9154a6', '#9156a6', '#9157a7', '#9258a7', '#9259a8', '#925aa8', '#925ba9', '#925da9', '#925faa', '#9260ab', '#9260ab', '#9263ac', '#9264ac', '#9265ad', '#9266ae', '#9268ae', '#9269ae', '#926aaf', '#926bb0', '#926cb0', '#926eb1', '#926fb1', '#9270b2', '#9271b2', '#9273b3', '#9274b3', '#9275b4', '#9277b5', '#9277b5', '#9278b6', '#927ab6', '#927bb7', '#927cb7', '#927eb8', '#927fb8', '#9280b9', '#9281ba', '#9282ba', '#9284bb', '#9285bb', '#9285bc', '#9187bc', '#9188bd', '#918abd', '#918bbe', '#918cbf', '#918dbf', '#918ec0', '#918fc0', '#9191c1', '#9092c2', '#9094c2', '#9094c2', '#9095c3', '#9096c3', '#8f99c4', '#8f9ac5', '#8f9ac5', '#8f9bc6', '#8f9cc6', '#8f9dc7', '#8e9fc8', '#8ea0c8', '#8ea2c9', '#8ea3c9', '#8da5ca', '#8da5ca', '#8da6cb', '#8da7cb', '#8ca9cc', '#8caacc', '#8caccd', '#8bacce', '#8badce', '#8baecf', '#8ab0d0', '#8ab2d0', '#8ab2d1', '#8ab4d1', '#89b4d1', '#89b5d2', '#89b7d2', '#88b8d3', '#88bad4', '#87bad4', '#87bbd5', '#86bdd6', '#86bed6', '#86c0d7', '#85c0d7', '#85c1d8', '#84c3d8', '#84c4d9', '#83c5d9', '#83c6da', '#82c8da', '#82c8db', '#81cadc', '#81cbdc', '#80ccdd', '#81cddd', '#84cfdd', '#85cfdd', '#87d0dd', '#8ad0de', '#8dd1de', '#8fd2de', '#90d2de', '#92d4de', '#95d5de', '#97d5de', '#98d6de', '#9bd7de', '#9dd7df', '#a0d8df', '#a1d9df', '#a2dadf', '#a5dadf', '#a7dbdf', '#aadcdf', '#abdddf', '#acdde0', '#afdfe0', '#b1dfe0', '#b3e0e0', '#b4e1e0', '#b7e2e0', '#bae2e1', '#bae3e1', '#bee3e2', '#c0e4e3', '#c1e5e3', '#c4e6e3', '#c6e6e4', '#c8e7e4', '#cbe7e5', '#cde8e5', '#cee9e6', '#d2e9e7', '#d3eae7', '#d5eae7', '#d8ebe8', '#d9ece8', '#dcece9', '#deedea', '#dfeeea', '#e2eeea', '#e5efeb', '#e6f0eb', '#e9f0ec', '#ebf1ed', '#ecf2ed', '#eff3ee', '#f1f3ee')

		 )
	stevens_dat = c4a_data_as_is(stevens_seq, series = "stevens", type = "seq")
	c4a_load(stevens_dat)
})

## Earth sciences: e.g. NCL/Panoply
local({
	meteo = rcolors::colors_group$meteoswiss

	#names(meteo) = sub("_[0-9]{1,2}lev$", "", names(meteo))

	meteo_type = rep("seq", length(meteo))
	meteo_type[names(meteo) %in% c("hotcold_18lev", "hotcolr_19lev", "precip_diff_12lev", "precip4_diff_19lev")] = "div"

	meteo_type[sapply(meteo, length) == 2] = "cat"

	ocean = rcolors::colors_group$oceanography
	names(ocean) = sub("cmocean_", "", names(ocean), fixed = TRUE)

	ocean_type = rep("seq", length(meteo))
	ocean_type[names(ocean) %in% c("balance", "curl", "delta")] = "div"


	gmt = rcolors::colors_group$gmt
	names(gmt) = sub("GMT_", "", names(gmt), fixed = TRUE)

	gmt_type = rep("seq", length(gmt))
	gmt_type[names(gmt) %in% c("polar", "red2green", "no_green", "split", "precip4_diff_19lev")] = "div"


	c4a_load(c4a_data_as_is(meteo, series = "meteo", type = meteo_type))
	c4a_load(c4a_data_as_is(ocean, series = "ocean", type = ocean_type))
	c4a_load(c4a_data_as_is(gmt, series = "gmt", type = gmt_type))
})



if (FALSE) {
	### tree colors
	library(treemap)
	for (child in 2:4) {
		dseq = if (child == 2) 3:4 else 2
		for (depth in dseq) {
			res = rep(list(1:child),depth)
			names(res) = paste0("index", 1:depth)
			df = as.data.frame(do.call(expand.grid, rev(res))[,depth:1])
			tp = treepalette(df, palette.HCL.options = list(hue_fraction = .8))
			c4a_plot(tp$HCL.color)
		}
	}
}


.z = get("z", .C4A)
.s = get("s", .C4A)

.z$cit = NULL
.z$bib = NULL

.zbib = bibtex::read.bib("build/references.bib")
.zdes = local({
	df = read.csv("build/description.csv")
	structure(df$description, names = df$series)
})

save(.z, .s, .zbib, .zdes, file="R/sysdata.rda", compress="xz")

source("build/build_data.R")
source("build/build_naming_model.R")
