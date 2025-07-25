#' Build and load palette data
#'
#' Build palette data. Both `c4a_data` and `c4a_data_as_is` build data palette. The difference is that the former may restructure the palette colors (see details) whereas the latter takes the palette colors as they are. Data can subsequently be loaded into cols4all via \code{\link{c4a_load}}. The `c4a_data` function can also be used to read `c4a_info` objects, which contain data for a single palette.
#'
#' In cols4all, palettes are organized by series and by type. The **series** or 'family' specifies where the palettes belong to. For instance `"brewer"` stands for the color palettes from ColorBrewer. Run \code{\link{c4a_series}} to get an overview of loaded series. The **type** specifies what kind of palette it is; see \code{\link{c4a_types}} for a description of the implemented ones.
#'
#' This function structures the palette data, such that it is consistent with the other palette data. This includes:
#'
#' * Palette names are made consistent. We use the convention `"my_series.my_palette"`, so all lower case, a period to separate the series name from the palette name, and underscores to separate words.
#' * (Only for `c4a_data`, bypassed for `c4a_data_as_is`)  Categorical palettes: black is removed from categorical palettes, and a grayscale color is assigned to be used for missing values (other grayscale colors are removed). Sequential palettes are sorted from light to dark.
#'
#' Indexing: for a categorical `"cat"` palette, an optional `"index"` attribute determines which colors to use for which lengths: if the palette consists of k colors, index should be a list of k, where the i-th element is an integer vector of length i with values 1,2,...,k. See `c4a_info("rainbow")` and  for an example.
#'
#' Range: sequential and diverging palettes are usually defined for 9+ colors. The optional `"range_matrix"` attribute determines that range is used for less colors. It is a n x 2 matrix where row i defines the applied range of a palette of length i. For sequential palettes a range `c(0,1)` means that the palette is generated (via a color ramp) between the two outermost colors. For diverging palettes, a range `c(x, y)` means that both sides of the palette are generated (via a color ramp) from `x`, which is the distance to the center color, to `y` which represents both outermost colors.
#'
#' The range is automatically set for sequential and diverging palettes that have no `"index"` or `"range_matrix"` attribute via the parameter `range_matrix_args`, which is a list per palette. The arguments for a sequential palette are: `nmin` the minimum number of colors for which the range is reduced, `nmax`, the number of colors for which the range is set to `c(0,1)`, `slope_min` and `slope_max` determine the slopes of range reduction from a palette of length `nmax` to `nmin`, and `space` sets the color space for which the color ramp is applied (`"rgb"` or `"Lab"`). The arguments for a diverging palette are the same, but only one `slope` is used (namely for the outermost colors).
#'
#' It may take some time to process, especially large categorical palettes, because of calculations of the color blind checks.
#'
#' @param x either a named list of color palettes or a \code{\link{c4a_info}} object. For the first case: see details for indexing. The second case will bypass the other arguments.
#' @param xNA colors for missing values. Vector of the same length as x (or length 1). For `NA` values, the color for missing values is automatically determined (preferable a light grayscale color, but if it is indistinguishable by color blind people, a light color with a low chroma value is selected)
#' @param types character vector of the same length as x (or length 1), which determines the type of palette: `"cat"`, `"seq"`, `"div"`, `"cyc"`, `"bivs"`, `"bivc"`, `"bivd"`, or `"bivg"`. See details.
#' @param series a character vector of the same length as x (or length 1), which determines the series.
#' @param nmin,nmax,ndef minimum / maximum / default number of colors for the palette. By default: `nmin = 1`, for `"cat"` `nmax` and `ndef` the number of supplied colors. For the other types, `nmax` is `Inf`. `ndef` is 7 for `"seq"`, 9. For diverging palettes, these numbers refer to the number of columns. (See `mmin`, `mmax`, `mdef` for the rows)
#' @param mmin,mmax,mdef minimum / maximum / default number of rows for bivariate palettes.
#' @param format.palette.name should palette names be formatted to lowercase/underscore format?
#' @param remove.blacks,remove.whites,take.gray.for.NA,remove.other.grays These arguments determine the processing of grayscale colors for categorical `"cat"` palettes: if `remove.blacks` and there are (near) blacks, these are removed first. Next, if `take.gray.for.NA`, `xNA` is `NA`, and a palette contains at least one grayscale color (which can also be white), this is used as color for missing values. In case there are more than one grayscale color, the lightest is taken. `remove.other.grays` determines what happens with the other grays.
#' @param light.to.dark should sequential `"seq"` palettes be automatically ordered from light to dark?
#' @param remove.names should individual color names be removed?
#' @param biv.method method to a create bivariate palette. Options are `"byrow"` means that the colors are wrapped row-wise to a color matrix where the number of rows and columns is automatically determined, `"byrowX"` the same but with X (integer between 2 and 9) columns, `"bycol"` and `"bycolX` similar but wrapped column-wise. `"div2seqseq"` and `"div2catseq` means that colors are extracted from a divering palette. The former translates colors into a matrix with the neutral color in the diagonal, while the latter places the neutral color in the middle column. `"seq2uncseq"`
#' @param space color space in which interpolated colors are determined. Options: `"rgb"` (RGB) and `"Lab"` (CIE Lab).
#' @param range_matrix_args list of lists, one for each palette. Each such list specifies the range of sequential and diverging palettes, in case they are not indexed. See details.
#' @param bib bibtex reference in the form of a `utils::bibentry` object.
#' @param description description of the series. If `series` contains multiple series (rather than one value), please specify a vector of the same length as `series`. See \code{\link{c4a_series}} for the descriptions of the currently loaded series.
#' @param ... passed on to `c4a_data`
#' @example ./examples/c4a_data.R
#' @return `c4a_data` object, which is a list of four items: `data`, `s`, `citation`, and `description`
#' @rdname c4a_data
#' @name c4a_data
#' @export
c4a_data = function(x, xNA = NA, types = "cat", series = "x", nmin = NA, nmax = NA, ndef = NA, mmin = NA, mmax = NA, mdef = NA, format.palette.name = TRUE, remove.blacks = NA, remove.whites = NA, take.gray.for.NA = FALSE, remove.other.grays = FALSE, light.to.dark = FALSE, remove.names = TRUE, biv.method = "byrow", space = "rgb", range_matrix_args = list(NULL), bib = NA, description = NA) {

	check_installed_packages("colorblindcheck")

	if (inherits(x, "c4a_info")) {
		if (is.null(x$bib)) {
			bib = NA
			nbib = 1
		} else {
			bib = character2bibentry(x$bib)
			nbib = -1
			names(bib) = NULL
		}
		x$cit = NULL
		x$bib = NULL
		x$reverse = NULL
		x$diag_flip = NULL
		x$palette = I(list(x$palette))
		z = as.data.frame(x)
		rownames(z) = NULL
	} else {
		# check color list
		if (!is.list(x)) stop("x is not a list")
		nms = names(x)
		if (is.null(nms)) stop("x must be named")
		#x = lapply(x, validate_colors, name = "x", from_list = TRUE)
		x = mapply(validate_colors, x = x, name = nms, MoreArgs = list(from_list = TRUE), SIMPLIFY = FALSE)

		# number of palettes
		k = length(x)

		# make everything length k (number of palettes)
		args = setdiff(ls(), c("x", "k", "nms"))
		length(range_matrix_args)

		# manual preprocessing
		if (!is.list(range_matrix_args[[1]])) range_matrix_args = list(range_matrix_args)

		nbib = if (is.na(bib) || inherits(bib, "bibentry")) 1 else k
		if (nbib == 1) bib = list(bib)

		for (arg in args) assign(arg, rep(get(arg), length.out = k), envir = environment())

		# validate na colors
		if (any(!is.na(xNA))) xNA[!is.na(xNA)] = validate_colors(xNA[!is.na(xNA)], name = "xNA")

		# check types
		types_supported = unname(.C4A$types)
		if (!all(types %in% types_supported)) stop("Unknown types found. Currently only", paste(types_supported, collapse = ","), "are supported")



		lst = list(pal = x,
				   type = types,
				   colNA = xNA,
				   take.gray.for.NA = take.gray.for.NA,
				   remove.other.grays = remove.other.grays,
				   remove.blacks = remove.blacks,
				   remove.whites = remove.whites,
				   light.to.dark = light.to.dark,
				   remove.names = remove.names,
				   biv.method = biv.method,
				   space = space,
				   range_matrix_args = range_matrix_args)


		res = do.call(mapply, c(list(FUN = process_palette, SIMPLIFY = FALSE), lst))
		x = lapply(res, "[[", "pal")
		xNA = sapply(res, "[[", "colNA", USE.NAMES = FALSE)
		reversed = sapply(res, "[[", "reversed")



		if (format.palette.name[1]) {
			nms = format_name(nms)
			if (any(reversed)) {
				nms2 = nms[reversed]
				ss = strsplit(nms2, "_", fixed = TRUE)
				ss2 = sapply(ss, function(s) {
					s2 = if (length(s) == 2 && !(s[1] %in% c("light", "dark"))) rev(s) else s
					paste(s2, collapse = "_")
				}, USE.NAMES = FALSE)
				isdiff = (ss2 != nms2)
				if (any(isdiff)) {
					message("Some palettes have been reversed (because of the cols4all convention that seq palettes are arranged from light to dark). Therefore they may have automatically be renamed. Please check and if needed change the argument settings of tm_series_add.\nOld names: ", paste(nms2[isdiff], collapse = ", "), "\nNew names: ", paste(ss2[isdiff], collapse = ", "))
				}
				if (any(!isdiff)) {
					message("Some palettes have been reversed (because of the cols4all convention that seq palettes are arranged from light to dark), but the names have not been changed. Please check and if needed change names manually.\nThe palettes are: ", paste(nms2[!isdiff], collapse = ", "), ".")
				}
				nms[reversed] = ss2
			}
		} else {
			if (any(reversed)) {
				nms2 = nms[reversed]
				message("Some palettes have been reversed, but the names have not been changed. Please check and if needed change names manually or try with format.palette.name.\nThe palettes are: ", paste(nms2, collapse = ", "), ".")
			}
		}


		seriesID = which(series != "")
		fnms = nms
		if (length(seriesID)) fnms[seriesID] = paste0(series[seriesID], ".", fnms[seriesID])


		if (anyDuplicated(fnms)) {
			stop("Duplicated names found: ", paste(fnms[anyDuplicated(fnms)], collapse = ", "))
		}


		names(x) = nms

		z = data.frame(name = nms, series = series, fullname = fnms, type = types, palette = I(x), na = xNA, nmin = nmin, nmax = nmax, ndef = ndef, mmin = mmin, mmax = mmax, mdef = mdef)
		rownames(z) = NULL

	}


	z$nmax = mapply(function(pal, type, nmax) {
		index = attr(pal, "index")
		if (!is.na(nmax)) {
			nmax
		} else if (type == "cat") {
			if (is.null(index)) length(pal) else length(index[[length(index)]])
		} else if (type == "bivc") {
			ncol(pal)
		} else {
			Inf
		}
	}, z$palette, z$type, z$nmax, SIMPLIFY = TRUE, USE.NAMES = FALSE)

	z$nmin = mapply(function(pal, nmin) {
		index = attr(pal, "index")
		if (!is.na(nmin)) {
			nmin
		} else if (is.null(index)){
			1
		} else {
			which(vapply(index, length, FUN.VALUE = integer(1)) != 0L)[1]
		}
	}, z$palette, z$nmin, SIMPLIFY = TRUE, USE.NAMES = FALSE)





	z$ndef = mapply(function(nmax, type) {
		if (!is.infinite(nmax)) nmax else unname(.C4A$ndef[type])
	}, z$nmax, z$type, USE.NAMES = FALSE)

	z$mmax = mapply(function(pal, mmax) {
		if (!is.na(mmax)) {
			mmax
		} else if (is.matrix(pal)) {
			Inf
		} else {
			1
		}
	}, z$palette, z$mmax, SIMPLIFY = TRUE, USE.NAMES = FALSE)
	z$mmin[is.na(z$mmin)] = 1L
	z$mdef = mapply(function(mmax, type) {
		if (!is.infinite(mmax)) mmax else unname(.C4A$mdef[type])
	}, z$mmax, z$type, USE.NAMES = FALSE)


	#z$ndef = ifelse(!is.infinite(z$nmax), z$nmax, ifelse(z$type == "seq", 7, ifelse(z$type == "div", 9, 3)))

	s = series_add_get_scores(z)


	# .z = .C4A$z
	# .s = .C4A$s
	# .zbib = .C4A$zbib
	# .zdes = .C4A$zdes

	# add citations
	if (nbib == -1) {
		# bypass in case x is c4a_info object
		zb = bib
		zb = list(zb)
		names(zb) = z$fullname
	} else if (nbib == 1) {
		if (!is.na(bib[[1]])) {
			zb = bib[1]
			if (any(z$series != z$series[1])) stop("One bib item defined, while multiple series: bib items are organized by series and optionally palettes")
			zb[[1]]$name = z$series[1]
			names(zb) = z$series[1]
			#if (z$series[1] %in% names(.zbib)) stop("Citation for series ", z$series[1], " already defined")
		} else {
			zb = NULL
		}
	} else {
		zb = mapply(function(b, nm) {
			names(b) = nm
			b
		}, bib, z$fullname, SIMPLIFY = FALSE)
		names(zb) = z$fullname
	}

	# add series info
	ids = !duplicated(z$series)
	if (is.na(description[1])) {
		zdes = NULL
	} else {
		zdes = structure(description[ids], names = z$series[ids])
	}


	structure(list(data = z, scores = s, citation = zb, description = zdes), class = "c4a_data")
}

#' @rdname c4a_data
#' @name c4a_load
#' @param data cols4all data created with `c4a_data`
#' @param overwrite in case the palettes already exist (i.e. the full names), should the old names be overwritten?
#' @export
c4a_load = function(data, overwrite = FALSE) {
	z = data$data
	s = data$scores
	zbib = data$citation
	zdes = data$description

	fnms = z$fullname

	z2 = .C4A$z
	s2 = .C4A$s
	zbib2 = .C4A$zbib
	zdes2 = .C4A$zdes


	if (!is.null(z2)) {
		z2$bib = NULL
		z2$cit = NULL
		if (any(fnms %in% z2$fullname)) {
			if (overwrite) {
				dupl = (z2$fullname %in% fnms)
				z2 = z2[!dupl,]
				s2 = s2[!dupl,,]
				zbib2 = zbib2[!(names(zbib2) %in% fnms)]
				zdes2 = zdes2[!(names(zdes2) %in% fnms)]
			} else {
				stop("Fulnames already exist: ", paste(intersect(fnms, z2$fullname), collapse = ", "))
			}
		}

		z = rbind(z2, z)
		s = abind::abind(s2, s, along=1)
	}

	if (!is.null(zbib2)) {
		zbib = do.call(c, c(list(zbib2), zbib)) # to do: check for duplicates
	}
	zdes = c(zdes2, zdes) # to do: check


	.C4A$z = z
	.C4A$s = s
	.C4A$zbib = zbib
	.C4A$zdes = zdes
	attach_bib()
	fill_P()
	invisible(NULL)

}



#' @rdname c4a_data
#' @name c4a_data_as_is
#' @export
c4a_data_as_is = function(..., format.palette.name = FALSE, remove.blacks = FALSE, remove.whites = FALSE, take.gray.for.NA = FALSE, remove.other.grays = FALSE, light.to.dark = FALSE, remove.names = FALSE) {

	args = c(list(...), list(format.palette.name = format.palette.name, take.gray.for.NA = take.gray.for.NA, remove.other.grays = remove.other.grays, remove.blacks = remove.blacks, remove.whites = remove.whites, light.to.dark = light.to.dark, remove.names = remove.names))
	do.call(c4a_data, args)
}




check_z = function(z) {
	name <- series <- fullname <- type <- nmax <- NULL
	if (!is.data.frame(z) || !setequal(c("name", "series", "fullname", "type", "palette", "na", "nmin", "nmax", "ndef", "mmin", "mmax", "mdef"), names(z))) stop("data should be a dataframe of colums: name, series, fullname, type, palette, na, nmin, nmax, ndef, mmin, mmax, and mdef")

	within(z, {
		if (!is.character(name)) stop("x$data$name should be a character column", call. = FALSE)
		if (!is.character(series)) stop("x$data$series should be a character column", call. = FALSE)
		if (!is.character(fullname)) stop("x$data$fullname should be a character column", call. = FALSE)
		if (!is.character(type)) stop("x$data$type should be a character column", call. = FALSE)
		if (!is.list(palette)) stop("x$data$palette should be a list column", call. = FALSE)
		if (!is.character(na) || all(is.na(na))) stop("x$data$na should be a character column", call. = FALSE)

		if (anyDuplicated(fullname)) stop("x$data$fullname should consist of unique values", call. = FALSE)
		if (!all(type %in% unname(.C4A$types))) stop("x$data$type should consist of", paste(unname(.C4A$types), collapse = ","), "values only", call. = FALSE)
		if (!is.numeric(nmax)) stop("x$data$nmax should be a numeric column", call. = FALSE)

		palette = I(lapply(palette, validate_colors))
		if (any(!is.na(na))) na[!is.na(na)] = validate_colors(na[!is.na(na)])
	})
}

check_s = function(s, n) {
	if (!is.array(s)) {
		message("x$scores is not an array", call. = FALSE)
		return(FALSE)
	}
	d = dim(s)

	snames = c(.C4A$sc, .C4A$hcl, .C4A$rgb)

	if (d[1] != n) {
		message("number of rows (first dim) of x$s does not correspond to the number of rows in x$data", call. = FALSE)
		return(FALSE)
	}
	if (!setequal(dimnames(s)[[2]], snames)) {
		message("columns (second dim) in x$scores should correspond to", paste(snames, collapse = ","), call. = FALSE)
		return(FALSE)
	}
	if (d[3] != max(.C4A$nmax)) {
		message("Third dimension of x$scores should be", d[3], call. = FALSE)
		return(FALSE)
	}
	TRUE
}

character2bibentry = function(x) {
	if (!requireNamespace("bibtex")) stop("bibtex package required", call. = FALSE)
	bibfile = tempfile(fileext = "bib")
	writeLines(con = bibfile, x)
	bibtex::read.bib(bibfile)
}

