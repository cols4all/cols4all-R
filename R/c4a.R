#' Get a cols4all color palette
#'
#' Get a cols4all color palette: `c4a` returns the colors of the specified palette, `c4a_na` returns the color for missing value that is associated with the specified palette, and `c4a_ramp` returns a color ramp function.  Run \code{\link{c4a_gui}} to see all available palettes, which are also listed with \code{\link{c4a_palettes}}.
#'
#' @param palette name of the palette. See \code{\link{c4a_palettes}} for available palettes. If omitted, the default palette is provided by `c4a_default_palette`. The palette name can be prefixed with a `"-"` symbol, which will reverse the palette (this can also be done with the `reverse` argument). For bivariate palettes, a `"-"` means reversed horizontally (columns), a `"|"`means reversed vertically (row), and a `"+"` means reversed in both directions. In addition, a `"//"` or `"\\"` will flip the palette diagonally. This can be used in combination with `"-"`, `"|"`, or `"+"`. E.g. `"-//"` will reverse the columns and flip the palette diagonally.
#' @param n number of colors. If omitted then: for type `"cat"` the maximum number of colors is returned, for types `"seq"`, `"div"`, and `"cyc"`, 7 , 9, and 9 colors respectively. For bivariate palettes `n` is the number of columns.
#' @param m number of rows in case type is bivariate, so one of `"bivs"`, `"bivc"`, `"bivd"` or  `"bivg"` (see \code{\link{c4a_types}} for descriptions)
#' @param type type of color palette, in case `palette` is not specified: one of `"cat"`, `"seq"`, `"div"`, `"cyc"`, `"bivs"`, `"bivc"`, `"bivd"`, `"bivg"`. Run \code{\link{c4a_types}} for descriptions.
#' @param reverse should the palette be reversed? In case of a bivariate palette, a vector of two: the first indicates the horizontal direction (columns) and the second the vertical (rows).
#' @param diag_flip should a bivariate palette be flipped diagonally?
#' @param order order of colors. Only applicable for `"cat"` palettes
#' @param range a vector of two numbers between 0 and 1 that determine the range that is used for sequential and diverging palettes. The first number determines where the palette begins, and the second number where it ends. For sequential `"seq"` palettes, 0 means the leftmost (normally lightest) color, and 1 the rightmost (often darkest) color. For diverging `"seq"` palettes, 0 means the middle color, and 1 both extremes. If only one number is provided, this number is interpreted as the endpoint (with 0 taken as the start).
#' @param colorsort Sort the colors. Options: `"orig"` (original order), `"Hx"` (hue, where x is a starting number from 0 to 360), `"C"` (chroma), `"L"` (luminance). All these options are available for `"cat"` palettes, only the last one for `"seq"`, and none for the other palette types.
#' @param format format of the colors. One of: `"hex"` character vector of hex color values, `"rgb"` 3 column matrix of RGB values, `"hcl"` 3-column matrix of HCL values, or one of the color classes from \code{\link[colorspace:color-class]{colorspace}}
#' @param nm_invalid what should be done in case `n` or `m` is larger than the maximum number of colors or smaller than the minimum number? Options are `"error"` (an error is returned), `"repeat"`, the palette is repeated, `"interpolate"` colors are interpolated. For categorical `"cat"` palettes only.
#' @param verbose should messages be printed?
#' @return A vector of colors (`c4a`) and a color (`c4a_na`)
#' @importFrom grDevices col2rgb colorRampPalette colors gray.colors rgb grey
#' @importFrom methods as
#' @importFrom spacesXYZ DeltaE
#' @importFrom stats na.omit
#' @example ./examples/c4a.R
#' @rdname c4a
#' @name c4a
#' @export
c4a = function(palette = NULL, n = NA, m = NA, type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), reverse = FALSE, diag_flip = FALSE, order = NULL, range = NA, colorsort = "orig", format = c("hex", "rgb", "hcl", "RGB", "XYZ", "HSV", "HLS", "LAB", "polarLAB", "LUV", "polarLUV"), nm_invalid = c("error", "repeat", "interpolate"), verbose = TRUE) {
	calls = names(match.call(expand.dots = TRUE)[-1])

	type = match.arg(type)

	if (identical(format, "HCL") && verbose) {
		message("As of cols4all 0.8, the formats \"RGB\" and \"HCL\" have been renamed to lower case (to prevent conflicts with the newly supported colorspace classes)")
		format = "hcl"
	}
	if (identical(format, "RGB") && verbose) {
		message("As of cols4all 0.8, the formats \"RGB\" and \"HCL\" have been renamed to lower case (to prevent conflicts with the newly supported colorspace classes)")
	}

	format = match.arg(format)

	nm_invalid = match.arg(nm_invalid)

	if (is.null(palette)) {
		palette = c4a_default_palette(type)
		if ("palette" %in% calls && verbose) message("Argument palette is specified as NULL, therefore returning default palette: \"", palette, "\"")
		mes = paste0("These are the colors from palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}

	x = c4a_info(palette, verbose = verbose, no.match = {if (verbose) "message" else "null"})

	# update type
	type = x$type

	if (is.null(x)) return(invisible(NULL))

	reverse = rep(reverse, length.out = 2)

	reverse = xor(reverse, x$reverse)

	if (is.na(n)) n = x$ndef
	if (is.na(m)) m = if (is.na(x$mdef)) n else x$mdef

	if (nm_invalid == "error") {
		tail_str = if (substr(type, 1, 3) == "biv") " columns of colors" else " colors"
		if (n > x$nmax) {
			if (x$nmax == x$nmin) {
				stop("Palette ", palette, " only supports ", x$nmax, tail_str)
			} else {
				stop("Palette ", palette, " only supports up to ", x$nmax, tail_str)
			}
		} else if (n < x$nmin) {
			stop("Palette ", palette, " should only be used with a minimum of ", x$nmin, tail_str)
		}

		if (m > x$mmax) {
			if (x$mmax == x$mmin) {
				stop("Palette ", palette, " only supports ", x$mmax, " rows of colors.")
			} else {
				stop("Palette ", palette, " only supports maximally ", x$mmax, " rows of colors.")
			}
		} else if (m < x$mmin) {
			stop("Palette ", palette, " should only be used with a minimum of ", x$mmin, " rows of colors.")
		}
	}

	x$range = range
	if (is.na(n)) n = x$ndef
	if (is.na(m)) m = n
	#if (substr(type, 1, 3) == "biv" && is.na(m)) m = n
	x$nm_invalid = nm_invalid


	pal = do.call(get_pal_n, c(list(n = n, m = m, colorsort = colorsort), x))

	pal = if (!is.null(order)) {
		if (type != "cat") {
			if (verbose) message("order not used for palettes of type \"", type, "\"")
			pal
		} else {
			if (!all(order %in% 1L:n)) stop("order should consist of numbers 1 to ", n)
			pal[order]
		}
	} else pal

	if (!is.null(mes) && verbose) message(mes)






	if (format == "hex") {
		pal
	} else {
		cols = colorspace::hex2RGB(pal)

		if (format == "rgb") {
			cols@coords * 255
		} else if (format == "hcl") {
			as(cols, "polarLUV")@coords[,c("H", "C", "L"), drop = FALSE]
		} else {
			as(cols, format)
		}
	}
}

#' @param space a character string; interpolation in RGB or CIE Lab color spaces
#' @param interpolate use spline or linear interpolation
#' @param ... passed on to `c4a`.
#' @rdname c4a
#' @name c4a_ramp
#' @export
c4a_ramp = function(..., space = c("rgb", "Lab"),
					interpolate = c("linear", "spline")) {
	space = match.arg(space)
	interpolate = match.arg(interpolate)
	args = list(...)
	pal = do.call(c4a, args)
	if (is.null(pal)) return(invisible(NULL))
	colorRampPalette(pal, space = space, interpolate = interpolate)
}


#' Get information from a cols4all palette
#'
#' Get information from a cols4all palette
#'
#' @param palette name of the palette
#' @param no.match what happens is no match is found? Options: `"message"`: a message is thrown with suggestions, `"error"`: an error is thrown, `"null"`: `NULL` is returned
#' @param verbose should messages be printed?
#' @return list with the following items: name, series, fullname, type, palette (colors), na (color), nmax, and reverse. The latter is `TRUE` when there is a `"-"` prefix before the palette name.
#' @export
c4a_info = function(palette, no.match = c("message", "error", "null"), verbose = TRUE) {
	if (!is.character(palette)) stop("palette should be a character value", call. = FALSE)
	if (length(palette) != 1L) stop("palette should be a character value (so length 1)", call. = FALSE)

	no.match = match.arg(no.match)

	res = parse_prefixed_text(palette)

	palette = res$name
	is_rev = res$is_rev
	is_diag = res$is_diag

	z = .C4A$z

	fullname = c4a_name_convert(palette, no.match = no.match, verbose = verbose)

	if (is.null(fullname)) return(invisible(NULL))

	palid = which(fullname == z$fullname)

	x = as.list(z[palid, ])
	x$reverse = is_rev
	x$diag_flip = is_diag
	x$palette = x$palette[[1]]

	structure(x, class = c("c4a_info", "list"))
}




#' @rdname c4a
#' @name c4a_na
#' @export
c4a_na = function(palette = NULL, type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"), verbose = TRUE) {
	type = match.arg(type)
	if (is.null(palette)) {
		palette = c4a_default_palette(type)
		mes = paste0("This is the color for missing values associated with palette \"", palette, "\", the default for type \"", type, "\":")
	} else {
		mes = NULL
	}


	x = c4a_info(palette, verbose = verbose)

	if (!is.null(mes) && verbose) message(mes)

	x$na
}


get_zp = function(p, n = NA, no.match, verbose) {
	x = c4a_info(p, no.match, verbose)
	if (is.na(n)) {
		n = x$ndef
	}
	z = data.frame(name = x$name, series = x$series, fullname = x$fullname, type = x$type, n = n)
}

parse_prefixed_text <- function(x) {
	# Pattern matches:
	# - optional symbol: +, -, or |
	# - optional slash prefix: // or \ (single backslash)
	pattern <- "^([-+|]?)(//|\\\\)?(.*)$"

	matches <- regexec(pattern, x)
	result <- regmatches(x, matches)[[1]]

	part_rev  <- ifelse(length(result) >= 2, result[2], "")
	part_diag  <- ifelse(length(result) >= 3, result[3], "")
	name   <- ifelse(length(result) >= 4, result[4], x)

	is_rev = c(part_rev %in% c("-", "+"), part_rev %in% c("|", "+"))
	is_diag = part_diag %in% c("//", "\\")

	if (is_diag) is_rev = rev(is_rev)
	if (part_diag == "\\") is_rev = !is_rev

	list(name = name,
		 is_rev = is_rev,
		 is_diag = is_diag)
}


