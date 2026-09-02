#' Get information from a cols4all palette
#'
#' Get information from a cols4all palette
#'
#' @param palette name of the palette
#' @param type type of palettes (in case palette is not specified)
#' @param series series name (in case palette is not specified)
#' @param n number of colors
#' @param no.match what happens is no match is found? Options: `"message"`: a message is thrown with suggestions, `"error"`: an error is thrown, `"null"`: `NULL` is returned
#' @param verbose should messages be printed?
#' @param include_tritan should tritan (blue-yellow color vision deficiency,
#'   ~1 in 10,000 people) be included in the worst-case color-blind-friendly
#'   score, alongside deutan and protan (red-green, together ~4% of the
#'   population)? Set to `FALSE` to score on deutan+protan only.
#' @return list with the following items: name, series, fullname, type, palette (colors), na (color), nmax, and reverse. The latter is `TRUE` when there is a `"-"` prefix before the palette name.
#' @export
#' @example examples/c4a_scores.R
c4a_scores = function(palette = NULL, type = NULL, series = NULL, n = NA, no.match = c("message", "error", "null"), verbose = TRUE, include_tritan = TRUE) {
	if (!is.null(palette)) {
		z = get_zp(palette, n, no.match = no.match, verbose = verbose)
	} else {
		if (is.null(type)) stop("Please specify either palette or type (optionally in combination with series)")
		if (is.na(n)) n = .C4A$ndef[[type]]
		pals = c4a_palettes(type = type, series = series)
		z = do.call(rbind, lapply(pals, get_zp, no.match = no.match, verbose = verbose))

	}
	show_attach_scores(z, include_tritan = include_tritan)
}
