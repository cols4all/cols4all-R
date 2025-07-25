rampPal = function(palette, n, space = c("rgb", "Lab")) {
	space = match.arg(space)
	if (length(palette) == n) {
		attributes(palette) = NULL
		palette
	} else if (n == 1) {
		colorRampPalette(palette, space = space, interpolate = "linear")(3)[2]
	} else {
		colorRampPalette(palette, space = space, interpolate = "linear")(n)
	}
}

div_rev = function(x) {
	h = get_hc_or_l(x, "H")
	#prop = hcl_prop(x)
	hL = h[round(length(x) * .25)]
	hR = h[round(length(x) * .75)]
	hL > hR
}

get_pal_n = function(n, m = NA, colorsort = "orig", range = NA, reverse = c(FALSE, FALSE), diag_flip = FALSE, type, palette, nmin, nmax, ndef, mmin, mmax, mdef, nm_invalid = "error", ...) {


	if (is.na(m)) m = n

	if (substr(type, 1, 3) == "biv" && diag_flip) {
		m_tmp = m
		m = n
		n = m_tmp
	}

	n_orig = n
	m_orig = m
	if (n > nmax || n < nmin) {
		if (nm_invalid == "error") return(NULL)
		n = ndef
	}
	if (!is.na(m) &&  (m > mmax || m < mmin)) {
		if (nm_invalid == "error") return(NULL)
		m = mdef
	}
	index = attr(palette, "index")
	range_matrix = attr(palette, "range_matrix")
	space = attr(palette, "space")

	x = if (type == "cat") {
		if (is.null(index)) {
			palette[1:n]
		} else {
			palette[index[[n]]]
		}
	} else if (type %in% c("seq", "div", "cyc")) {
		if (is.na(range[1])) {
			if (!is.null(index)) {
				pal = palette[index[[min(n, length(index))]]]
				rng = c(0, 1)
			} else if (!is.null(range_matrix)) {
				pal = palette
				rng = range_matrix[min(n, nrow(range_matrix)), ]
			} else {
				pal = palette
				rng = c(0, 1)
			}
		} else {
			if (!is.null(index)) {
				pal = palette[index[[length(index)]]]
			} else {
				pal = palette
			}
			rng = range
		}

		if (type %in% c("seq", "cyc")) {
			if (rng[1] == 0 && rng[2] == 1) {
				rampPal(pal, n, space = space)
			} else {
				rngIDs <- round(seq(rng[1]*100, rng[2]*100, length.out=n))+1
				rampPal(pal, 101, space = space)[rngIDs]
			}
		} else {
			if (rng[1] == 0 && rng[2] == 1) {
				rampPal(pal, n, space = space)
			} else {
				breaks = seq(-10,10, length.out=n+1)
				rngIDs <- map2divscaleID(breaks=breaks, range=rng)
				rampPal(pal, 101, space = space)[rngIDs]
			}
		}
	} else if (substr(type, 1, 3) == "biv") {
		if (is.na(range[1])) range = c(0, 1)
		if (all(dim(palette) == c(m, n)) && range[1] == 0 && range[2] == 1) {
			palette
		} else {
			rangeIDsm <- round(seq(range[1]*100, range[2]*100, length.out=m))+1
			rangeIDsn <- round(seq(range[1]*100, range[2]*100, length.out=n))+1

			if (type != "bivc") {
				# stretch both columns and rows
				p2 = t(apply(palette, MARGIN = 1, FUN = function(x) {
					rampPal(x, 101, space = space)[rangeIDsn]
				}))
				res = apply(p2, MARGIN = 2, FUN = function(x) {
					rampPal(x, 101, space = space)[rangeIDsm]
				})
			} else {
				# stretch rows only (columns to index, like cat)
				p2 = palette[, 1L:n]
				res = apply(p2, MARGIN = 2, FUN = function(x) {
					rampPal(x, 101, space = space)[rangeIDsm]
				})
			}


			# make sure grays are really gray (not always the case due to rounding and color space artefacts)
			if (type == "bivs" && n == m) {
				if (aregreys(diag(palette))) {
					diag(res) = convert2grey(diag(res))
				}
			} else if (type == "bivd") {
				if (aregreys(palette[, (ncol(palette) + 1)/2])) {
					res[, (ncol(res)+1)/2] = convert2grey(res[, (ncol(res)+1)/2])
				}
			}
			res
		}
	}


	# invalid n/m without error: repeat or interpolate
	if (substr(type, 1, 3) == "biv") {
		# columns
		if (n_orig != n) {
			if (nm_invalid == "repeat") {
				x = x[, rep(1:ncol(x), length.out = n_orig)]
			} else if (nm_invalid == "interpolate") {
				x = t(apply(x, MARGIN = 2, FUN = function(x) {
					rampPal(x, n_orig)
				}))
			}
		}
		# rows
		if (m_orig != m) {
			if (nm_invalid == "repeat") {
				x = x[rep(1:nrow(x), length.out = m_orig), ]
			} else if (nm_invalid == "interpolate") {
				x = apply(x, MARGIN = 1, FUN = function(x) {
					rampPal(x, m_orig)
				})
			}
		}
	} else {
		if (n_orig != n) {
			if (nm_invalid == "repeat") {
				x = rep(x, length.out = n_orig)
			} else if (nm_invalid == "interpolate") {
				x = rampPal(x, n_orig)
			}
		}
	}

	x2 = if (colorsort != "orig") {
		if (type == "cat") {
			sby = substr(colorsort, 1, 1)
			if (sby == "H") {
				if (nchar(colorsort) == 1) {
					Hstart = 0
				} else {
					Hstart = tryCatch({
						as.integer(substr(colorsort, 2, nchar(colorsort)))
					}, warning = function(e) {
						warning("colorsort invald: number behind \"H\" should be an integer between 0 and 360", call. = FALSE)
						0
					})
				}
			}
			v = get_hc_or_l(x, sby)
			if (sby == "H") {
				v = v + Hstart
				v[v>360] = v[v>360] - 360
			}
			x[order(v)]
		} else if (type == "seq") {
			if (colorsort == "L") {
				ls = get_hc_or_l(x, "L")
				ls_sg = sign(ls[-1] - ls[-length(ls)])
				if (all(ls_sg>=0)) {
					x = rev(x)
				}
			} else {
				warning("colorsort invalid: for type \"seq\", the options are \"orig\" and \"L\"", call. = FALSE)
			}
			x
		} else if (type == "div") {
			if (substr(colorsort, 1, 1) == "H") {
				if (div_rev(x)) x = rev(x)
			} else {
				warning("colorsort invalid: for type \"div\", the options are \"orig\" and \"H\"", call. = FALSE)
			}
			x
		} else {
			x
		}
	} else {
		x
	}

	if (substr(type, 1, 3) == "biv") {
		if (reverse[1]) {
			x3 = x2[, ncol(x2):1L]
		} else {
			x3 = x2
		}

		if (reverse[2]) {
			x3 = x3[nrow(x3):1L, ]
		}
		if (diag_flip) x3 = t(x3)
	} else {
		x3 = if (reverse[1]) rev(x2) else x2
	}
	x3
}




map2divscaleID <- function(breaks, n=101, range=1) {
	nbrks <- length(breaks)

	if (length(range)==1) {
		range <- c(0, range)
	}
	crange <- range[2] - range[1]

	lw <- breaks[1]
	hg <- breaks[nbrks]

	# omit infinity values
	if (lw==-Inf) lw <- breaks[2]
	if (hg==Inf) hg <- breaks[nbrks-1]
	mx <- max(abs(c(lw, hg)))


	is.div <- any(breaks<0) && any(breaks>0)

	cat0 <- !any(breaks==0)

	h <- ((n-1)/2)+1

	if (is.div && !cat0) {
		npos <- sum(breaks>0)
		nneg <- sum(breaks<0)
		step <- round((h-1)*crange/((max(npos, nneg)-.5)*2))
	} else {
		npos <- sum(breaks>=0) - !is.div
		nneg <- sum(breaks<=0) - !is.div
		step <- 0
	}

	pid <- h + step
	nid <- h - step

	ids <- rep(h, nbrks-1)
	if (npos>0) ids[(nbrks-npos):(nbrks-1)] <- pid +
		seq((n-pid)/mx*hg*range[1], (n-pid)/mx*hg*range[2], length.out=npos)
	if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-lw*range[2]), nid-((nid-1)/mx*-lw*range[1]),
								   length.out=nneg)
	if (is.div && cat0) ids[nneg] <- h
	round(ids)
}
