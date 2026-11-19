# Display color temperature simulation (c4a_gui() and c4a_table())
#
# A display set to a warmer white point (e.g. night mode) physically emits
# much warmer light than viewers perceive: they partly adapt to the new white.
# Instead of the physical shift, colors are mapped such that white becomes the
# page background color of the GUI for that setting (body.temp-XXXX in
# inst/www/light.css), which approximates the perceived (partly adapted) white.
# Other colors follow via a von Kries transform in Bradford cone space, so the
# palette colors and the page background are consistent.

# Perceived white per display color temperature; keep in sync with inst/www/light.css
.coltemp_white = c(`6500` = "#FFFFFF",
				   `4000` = "#F4F1EA",
				   `2700` = "#F3E6D3",
				   `2200` = "#ECD6B3")

.bradford = matrix(c( 0.8951,  0.2664, -0.1614,
					 -0.7502,  1.7135,  0.0367,
					  0.0389, -0.0685,  1.0296), nrow = 3, byrow = TRUE)

sim_coltemp = function(cols, coltemp = "6500") {
	coltemp = as.character(coltemp)
	if (!(coltemp %in% names(.coltemp_white))) stop("color temperature should be one of ", paste(names(.coltemp_white), collapse = ", "), call. = FALSE)
	if (coltemp == "6500") return(cols)

	to_xyz = function(x) colorspace::coords(as(colorspace::hex2RGB(x), "XYZ"))

	# von Kries in Bradford cone space: from display white to perceived white
	gains = (.bradford %*% to_xyz(.coltemp_white[coltemp])[1, ]) / (.bradford %*% to_xyz("#FFFFFF")[1, ])
	M = solve(.bradford) %*% diag(as.vector(gains)) %*% .bradford

	xyz = to_xyz(cols) %*% t(M)
	rgb = colorspace::coords(as(colorspace::XYZ(xyz), "RGB"))
	colorspace::hex(colorspace::RGB(pmin(pmax(rgb, 0), 1)))
}
