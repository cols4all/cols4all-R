test_that("color vision simulations return valid colors", {
	p = c4a("tol.bright")
	for (cvd in c("none", "bw", "deutan", "protan", "tritan")) {
		x = cols4all:::sim_cvd(p, cvd)
		expect_length(x, length(p))
		expect_match(x, "^#[0-9A-F]{6}$")
	}
	expect_equal(cols4all:::sim_cvd(p, "none"), p)
})

test_that("color temperature maps white to the perceived white", {
	expect_equal(cols4all:::sim_coltemp("#FFFFFF", "2200"), "#ECD6B3")
	expect_equal(cols4all:::sim_coltemp("#000000", "2200"), "#000000")
	expect_equal(cols4all:::sim_coltemp(c("#4477AA", "#EE6677"), "6500"), c("#4477AA", "#EE6677"))
	expect_error(cols4all:::sim_coltemp("#FFFFFF", "3000"), "should be one of")
	# black and white combined with color temperature
	expect_length(cols4all:::sim_cvd(c4a("tol.bright"), "bw", coltemp = "2700"), 7)
})
