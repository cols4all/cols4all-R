test_that("palette_prob_bg returns a symmetric percentage matrix", {
	p = c4a("tol.bright")
	m = palette_prob_bg(p, thickness = 0.05)
	expect_equal(dim(m), c(7, 7))
	expect_true(all(is.na(diag(m))))
	expect_true(all(m[!is.na(m)] >= 0 & m[!is.na(m)] <= 100))
	expect_equal(unname(m), unname(t(m)))
})

test_that("point sizes below the validated range are clamped with a warning", {
	p = c4a("tol.bright")
	expect_warning(m1 <- palette_prob_bg(p, thickness = 0.15, mark = "point"), "only valid")
	m2 = palette_prob_bg(p, thickness = 0.25, mark = "point")
	expect_equal(m1, m2)
	# green and purple are easy to tell apart
	expect_equal(m2[3, 6], 100)
})

test_that("line and point models are monotone in size", {
	pl = line_discrim_prob("#4477AA", "#66CCEE", thickness = c(0.05, 0.1, 0.2))$p_discriminable
	expect_true(all(diff(pl) >= 0))
	pp = point_discrim_prob("#4477AA", "#66CCEE", size = c(0.25, 0.5, 1))$p_discriminable
	expect_true(all(diff(pp) >= 0))
})
