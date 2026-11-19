test_that("filters only keep palettes that pass", {
	p = cols4all:::prep_table("cat", n = 4, filters = "cbf", verbose = FALSE, include_tritan = FALSE)
	expect_gt(nrow(p$zn), 0)
	expect_true(all(p$zn$cbfriendly >= 1))
	p2 = cols4all:::prep_table("seq", n = 7, filters = c("cbf", "fair"), verbose = FALSE)
	if (!is.null(p2)) expect_true(all(p2$zn$cbfriendly >= 1 & p2$zn$fair == "H"))
})

test_that("an empty selection is explained", {
	ex = cols4all:::explain_no_palettes("cat", n = 7, series = "all", filters = "cbf", include_tritan = FALSE)
	expect_match(ex$title, "pass the filter \"Colorblind-friendly\"")
	expect_true(any(grepl("another number of colors", ex$details)))
	ex2 = cols4all:::explain_no_palettes("cat", n = 7, series = character(0))
	expect_match(ex2$title, "No palette series selected")
})

test_that("c4a_table works for all simulation options", {
	skip_if_not_installed("kableExtra")
	for (cvd in c("none", "bw", "deutan")) {
		expect_s3_class(c4a_table("cat", n = 5, series = "tol", cvd.sim = cvd, verbose = FALSE), "knitr_kable")
	}
	expect_s3_class(c4a_table("seq", n = 5, series = "tol", col.temp = 2200, verbose = FALSE), "knitr_kable")
	expect_message(x <- c4a_table("cat", n = 7, filters = "cbf"), "None of the")
	expect_null(x)
})
