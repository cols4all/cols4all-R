test_that("c4a returns palettes", {
	p = c4a("brewer.set1", 5)
	expect_length(p, 5)
	expect_match(p, "^#[0-9A-F]{6}$")
	expect_equal(rev(c4a("brewer.set1", 5)), c4a("-brewer.set1", 5))
	expect_length(c4a("tol.bright", 7), 7)
})

test_that("palette names are resolved", {
	for (nm in c("Greys", "RdYlGn", "Set1", "Spectral", "YlGnBu")) {
		expect_type(c4a_info(nm, verbose = FALSE)$fullname, "character")
	}
	expect_null(suppressMessages(c4a_info("no_such_palette_xyz", no.match = "null")))
})

test_that("renamed palettes still work, with a message", {
	expect_message(p <- c4a("tableau.red_blue_brown", 3), "renamed")
	expect_equal(p, c4a("tableau.blue_red_brown", 3))
	expect_silent(c4a_info("tableau.red_blue_brown", verbose = FALSE))
})

test_that("scico palettes of version 9 are included", {
	pals = c4a_palettes(series = "scico")
	expect_true(all(c("scico.delphi", "scico.ston", "scico.batlow_o") %in% pals))
})
