test_that("c4a_gui links to the website with the settings", {
	expect_equal(c4a_gui(browse = FALSE), "https://cols4all.org/")
	expect_equal(c4a_gui("seq", n = 5, series = c("brewer", "hcl"), browse = FALSE),
				 "https://cols4all.org/?type=seq&n=5&series=brewer,hcl")
})

test_that("c4a_gui adds own palettes to the link", {
	skip_if_not_installed("colorblindcheck")
	C4A = cols4all:::.C4A
	old = mget(c("z", "s", "zbib", "zdes"), envir = C4A)
	on.exit({
		for (nm in names(old)) assign(nm, old[[nm]], envir = C4A)
		cols4all:::attach_bib()
		cols4all:::fill_P()
	})
	x = c4a_data(list(okabe = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
					  greens = c("#F7FCF5", "#74C476", "#00441B")), types = c("cat", "seq"), series = "mine")
	c4a_load(x)
	x2 = c4a_data(list(pu_gn = c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6", "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")),
				  types = "bivs", series = "mine", biv.method = "byrow3")
	c4a_load(x2)
	expect_message(url <- c4a_gui(series = "mine", browse = FALSE), "Nothing is uploaded")
	expect_equal(url, paste0("https://cols4all.org/?series=",
		"&my=cat.mine_okabe.E69F00.56B4E9.009E73.F0E442.0072B2.D55E00.CC79A7",
		"&my=seq.mine_greens.F7FCF5.74C476.00441B",
		"&my=bivs.mine_pu_gn.3.E8E8E8.ACE4E4.5AC8C8.DFB0D6.A5ADD3.5698B9.BE64AC.8C62AA.3B4994"))
})
