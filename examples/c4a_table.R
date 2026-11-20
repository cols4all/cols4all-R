if (interactive()) {
# open the cols4all website
c4a_gui()

# sequential palettes with 5 colors of two series
c4a_gui(type = "seq", n = 5, series = c("brewer", "scico"))
}

if (requireNamespace("colorblindcheck") && interactive()) {
# test an own palette: it is added to the website
x = c4a_data(list(my_pal = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")),
	types = "cat", series = "mine")
c4a_load(x)
c4a_gui()
}

if (requireNamespace("kableExtra") && interactive()) {
# categorical palettes with maximum number of colors
c4a_table(type = "cat")

# sort sequential palettes by hue
c4a_table(type = "seq", n = 7, sort = "H")

# sort sequential palettes by hue type (how many hues are used)
c4a_table(type = "seq", n = 5, sort = "hues")
}
