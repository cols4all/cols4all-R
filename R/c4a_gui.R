def_n = function(npref = NA, type, series, tab_nmin, tab_nmax) {

	nmin = suppressWarnings(min(tab_nmin[series, type], na.rm = TRUE))
	nmax = suppressWarnings(max(tab_nmax[series, type], na.rm = TRUE))
	if (is.infinite(nmin)) return(NULL)

	if (is.na(npref)) npref = switch(type, cat = 7, seq = 7, div = 9, 3)
	if (is.infinite(nmax)) nmax = 15

	if (type == "div") {
		if (nmin < 3) nmin = 3
		if (npref < 3) npref = 3
	} else {
		if (nmin < 2) nmin = 2
	}

	n = if (is.infinite(nmin)) {
		npref # should not happen or else throw a message elsewhere
	} else if (nmin <= npref && nmax >= npref) {
		npref
	} else if (is.infinite(nmax)) {
		nmin
	} else {
		nmax
	}
	list(n = n, nmin = nmin, nmax = nmax)
}

check_installed_packages = function(packages) {
	x = vapply(packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)

	r = packages[!x]

	if (any(!x)) {
		message(paste0("The package",
		   ifelse(sum(!x) > 1, "s ", " "),
		   paste(r, collapse = ", "),
		   ifelse(sum(!x) > 1, " are", " is"),
		   " required. Please install ",
		   ifelse(sum(!x) > 1, "them", "it"),
		   " with:\ninstall.packages(",
		   ifelse(sum(!x) > 1,
		   	   paste0("c(\"", paste(r, collapse = "\", \""), "\"))"),
		   	   paste0("\"", paste(r, collapse = "\", \""), "\")"))))

		FALSE
	} else {
		TRUE
	}
}

h4title = function(title, inline = FALSE) shiny::HTML(paste0("<h4 style='font-weight: bold;", {if (inline) "display: inline;" else ""}, "'>", title ,"</h4>"))

# Filter choices of the overview table, per palette type
gui_filters = function(type) {
	filters = structure(names(.filter_labels), names = unname(.filter_labels))
	if (type == "cat") filters else filters[filters %in% c("cbf", "fair", "crW", "crB")]
}

#' @rdname c4a_gui
#' @name c4a_gui
#' @export
c4a_gui = function(type = "cat", n = NA, series = "all") {
	ani_off = shiny::icon("circle-xmark", "fa-2x")
	ani_on = shiny::icon("circle-info", "fa-2x")

	if (!check_installed_packages(c("shiny", "shinyjs", "kableExtra", "colorblindcheck", "plotly"))) return(invisible(NULL))


	shiny::addResourcePath(prefix = "imgResources", directoryPath = system.file("img", package = "cols4all"))

	#############################
	## Catelogue tab
	#############################

	z = .C4A$z

	if (is.null(z)) stop("No palette data found. Either load data (c4a_data) or reload cols4all")

	z = z[order(z$fullname), ]

	tps = unname(.C4A$types)

	tab_nmin = tapply(z$nmin, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = min)
	tab_nmax = tapply(z$nmax, INDEX = list(z$series, factor(z$type, levels = tps)), FUN = max)
	tab_k = c4a_overview(zero.count.as.NA = TRUE)


	allseries = sort(unique(z$series))
	if (series[1] == "all") {
		series = allseries
	} else {
		if (!all(series %in% allseries)) message("These series do not exist: \"", paste(setdiff(series, allseries), collapse = "\", \""), "\"")
		series = intersect(series, allseries)
	}
	if (!length(series)) {
		message("No palette series to show. Either restart c4a_gui with different parameters, add palette data with c4a_load or c4a_sysdata_import")
		return(invisible(NULL))
	}

	types_available = names(which(apply(tab_nmin, MARGIN = 2, function(x)any(!is.na(x)))))

	stopifnot(length(types_available) > 0L)
	if (!(type %in% types_available)) {
		warning("type \"", type, "\" is not available/known")
		type = types_available[1]
	}


	ns = def_n(npref = n, type, series, tab_nmin, tab_nmax)

	types = .C4A$types

	types1 = .C4A$types1
	types2 = .C4A$types2

	if (nchar(type) == 3) {
		type1 = type
		type2 = if (type %in% names(types2)) unname(types2[[type]][1]) else ""
	} else {
		type2 = type
		type1 = substr(type, 1, 3)
	}
	type12 = paste0(type1, type2)

	init_pal_list = z$fullname[z$type == type12]


	# Book icon that shows `ref` (HTML, e.g. a citation) in a popover
	refIcon = function(ref, title = "Reference", size = "fa-lg") {
		shiny::tags$a(shiny::icon("book", size), tabindex = "0", role = "button", class = "ref-popover",
					  style = "margin-left: 2px; color: inherit; cursor: pointer;", title = title,
					  `data-toggle` = "popover", `data-trigger` = "click", `data-placement` = "bottom",
					  `data-html` = "true", `data-content` = ref)
	}

	# ref: optional HTML (e.g. a citation), shown in a popover via a book icon
	infoBoxUI = function(inp = NULL, title, ref = NULL) {
		ref_icon = if (!is.null(ref)) refIcon(ref)
		if (is.null(inp)) {
			# padding to compensate for button
			shiny::div(style="display: inline-block; padding: 5px;", h4title(title, inline = TRUE), ref_icon)
		} else {
			shiny::div(style="display: inline-block", h4title(title, inline = TRUE), shiny::actionButton(inp, "", ani_on, style = "border: none;"), ref_icon)
		}
	}

	ref_szafir = paste0("The percentages are the expected share of people that can tell two lines or points of the chosen size apart, ",
						"according to the model of:<br><br>",
						"Szafir, D.A. (2018). Modeling Color Difference for Visualization Design. ",
						"<i>IEEE Transactions on Visualization and Computer Graphics</i>, 24(1), 392-401. ",
						"<a href='https://doi.org/10.1109/TVCG.2017.2744359' target='_blank'>doi:10.1109/TVCG.2017.2744359</a>")

	ref_coltemp = paste0("Does your palette still work in warm light? ",
						 "Warm light contains less blue, so under lamp or candle light colors are harder to tell apart than in daylight, ",
						 "especially blues and light yellows. Pick a warmer setting and check which colors start to look alike.<br><br>",
						 "<i>This is an approximation that takes into account that your eyes partly adjust to warm light. ",
						 "Details: <code>?c4a_table</code>.</i>")
	plotOverlay = function(outputId, width, height, id, click = NULL) {
		shiny::div(style = paste0("position: relative; width: ", width, "; height: ", height, ";"),
				   shiny::plotOutput(outputId = outputId, width = width, height = height, click = click),
				   shiny::img(id = id, class = "hide", src = "", style = "pointer-events: none; position: absolute; left: 0px; right: 0px"))
	}

	ui = shiny::fluidPage(
		shinyjs::useShinyjs(),
		# reference popovers (see infoBoxUI); close when clicking elsewhere
		shiny::tags$script(shiny::HTML("
			$(function() {
				$('[data-toggle=\"popover\"]').popover({container: 'body'});
				$(document).on('click', function(e) {
					if ($(e.target).closest('.ref-popover, .popover').length === 0) $('.ref-popover').popover('hide');
				});
			});
		")),
		shiny::tags$head(shiny::includeCSS(system.file("www/light.css", package = "cols4all"))),
		shiny::tags$head(shiny::includeCSS(system.file("www/misc.css", package = "cols4all"))),
		shiny::tags$div(
			id = "app",
		shiny::absolutePanel(
			top = 25,
			right = 40,
			width = 90,
			shiny::checkboxInput("dark", "Dark mode", value = FALSE)),
		# shiny::absolutePanel(
		# 	top = 10,
		# 	right = 10,
		# 	width = 20,
		# 	shiny::actionButton("info", "", shiny::icon("info"),
		# 						style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
		# ),
		shiny::tabsetPanel(
			id="inTabset",
			shiny::tabPanel("Overview",
							value = "tab_catel",
					 shiny::fluidRow(
					 	shiny::column(width = 3,
					 				  shiny::img(src = "imgResources/cols4all_logo.png", height="200", class = "logo", align = "center", 'vertical-align' = "center")),
					 	shiny::column(width = 3,
					 				  shiny::radioButtons("type1", "Palette type", choices = types1, selected = type1),
					 				   				  				  shiny::conditionalPanel(
					 				   				  				  	condition = "input.type1 == 'biv'",
					 				   				  				  	shiny::selectizeInput("type2", "Subtype", choices = types2[["biv"]], selected = type2)),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 != 'biv'",
					 				  	shiny::sliderInput("n", "Number of colors", min = ns$nmin, max = ns$nmax, value = ns$n, ticks = FALSE)),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 == 'seq' || input.type1 == 'div' || input.type1 == 'cyc'",
					 				  	shiny::checkboxInput("continuous", "Show as continuous palette", value = FALSE)
					 				  ),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 == 'biv'",
					 				  	shiny::fluidRow(
					 				  		shiny::column(6,
					 				  					  shiny::sliderInput("nbiv", "Number of columns", min = 2, max = ifelse(type12 == "bivc", 10, 7), value = 3, ticks = FALSE)),
					 				  		shiny::column(6,
					 				  					  shinyjs::disabled(shiny::sliderInput("mbiv", "Number of rows", min = 3, max = 7, value = 3, ticks = FALSE))))),
					 				  shiny::checkboxInput("na", "Color for missing values", value = FALSE),
					 				  shiny::uiOutput("sortcolorUI"),
					 				  shiny::conditionalPanel(
					 				  	condition = "input.type1 == 'seq' || input.type1 == 'div'",
					 				  	shiny::fluidRow(
					 				  		shiny::column(4,
					 				  					  #shiny::br(),
					 				  					  shiny::radioButtons("auto_range", label = "Range", choices = c("Automatic", "Manual"), selected = "Automatic")),
					 				  		shiny::conditionalPanel(
					 				  			condition = "input.auto_range == 'Manual'",
					 				  			shiny::column(8,
					 				  						  shiny::div(style = "font-size:0;margin-bottom:-10px", shiny::sliderInput("range", "", min = 0, max = 1, value = c(0,1), step = .05)),
					 				  						  shiny::uiOutput("range_info"))
					 				  		)
					 				  	))
					 	),

					 	shiny::column(width = 3,
					 				  # Static, not renderUI: get_cols()'s observer (below) already
					 				  # rebuilds this dropdown's full choice list type-aware, straight
					 				  # from table_columns() -- and it bails out if input$sort doesn't
					 				  # exist yet, which a renderUI version briefly wouldn't on first
					 				  # load, silently skipping that first population.
					 				  shiny::selectizeInput("sort", "Sort palettes", choices = c(Name = "name", Rank = "rank"), selected = "name"),
					 				  # A small gap here specifically: light.css zeroes .form-group/
					 				  # .checkbox margins everywhere (that's what keeps the filter
					 				  # checkboxes tight), but that leaves this checkbox flush against
					 				  # the selectize widget's own visible border/padding above it,
					 				  # which reads as cramped in a way checkbox-to-checkbox doesn't.
					 				  shiny::div(style = "margin-top: 8px;", shiny::checkboxInput("sortRev", "Reverse sorting", value = FALSE)),
					 				  # Only affects the "Colorblind-friendly" filter below (and,
					 				  # for types without a per-CVD column breakdown, the CBF
					 				  # badge) -- NOT the Separability columns in the categorical
					 				  # table, which always show all 4 (N/D/P/T) individually.
					 				  # Unchecked by default: tritan is ~400x rarer than deutan+
					 				  # protan combined (~1 in 10,000 vs ~4% of the population).
					 				  shiny::checkboxInput("include_tritan", "Include tritan in colorblind-friendly filter", value = FALSE),
					 				  #shiny::div(style = "margin-bottom: 5px;", shiny::strong("Select")),
					 				  shiny::div(class = "control-label2", "Filter"),
					 				  shiny::checkboxGroupInput("filters", label = "", choices = gui_filters(type12), inline = FALSE),

					 				  shiny::div(class = "control-label2", "Palette series"),

					 				  			 					  shiny::div(class = 'multicol',
					 				  			 					  		   shiny::checkboxGroupInput("series", label = "", choices = allseries, selected = series, inline = FALSE)),
					 				  			 					  shiny::fluidRow(
					 				  			 					  	shiny::column(12, align="right",
					 				  			 					  				  shiny::actionButton("all", label = "All"),
					 				  			 					  				  shiny::actionButton("none", label = "None"),
					 				  			 					  				  shiny::actionButton("overview", label = "Overview")))


					 				  #shiny::
					 				  ),
					 	shiny::column(width = 3,
					 				  shiny::radioButtons("coltemp", shiny::tagList("Display temperature (approx.)", refIcon(ref_coltemp, title = "About this simulation", size = NULL)), choices = c("6500 K (standard)" = "6500", "4000 K" = "4000", "2700 K" = "2700", "2200 K (e.g. night mode)" = "2200"), selected = "6500"),
					 				  shiny::radioButtons("cvd", "Color vision", choices = c(Normal = "none", "Black & white" = "bw", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none"),
					 				  shiny::radioButtons("textformat", "Text", choices = c("None" = "none", "Hex" = "hex", "RGB" = "RGB", "HCL" = "HCL"), inline = T),
					 				  shiny::div(class = "control-label3", "Underlying scores"),
					 				  shiny::div(style = "margin-top: 5px;", shiny::checkboxInput("advanced", "Show scores", value = FALSE))
					 				  ),

					 	),

					 shiny::fluidRow(
					 	shiny::column(
					 		width = 12,
					 		shiny::uiOutput("show"))
					 )
			),
			shiny::tabPanel("Color Discrimination",
							value = "tab_cvd",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("cbfPal", "Palette", choices = init_pal_list),
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoSimu", "Color blindness simulation"),
											  plotOverlay("cbfSimu", width = "800px", height = "150px", "aniSimu"))),
							# "Separability" (infoSimi) + chart type/size/numbers sit once, in the
							# same row as "Hue lines" -- not repeated per CVD-state row -- so the
							# four Hue-lines/matrix/example row-groups below stay vertically
							# aligned across all three columns, and there's no separate
							# header-only row above column 1 creating whitespace there.
							shiny::fluidRow(
								shiny::column(width = 4,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoHueLines", "Hue lines")),
								shiny::column(width = 6,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoSimi", "Separability", ref = ref_szafir),
											  shiny::tags$div(style = "margin-top:8px; max-width:220px;",
											  	shiny::radioButtons("cbfType", NULL, choices = c("Lines", "Points"), inline = TRUE),
											  	shiny::tags$div(style = "margin-top:10px",
											  	shiny::conditionalPanel(
											  		condition = "input.cbfType == 'Lines'",
											  		shiny::sliderInput("cbfLineSize", NULL, min = 0.05, max = 0.2, value = 0.05, step = 0.05, post = "\u00b0", ticks = TRUE)
											  	),
											  	shiny::conditionalPanel(
											  		condition = "input.cbfType == 'Points'",
											  		# Starts at the validated 0.25deg floor of Szafir's point model: below
											  		# ~0.16deg its a*/b* slopes go negative, so chromatic differences count
											  		# for (almost) nothing (see palette_prob_bg.R)
											  		shiny::sliderInput("cbfPointSize", NULL, min = 0.25, max = 0.5, value = 0.25, step = 0.05, post = "\u00b0", ticks = TRUE)
											  	)),
											  	shiny::checkboxInput("cbfShowNumbers", "Show percentage numbers", value = FALSE)
											  )),
								# Screen calibration for the line/point example charts below
								# (cbf_ex1, same 150px-wide column): two blank/optional inputs,
								# same unit (whichever the user likes -- only their ratio is
								# used, see cbf_px_per_deg()), plus a reference line rendered at
								# a known, fixed 150px so it can be measured against a physical
								# ruler. Uncalibrated (either field blank) falls back to
								# visual_angle_to_px()'s existing 30in/96dpi rule of thumb.
								shiny::column(width = 2,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI(title = "Calibration"),
											  shiny::div(style = "margin-top:8px;",
											  	shiny::numericInput("calibViewDist", "Viewing distance", value = 650, min = 0),
											  	shiny::div(style = "margin-top:8px;", shiny::numericInput("calibRefLen", "Ruler measurement", value = 27, min = 0)),
											  	shiny::tags$div(style = "width:150px; height:0; border-top:3px solid currentColor; margin-top:10px;"),
											  	shiny::tags$div(style = "font-size:85%; color:#888; margin-top:4px;", "Measure with a ruler (same unit as viewing distance).")
											  ))),
							# Background Adjustment (bc_adj) is temporarily removed from the UI --
							# we don't yet have a validated model for it (see palette_prob_bg()'s
							# lum_adjust caveats). The plumbing (bc_adj param, throughout
							# plot_matrix()/get_prob_matrix()) is untouched, just hardcoded to
							# FALSE below, so re-adding the checkbox later is a one-line change.
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("Normal color vision"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfHL", width = "375px", height = "375px", "aniHL")),
											shiny::column(width = 6, plotOverlay("cbfSimi", width = "500px", height = "375px", "aniSimi", click = "cbfSimi_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex1", height = "375px", width = "150px"))),
							shiny::fluidRow(
								shiny::column(width = 4,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoConf", "Confusion lines")),
								shiny::column(width = 6,
											  shiny::br(),
											  shiny::br(),
											  infoBoxUI("infoPSimi", "Separability (perceived)"))),
								#shiny::column(width = 6, shiny::markdown("<br/><br/>
					  #### **Distance matrices**"))),
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("Deutan (red-green blind)"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfCL1", width = "375px", height = "375px", "aniCL1")),
											shiny::column(width = 6, plotOverlay("cbfPSimi1", width = "500px", height = "375px", "aniPSimi1", click = "cbfPSimi1_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex2", height = "375px", width = "150px"))),
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("<br/><br/>Protan (also red-green blind)"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfCL2", width = "375px", height = "375px", "aniCL2")),
											shiny::column(width = 6, plotOverlay("cbfPSimi2", width = "500px", height = "375px", "aniPSimi2", click = "cbfPSimi2_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex3", height = "375px", width = "150px"))),
							shiny::fluidRow(shiny::column(width = 12, shiny::markdown("<br/><br/>Tritan (blue-yellow)"))),
							shiny::fluidRow(shiny::column(width = 4, plotOverlay("cbfCL3", width = "375px", height = "375px", "aniCL3")),
											shiny::column(width = 6, plotOverlay("cbfPSimi3", width = "500px", height = "375px", "aniPSimi3", click = "cbfPSimi3_click")),
											shiny::column(width = 2, shiny::plotOutput("cbf_ex4", height = "375px", width = "150px")))),
			shiny::tabPanel("HCL Analysis",
							value = "tab_cl",

							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("CLPal", "Palette", choices = init_pal_list))),
							shiny::fluidRow(
								shiny::column(width = 4,
											  infoBoxUI(title = "HCL space"),
											  shiny::img(src = "imgResources/hcl_spacex1.png", srcset = "imgResources/hcl_spacex1.png 1x, imgResources/hcl_spacex2.png 2x"),
											  shiny::sliderInput("rangeH", min = 0, max = 360, value = c(0, 360), step = 10, label = "Hue"),
											  shiny::sliderInput("rangeC", min = 0, max = 180, value = c(0, 180), step = 10, label = "Chroma"),
											  shiny::sliderInput("rangeL", min = 0, max = 100, value = c(0, 100), step = 10, label = "Luminance"),
											  shiny::checkboxInput("hclspacepal", "Palette colors", FALSE)),
								shiny::column(width = 8,
											  plotly::plotlyOutput("hclspace", height = "600px"))
											  ),
							shiny::fluidRow(
								shiny::column(width = 6,
											  infoBoxUI("infoHUE", "Hue necklace"),
											  plotOverlay("anaHUE", width = "400px", height = "400px", "aniHUE"))),
							shiny::fluidRow(
								shiny::column(width = 6,
											  infoBoxUI("infoCL", "Chroma-Luminance"),
											  plotOverlay("anaCL", width = "600px", height = "600px", "aniCL")),
								shiny::column(width = 6,
											  infoBoxUI("infoFair", "Fairness"),
											  plotOverlay("anaFair", width = "600px", height = "600px", "aniFair")))),

			shiny::tabPanel("Contrast",
							value = "tab_cont",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("contrastPal", "Palette", choices = init_pal_list))),
				 	shiny::fluidRow(
				 		shiny::column(width = 6,
				 					  shiny::uiOutput("cr_title"),
				 					  shiny::plotOutput("cr_plot")),
				 		shiny::column(width = 6,
				 					  shiny::markdown("**Options**"),
				 					  shiny::checkboxGroupInput("cr_plot_opts", "", c("Sort" = "sort", "Show WCAG criteria" = "wcag", "Show equiluminant groups" = "equi"), selected = ""))),
			 		shiny::fluidRow(
			 			shiny::column(width = 6,
			 						  infoBoxUI("infoCR", "Contrast ratios between colors"),
				 					  plotOverlay("table", width = "400px", height = "300px", "aniTable", click = "table_click")),
				 		shiny::column(width = 6,
	 					  shiny::markdown("<br></br>
#### **Text readability**
"),
	 					  shiny::uiOutput("textCR"),
	 					  shiny::plotOutput("textPlot", height = "200", width = "400")
				 		)),

				 	shiny::fluidRow(
				 		shiny::column(width = 12,
				 					  shiny::markdown("<br/><br/>
				 					  #### **Border lines needed?**
				 					  "),
				 					  shiny::uiOutput("bordersCR"))),
				 	shiny::fluidRow(
				 		shiny::column(width = 3,
				 					  shiny::markdown(""),
				 					  shiny::radioButtons("chart", "Example chart", c("Choropleth", "Barchart"), "Choropleth", inline = FALSE),
			 					  	  shiny::selectizeInput("borders", "Borders", choices = c("black", "white"), selected = "black"),
				 					  shiny::sliderInput("lwd", "", min = 0, max = 3, step = 1, value = 0)),
				 		shiny::column(
				 			width = 9,
				 			shiny::plotOutput("ex", height = "300px", width = "600px")
				 		)

				 	),
				 	shiny::fluidRow(
				 		shiny::column(width = 12,
				 					  shiny::markdown("**Optical Illusion Art**"),
				 					  shiny::plotOutput("ex_plus", height = "703", width = "900"),
				 					  shiny::markdown("<br/><br/>_Plus Reversed_ by Richard Anuszkiewicz (1960)"),
				 					  shiny::checkboxInput("plus_rev_original", "Use optical illusion's original colors", value = FALSE),
				 		)
					)

			),
			shiny::tabPanel("3D Blues",
							value = "tab_floating",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("floatPal", "Palette", choices = init_pal_list))),

							shiny::fluidRow(
								shiny::column(width = 8,
											  infoBoxUI("infoBlues", "Chromostereopsis"),
											  plotOverlay("blues", width = "550px", height = "550px", "aniBlues"),
											  shiny::markdown("<br/><br/>[_Visual illusion by Michael Bach_](https://michaelbach.de/ot/col-chromostereopsis/)"),
											  shiny::checkboxInput("float_original", "Use optical illusion's original colors", value = FALSE),
											  shiny::checkboxInput("float_rev", "Reverse colors", value = FALSE)),
								shiny::column(width = 4,
											  shiny::plotOutput("float_letters", "Float letter", height = 80, width = 300),
											  shiny::uiOutput("float_selection"),
											  shiny::plotOutput("float_letters_AB", "Float letter", height = 150, width = 300)
											  ))),

			shiny::tabPanel("Naming",
							value = "tab_name",
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::selectizeInput("namePal", "Palette", choices = init_pal_list),
											  infoBoxUI("infoName", "Naming table (in development)"),
											  plotOverlay("anaName", width = "1000px", height = "600px", "aniName"))),
							shiny::fluidRow(
								shiny::column(width = 12,
											  shiny::sliderInput("nameAlpha", "Clarity level", min = .5, max = .C4A$naming_softmax$a + 2, step = .5, value = .C4A$naming_softmax$a)))),


			shiny::tabPanel("Application",
							value = "tab_app",
				shiny::wellPanel(
				shiny::fluidRow(
					shiny::column(width = 4,
								  shiny::selectizeInput("APPPal", "Palette", choices = init_pal_list)),
					shiny::column(width = 4,
								  shiny::selectizeInput("APPcvd", "Color vision deficiency", choices = c(Normal = "none", "Black & white" = "bw", 'Deutan (red-green blind)' = "deutan", 'Protan (also red-green blind)' = "protan", 'Tritan (blue-yellow)' = "tritan"), selected = "none")))),

					h4title("Points"),
					shiny::fluidRow(
						shiny::column(width = 8, shiny::plotOutput("DOTplot", width = 800, height = 400)),
						shiny::column(width = 4,
									  shiny::radioButtons("DOTdist", "Color distribution", choices = c(Random = "random", Concentric = "concentric"), selected = "random"),
									  shiny::sliderInput("DOTsize", "", min = .5, max = 5, step = .5, value = 1),
									  shiny::selectizeInput("DOTborders", "Borders", choices = c("black", "white"), selected = "black"),
									  shiny::sliderInput("DOTlwd", "", min = 0, max = 3, step = 1, value = 0))),
					h4title("Lines"),
					shiny::fluidRow(
						shiny::column(width = 8, shiny::plotOutput("LINEplot", width = 800, height = 400)),
						shiny::column(width = 4,
									  shiny::checkboxInput("LINEstack", "Stacked", value = FALSE),
									  shiny::sliderInput("LINElwd", "Line width", min = 1, max = 5, step = 1, value = 3))),

					h4title("Polygons"),
					shiny::fluidRow(
						shiny::column(width = 8, shiny::plotOutput("MAPplot", width = 800, height = 400)),
						shiny::column(width = 4, shiny::radioButtons("MAPdist", "Color distribution", choices = c(Random = "random", Gradient = "gradient"), selected = "random"),
									  shiny::selectizeInput("MAPborders", "Borders", choices = c("black", "white"), selected = "black"),
									  shiny::sliderInput("MAPlwd", "", min = 0, max = 3, step = 1, value = 1)
						)),
					h4title("Text"),
						shiny::fluidRow(
							shiny::column(width = 8,
										  shiny::plotOutput("TXTplot1", width = 800, height = 120),
										  shiny::plotOutput("TXTplot2", "Text", width = 800, height = 120)),
							shiny::column(width = 4,
										  shiny::sliderInput("TXTfsize", "Font size", min = 4, max = 72, step = 1, value = 12),
										  shiny::radioButtons("TXTfface", "Font face", choices = c(Plain = "plain", Bold = "bold", Italic = "italic")))),

		)
	)
	))


	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	##############################################################                                             ######################################################################
	##############################################################           Server                            ######################################################################
	##############################################################                                             ######################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################
	#################################################################################################################################################################################


	server = function(input, output, session) {
		#############################
		## Catelogue tab
		#############################

		series_d = shiny::debounce(shiny::reactive(input$series), 300)

		get_type12 = shiny::reactive({
			type1 = input$type1
			type12 = if (type1 %in% names(types2)) {
				input$type2
			} else {
				type1
			}
		})

		anno = shiny::reactiveValues(simu = FALSE, hue_lines = FALSE, cvd = FALSE, simi = FALSE, psimi = FALSE, conf_lines = FALSE, hue_neck = FALSE, cl_plot = FALSE, fair_plot = FALSE, naming = FALSE, cr = FALSE, blues = FALSE)

		# Selected palette: set by the overview table (via get_prep()) and by the palette selectors of the other tabs
		tab_vals = shiny::reactiveValues(pal = character(0), pal_name = character(0), pals = NULL, pals_byname = NULL,
										 n = NA, m = NA, na = FALSE, type = type12)

		shiny::observeEvent(input$all, {
			shiny::freezeReactiveValue(input, "series")
			shiny::updateCheckboxGroupInput(session, "series", selected = allseries)
		})

		shiny::observeEvent(input$none, {
			shiny::freezeReactiveValue(input, "series")
			shiny::updateCheckboxGroupInput(session, "series", selected = character())
		})

		shiny::observeEvent(get_cols(), {
			if (is.null(input$sort)) return(NULL)
			cols = get_cols()
			sort = shiny::isolate(input$sort)
			shiny::freezeReactiveValue(input, "sort")
			sortNew = if (sort %in% cols) sort else "name"
			shiny::updateSelectizeInput(session, "sort", choices  = cols,selected = sortNew)
		})

		shiny::observeEvent(input$dark, {

			if ( ! input$dark ) {
				shinyjs::removeClass(selector = "body", class = "dark")
			} else {
				shinyjs::addClass(selector = "body", class = "dark")
			}

		})

		shiny::observeEvent(input$coltemp, {
			shinyjs::runjs(sprintf("
			    document.body.classList.remove('temp-6500','temp-4000','temp-2700','temp-2200');
			    document.body.classList.add('temp-%s');
			  ", input$coltemp))

		})

		shiny::observeEvent(get_type12(), {
			type = get_type12()

			if (!(type %in% types_available)) return(NULL)

			# keep the selected filters that are still available for this type
			fltrs = gui_filters(type)
			shiny::freezeReactiveValue(input, "filters")
			shiny::updateCheckboxGroupInput(session, "filters", choices = fltrs, selected = intersect(shiny::isolate(input$filters), fltrs))

			if (is.null(input$n)) return(NULL)

			if (type %in% c("cat", "seq", "div", "cyc")) {
				series = series_d()
				if (is.null(series)) return(NULL)

				ns =  def_n(npref = input$n, type, series, tab_nmin, tab_nmax)
				shiny::freezeReactiveValue(input, "n")
				shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
			} else {
				ndef = unname(.C4A$ndef[type])

				mdef = unname(.C4A$mdef[type])
				if (is.na(mdef)) {
					shinyjs::disable("mbiv")
				} else {
					shinyjs::enable("mbiv")
				}
				nmin = if (type == "bivd") 3 else 2
				nmax = if (type == "bivc") 10 else 7
				nstep = if (type == "bivd") 2 else 1

				if (is.na(mdef)) mdef = ndef

				if (is.infinite(ndef) || is.infinite(mdef)) return(NULL)

				shiny::freezeReactiveValue(input, "nbiv")
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "nbiv", value = ndef, min = nmin, max = nmax, step = nstep)
				shiny::updateSliderInput(session, "mbiv", value = mdef, min = nmin, step = nstep)
			}
		})


		shiny::observeEvent(series_d(), {
			type = get_type12()

			if (!(type %in% c("cat", "seq", "div", "cyc"))) return(NULL)

			series = series_d()

			if (is.null(series)) return(NULL)
			ns =  def_n(npref = input$n, type, series, tab_nmin, tab_nmax)
			if (is.null(ns)) return(NULL)
			shiny::freezeReactiveValue(input, "n")
			shiny::updateSliderInput(session, "n", min = ns$nmin, max = ns$nmax, value = ns$n)
		})

		shiny::observeEvent(input$overview, {
			shiny::showModal(shiny::modalDialog(title = "Number of palettes per series (rows) and type (columns)",
												shiny::renderTable(tab_k, na = "", striped = TRUE, hover = TRUE, bordered = TRUE),
												shiny::div(style="font-size: 75%;", shiny::renderTable(.C4A$type_info)),
												footer = shiny::modalButton("Close"),
												style = "color: #000000;",
												size = "l"))
		})

		get_cols = shiny::reactive({
			type = get_type12()
			res = table_columns(type, input$advanced)


			xl = c(res$ql, res$sl)
			xn = c(res$qn, res$sn)

			# Sort-only extras (e.g. cat/bivc's two "overall" separability
			# aggregates) stay choosable here even when "Show scores" is off
			# and they have no visible column of their own -- see
			# table_columns()'s qn_sort/qs_sort.
			extra_n = setdiff(res$qn_sort, res$qn)
			if (length(extra_n)) {
				xn = c(xn, extra_n)
				xl = c(xl, gsub("&nbsp;", "", .C4A$labels[extra_n]))
			}

			anyD = duplicated(xl)
			structure(c("name", xn[!anyD]), names = c("Name", xl[!anyD]))
		})

		# All table settings, as plain values. Used as cache key for both
		# get_prep() and output$show
		get_values = shiny::reactive({
			if (is.null(input$sort) || input$sort == "") return(NULL)
			type = get_type12()
			n = input$n
			series = series_d()
			if (is.null(n) || !length(series)) return(NULL)
			isbiv = substr(type, 1, 3) == "biv"

			list(type = type,
				 n = if (isbiv) input$nbiv else n,
				 m = if (isbiv) input$mbiv else NA,
				 continuous = !isbiv && isTRUE(input$continuous),
				 cvd = input$cvd,
				 coltemp = input$coltemp,
				 sort = paste0({if (input$sortRev) "-" else ""}, input$sort),
				 series = series,
				 show.scores = input$advanced,
				 include_tritan = isTRUE(input$include_tritan),
				 columns = if (isbiv) input$nbiv else if (n > 16) 12 else n,
				 na = input$na,
				 range = if (input$auto_range == "Automatic") NA else input$range,
				 filters = if (is.null(input$filters)) character(0) else input$filters,
				 textcol = if (input$textformat == "none") "same" else "auto",
				 format = if (input$textformat == "none") "hex" else input$textformat,
				 colorsort = if (is.null(input$sortcolor) || !(type %in% c("cat", "seq"))) "orig" else if (input$sortcolor == "H") paste0(input$sortcolor, input$Hstart) else input$sortcolor)
		})

		get_prep = shiny::bindCache(shiny::reactive({
			v = get_values()
			if (is.null(v)) return(NULL)
			if (substr(v$type, 1, 3) == "biv") {
				prep_table(type = v$type, n = v$n, m = v$m, sort = v$sort, series = v$series, range = v$range, colorsort = v$colorsort, show.scores = v$show.scores, columns = v$columns, verbose = FALSE, filters = v$filters, include_tritan = v$include_tritan)
			} else {
				prep_table(type = v$type, n = v$n, continuous = v$continuous, sort = v$sort, series = v$series, range = v$range, colorsort = v$colorsort, show.scores = v$show.scores, columns = v$columns, verbose = FALSE, filters = v$filters, include_tritan = v$include_tritan)
			}
		}), get_values()[c("type", "n", "m", "continuous", "sort", "series", "range", "colorsort", "show.scores", "columns", "filters", "include_tritan")])


		shiny::observeEvent(input$nbiv, {
			nbiv = input$nbiv
			type = get_type12()
			if (type == "bivs") {
				shiny::freezeReactiveValue(input, "mbiv")
				shiny::updateSliderInput(session, "mbiv", value = nbiv)
			}
		})

		output$sortcolorUI = shiny::renderUI({
			type = get_type12()
			if (type == "cat") {
				shiny::tagList(
					shiny::fluidRow(
						shiny::column(6,
									  shiny::radioButtons("sortcolor", "Sort colors", choices = c("Original" = "orig", Hue = "H", Chroma = "C", Luminance = "L"), inline = FALSE)),
						shiny::column(6,
									  shiny::conditionalPanel(
									  	condition = "input.sortcolor == 'H'",
									  	shiny::sliderInput("Hstart", "Hue start", min = 0, max = 360, step = 10, value = 0, ticks = FALSE, dragRange = FALSE)
									  )
						))
				)
			} else if (type == "seq") {
				shiny::radioButtons("sortcolor", "Sorting order", choices = c("Original" = "orig", "Light to dark" = "L"), inline = FALSE)
			} else {
				# div: not working well (because color sorting is based on whether H[25%] > H[75%] while palette sorting is based on HL, which is a by-product of Hwidth)
				NULL
			}
		})

		output$range_info = shiny::renderUI({
			type = get_type12()

			if (type == "div") {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Middle<span style='float:right;'>Both sides</span></div>")
			} else {
				shiny::HTML("<div style='font-size:70%; color:#111111; text-align:left;'>Left<span style='float:right;'>Right</span></div>")
			}
		})

		#############################
		## Selected palette (shared by all other tabs)
		#############################

		pal_inputs = c("cbfPal", "CLPal", "namePal", "contrastPal", "floatPal", "APPPal")

		pal_colors = function(pal_name, n, m, na) {
			cols = as.vector(c4a(pal_name, n = n, m = m))
			if (na) cols = c(cols, c4a_na(pal_name))
			cols
		}

		# Set the selected palette. The selected color pairs are only reset
		# when the colors have changed
		set_palette = function(pal_name, cols, type, na) {
			changed = !identical(cols, shiny::isolate(tab_vals$pal))
			tab_vals$pal_name = pal_name
			tab_vals$type = type
			tab_vals$na = na
			if (changed) {
				b = approx_blues(cols)
				r = approx_reds(cols)
				tab_vals$pal = cols
				tab_vals$palBW = unique(c(cols, "#FFFFFF", "#000000"))
				tab_vals$b = b
				tab_vals$r = r
				tab_vals$idA1 = 1L
				tab_vals$idA2 = 2L
				tab_vals$idB1 = 1L
				tab_vals$idB2 = 2L
				tab_vals$CR = colorspace::contrast_ratio(cols[1], cols[2])
				# colors (not indices) of the 3D Blues tab
				tab_vals$idC1 = cols[which.max(b)]
				tab_vals$idC2 = cols[which.max(r)]
			}
		}

		clear_palette = function() {
			tab_vals$pal = character(0)
			tab_vals$na = FALSE
			tab_vals$pals = character(0)
			tab_vals$pals_byname = character(0)
			tab_vals$pal_name = character(0)
			tab_vals$palBW = character(0)
			tab_vals$idA1 = integer(0)
			tab_vals$idA2 = integer(0)
			tab_vals$idB1 = integer(0)
			tab_vals$idB2 = integer(0)
			tab_vals$CR = numeric(0)
			tab_vals$idC1 = character(0)
			tab_vals$idC2 = character(0)
			tab_vals$b = numeric(0)
			tab_vals$r = numeric(0)
			tab_vals$type = character(0)
		}

		# Overview tab -> selected palette
		shiny::observe({
			values = get_values()
			prep = get_prep()
			pals = prep$zn$fullname

			shiny::isolate({
				if (!is.null(values) && length(pals)) {
					tab_vals$pals = pals
					tab_vals$pals_byname = sort(pals)
					tab_vals$n = values$n
					tab_vals$m = values$m

					pal_name = tab_vals$pal_name
					if (!length(pal_name) || !(pal_name %in% pals)) pal_name = pals[1]

					set_palette(pal_name, pal_colors(pal_name, values$n, values$m, values$na), values$type, values$na)
				} else {
					clear_palette()
				}
			})
		})

		# Palette choices of the other tabs
		shiny::observe({
			choices = tab_vals$pals_byname
			if (is.null(choices)) return(NULL)
			sel = shiny::isolate(tab_vals$pal_name)
			for (id in pal_inputs) {
				if (length(choices)) {
					shiny::updateSelectizeInput(session, id, choices = choices, selected = sel)
					shinyjs::enable(id)
				} else {
					shiny::updateSelectizeInput(session, id, choices = character(0))
					shinyjs::disable(id)
				}
			}
		})

		# Selected palette in the other tabs
		shiny::observeEvent(tab_vals$pal_name, {
			sel = tab_vals$pal_name
			if (!length(sel)) return(NULL)
			for (id in pal_inputs) shiny::updateSelectizeInput(session, id, selected = sel)
		})

		# Other tabs -> selected palette
		update_reactive = function(pal) {
			if (is.null(pal) || pal == "" || identical(pal, tab_vals$pal_name)) return(NULL)

			x = c4a_info(pal)
			n = tab_vals$n
			m = tab_vals$m
			if (length(n) != 1 || is.na(n)) n = x$ndef
			if (length(m) != 1) m = NA
			if (is.na(m) && (isTRUE(n > x$nmax) || isTRUE(n < x$nmin))) return(NULL)

			na = isTRUE(tab_vals$na)
			set_palette(pal, pal_colors(pal, n, m, na), x$type, na)
		}

		lapply(pal_inputs, function(id) {
			shiny::observeEvent(input[[id]], update_reactive(input[[id]]))
		})

		output$show = shiny::bindCache(shiny::renderUI({
			values = get_values()
			prep = get_prep()
			tab = if (is.null(values) || is.null(prep)) {
				NULL
			} else {
				progress = shiny::Progress$new()
				on.exit(progress$close())
				progress$set(message = "Colors in progress...", value = 0)
				plot_table(p = prep, text.format = values$format, text.col = values$textcol, include.na = values$na, cvd.sim = values$cvd, coltemp = values$coltemp, verbose = FALSE)
			}
			if (is.null(tab)) {
				ex = if (is.null(values)) {
					list(title = "No palette series selected.", details = "Select one or more palette series.")
				} else {
					explain_no_palettes(type = values$type, n = values$n, m = values$m, series = values$series, filters = values$filters, include_tritan = values$include_tritan)
				}
				return(shiny::div(class = "no-palettes", style = "margin: 20px 0; font-size: 14px;",
								  shiny::tags$b(ex$title),
								  if (length(ex$details)) shiny::tags$ul(style = "margin-top: 8px;", lapply(ex$details, shiny::tags$li))))
			}
			shiny::HTML(tab)
		}), get_values())

		#############################
		## CBF tab
		#############################

		cvds = c("deutan", "protan", "tritan")

		output$cbfSimu = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)
			c4a_plot_cvd(tab_vals$pal, dark = input$dark, include.na = tab_vals$na)
		})

		output$cbfHL = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)
			c4a_plot_confusion_lines(tab_vals$pal, cvd = "none", dark = input$dark)
		})

		lapply(1:3, function(i) {
			output[[paste0("cbfCL", i)]] = shiny::renderPlot({
				if (!length(tab_vals$pal)) return(NULL)
				c4a_plot_confusion_lines(tab_vals$pal, cvd = cvds[i], dark = input$dark)
			})
		})

		# Separate sliders per chart type (cbfLineSize / cbfPointSize), since
		# the two Szafir (2018) models have different fitted/validated ranges
		# -- lines 0.05-0.35 deg, points 0.25-2.0 deg (see palette_prob_bg()'s
		# documentation) -- and aren't evenly spaced, so one slider can't
		# serve both directly. "Map" is removed for now -- Szafir's models
		# don't transfer to filled polygons (see the design discussion).
		cbf_mark = shiny::reactive({
			if (input$cbfType == "Points") "point" else "line"
		})
		cbf_thickness = shiny::reactive({
			if (input$cbfType == "Points") input$cbfPointSize else input$cbfLineSize
		})

		# Probability matrices, computed apart from the plots so that
		# selecting a color pair (click) only redraws them
		prob_matrices = shiny::reactive({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			thickness = cbf_thickness()
			mark = cbf_mark()
			sapply(c("none", cvds), function(cvd) {
				get_prob_matrix(pal, cvd = cvd, whole_matrix = TRUE, thickness = thickness, mark = mark)
			}, simplify = FALSE)
		})

		render_dist_matrix = function(cvd) {
			shiny::renderPlot({
				ms = prob_matrices()
				if (is.null(ms)) return(NULL)
				c4a_plot_dist_matrix(tab_vals$pal, cvd = cvd, id1 = tab_vals$idA1, id2 = tab_vals$idA2, dark = input$dark, show_numbers = input$cbfShowNumbers, bc_adj = FALSE, m = ms[[cvd]])
			})
		}

		output$cbfSimi = render_dist_matrix("none")
		lapply(1:3, function(i) {
			output[[paste0("cbfPSimi", i)]] = render_dist_matrix(cvds[i])
		})

		# Screen calibration (see calibViewDist/calibRefLen in the UI, and the
		# 150px reference line next to them): both inputs are in whatever
		# arbitrary unit the user measured with, as long as it's the same
		# unit for both -- only their ratio (px per unit) is used, so no
		# unit conversion is needed. `2 * viewing_distance * tan(deg/2)` is
		# the textbook-correct chord formula -- deliberately NOT the same as
		# visual_angle_to_px()'s own default ("szafir" convention, factor 1,
		# picked there to match Szafir's own preview figures); its docs
		# explicitly call out "standard" (factor 2, used here) as what you
		# want once calibrating against a real measured display, which is
		# exactly this. So the rendered size can jump when calibration is
		# first filled in -- that's the uncalibrated approximation being
		# corrected, not a bug. Uncalibrated (either field blank/invalid)
		# falls back to visual_angle_to_px()'s existing 30in/96dpi default.
		cbf_px_per_deg = function(deg) {
			vd = input$calibViewDist
			rl = input$calibRefLen
			calibrated = !is.null(vd) && !is.null(rl) && !is.na(vd) && !is.na(rl) && vd > 0 && rl > 0
			if (!calibrated) return(visual_angle_to_px(deg))
			px_per_unit = 150 / rl # 150: the reference line's fixed CSS width
			len = 2 * vd * tan(deg * pi / 360)
			len * px_per_unit
		}

		cbf_lines = function(cols, cvd) {
			cols_cvd = sim_cvd(cols, cvd)
			lwd = cbf_px_per_deg(cbf_thickness())
			c4a_plot_lines(cols = cols_cvd[1:2], lwd = lwd, asp = .9, dark = input$dark)
		}

		cbf_points = function(cols, cvd) {
			cols_cvd = sim_cvd(cols, cvd)
			# Point diameter -> grid "char" size is just a rough visual proxy
			# (same px conversion as cbf_lines()'s lwd, rescaled), unlike the
			# underlying probability, which uses palette_prob_bg()'s own
			# validated Szafir point-mark model (mark = "point").
			size = cbf_px_per_deg(cbf_thickness()) / 5
			c4a_plot_scatter(cols = cols_cvd, size = size, lwd = 0, dark = input$dark)
		}

		lapply(1:4, function(i) {
			output[[paste0("cbf_ex", i)]] = shiny::renderPlot({
				if (!length(tab_vals$idA1)) return(NULL)
				cols = tab_vals$pal[c(tab_vals$idA1, tab_vals$idA2)]
				cvd = c("none", cvds)[i]
				if (input$cbfType == "Points") cbf_points(cols, cvd) else cbf_lines(cols, cvd)
			})
		})

		get_click_id = function(pal, x, y) {
			n = length(pal)

			brks_x = seq(0.04, 0.76, length.out = n + 2)
			brks_y = seq(0, 1, length.out = n + 2)

			x_id = as.integer(cut(x, breaks = brks_x)) - 1
			y_id = n + 1 - as.integer(cut(y, breaks = brks_y))

			if (!is.na(x_id) && (x_id < 1 || x_id > n)) x_id = NA
			if (!is.na(y_id) && (y_id < 1 || y_id > n)) y_id = NA

			list(x = x_id, y = y_id)
		}

		lapply(c("cbfSimi_click", paste0("cbfPSimi", 1:3, "_click")), function(id) {
			shiny::observeEvent(input[[id]], {
				pal = tab_vals$pal
				if (!length(pal)) return(NULL)

				ids = get_click_id(pal, input[[id]]$x, input[[id]]$y)

				if (!is.na(ids$x)) tab_vals$idA2 = ids$x
				if (!is.na(ids$y)) tab_vals$idA1 = ids$y
			})
		})


		shiny::observeEvent(input$table_click, {
			pal = tab_vals$palBW
			if (!length(pal)) return(NULL)

			ids = get_click_id(pal, input$table_click$x, input$table_click$y)

			if (!is.na(ids$x)) tab_vals$idB2 = ids$x
			if (!is.na(ids$y)) tab_vals$idB1 = ids$y

			if (!is.na(ids$x) || !is.na(ids$y)) {
				cr = colorspace::contrast_ratio(pal[tab_vals$idB1], pal[tab_vals$idB2])
				#print(cr)
				tab_vals$CR = cr
			}
		})



		#############################
		## HCL analysis tab
		#############################

		output$hclspace = plotly::renderPlotly({

			Hr = input$rangeH
			Cr = input$rangeC
			Lr = input$rangeL


			pal = if (input$hclspacepal) {
				tab_vals$pal
			} else NULL
			c4a_plot_hcl_space(Hmin = Hr[1], Hmax = Hr[2],
							   Cmin = Cr[1], Cmax = Cr[2],
							   Lmin = Lr[1], Lmax = Lr[2],
							   colors = pal)
		})


		output$anaHUE = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)

			# width = switch(tab_vals$type,
			# 			   seq = "total",
			# 			   div = "halves",
			# 			   "total")
			width = "none"

			pal = tab_vals$pal
			c4a_plot_hues(pal, dark = input$dark, width = width)
		})
		# output$anaRGB1 = shiny::renderPlot({
		# 	if (!length(tab_vals$pal)) return(NULL)
		#
		# 	pal = tab_vals$pal
		# 	c4a_plot_rgb_space(pal, cvd = "none", dark = input$dark, L = paste0("L", input$rgbL))
		# })

		output$anaCL = shiny::renderPlot({
			pal = tab_vals$pal

			if (!length(pal)) return(NULL)

			c4a_plot_CL(pal, Lrange = TRUE, dark = input$dark)
		})

		output$anaFair = shiny::renderPlot({
			pal = tab_vals$pal

			if (!length(pal)) return(NULL)

			type = tab_vals$type

			c4a_plot_fair(pal, dark = input$dark, type = if (type == "cat") "LC" else "C")
		})



		#############################
		## Contrast tab
		#############################

		output$cr_title = shiny::renderUI({
			shiny::markdown(paste0("#### Contrast ratios with ", ifelse(input$dark, "black", "white"), " (background)"))
		})

		output$ex_plus = shiny::renderPlot({
			borders = input$borders
			lwd = input$lwd

			if (input$plus_rev_original) {
				c4a_plot_Plus_Reversed(orientation = "landscape", borders = borders, lwd = lwd)
			} else {
				pal = tab_vals$palBW

				col1 = pal[tab_vals$idB1]
				if (!length(col1)) return(NULL)
				col2 = pal[tab_vals$idB2]

				c4a_plot_Plus_Reversed(col1, col2, orientation = "landscape", borders = borders, lwd = lwd)

			}
		})

		output$ex = shiny::renderPlot({
			pal = tab_vals$palBW

			col1 = pal[tab_vals$idB1]
			if (!length(col1)) return(NULL)
			col2 = pal[tab_vals$idB2]

			borders = input$borders
			lwd = input$lwd
			if (input$chart == "Barchart") {
				c4a_plot_bars(col1 = col1, col2 = col2, borders = borders, lwd = lwd, dark = input$dark)
			} else {
				c4a_plot_map(col1 = col1, col2 = col2, borders = borders, lwd = lwd, dark = input$dark)
			}
		})

		output$table = shiny::renderPlot({

			id1 = tab_vals$idB1
			id2 = tab_vals$idB2
			pal = tab_vals$palBW
			if (!length(pal)) return(NULL)

			c4a_plot_CR_matrix(pal, id1 = id1, id2 = id2, dark = input$dark)
		})

		output$cr_plot = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			opts = input$cr_plot_opts
			c4a_plot_CR(pal, dark = input$dark, sort =  ("sort" %in% opts), lines_WCAG = ("wcag" %in% opts), lines_equiluminance = ("equi" %in% opts))
		})




		output$textCR = shiny::renderUI({
			cr = tab_vals$CR
			if (!length(cr)) return(NULL)

			txt = if (cr >= 7) {
				"safe to print text according to [WCAG 2.2](https://www.w3.org/TR/WCAG22/) level **AAA**"
			} else if (cr >= 4.5) {
				"safe to print text according to the [WCAG 2.2](https://www.w3.org/TR/WCAG22/) level **AA**"
			} else if (cr >= 3) {
				"safe to print text according to the [WCAG 2.2](https://www.w3.org/TR/WCAG22/) level **A**"
			} else {
				"not safe to print text according to the [WCAG 2.2](https://www.w3.org/TR/WCAG22/)"
			}

			shiny::markdown(paste0("**Contrast ratio** (", sprintf("%.2f", round(cr, 1)), "): ", txt))
		})

		output$bordersCR = shiny::renderUI({
			cr = tab_vals$CR
			if (!length(cr)) return(NULL)

			txt = if (cr <= 1.2) {
				"strongly recommended to use border lines when plotting these colors next to each other"
			} else if (cr <= 1.5) {
				"recommended to use border lines when plotting these colors next to each other"
			} else if (cr <= 2) {
				"consider to use border lines when plotting these colors next to each other"
			} else {
				"colors can be plot next to each other without border lines"
			}
			shiny::markdown(paste0("**Contrast ratio** (", sprintf("%.2f", round(cr, 1)), "): ", txt))
		})

		output$textPlot = shiny::renderPlot({
			pal = tab_vals$palBW
			col1 = pal[tab_vals$idB1]
			if (!length(col1)) return(NULL)
			col2 = pal[tab_vals$idB2]

			c4a_plot_text2(c(col1, col2), dark = input$dark)
		})



		#############################
		## Naming tab
		#############################

		output$anaName = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			c4a_plot_names(pal, dark = input$dark, a = input$nameAlpha)
		})


		##############################
		## Floating tab
		##############################



		output$float_letters = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			c4a_plot_text(pal, dark = input$dark, size = 1.25, frame = TRUE)
		})


		output$float_selection = shiny::renderUI({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			b = tab_vals$b
			r = tab_vals$r

			ids = c(which.max(b)[1], which.max(r)[1])

			#br = get_blue_red()

			if (max(b) > .C4A$Blues && max(r) > 100) {
				btext = paste0("This illusion will likely occur with blue color ", c(LETTERS, letters)[ids[1]], " and a red color (e.g. ", c(LETTERS, letters)[ids[2]], "), at least with a dark background")
			} else if (max(b) > 100 && max(r) > 100) {
				btext = paste0("This illusion could occur with blue color ", c(LETTERS, letters)[ids[1]], " and a red color (e.g. ", c(LETTERS, letters)[ids[2]], "), at least with a dark background")
			} else if (max(b) > 100 && max(r) <= 100) {
				btext = paste0("This illusion will probably not occur, because the palette does not contain a red(dish) color")
			} else {
				btext = paste0("This illusion will probably not occur, because the palette does not contain any blue color")
			}

			shiny::tagList(
				shiny::markdown(paste(btext)),
				shiny::selectizeInput("float_col1", "Color 1", choices = c(LETTERS, letters)[1:length(pal)], selected = c(LETTERS, letters)[ids[1]]),
				shiny::selectizeInput("float_col2", "Color 2", choices = c(LETTERS, letters)[1:length(pal)], selected = c(LETTERS, letters)[ids[2]])
			)
		})

		shiny::observeEvent(input$float_col1, {
			tab_vals$idC1 = tab_vals$pal[which(c(LETTERS, letters) == input$float_col1)]
		})

		shiny::observeEvent(input$float_col2, {
			tab_vals$idC2 = tab_vals$pal[which(c(LETTERS, letters) == input$float_col2)]
		})

		output$float_letters_AB = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)

			cols = c(tab_vals$idC1, tab_vals$idC2)


			lL = c(LETTERS,letters)[c(which(pal == cols[1]),
					which(pal == cols[2]))]
			if (length(lL) == 2) {
				c4a_plot_text(cols, words = lL, dark = input$dark, size = 1.25, frame = TRUE)
			}
		})

		output$blues = shiny::renderPlot({
			if (!length(tab_vals$pal)) return(NULL)
			if (!input$float_original) {
				pal = tab_vals$pal


				cols = c(tab_vals$idC1, tab_vals$idC2)

				if (input$float_rev) cols = rev(cols)
				c4a_plot_floating_rings(col1 = cols[2], col2 = cols[1], dark = input$dark)
			} else {
				cols = c("#FF0000", "#0000FF")
				if (input$float_rev) cols = rev(cols)
				c4a_plot_floating_rings(cols[1], cols[2])
			}
		})




		##############################
		## Application tab
		##############################

		output$MAPplot = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_map(pal2, borders = input$MAPborders, lwd = input$MAPlwd, include.na = tab_vals$na, dark = input$dark, dist = input$MAPdist)
		})

		output$DOTplot = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_scatter(pal2,  borders = input$DOTborders, lwd = input$DOTlwd, size = input$DOTsize, dark = input$dark, dist = input$DOTdist)
		})

		output$TXTplot1 = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_text(pal2, dark = input$dark, size = input$TXTfsize, face = input$TXTfface)
		})

		output$TXTplot2 = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)
			c4a_plot_text(pal2, dark = input$dark, size = input$TXTfsize, face = input$TXTfface, frame = TRUE)
		})

		output$LINEplot = shiny::renderPlot({
			pal = tab_vals$pal
			if (!length(pal)) return(NULL)
			pal2 = sim_cvd(pal, input$APPcvd)

			c4a_plot_lines(pal2, dark = input$dark, lwd = input$LINElwd, stacked = input$LINEstack)
		})


		##############################
		## Info animations
		##############################


		oE = function(i, a, id, gif) {
			shiny::observeEvent(input[[i]], {
				anno[[a]] = !anno[[a]]
				if (anno[[a]]) {

					for (j in 1:length(id)) {
						if (length(id) == 1) {
							d = id[1]
						} else {
							d = id[j]
						}
						if (length(gif) == 1) {
							g = gif[1]
						} else {
							g = gif[j]
						}
						gs = paste(paste0("imgResources/", g, 1:2,"x.gif ", 1:2, "x"), collapse = ", ")
						g1 = paste0("imgResources/", g, "1x.gif")
						shinyjs::removeClass(id = d, class = "hide")
						shinyjs::runjs(paste0("
							var logo = document.getElementById('", d, "');
							logo.src = '", g1, "';
							logo.srcset = '", gs, "';
						"))
					}

					shiny::updateActionButton(session, i, icon = ani_off)
				} else {
					for (d in id) {
						shinyjs::addClass(id = d, class = "hide")
						shinyjs::runjs(paste0("
						var logo = document.getElementById('", d, "');
						logo.src = '';
						logo.srcset = '';
					"))
					}
					shinyjs::addClass(id = id, class = "hide")
					shiny::updateActionButton(session, i, icon = ani_on)
				}

			})
		}

		oE("infoHueLines", "hue_lines", "aniHL", "hue_lines")
		oE("infoConf", "conf_lines", c("aniCL1", "aniCL2", "aniCL3"), c("conf_linesD", "conf_linesP", "conf_linesT"))
		oE("infoSimi", "simi", "aniSimi", "simi")
		oE("infoPSimi", "psimi", c("aniPSimi1", "aniPSimi2", "aniPSimi3"), c("simiD", "simiP", "simiT"))
		oE("infoSimu", "simu", "aniSimu", "simu")
		oE("infoHUE", "hue_neck", "aniHUE", "hue_neck")
		oE("infoCL", "cl_plot", "aniCL", "cl_plot")
		oE("infoFair", "fair_plot", "aniFair", "fair_plot")
		oE("infoName", "naming", "aniName", "naming")

		oE("infoCR", "cr", "aniTable", "table")
		oE("infoBlues", "blues", "aniBlues", "blues")

	}
	shiny::shinyApp(ui = ui, server = server)
}
