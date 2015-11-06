library(shiny)
library(lubridate)
library(zoo)
library(dplyr)
library(ggvis)
library(quantmod)
library(tidyr)



reacts <- reactiveValues(yields = NULL, updateDate = NULL,
												 rate_targets  = NULL,
												 gdp = NULL, spDate = NULL, csDate = NULL, gDate = NULL, defDate = NULL)

load("./reacts")
source("./helpers.R")

rebuild <- FALSE

# Once per day, check if interest rates have changed.
update_function <- function() {
	invalidateLater(1000 * 60 * 60 * 6, NULL)
	withProgress(message = "Updating Data...",
							 value = 1, min = 0, max = 16,
							 isolate(
							 	if (Sys.Date() - reacts$updateDate > 1 | rebuild ) {
							 		incProgress(0.5, detail = "Data stale, updating...")
							 		reacts$yields <- makeYields()
							 		incProgress(2, detail = "Calculated yields...")
							 		slc <- make_gdp()
							 		incProgress(amount = 3, detail = "Calculated data, saving...")
							 		reacts$rate_targets <- slc[["rt"]]
							 		reacts$gdp <- slc[["gd"]]
							 		reacts$spDate <- slc$spDate
							 		reacts$csDate <- slc$csDate
							 		reacts$gDate <- slc$gDate
							 		reacts$defDate <- slc$defDate
							 		reacts$updateDate <- Sys.Date()
							 		save(reacts, file = "./reacts")
							 	}
							 ))
}


total_size <- 280
ratios <- c(1, 0.5)
width <- 600

y_ramp_1 <- colorRampPalette(c( "grey5", "grey40"))
ramp <- y_ramp_1
distAdj <- .025

# ggvis will animate if it is fed a reactive function that returns a data.frame, instead of a data.frame.
# By defining the reactive function to either return a data.frame with data, or an empty one with all necessary
# columns defined but no rows, adding features to the ggvis can be animated rather than having to redraw when the user
# makes a selection.
no_data_df <- data.frame(tag = factor(x = character(0), levels = c("targetHigh", "target", "targetLow")),
												 value = numeric(0), period2 = numeric(0),
												 direction = factor(x = character(0), levels = c("down", "up")),
												 adjustedPos = numeric(0), idx = numeric(0),
												 gdp_ret = numeric(0), gspc_ret = numeric(0),
												 cs20_ret = numeric(0), def_ret = numeric(0), def_color = character(0))


shinyServer(function(input, output, session) {
	updater <- observe({update_function()})

	# Adjust the target date slider
	observe({
		start <- input$dates[1]
		end <- input$dates[2]
		updateSliderInput(session, "target",
											min = start,
											max = end)
	})

	# Cache this since we use it so often
	wiggleStart <- reactive(
		input$target %m-% months(input$wiggle)
	)

	# The number of wiggles.  Updates whenever the date changes (each animation frame).
	y_wave_count <- reactive(
		length(
			unique(
				reacts$yields$idx[reacts$yields$idx %within% (wiggleStart() %--% input$target)]
			)
		) - 1
	)

	# Aesthetics consumable by ggvis, for the coloration of the yield curve waves.
	# This is reactive to update if the number of wiggles changes
	# The number of wiggles is a distinct observable because it should not change each frame
	# unless the animation has reached the beginning or end.
	y_static <- reactive({
		data.frame(
			rnk = (y_wave_count() + 1):1,
			y_colors = c("black", y_ramp_1(y_wave_count())),
			y_opacity = as.character(c(1, seq(from = 0.8, to = 0.4, length = y_wave_count()))),
			y_sizes = c("30", rep("0",y_wave_count())),
			y_stroke_widths = c("3", as.character(seq(from = 1, to = 0.6, length = y_wave_count())))
		)
	})

	# The aesthetics + the actual positions for the points and lines
	yChartData <- reactive({
		reacts$yields %>%
			filter( idx %within% (wiggleStart() %--% input$target)) %>%
			mutate(
				rnk = dense_rank(idx)
			) %>%
			left_join(y_static(),
								by = c("rnk" = "rnk")) %>%
			group_by(idx)
	})


	# Another reactive function used to animate ggvis
	yTitle <- reactive(
		data.frame(t = format(input$target, format = "Yield Curve at %b, %Y"))
	)

	# data.frame of the macroeconomic lines to be shown in the full-time-period chart.
	# updates only when the user changes the selection of what data to show
	bigGdpData <- reactive(
		# the conditional is whether we are showing raw numbers or adjusting for inflation
		if ("def" %in% input$viewOptions) {
			reacts$gdp %>%
				mutate(
					def_ret = ifelse(! idx %within% reacts$defDate,0,def_ret),
					gdp_ret = gdp_ret - def_ret,
					gspc_ret = gspc_ret - def_ret,
					cs20_ret = cs20_ret - def_ret,
					def_color = "green",
					adjustedPos = 0,
					adjustedPos = ifelse(as.character(direction) == "down", gdp_ret + distAdj, gdp_ret - distAdj),
					gdp_ret = ifelse(! idx %within% reacts$gDate, 0, gdp_ret)
				)
		} else {
			reacts$gdp %>% mutate(
				def_color = "transparent",
				adjustedPos = 0,
				adjustedPos = ifelse(as.character(direction) == "down", gdp_ret + distAdj, gdp_ret - distAdj),
				gdp_ret = ifelse(! idx %within% reacts$gDate, 0, gdp_ret)
			)
		} %>% arrange(idx)
	)

	# The time range of the smaller economic chart
	small_gdp_startdate <- reactive(
		if (input$dates[2] - input$dates[1] > (365 * 4)) max( input$target %m-% months(4 * 6), as.Date("1964-01-01"))
		else input$dates[1]
	)
	small_gdp_enddate <- reactive(
		if (input$dates[2] - input$dates[1] > (365 * 4)) input$target %m+% months(4 * 6)
		else input$dates[2]
	)

	# The economic data for the small chart
	small_gdp_data <- reactive(
		bigGdpData() %>%
			filter(idx %within% (small_gdp_startdate() %m-% months(input$wiggle + 2) %--% small_gdp_enddate()))
	)

	# Additional lines that may be shown at the user's option; the "metafilter"
	# works around a ggvis bug that would otherwise cause ugly lines to appear in the
	# animation.
	small_gdp_metafilter <- reactive(
		if ("cs" %in% input$viewOptions) reacts$csDate
		else interval(as.Date("1900-01-01"), as.Date("1900-01-02"))
	)
	small_gdp_cs20 <- reactive(
		small_gdp_data() %>%
			filter(idx %within% small_gdp_metafilter())
	)
	small_gdp_gspc <- reactive(
		small_gdp_data() %>%
			filter(idx %within% small_gdp_metafilter())
	)

	# Isolate a data.frame of just the rate changes, so it gets the appropriate symbols on the chart
	small_gdp_rate_changes <- reactive(
		small_gdp_data() %>%
			filter(direction %in% c("down", "up"))
	)

	# The height of the charts can change based on what is shown, to make room
	borders <- reactive(
		list(top = max(small_gdp_data()$gdp_ret,
									 small_gdp_data()$def_ret,
									 small_gdp_cs20()$cs20_ret), # check this one
				 bottom = min(
				 	small_gdp_data()$gdp_ret,
				 	small_gdp_data()$def_ret,
				 	small_gdp_cs20()$cs20_ret)
		)
	)

	# Shows on the economic charts the area shown on the wiggle chart
	small_highlighter <- reactive({
		sc <- c(input$target %m+% months(1),
						rev(seq(to = input$target,
										from = wiggleStart(),
										by = "month")))
		data.frame(stringsAsFactors = FALSE,
							 x = sc[1:(length(sc) - 1)],
							 x2 = sc[-1],
							 y = round(borders()$top, 1) + 0.1,
							 y2 = round(borders()$bottom, 1) - 0.1,
							 col = c("red", ramp(length(sc) - 2)),
							 opa = c(0.2, rep(0.1, length(sc) - 2))
		)
	})



	# Draw the animated yield curve
	yChartData %>% ggvis(x = ~period2, y = ~value) %>%
		scale_numeric("y", trans = "pow",exponent = 0.75, expand = 0, domain = c(0.1, 14), nice = TRUE) %>%
		ggvis::add_axis("y", format = "<2.1", title = "Yield",
										values = c(0.5, 1, 2, 4, 6, 8, 10, 12, 14)) %>%
		ggvis::add_axis("x", values = c(0.5, 1, 2, 3, 5, 7, 10), format =  ">2.1",
										title = "") %>%
		set_options(width = width, height = total_size) %>%
		layer_smooths(strokeWidth := ~y_stroke_widths, stroke := ~y_colors,
									opacity := ~y_opacity, span = 0.6) %>%
		layer_points(size := ~y_sizes, opacity := ~y_opacity,
								 fill := ~y_colors) %>%
		hide_legend("stroke") %>%
		layer_text(data = yTitle, text := ~t, x := 230, y := 14,
							 font := "times", fontStyle := "bold", fontSize := "16") %>%
		layer_points(data = reactive(
			reacts$rate_targets %>%
				filter(idx == as.yearmon(input$target))
		),
		stroke := "firebrick", fill := "firebrick", size := 60,
		prop("shape", x = ~tag, scale = "targetShapes"), opacity := 0.5) %>%
		scale_ordinal("targetShapes",
									domain = c("targetHigh", "target", "targetLow"),
									range = c("triangle-down", "circle", "triangle-up")) %>%
		bind_shiny(plot_id = "bigYields")


	bigGdpData %>%  ggvis(x = ~idx) %>%
		layer_lines(y = ~gdp_ret, stroke := "blue", strokeWidth := 0.7) %>%
		layer_lines(y = ~def_ret, stroke := ~def_color, strokeWidth := 0.4, opacity := 0.7) %>%
		layer_paths(data = reactive(
			if ("sp" %in% input$viewOptions) bigGdpData()  %>%
				filter(idx %within% reacts$spDate)
			else no_data_df
		), y = ~gspc_ret, stroke := "purple", strokeWidth := 0.4, opacity := 0.7) %>%
		layer_paths(data = 	reactive(
			if ("cs" %in% input$viewOptions)  bigGdpData() %>%
				filter(idx %within% reacts$csDate)
			else no_data_df
		), y = ~cs20_ret, stroke := "orange", strokeWidth := 0.4, opacity := 0.9) %>%
		layer_lines(y = 0, stroke := "black", strokeWidth := 0.8, opacity := 0.9) %>%
		scale_numeric("y", domain = c(-0.1, 0.2), clamp = TRUE) %>%
		ggvis::add_axis("y", title = "CC Ann. Rate of Change", format = "<0%") %>%
		ggvis::add_axis("x", title = "", subdivide = 12, format = "") %>%
		set_options(width = 900, height = total_size * ratios[2]) %>%
		layer_rects(data = reactive(
			data.frame(stringsAsFactors = FALSE,
								 x = input$dates[1] %m-% months(2), x2 = input$dates[2])
		), fill := "skyblue", strokeWidth := 0,
		x =  reactive(input$dates[1] %m-% months(2)) , y = 0.2,
		x2 = reactive(input$dates[2]), y2 = -0.1 , fillOpacity := 0.2,
		strokeOpacity := 0) %>%
		layer_rects(data = small_highlighter, fill := ~col, strokeWidth := 0,
								x = ~x, y = 0.2,
								x2 = ~x2, y2 = -0.1 , fillOpacity := ~opa,
								strokeOpacity := 0) %>%
		layer_points(data = reactive(bigGdpData() %>%
																 	filter(direction %in% c("down", "up")))
								 , y = ~adjustedPos,
								 shape = ~direction, size := "10",
								 stroke := "firebrick", fill := "transparent", opacity := 0.3) %>%
		scale_ordinal("shape", domain = c("down", "up"), range = c("triangle-down", "triangle-up")) %>%
		hide_legend("shape") %>%
		hide_legend("fill") %>%
		hide_legend("stroke") %>%
		scale_datetime("x", nice = "year", expand = 0.0125) %>%
		bind_shiny(plot_id = "bigGdp")



	small_gdp_data %>%  ggvis(x = ~idx) %>%
		layer_lines(y = ~gdp_ret, stroke := "blue", strokeWidth := 0.7) %>%
		layer_lines(y = ~def_ret, stroke := ~def_color, strokeWidth := 0.4, opacity := 0.8) %>%
		layer_paths(data = small_gdp_gspc, y = ~gspc_ret, stroke := "purple", strokeWidth := 0.4, opacity := 0.8) %>%
		layer_paths(data = small_gdp_cs20, y = ~cs20_ret, stroke := "orange", strokeWidth := 0.6, opacity := 0.9) %>%
		layer_lines(y = 0, stroke := "black", strokeWidth := 0.8, opacity := 1) %>%
		ggvis::add_axis("y", title = "CC Annual Rate of Change", format = "<0%") %>%
		scale_numeric("y", trans = "pow", nice = TRUE, expand = 0, exponent = 0.75) %>%
		set_options(width = width, height = total_size * ratios[2]) %>%
		layer_rects(data = small_highlighter,
								x = ~x, y = ~y, x2 = ~x2, y2 = ~y2,
								fill := ~col, strokeWidth := 0,
								#								stroke := ~col,
								opacity := ~opa) %>%
		layer_points(data = small_gdp_rate_changes, y = ~adjustedPos,
								 shape = ~direction, size := "10",
								 stroke := "firebrick", fill := "transparent", opacity := 0.6) %>%
		scale_ordinal("shape", domain = c("down", "up"), range = c("triangle-down", "triangle-up")) %>%
		hide_legend("shape") %>%
		hide_legend("fill") %>%
		hide_legend("stroke") %>%
		scale_datetime("x",
									 clamp = TRUE, expand = 0) %>%
		ggvis::add_axis("x", title = "", offset = 0, tick_size_major = 15,
										subdivide = 3, tick_size_minor = 4,
										properties = axis_props(labels = list(angle = -45))) %>%
		bind_shiny(plot_id = "smallGdp")

	updater$resume()

})

