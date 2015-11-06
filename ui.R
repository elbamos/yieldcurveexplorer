
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(lubridate)
library(zoo)

diff_months <- function(a, b) { round(abs((as.yearmon(a) - as.yearmon(b)) * 12))}

endDate <- function() {year(Sys.Date()) + (month(Sys.Date())/12) - 1/12}

ref_date <- as.Date("1962-01-01")

shinyUI(fluidPage(

	# Application title
	titlePanel("Interest Rates, Economic Growth, and Asset Prices"),

	verticalLayout(
		fluid = FALSE,
		sidebarLayout(
			sidebarPanel(
				 width = 4,
				helpText("This is a demonstration app intended to explore the interplay among GDP growth, the Federal benchmark or target rates, and the yield curve."),
				helpText("The app checks for new data daily from the St. Louis Fed Economic Research website."),
				sliderInput("dates", "Date Range", min = ref_date,
										max = Sys.Date() %m-% months(1),
										value = c(as.Date("2007-06-01"),
															Sys.Date() %m-% months(1)),
										timeFormat = "%b %Y"),
				helpText("This shows the range that will be animated."),
				sliderInput(inputId = "target", label = "Date",
										min = Sys.Date() %m-% months(36),
										max = Sys.Date() %m-% months(1),
										value = Sys.Date() %m-% months(12),
										animate = animationOptions( loop = T),
										step = 30,
										timeFormat = "%b %Y"),
				helpText("This is for picking a particular date to show."),
				checkboxGroupInput("viewOptions", "Additional Data to Show",
													 c("S&P (scaled 25%)" = "sp",
													 	"Case-Shiller HPI" = "cs",
													 	"Inflation Adjustment" = "def"))
			),
			mainPanel(
				absolutePanel(top = 0, height = 280,
											ggvisOutput("bigYields"),
											sliderInput("wiggle", "Wiggle Depth",
																	min = 3, max =18, value = 3,
																	ticks = TRUE)),
				absolutePanel(top = 380, height = 150,
											ggvisOutput("smallGdp"))
			)
		),
		absolutePanel(width = '100%', height = 300, ggvisOutput("bigGdp"))
	)
))
