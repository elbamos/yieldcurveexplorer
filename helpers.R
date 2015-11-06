library(magrittr)
library(quantmod)
library(shiny)

yieldConversionFrame <- data.frame(
	stringsAsFactors = FALSE,
	symbols = c("DGS5","DGS3","DGS3MO","DGS2","DGS7","DGS1MO","DGS6MO", "DGS10","DGS1"),
	period2 = c(5, 3, 1/4, 2, 7, 1/12, 1/2, 10, 1)
	)


makeYields <- function() {
	incProgress(amount = 1, detail = "Fetching yields from FRED...")

	quantmod::getSymbols(Symbols = yieldConversionFrame$symbols, src="FRED")
	incProgress(amount = 3, detail = "Got yields, merging...")

	tmp <- merge(DGS1MO, DGS3MO, DGS6MO, DGS1, DGS2, DGS3, DGS5, DGS7, DGS10)
	incProgress(amount = 1, detail = "Recombining yield data...")
	data.frame(idx = as.yearmon(index(tmp)), tmp)  %>%
		filter(idx > as.yearmon(as.Date("12/31/1958", format = "%d/%M/%Y"))) %>%
		group_by(idx) %>%
		summarize_each(funs(m1 = mean(., na.rm=TRUE))) %>%
		gather(key = "Period", value = "value", -idx) %>%
		filter(complete.cases(value)) %>%
		left_join(yieldConversionFrame, by = c("Period" = "symbols")) %>%
		ungroup() %>%
		mutate(idx = as.Date(idx)) %>%
		select(-Period) %>%
		arrange(idx)
}

#SPCS20RPSNSA

make_gdp <- function() {

	incProgress(amount = 1, detail = "Fetching FRED data...")
	load("./rateTargets")
	getSymbols(c("DFEDTARU", "DFEDTARL", "GDP", "GDPDEF", "USCSCOMHPISA"), src="FRED")

	incProgress(amount = 1, detail = "Got FRED Data...")

	rt <- merge(DFEDTARU, DFEDTARL)
	rm(DFEDTARU, DFEDTARL)
	gc()
	ret = list()
	ret$rt <- rt %>% data.frame(idx = as.yearmon(index(rt)), rt) %>%
		gather(key = "upordown", value = "value", -idx) %>%
		mutate(tag = factor(
			ifelse(upordown == "DFEDTARU", "targetHigh", "targetLow"),
			levels = c("target","targetHigh", "targetLow")
			),
			period2 = 0.25
			) %>%
		group_by(idx, tag) %>%
		summarize(value = median(value, na.rm = TRUE),
							period2 = mean(period2, na.rm=TRUE)) %>%
		filter(complete.cases(value)) %>%
		ungroup() %>%
		bind_rows(filter(rateTargets, tag == "target")) %>%
		mutate(idx = as.yearmon(idx))

	incProgress(amount = 1, detail = "Fetching S&P...")

	getSymbols("^GSPC")
	incProgress(amount = 1, detail = "Calculating asset prices...")

	ret$spDate <- as.Date(first(index(GSPC))) %--% as.Date(last(index(GSPC)))
	ret$csDate <- as.Date(first(index(USCSCOMHPISA))) %--% as.Date(last(index(USCSCOMHPISA)))
	ret$gDate <- as.Date(first(index(GDP))) %--% as.Date(last(index(GDP)))
	ret$defDate <- as.Date(first(index(GDPDEF))) %--% as.Date(last(index(GDPDEF)))

	incProgress(amount = 1, detail = "Merging Data...")

	gd <- merge(xts(x = NULL, order.by = seq(as.Date("1958-01-01"), Sys.Date(), 'days')),
							GDP, GDPDEF, USCSCOMHPISA, GSPC)
	rm(GSPC, GDP, GDPDEF, USCSCOMHPISA)
	gc()

	incProgress(amount = 1, detail = "Interpolating quarterly returns...")
	tp <- "log"
	gd$gdp_ret <- ((quarterlyReturn(gd$GDP, type = tp) + 1)^4) - 1
	gd$def_ret <- ((quarterlyReturn(gd$GDPDEF, type = tp) + 1)^4) - 1
	gd$gspc_ret <- ((quarterlyReturn(gd$GSPC.Adjusted, type = tp) + 1)^4) - 1
	gd$cs20_ret <- ((quarterlyReturn(gd$USCSCOMHPISA, type = tp) + 1)^4) - 1

	incProgress(amount = 1, detail = "Recombining data...")
	ret$gd <- gd %>% na.spline(na.rm = FALSE) %>%
		data.frame(idx = as.yearmon(index(gd))) %>%
		filter(idx > as.yearmon(ymd("1960/12/31")),
					 idx < as.yearmon(Sys.Date() %m-% months(1))) %>%
		group_by(idx) %>%
		summarize_each(funs = funs(m1 = mean(., na.rm=TRUE)),
									 ends_with("ret")) %>%
		ungroup() %>%
		mutate(gspc_ret = gspc_ret * .25,
					 idx = as.Date(idx)) %>%
		left_join(
			ret$rt %>%
				group_by(tag) %>%
				arrange(idx) %>%
				filter(value - lag(value) != 0) %>%
				transmute(idx = as.Date(idx),
									direction = ifelse(value - lag(value) < 0,"down","up")),
			by = c("idx" = "idx")) %>%
		mutate(
			direction = factor(ifelse(is.na(direction), "none", direction)),
			line0 = 0,
			adjustedPos = gdp_ret) %>%
		select(-tag)
	incProgress(amount = 3, detail = "Merged data...")
	gc()
	ret
}

diff_months <- function(a, b) { round((as.yearmon(a) - as.yearmon(b)) * 12)}
