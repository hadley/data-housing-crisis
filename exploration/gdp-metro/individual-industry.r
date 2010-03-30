# Are particular industries protective?

library(ggplot2)

gdp <- subset(read.csv("gdp-summary.csv"), year == 2006)
gdpi <- cast(gdp, fips ~ indust, value = "gdp.prop")

hpi_peaks <- read.csv("hpi-peaks.csv") 
names(hpi_peaks)[1] <- "fips" 


gdpi <- merge(gdpi, hpi_peaks[c("fips", "percent_change")], by = "fips")

lm(percent_change ~ . - fips, data = gdpi)