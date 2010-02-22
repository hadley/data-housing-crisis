# Try and find industries that are protective against the housing crisis
# 
# Pick out top and bottom 10 cities based on hpi change.
# Compare gdp for each.

# Add ranks use rank(-max_change, ties.method = "first")
# subset(,  rank <= 10 | rank > 900)

library(ggplot2)
gdp <- read.csv("gdp-selected.csv")
gdp_growth <- read.csv("gdp-growth.csv")
hpi_peaks <- read.csv("hpi-peaks.csv") 
names(hpi_peaks)[1] <- "fips" 

# Explore cities that peaked in 2009 -----------------------------------------
peak2009 <- subset(maximum_hpi, percent_change == 0)
peak2009_hpi <- subset(hpi, fips_msa %in% peak2009$fips_msa)
table(peak2009_hpi$state) / 37

qplot(time, hpi, data = peak2009_hpi, geom = "line") + facet_wrap(~ city_state)

# Combine gdp and hpi --------------------------------------------------------

gdp_hpi <- merge(hpi_peaks, gdp, by = "fips", all.x = TRUE)
selected <- subset(gdp_hpi, rank <= 10 | rank > 310)

qplot(year, gdp, data = selected, colour = rank, geom="line", group = metro, facets=~ industry, log = "y")

# Agriculture, foresty & fishing
#  - interesting! the low ranking towns have (on average) much higher values
qplot(year, gdp, data = subset(selected, indust == 3), 
  colour = rank, geom="line", group = metro, log = "y")


# Just look at construction
qplot(year, gdp, data = subset(selected, indust == 11), 
  colour = rank, geom="line", group = metro, log = "y")

# Combine gdp growth and hpi -------------------------------------------------

growth_hpi <- merge(hpi_peaks, gdp_growth, by = "fips", all.x = TRUE)

qplot(start, industry, data = growth_hpi, colour = percent_change, log = "x")

qplot(start, percent_change, data = growth_hpi, log = "x") + facet_wrap(~ industry)
qplot(growth, percent_change, data = growth_hpi) + facet_wrap(~ industry) + xlim(0.8, 1.2)

qplot(cut_number(percent_change, 4), growth, data = subset(growth_hpi, !is.na(percent_change) & !is.na(industry)), geom = "boxplot") + facet_wrap(~ industry)

