library(ggplot2)
gdp <- read.csv("gdp-selected.csv")
gdp_growth <- read.csv("gdp-growth.csv")
hpi_peaks <- read.csv("hpi-peaks.csv") 
names(hpi_peaks)[1] <- "fips" 
hpi_peaks[5] <- NULL
gdp[8] <- NULL
#-------------------------------------------------------------------

gdp_hpi <- merge(hpi_peaks, gdp, by = "fips", all.x = TRUE)
selected <- subset(gdp_hpi, rank <= 10 | rank > 310)

# Agriculture, foresty & fishing
#  - interesting! the low ranking towns have (on average) much higher values
qplot(year, gdp, data = subset(selected, indust == 3), 
  colour = rank, geom="line", group = metro, log = "y")
qplot(year, index.gdp, data = subset(selected, indust == 3), 
  colour = rank, geom="line", group = metro, log = "y" )
qplot(year, index.gdp, data = subset(gdp_hpi, indust == 3), 
  colour = rank, geom="line", group = metro, log = "y" )

#look at initial gdp vs hpi change for Agriculture industry

qplot(index.gdp, percent_change, data=subset(gdp_hpi,indust==3 & year== 2001))

