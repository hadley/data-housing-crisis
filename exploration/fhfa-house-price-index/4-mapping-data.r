library(plyr)
library(ggplot2)
library(R.oo)
options(stringsAsFactors = F)
savePlot <- function(..., plot= TRUE, big = FALSE)
{
nameOfPlot <- substitute(...)
nameOfPlot <- gsub("_", "-", nameOfPlot)
cat("\nPrinting plot ", nameOfPlot,".pdf in folder 'exports'", sep ="")
if(plot)
if(big)
ggsave(..., file=paste("exports/", nameOfPlot,"-BIG.pdf",sep = "", collapse = ""), width=20, height=15) 
else
ggsave(..., file=paste("exports/", nameOfPlot,".pdf",sep = "", collapse = ""), width=10, height=6)
else
cat("\nJust Kidding!!!\n")
cat("\n")
}

# making countrywide plot of housing price drops
# below from 3-exports.r

hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")
time <- hpi[,"year"] + (hpi[,"quarter"] - 1) / 4
hpi <- cbind(hpi,time)
hpi$city_state <- paste(hpi$city, hpi$state, sep= ", ")

returnMaxTimeHPI <- function(d, maxcolumn) {
	unique(d[d[,maxcolumn] == max(d[,maxcolumn]), c("hpi","time", "fips_msa")])
	}
	
MaxHPI <- ddply(hpi, .(state,city) , returnMaxTimeHPI, maxcolumn = "hpi")

HPI09 <- ddply(hpi[ hpi[,"year"] >= 2009 ,], .(state,city) , returnMaxTimeHPI, maxcolumn = "hpi")


names(HPI09)[3] <- "hpi_2009" 


maximum_hpi <- merge(MaxHPI, HPI09[,1:3])

maximum_hpi$cs <- paste(maximum_hpi$city, maximum_hpi$state, sep= ", ")


maximum_hpi$change <- maximum_hpi$hpi - maximum_hpi$hpi_2009



mhpi <- maximum_hpi
names(mhpi)[5] <- "fips_cbsa"

# with geo-msa data we get 166 rows
locations <- read.csv("../geo-msa/msa-locations.txt", header = T)
locations <- locations[-1,]
names(locations)[2] <- "cs"
msa_hpi <- merge(mhpi, locations, by = c("cs"))

# with city-location data we get 643 extra rows because the msa's break out into cities
location <- read.csv("../../data/city-location/location-database2007.csv", header = T)
cities <- unique(location[,c(1,2,4,5,8)])
city_hpi <- merge(mhpi, cities, by = c("fips_cbsa", "state"))
# removing hawaii and alaska for graphing purposes
city_hpi <- city_hpi[city_hpi$state != "HI" & city_hpi$state != "AK",]


dropmap <- qplot(longitude, latitude, data = city_hpi, colour =   change, size = change, main = "Map of cumulative drop in HPI")
geo_change
savePlot(dropmap)

city_hpi$drop_rate <- with(city_hpi, change / (2009 - time))

ratemap <- qplot(longitude, latitude, data = city_hpi, colour =   drop_rate, size = drop_rate, main = "Map of drop rates in HPI")
ratemap
savePlot(ratemap)

city_hpi$year <- as.factor(trunc(city_hpi$time))


timemap <- qplot(longitude, latitude, data = na.omit(city_hpi), colour =   year, size = change, main = "When the HPI peaked where") + scale_colour_manual(value = c("dark violet", "red", "orange", "lime green", "sky blue"))
savePlot(timemap)