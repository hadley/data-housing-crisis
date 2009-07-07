# making countrywide plot of housing price drops
# below from 3-exports.r

hpi <- read.csv("HPI-metro-areas.csv")
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
location <- read.csv("../city-location/location_database2007.csv", header = T)
cities <- unique(location[,c(1,2,4,5,8)])
city_hpi <- merge(mhpi, cities, by = c("fips_cbsa", "state"))
# removing hawaii and alaska for graphing purposes
city_hpi <- city_hpi[city_hpi$state != "HI" & city_hpi$state != "AK",]


geo_change <- qplot(longitude, latitude, data = city_hpi, colour =   change, size = change, main = "Map of cumulative drop in HPI")
geo_change
ggsave("exports/changemap.pdf")

city_hpi$drop_rate <- with(city_hpi, change / (2009 - time))
geo_rate <- qplot(longitude, latitude, data = city_hpi, colour =   drop_rate, size = drop_rate, main = "Map of drop rates in HPI")
geo_rate
ggsave("exports/ratemap.pdf")

geo_time <- qplot(longitude, latitude, data = city_hpi, colour =   time, main = "When the HPI peaked where")
geo_time + scale_colour_gradientn(colour = rainbow(5))
ggsave("exports/timemap.pdf")