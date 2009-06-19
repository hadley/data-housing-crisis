# Exploring possible measures of tourism and growth(expansion) that might predict a cities' outcome in the housing bubble

# Industries

# 1 = All industries total (total GDP)
# 11 = construction
# 50 = finance and insurance
# 55 = Real estate and rental and leasing
#  56 = Real Estate
#  57 = Rental and leasing services
# 71 = Arts, Entertainment, and Recreation
#  72 = Performing Arts, museums,  and related activities
#  73 = amusement, gambling, and recreation
# 74 = Accomodation and food services
#  75 = Accomodation
#  76 = Food Services and drinking places
# 105 = Leisure and Hospitality


# importing data
options(stringsAsFactors = F)
gdp <- read.csv("Documents/git/data-housing-crisis/gdp-metro/clean-data.csv", header= T)
fips <- read.csv("Documents/git/data-housing-crisis/gdp-metro/fips_dictionary.csv", header = T)
fips <- fips[1:365,]
latitude <- read.csv("Documents/git/data-housing-crisis/city_latitude_longitude/latlng.csv", header= T)




# separating out city and state names in fips_dictionary
library(reshape)
fips <- cbind(fips[-1,1], colsplit(fips$Metropolitan.Area, split = ", ", names = c("msa", "state"))[,-3])

fips$state <- substr(fips$state,1,2)
for (i in 1:nrow(fips)){
	fips$city[i] <- strsplit(as.character(fips$msa[i]), "-")[[1]]
}
fips <- fips[-1,c(1,4,3)]
names(fips) <- c("msa_fips", "city", "state")
write.table(fips, "trimmed-fips-dictionary.csv", sep = ",", row = F)



# calculating role of tourism and growth in economy
library(plyr)
analyze_gdp <- function(df){
	total <- df$gdp[df$indust == 1]
	
	# tourism economy a combination of amusement, accomodation, 
	# and leisure (see top of script or industry dictionary)
	# tourism <- sum(df$gdp[df$indust == 71],
	#					df$gdp[df$indust == 74 ],
	#					df$gdp[df$indust == 105 ],
	#					na.rm = T)
	tourism <- df$gdp[df$indust == 105 ]
	
	# growth a combination of construction and real estate
	growth <- sum(df$gdp[df$indust == 11],
						df$gdp[df$indust == 55 ],
						na.rm = T)
						
	c( total = total,
		tourism = tourism,
		growth = growth,
		percent_tourism = round(tourism/total * 100, 2),
		percent_growth = round(growth/total * 100, 2))
}

# analyzing the gdp by location for each year
new_gdp <- ddply(gdp, .(fips, year), analyze_gdp)



# adding city and state information to new_gdp
fips$fips <- as.numeric(fips$msa_fips)
gdp_w_cs <- merge(new_gdp, fips, by.x = "fips", by.y = "fips", all.x = T)
gdp_w_cs <- gdp_w_cs[,c(8, 1, 9:10, 2:7)]		

# adding latitude and longitude information to gdp
gdp_final <- merge(gdp_w_cs, latitude, by.x = c("city", "state"), by.y = c("city", "state"), all.x = T)			
write.table(gdp_final, "tourism_and_growth.csv", sep = ",", row = F)

# NOTE: 3 cities shared a name with two cities in the latitude longitude data base. They'll get plotted twice - once in each location - since I haven't gone through to delete the incorrect location.





# Plotting results
library(ggplot2)
# tourism
a <- qplot(longitude, latitude, data = gdp_final[gdp_final$year == 2004,], colour = percent_tourism, size = tourism, main = "Cities by amount of  tourism dollars spent in 2004")
a + scale_colour_gradient("percent of GDP", trans = "sqrt") + scale_size("total dollars", trans = "log10")


# growth
b <- qplot(longitude, latitude, data = gdp_final[gdp_final$year == 2004,], colour = percent_growth, size = growth, main = "Cities by amount of Real Estate and Construction dollars spent 2004")
b + scale_colour_gradient("percent of GDP") + scale_size("total dollars", trans = "log10")



