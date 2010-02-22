# Try and find industries that are protective against the housing crisis
# 
# Pick out top and bottom 10 cities based on hpi change.
# Compare gdp for each.

# Add ranks use rank(-max_change, ties.method = "first")
# subset(,  rank <= 10 | rank > 900)

library(ggplot2)
options(stringsAsFactors= FALSE)
library(stringr)

hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")
hpi$time <- hpi[,"year"] + (hpi[,"quarter"] - 1) / 4

hpi$city_state <- paste(hpi$city, hpi$state, sep= ", ")

#calculating max change and date of max

returnMaxTimeHPI <- function(d, maxcolumn)
	unique(d[d[,maxcolumn] == max(d[,maxcolumn]), c("hpi","time")])


MaxHPI <- ddply(hpi, .(fips_msa) , returnMaxTimeHPI, maxcolumn = "hpi")


HPI09 <- ddply(hpi[ hpi[,"year"] >= 2009 ,], .(fips_msa) , returnMaxTimeHPI, maxcolumn = "hpi")


names(HPI09)[2] <- "hpi_2009" 
names(MaxHPI)[c(2,3)]<- c("max_hpi","max_time")

maximum_hpi <- merge(MaxHPI, HPI09, by= "fips_msa")

maximum_hpi$percent_change<- maximum_hpi$max_hpi / maximum_hpi$hpi_2009 * 100 - 100

#Rankings the percent change, and finding the top 10 and bottom ten cities

maximum_hpi$rank <- rank(-maximum_hpi["percent_change"], ties="first")

#Anything that was ranked 310 and higher had a percent change of 0, didn't think that finding bottom ten was sufficient

subset(maximum_hpi, rank <= 10 | rank > 310)

#merge gdp data set with maximum_hpi by fips_msa

gdp <- read.csv("../../data/gdp-metro/gdp-metro.csv")

indust <- read.csv("../../data/gdp-metro/indust-dictionary.csv")
fips <-  read.csv("../../data/gdp-metro/fips-dictionary.csv")

top_indust <- subset(gdp, indust %in% c(11, 3, 10, 12, 36, 45, 58, 63, 62, 66, 78, 67, 55, 12, 71, 50, 6, 74, 100, 104))

gdp<- merge(top_indust, indust, by = "indust")
gdp <- merge(gdp, fips, by = "fips")

# Make industry and city labels small enough to plot

gdp$industry <- abbreviate(gdp$Industry, 8)
gdp$Industry <- NULL

gdp$metro <- str_replace(gdp$Metropolitan.Area, " \\(MSA\\)", "")
gdp$metro <- abbreviate(gdp$metro, 15)
gdp$Metropolitan.Area <- NULL

gdp <- gdp[!is.na(gdp$gdp), ]

#Running into problem- the merged information does not contain any rank of less thank 10... i don't know what happend?

names(maximum_hpi)[1] <- "fips" 


gdp_hpi <- merge(gdp, maximum_hpi, by="fips")

selected <-subset(gdp_hpi, rank<=10 | rank > 310)

qplot(year, gdp, data = selected, colour = rank, geom="line", facets=~ metro, log = "y")




#subset top and bottom, color rank, gdp, time