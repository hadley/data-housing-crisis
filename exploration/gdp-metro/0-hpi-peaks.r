library(ggplot2)
options(stringsAsFactors= FALSE)
library(stringr)

hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")
hpi$time <- hpi$year + (hpi$quarter - 1) / 4

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
maximum_hpi <- maximum_hpi[order(-maximum_hpi$percent_change), ]

write.table(maximum_hpi, "hpi-peaks.csv", sep = ",", row = F)