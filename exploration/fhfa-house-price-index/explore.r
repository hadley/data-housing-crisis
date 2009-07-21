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
ggsave(..., file=paste("exports/", nameOfPlot,".pdf",sep = "", collapse = ""), width=8, height=6)
else
cat("\nJust Kidding!!!\n")
cat("\n")
}






hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv", header = T)
#"../fhfa-house-price-index/fhfa-house-price-index-msa.csv"
hpi$time <- hpi$year + hpi$quarter/4
hpi <- na.omit(hpi)

# qplot(data = hpi, year, hpi, geom = "line") + facet_wrap(~ city) + opts(legend.position = "none")
# ggsave(file="all_hpi.pdf", width=20, height=15) 

get_max <- function(df){
	hpi2009 <- df$hpi[df$time == 2009]
	df <- df[df$hpi == max(df$hpi),]
	df$change <- hpi2009 - df$hpi
	df
} 

maxhpi <- ddply(hpi, .(fips_msa), get_max)
maxhpi <- maxhpi[,c(1,2,3,6,8,9)]
names(maxhpi)[3] <- "cbsa"

maxhpi$rate <- with(maxhpi,change/(2009.25 - time))
maxhpi <- within(maxhpi, per_change <- change/hpi)
maxhpi <- within(maxhpi, per_rate <- rate / hpi)
maxhpi <- within(maxhpi, state <- as.character(state))
for (i in 1:nrow(maxhpi))
	maxhpi$state[i] <- trim(gsub("MSAD", "", maxhpi$state[i]))
	
outcome <- qplot(data = maxhpi, hpi, per_rate, colour = state, geom = "text", label = state, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
savePlot(outcome)
ggsave(file="exports/outcome-by-max-price.pdf", width=6, height=6)

pop <- read.csv("pop-cbsa.csv", header = T)
pop <- pop[-c(1,2),c(1,3)]
pop$cbsa <- as.numeric(pop$cbsa)
pop$pop2k <- as.numeric(pop$pop2k)

maxhpi <- merge(maxhpi, pop, by = "cbsa", all.x = T)

FLoutcome <- qplot(data = maxhpi[maxhpi$state == "FL",], hpi, per_rate, geom = "text", label = state, main = "Florida Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
FLoutcome
savePlot(FLoutcome)

# FLoutcome_pop <- qplot(data = maxhpi[maxhpi$state == "FL",], hpi, per_rate, colour = state, geom = "text", label = state, size = pop2k, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
# FLoutcome_pop
# savePlot(FLoutcome_pop)


CAoutcome <- qplot(data = maxhpi[maxhpi$state == "CA",], hpi, per_rate, geom = "text", label = state, main = "California Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
CAoutcome
savePlot(CAoutcome)
# California cities are all over the map.  Florida cities have a stronger relationship between max hpi and outcome

# CAoutcome_pop <- qplot(data = maxhpi[maxhpi$state == "CA",], hpi, per_rate, colour = state, geom = "text", label = state, size = pop2k, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
# CAoutcome_pop
# savePlot(CAoutcome_pop)

AZoutcome <- qplot(data = maxhpi[maxhpi$state == "AZ",], hpi, per_rate,  geom = "text", label = state, main = "Arizona Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
AZoutcome
savePlot(AZoutcome)

NVoutcome <- qplot(data = maxhpi[maxhpi$state == "NV",], hpi, per_rate, geom = "text", label = state, main = "Nevada Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
NVoutcome
savePlot(NVoutcome)


FCANoutcomes <- qplot(data = maxhpi[maxhpi$state %in% c("AZ", "CA", "FL", "NV", "OR"),], hpi, per_rate, geom = "text", label = state, colour = state, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none") + facet_wrap( ~ state)
FCANoutcomes
savePlot(FCANoutcomes)

# should California really be more than one state?

CAcityoutcome <- qplot(data = maxhpi[maxhpi$state == "CA",], hpi, per_rate, colour = state, geom = "text", label = city, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
CAcityoutcome
savePlot(CAcityoutcome, big = T)

location <- read.csv("loc-cbsa.csv", header = T)
location <- location[-c(1,2), c(1,3,4)]
names(location) <- c("cbsa", "longitude", "latitude")
location$cbsa <- as.numeric(location$cbsa)
location$longitude <- as.numeric(location$longitude)
location$latitude <- as.numeric(location$latitude)

maxhpi <- merge(maxhpi, location, by = "cbsa", all.x = T)

calhpi <-na.omit(maxhpi[maxhpi$state == "CA",])
for (i in 1:nrow(calhpi)){
	if (calhpi$latitude[i] >= 36.14)
		calhpi$n_or_s[i] <- "n"
	else
		calhpi$n_or_s[i] <- "s"
}


for (i in 1:nrow(calhpi)){
	if (calhpi$latitude[i] <= 36.14 & calhpi$longitude[i] <= -121.768)
		calhpi$region[i] <- "bay_area"
	else if (calhpi$latitude[i]<= 35.38)
		calhpi$region[i] <- "so_cal"
	else
		calhpi$region[i] <- "other"
}

LA <- maxhpi[210,]
LA$latitude <- 33.93
LA$longitude <- -118.40
LA$n_or_s <- "s"
LA$region <- "so_cal"

SanFran <- maxhpi[316,]
SanFran$latitude <- 37.75
SanFran$longitude <- -122.68
SanFran$n_or_s <- "n"
SanFran$region <- "bay_area"

Oakland <- maxhpi[252,]
Oakland$latitude <- 37.73
Oakland$longitude <- -122.22
Oakland$n_or_s <- "n"
Oakland$region <- "bay_area"

Irvine <- maxhpi[319,]
Irvine$latitude <- 33.67
Irvine$longitude <- -117.88
Irvine$n_or_s <- "s"
Irvine$region <- "so_cal"

calhpi <- rbind(calhpi, LA, SanFran, Oakland, Irvine) 
calhpi[c(9, 14, 16, 19, 20, 26, 27),14] <- "bay_area"
calhpi$region <- c("central valley",
					"central valley",
					"southern desert",
					"central valley",
					"central valley",
					"central valley",
					"central valley",
					"central valley",
					"bay area",
					"so cal",
					"central valley",
					"so cal",
					"central valley",
					"bay area",
					"so cal",
					"bay area",
					"so cal",
					"so cal",
					"bay area",
					"bay area",
					"central valley",
					"bay area",
					"central valley",
					"central valley",
					"so cal",
					"bay area",
					"bay area",
					"so cal")

CalByRegion <- qplot(data = calhpi, hpi, per_rate, colour = region,  geom = "text", label = state,main = "California Max HPI by region vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") 
CalByRegion
savePlot(CalByRegion)
# northern cal vs southern cal doesn't seem to matter.  But the point cloud breaks out into regions. Metropolitan regions appear to the right of agricultural regions (higher hpi).

calhpi2 <- hpi[hpi$state == "CA" | hpi$state == "CA  MSAD",]
names(calhpi2)[3] <- "cbsa"
toget <- calhpi[,c(1,14)]

calhpi2 <- merge(calhpi2, toget, by = "cbsa")

CalHpiByRegion <- qplot(data = calhpi2, time, hpi, geom = "line", group = cbsa, colour = region, main = "California HPI by region")
CalHpiByRegion
savePlot(CalHpiByRegion)
# metropolitan curves lie above agricultural curves

calhpi2 <- merge(calhpi2, pop, by = "cbsa", all.x = T)

# qplot(data = calhpi2, time, hpi, geom = "line", group = cbsa, colour = pop2k, main = "California HPI by region")

qplot(data = calhpi2, time, hpi, geom = "line", group = region, colour = region, main = "California HPI by region")
