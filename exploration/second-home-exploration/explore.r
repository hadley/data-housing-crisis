library(ggplot2)
library(MASS)
library(mgcv)
library(R.oo)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

# Helper functions ----------------------------------------------------------
savePlot <- function(..., plot= TRUE, big = TRUE)
{
nameOfPlot <- substitute(...)
nameOfPlot <- gsub("_", "-", nameOfPlot)
cat("\nPrinting plot ", nameOfPlot,".pdf in folder 'exports'", sep ="")
if(plot)
if(big)
ggsave(..., file=paste("exports/", nameOfPlot,"_BIG.pdf",sep = "", collapse = ""), width=20, height=15) 
else
ggsave(..., file=paste("exports/", nameOfPlot,".pdf",sep = "", collapse = ""), width=8, height=6)
else
cat("\nJust Kidding!!!\n")
cat("\n")
}

index <- function(x) x / x[1]




shomes <- read.csv("data/shomes.csv", header = T)
st_shomes <- read.csv("data/shomes_by_state.csv", header = T)


# shomes_by-state_free_scales <- qplot(year, owner_occ, data = st_shomes, geom = "line") + facet_wrap( ~ state, scales = "free") + opts(legend.position = "none")

# shomes_by_state <- qplot(year, owner_occ, data = st_shomes, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
# savePlot(shomes_by_state)

by_year <- ddply(st_shomes, .(year), function(df) sum(df$total))
st_shomes <- merge(st_shomes, by_year, by = "year", all.x = T)
names(st_shomes)[7] <- "ACS_size"
st_shomes$prop <- st_shomes$total/st_shomes$ACS_size

ACS_proportions <- qplot(year, prop, data = st_shomes, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(ACS_proportions)
# size of data set doubled in 2005
# ACS decided to add the most additional respondents in FL, CA, and TX
# suggests looking into population

per_shomes_by_state <- qplot(year, per_owner, data = st_shomes, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(per_homes_by_state)
# DC has unusually low home ownership rates

per_shomes_by_state_free_scales <- qplot(year, per_owner, data = st_shomes, geom = "line") + facet_wrap( ~ state, scales = "free") + opts(legend.position = "none")
savePlot(per_shomes_by_state_free_scales)

# st_shomes <- ddply(st_shomes, "state", transform, index = index(prop))
# qplot(year, index, data = try, geom = "line", colour = state)
# + facet_wrap( ~ state) + opts(legend.position = "none")

# looking at florida
florida <- shomes[shomes$year == 2007 & shomes$state == "FL",]
qplot(year, per_owner, data = florida, geom = "text", label = fips_puma) 
savePlot(florida)
# the percent of second homes fluctuates among pumas.  The highest rates occur in and near downtown Miami (4009, 4010) and downtown Orlando (2204). Implying that our measure of second homes measures rental properties more than vacation homes.


# combining with hpi data
hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv", header = T)
hpi$time <- hpi$year + hpi$quarter/4
hpi <- na.omit(hpi)


get_max <- function(df){
	hpi2009 <- df$hpi[df$time == 2009]
	df <- df[df$hpi == max(df$hpi),]
	df$change <- hpi2009 - df$hpi
	df
} 

maxhpi <- ddply(hpi, .(fips_msa), get_max)
maxhpi <- maxhpi[,c(1,2,3,6,8,9)]
names(maxhpi)[3] <- "cbsa"

convert <- read.csv("../../geo-convert/puma-cbsa.txt",header = T)
convert$fips_st <- as.numeric(convert$state)
convert$fips_puma <- as.numeric(convert$puma5)
convert$cbsa <- as.numeric(convert$cbsa)
names(convert)[2] <- "fips_puma"
convert <- convert[-1,c(3,7,8,9)]

shomes <- merge(shomes, convert, by = c("fips_st", "fips_puma"))
shomes <- merge(shomes, maxhpi, by = "cbsa", all.x = T)

shomes2006 <- shomes[shomes$year == 2006,]
shomes2006$afact <- as.numeric(shomes2006$afact)

make_puma_hpi <- function(df){
	df$max_hpi <- df$afact * df$hpi
	df$ptime <- df$afact * df$time
	df$pchange <- df$afact * df$change
	c(per_owner = df$per_owner[1],
		max_hpi = sum(df$max_hpi),
		time = sum(df$ptime),
		change = sum(df$pchange),
		state = df$state[1],
		city = df$city[1])
}
shomes2006 <- ddply(shomes2006, .(fips_st, fips_puma), make_puma_hpi)

write.table(shomes2006, "shomes2006.csv", sep = ",", row = F)

occupancy_hpi <- qplot(data = shomes2006, per_owner, max_hpi, colour = state, main = "Max HPI vs. Owner Occupancy rate")
occupancy_hpi
savePlot(occupancy_hpi)
# no relation between second homes and max hpi

occupancy_change <- qplot(data = shomes2006, per_owner, per_change/time, colour = state, main = "Max HPI vs. Owner Occupancy rate")
occupancy_change
savePlot(occupancy_change)
# no clear relation between second homes and price change

occupancy_time <- qplot(data = shomes2006, time, per_owner, colour = state, main = "Max HPI vs. Owner Occupancy rate")
occupancy_time
savePlot(occupancy_time)
# no clear relation between second homes and time of peak


qplot(data = shomes2006, change, max_hpi, colour = state)
# it seems like you couldn't have a large max_hpi without a large change

shomes2006$per_change <- with(shomes2006, change / max_hpi)
qplot(data = shomes2006, max_hpi, per_change, colour = state, geom = "text", label = state) + opts(legend.position = "none")
	
maxhpi$rate <- with(maxhpi,change/(2009.25 - time))
maxhpi <- within(maxhpi, per_change <- change/hpi)
maxhpi <- within(maxhpi, per_rate <- rate / hpi)
maxhpi <- within(maxhpi, state <- as.character(state))
for (i in 1:nrow(maxhpi))
	maxhpi$state[i] <- trim(gsub("MSAD", "", maxhpi$state[i]))


outcome <- qplot(data = maxhpi, hpi, per_rate, colour = state, geom = "text", label = state, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
savePlot(outcome)
# ggsave(file="exports/outcome-by-max-price.pdf", width=8, height=6)