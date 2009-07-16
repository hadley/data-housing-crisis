library(plyr)
hpi <- read.csv("../fhfa-house-price-index/fhfa-house-price-index-msa.csv", header = T)
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

maxhpi$rate <- with(maxhpi,change/(2009.25 - time))
maxhpi <- within(maxhpi, per_change <- change/hpi)
maxhpi <- within(maxhpi, per_rate <- rate / hpi)
maxhpi <- within(maxhpi, state <- as.character(state))
for (i in 1:nrow(maxhpi))
	maxhpi$state[i] <- trim(gsub("MSAD", "", maxhpi$state[i]))
	
outcome <- qplot(data = maxhpi, hpi, per_rate, colour = state, geom = "text", label = state, main = "Max HPI vs. outcome", ylab = "Rate of change per year (as percentage of max HPI)", xlab = "Maximum HPI (2005-2009)") + opts(legend.position = "none")
savePlot(outcome)
ggsave(file="exports/outcome-by-max-price.pdf", width=8, height=6)