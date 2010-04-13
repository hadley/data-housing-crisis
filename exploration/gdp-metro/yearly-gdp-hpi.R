# describe overall patterns
# TODO: colour CA, FL vs. others
# describe individual city trajectories big.drop
# figure out how to calculate mean correctly- if we want to calculate geometric mean it is (product(changes(i))^(1/n)
## Isn't used in this code, but is used in others --> Annual growth rate = (Vpresent/Vpast)^(1/n)-1 NOT just total growthrate divided by number of years.

library(ggplot2)
library(stringr)
library(plyr)

gdp<-read.csv("gdp-summary.csv")
hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")


#calculating yearly change: data is in quaters to took the yearly data from quater 4 only
hpi.select<-subset(hpi,quarter==4) 


hpi.select <- ddply(hpi.select, c("fips_msa"), transform, 
  hpi.delta = c(diff(hpi) / diff(year), NA) / hpi,
  .progress = "text")

hpi.gdp <- merge(gdp, hpi.select, 
	by.x=c("fips", "year"), by.y= c("fips_msa", "year"))


cons <- subset(hpi.gdp, indust == 11 & year < 2008)
qplot(gdp.prop, gdp.delta, data = cons) + facet_wrap(~ year)
qplot(gdp, gdp.delta, data = cons) + facet_wrap(~ year)

# Remove cities with > 50% change
big.delta <- subset(cons, abs(gdp.delta) > 0.5)

cons.safe <- subset(cons, !(fips %in% big.delta$fips))
cons.safe$cafl <- with(cons.safe, 
  ifelse(state %in% c("CA", "FL"), as.character(state), "other"))

ggplot(cons.safe, aes(hpi.delta, gdp.delta)) +
 geom_vline(xintercept = 0, colour = "grey50") + 
 geom_hline(yintercept = 0, colour = "grey50") +
 geom_point(aes(colour= cafl, order = -xtfrm(cafl))) + 
 facet_wrap(~ year, nrow = 1) + 
 coord_equal() + 
 scale_colour_manual(values = c("CA" = "#F8766D", "FL" = "#00BFC4", "other" = "grey50")) + 
 xlab("Change in HPI") + ylab("Change in GDP")

ggsave("yearly-gdp-hpi.pdf", width = 12, height = 4)

# Look at changes in individual cities ----------------------------------
# Is there a consistent pattern?

# Compute per city summay statistics
biggest <- ddply(cons.safe, c("metro", "fips"), summarise,
	gdp.sd = sd(gdp.delta), 
	hpi.sd = sd(hpi.delta),
	gdp.mean = exp(mean(log(1 + gdp.delta))),
	hpi.mean = exp(mean(log(1 + hpi.delta))),
	n = length(hpi.delta))
# Only want to look at cities with data for all years
biggest <- subset(biggest, n == 7)
qplot(gdp.mean, hpi.mean, data = biggest)
qplot(gdp.sd, hpi.sd, data = biggest)

# Make base plot
base <- ggplot(mapping = aes(hpi.delta, gdp.delta)) + 
  geom_vline(xintercept = 0, colour = "grey50") + 
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_path(colour = "grey50") +
  geom_point(aes(colour = year)) + 
  facet_wrap(~ metro)
  
base %+% merge(cons.safe, subset(biggest, rank(hpi.mean) < 20))
base %+% merge(cons.safe, subset(biggest, rank(-hpi.mean) < 20))

# Pull out cities that had biggest drop in 2007
big.drop <- subset(cons.safe, hpi.delta < -0.2 & year == 2007)$fips
base %+% subset(cons.safe, fips %in% big.drop)
#looking at this, initially delta.hpi and delta.gdp are even. There there is an steady and closely linear increase in both. Then there is a HUGE decrease in HPI, then a huge drop in GDP

# Show average trajectory

big.droppers <- subset(cons.safe, fips %in% big.drop)
avg <- ddply(big.droppers, "year", summarise,
	gdp.delta = mean(gdp.delta),
	hpi.delta = mean(hpi.delta))

ggplot(avg, aes(gdp.delta, hpi.delta)) +
  geom_vline(xintercept = 0, colour = "grey50") + 
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_path(colour = "grey50") +
  geom_point(aes(colour = year)) + 
  coord_equal()

state_avgs <- ddply(cons.safe, c("state", "year"), summarise,
	gdp.delta = mean(gdp.delta),
	hpi.delta = mean(hpi.delta))
state_avgs$state <- factor(state_avgs$state)
state_avgs <- subset(state_avgs, str_length(state) == 2)

ggplot(state_avgs, aes(gdp.delta, hpi.delta, group = state)) +
  geom_vline(xintercept = 0, colour = "grey50") + 
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_path(colour = "grey50") +
  geom_point(aes(colour = year)) + 
  facet_wrap(~ state)+ 
  coord_equal()
#it might be true that if house prices increased, but there was no major increase in GDP a city was safe