# graphing cbs_data
library(ggplot2)

totals <- qplot(year + month/12, total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Total number of responders")
totals
# Miami, Honolulu, and Las Vegas have most responders. Is there a seasonal pattern?

totals_s <- qplot(month, total, data = cbs_data, geom = "line", colour = city, facets = year ~ ., main = "Total number of responders")
totals_s
# no


num_retirees <- qplot(year + month/12, retired, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "number of retired responders")
num_retirees
# mirrors total number of respondents

per_retirees <- qplot(year + month/12, retired/total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Percent of retired responders")
per_retirees
# almost all equal. Orlando near bottom. Smallest cities have greatest variation.

num_leisure <- qplot(year + month/12, leisure, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Number of responders who work in the leisure and hospitality fields")
num_leisure
# vacation spots seem higher
# strong seasonal trend that seems to reverse in 2007







per_leisure <- qplot(year + month/12, leisure/total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Percent of responders who work in the leisure and hospitality fields")
per_leisure
# again higher, but now the bigger cities get lost among the non-vacation spots

# could vacation spots be identified by a seasonal change in the number of leisure and hospitality workers?

leisure_s <- qplot(month, leisure, data = cbs_data, geom = "line", colour = city, facets = . ~ year, main = "Total number of leisure responders")
leisure_s
per_leisure_s <- qplot(month, leisure/total, data = cbs_data, geom = "line", colour = city, facets = . ~ year, main = "Percent of leisure responders")
per_leisure_s
# no
leisure07 <- qplot(month, leisure, data = cbs_data[cbs_data$city == "Las Vegas",], geom = "line", group = year, colour = year, main = "Total number of responders")
leisure07



num_acco <- qplot(year + month/12, accomodations, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Number of responders who work in the accomodations fields")
num_acco
# vacation spots seem higher. strong distinction

per_acco <- qplot(year + month/12, accomodations/total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Percent of responders who work in the accomodations fields")
per_acco

num_food <- qplot(year + month/12, food, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Number of responders who work in the food fields")
num_food
# not too different

per_food <- qplot(year + month/12, food/total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Percent of responders who work in the food fields")
per_food
# no difference. But perhaps more seasonal for vacation spots

num_arts <- qplot(year + month/12, accomodations, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Number of responders who work in the arts fields")
num_arts
# vacation spots seem higher. strong distinction
# stron seasonal trend that seems to reverse in 2007

per_arts <- qplot(year + month/12, accomodations/total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Percent of responders who work in the arts fields")
per_arts
# difference exist. lost for big cities


# making combined tourism variable
cbs_data <- within(cbs_data, tourism <- leisure + arts + accomodations)

num_tourism <- qplot(year + month/12, tourism, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Number of responders who work in tourism fields")
num_tourism
ggsave("exports/num_tourism.pdf")
# vacation spots seem higher. strong distinction
# stron seasonal trend that seems to reverse in 2007

per_tourism <- qplot(year + month/12, tourism/(total-retired), data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Percent of responders who work in tourism fields")
per_tourism

# could vacation spots be identified by a seasonal change in the number of tourism workers?
sea_change <- function (df) {
	c(tourism_sc = with(df, max(tourism) - min(tourism)), 
	leisure_sc = with(df,max(leisure) - min(leisure)), 
	accomodations_sc = with(df,max(accomodations) - min(accomodations)), 	
	arts_sc = with(df,max(arts) - min(arts)),
	food_sc = with(df,max(food) - min(food)),
	tourism_sc_p = with(df, (max(tourism/(total-retired)) - min(tourism/(total-retired)))/min(tourism/(total-retired))), 
	leisure_sc_p = with(df,(max(leisure/(total-retired)) - min(leisure/(total-retired)))/min(leisure/(total-retired))), 
	accomodations_sc_p = with(df,(max(accomodations/(total-retired)) - min(accomodations/(total-retired)))/(max(accomodations/(total-retired)))), 	
	arts_sc_p = with(df,(max(arts/(total-retired)) - min(arts/(total-retired)))/min(arts/(total-retired))),
	food_sc_p = with(df,(max(food/(total-retired)) - min(food/(total-retired)))/min(food/(total-retired))),
	destination = df$destination[1]
	)
}	
	
	
library(plyr)
sc_data <- ddply(cbs_data, .(year, city), sea_change)
		
quartz()

sc_tourism <- qplot(year, tourism_sc, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal number change of tourism jobs")
sc_tourism
# same for all cities except vegas
# note: 2009 only has 4 months so far

spc_tourism <- qplot(year, tourism_sc_p, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal percent change of tourism jobs")
spc_tourism
ggsave("exports/spc_tourism.pdf")
# almost nil for vacation spots except (myrtle beach)

sc_leisure <- qplot(year, leisure_sc, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal number change of leisure jobs")
sc_leisure
# slightly higher for vacation spots
# note: 2009 only has 4 months so far

spc_leisure <- qplot(year, leisure_sc_p, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal percent change of leisure jobs")
spc_leisure
ggsave("exports/spc_leisure.pdf")
# smaller for all vacation spots except myrtle beach

sc_acco <- qplot(year, accomodations_sc, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal number change of accomodations jobs")
sc_acco
# higher for vacation spots


spc_acco <- qplot(year, accomodations_sc_p, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal percent change of accomodations jobs")
spc_acco
# much lower for vacation spots except myrtle beach (other cities all have zero at some point

min_acco <- qplot(year + month/12, accomodations/total, data = cbs_data, geom = "line", colour = city, facets = ~ destination, main = "Number of responders who work in the accomodations fields")
min_acco



sc_food <- qplot(year, food_sc, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal number change of food and beverage jobs")
sc_food
# higher for vacation spots


spc_food <- qplot(year, food_sc_p, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal percent change of food and beverage jobs")
spc_food

# less variable for vacation cities

sc_arts <- qplot(year, arts_sc, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal number change of arts, entertainment and recreation jobs")
sc_arts
# higher for vacation spots


spc_arts <- qplot(year, arts_sc_p, data = sc_data, geom = "line", colour = city, facets = ~ destination, main = "Seasonal percent change of arts, entertainment and recreation jobs")
spc_arts

# overall pattern - seasonal number changes are bigger for vacation destinations, but the actual percent changes are quite small.


# Has tourism changed over time?
# So we'll remove the linear trend with a robust linear model
library(MASS)
deseas <- function(var) {
  model <- eval(substitute(var ~ factor(month), list(var = as.name(var))))
  unname(resid(rlm(model, data = hlist))) + mean(hlist[[var]], na.rm = TRUE)
}
hlist$sales_ds <- deseas("sales")
hlist$listings_ds <- deseas("listings")
hlist$inventory_ds <- deseas("inventory")
hlist$price_avg_ds <- deseas("price_avg")