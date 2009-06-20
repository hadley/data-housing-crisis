options(na.action = "na.exclude")
# Explore housing trends within Houston, 2000+
hlist <- subset(read.csv("texas-listings.csv"), msa == 280 & year >= 2000)
hdist <- subset(read.csv("texas-price-dist.csv"), msa == 280 & year >= 2000)

library(ggplot2)

# Strong seasonal effects make it difficult to see long term trend
qplot(year + month / 12, price_avg, data = hlist, geom = "line")
qplot(year + month / 12, listings, data = hlist, geom = "line")
qplot(year + month / 12, sales, data = hlist, geom = "line")
qplot(year + month / 12, sales / listings, data = hlist, geom = "line")
qplot(year + month / 12, inventory, data = hlist, geom = "line")

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

# Findings -------------------------------------------------------------------

# Average price took a big hit mig-late 2008
qplot(year + month / 12, price_avg_ds, data = hlist, geom = "line")

# Number of sales started slowing mid 2006, big drop mid 2008
qplot(year + month / 12, sales_ds, data = hlist, geom = "line")

# Listings start slowing at the start of 2006.  Leading indicator?
qplot(year + month / 12, listings_ds, data = hlist, geom = "line")
  
# Steady decline in success - lower percent are sold each month, and 
# the average length of time listed increases
qplot(year + month / 12, sales_ds / listings_ds, data = hlist, geom = "line")
qplot(year + month / 12, inventory_ds, data = hlist, geom = "line")

# Positive correlation between sales and average price?!
qplot(sales, price_avg, data = hlist)
qplot(sales_ds, price_avg_ds, data = hlist)


# Explore price distributions ------------------------------------------------

qplot(price_rng, value, data = hdist, geom = "line") + facet_wrap(~ year)
qplot(year, value, data = hdist, geom = "line") + facet_wrap(~ price_rng)