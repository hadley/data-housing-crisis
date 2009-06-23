library(ggplot2)
source("explore-helper.r")

# Explore housing trends within Houston, 2000+
hlist <- subset(read.csv("texas-listings.csv"), msa == 280 & year >= 2000)
hlist$date <- hlist$year + (hlist$month - 1) / 12

# Strong seasonal effects make it difficult to see long term trend
qplot(date, price_avg, data = hlist, geom = "line")
qplot(date, listings, data = hlist, geom = "line")
qplot(date, sales, data = hlist, geom = "line")
qplot(date, sales / listings, data = hlist, geom = "line")
qplot(date, inventory, data = hlist, geom = "line")

# So we'll remove the linear trend with a robust linear model
library(MASS)
hlist <- transform(hlist,
  sales_ds = deseas(sales, month), 
  listings_ds = deseas(listings, month),
  inventory_ds = deseas(inventory, month),
  price_avg_ds = deseas(price_avg, month)
)

# Findings -------------------------------------------------------------------

# Average price took a big hit mig-late 2008
qplot(date, price_avg_ds, data = hlist, geom = "line")

# Number of sales started slowing mid 2006, big drop mid 2008
qplot(date, sales_ds, data = hlist, geom = "line")

# Listings start slowing at the start of 2006.  Leading indicator?
qplot(date, listings_ds, data = hlist, geom = "line")
  
# Steady decline in success - lower percent are sold each month, and 
# the average length of time listed increases
qplot(date, sales_ds / listings_ds, data = hlist, geom = "line")
qplot(date, inventory_ds, data = hlist, geom = "line")

# Positive correlation between sales and average price?!
qplot(sales, price_avg, data = hlist)
qplot(sales_ds, price_avg_ds, data = hlist)


# Explore price distributions ------------------------------------------------
hdist <- subset(read.csv("texas-price-dist.csv"), msa == 280 & year >= 2000)

qplot(price_rng, value, data = hdist, geom = "line") + facet_wrap(~ year)
qplot(year, value, data = hdist, geom = "line") + facet_wrap(~ price_rng)