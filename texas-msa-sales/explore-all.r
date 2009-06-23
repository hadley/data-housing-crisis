options(na.action = "na.omit")
library(ggplot2)
library(MASS)
library(mgcv)

# Helper functions ----------------------------------------------------------
index <- function(x) x / x[1]
deseas <- function(var, month) {
  resid(rlm(var ~ factor(month))) + mean(var, na.rm = TRUE)
}
smooth <- function(var, date) {
  predict(gam(var ~ s(date)))
}

# Focus on recent trends
list <- subset(read.csv("texas-listings.csv"), year >= 2000 & msa != 800)
list$date <- list$year + (list$month - 1) / 12

# We know from our exploration of Houston data that many of the series
# have strong seasonal components.  It's a good idea to check that's true for 
# all cities.  We'll start by focussing on sales.

qplot(date, sales, data = list, geom = "line", group = msa)

# First, let's remove the strong seasonal effect.
list <- ddply(list, "msa", transform, 
  sales_ds = deseas(sales, month))
qplot(date, sales_ds, data = list, geom = "line", group = msa)

# Maybe we should index the cities to make them more comparable
list <- ddply(list, "msa", transform, 
  sales_ind = index(sales_ds))
qplot(date, sales_ind, data = list, geom = "line", group = msa)

# Alternatively we could plot on the logged data
qplot(date, log10(sales_ds), data = list, geom = "line", group = msa)

# Getting there, but what a mess! Let trying smoothing the data
list <- ddply(list, "msa", transform, 
  sales_sm = smooth(sales_ds, date))
qplot(date, sales_sm, data = list, geom = "line", group = msa)

list <- ddply(list, "msa", transform, 
  sales_sm_ind = index(sales_sm))
qplot(date, sales_sm_ind, data = list, geom = "line", group = msa)

# Which are those crazy cities?
names <- read.csv("msa-names.csv")
list <- merge(list, names, by = "msa")

qplot(date, sales_sm_ind, data = list, geom = "line") + facet_wrap(~ city)

# Palestine, Harlingen, Killeen-Fort Hood, El Paso
# Why are these cities different?
# Always a good idea to go back to the raw data and double-check
qplot(date, sales_ind, data = list, geom = "line") + facet_wrap(~ city)
