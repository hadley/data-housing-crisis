options(stringsAsFactors = FALSE)
library(plyr)

# Combine all listings data into single file ---------------------------------
paths <- dir("raw-listings", pattern = "\\.csv$", full = T)
names(paths) <- substr(basename(paths), 3, 5)

listings <- ldply(paths, read.csv, na.strings = "-")
names(listings) <- c("msa", "date", "sales", "volume", 
  "price_avg", "price_med", "listings", "inventory")

nc <- nchar(listings$date)
listings$month <- as.numeric(factor(tolower(substr(listings$date, nc - 2, nc)), 
  levels = c("jan", "feb", "mar", "apr", "may", "jun", 
  "jul", "aug",  "sep", "oct", "nov", "dec")))


listings$year <- rep(1990:2009, each = 12)[1:232]
listings$date <- NULL

write.table(listings, "texas-listings.csv", sep = ",", row = F)



# Combine all distribution data into single file -----------------------------
paths <- dir("raw-dist", pattern = "\\.csv$", full = T)
names(paths) <- substr(basename(paths), 3, 5)

dists <- ldply(paths, read.csv, na.strings = "-", skip = 1)
names(dists)[1] <- "msa"
dists$price_rng <- c(15, 35, 45, 55, 65, 75, 85, 95, 110, 130, 150, 170, 190, 225, 275, 350, 450, 550)

library(reshape)
distsm <- melt(dists, id = c("msa", "price_rng"))
distsm$year <- as.numeric(gsub("X", "", distsm$variable))
distsm$variable <- NULL
distsm <- distsm[c("msa", "year", "price_rng", "value")]

write.table(distsm, "texas-price-dist.csv", sep = ",", row = F)
