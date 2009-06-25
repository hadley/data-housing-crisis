library(ggplot2)

cons <- read.csv(gzfile("new-construction.csv.gz"), stringsAsFactors = F)
cons$size <- c("1" = "single", "2" = "multi", "3-4" = "multi", "5-Inf" = "multi")[cons$units]
cons$units <- c("1" = "house", "2" = "duplex", "3-4" = "townhouse", "5-Inf" = "apts")[cons$units]

# Pull out Texas
tx <- subset(cons, state == "TX")
tx <- tx[with(tx, order(city, year, month)), ]

# Collapse duplex, townhouses and apts into single multi categoriy
tx <- ddply(tx, c("city", "year", "month", "size"), summarise, 
  n = sum(housing_units), value = sum(valuation))
tx$date <- tx$year + (tx$month - 1) / 12

# Just look at Houston - annoying problem is that the name of the metro
# area changes
houston <- tx[grep("Houston", tx$city),]

# Big decline in single family homes
qplot(date, n, data = houston, geom = "line", colour = size)
# Average price rising steadily, but big drop recently
# Apartments on average half the value of single family homes
qplot(date, value / n, data = houston, geom = "line", colour = size)

# Calculate proportion that are single homes, based on both number of units
# and total value 
prop <- ddply(houston, c("date"), summarise,
  prop_n = n[2] / sum(n), prop_value = value[2] / sum(value))
  
# Lot of noise - not sure if any significant pattern
qplot(date, prop_n, data = prop, geom = "line") + geom_smooth()
qplot(date, prop_value, data = prop, geom = "line") + geom_smooth()