options(stringsAsFactors = FALSE)

# Load listings and add location names
names <- read.csv("msa-names.csv")
listing <- read.csv("texas-listings.csv")
listing <- merge(names, listing, by = "msa")

txloc <- subset(read.csv("../city-location/latlng.csv"), state == "TX")

cites <- merge(listing, txloc, by.x = "name", by.y = "city")

intersect(names$name, txloc$city)
setdiff(names$name, txloc$city)
names$name