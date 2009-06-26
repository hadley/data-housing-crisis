# Making location codes data base
options(stringsAsFactors = FALSE)


# Source 1 (city to county): http://www.census.gov/statab/ccdb/ccdbcityplace.html

city <- read.csv("latlng.csv")
county <- read.csv("citycounty.csv", skip = 11, header = F)
county <- county[-c(1319:1368),]

# dividing up fips code
for(i in 1:nrow(county)){
 	county$fips_st[i] <- as.numeric(substr(county$V1[i], 1,2))
 	county$fips_place[i] <- as.numeric(substr(county$V1[i], 3, 8))
}

# removing State Name rows
county <- county[county$fips_place != 0,]
# removing unwanted columns
county <- county[,c(3,15,16)]
names(county) <- c("county", "fips_st", "fips_place")


# merging with city data
location <- merge(city, county, by = c("fips_st", "fips_place"), all.x = T)


# separating cities with multiple counties
library(plyr)
expand <- function (df) {
	counties <- strsplit(df$county[1], ", ")
	if (is.na(df$county[1])) return(df)
	data.frame( fips_st = df$fips_st[1],
			fips_place = df$fips_place[1],
			longitude = df$longitude[1],
			latitude = df$latitude[1],
			city = df$city[1],
			state = df$state[1],
			county = unlist(counties))
}

location <- ddply(location, .(fips_st, fips_place), expand)
		

# source 2 (CBSA codes): http://www.census.gov/population/www/metroareas/metrodef.html
# CBSA codes refer to the Metropolitan SA code or the Micropolitan SA code depending on the city. This list is current from Nov 2007 to the present

download.file("http://www.census.gov/population/www/metroareas/lists/2007/List2.txt", "cbsa2007-11.txt")

whole_cbsa <- read.csv("cbsa2007-11.txt", skip = 10,sep = "@", header = F)

# extracting cbsa code
whole_cbsa$cbsa <- as.numeric(substr(whole_cbsa$V1,1,5))



wchar <- function(string)
	nchar(string, type = "width")

pos <- is.na(wchar(whole_cbsa[,1]))
whole_cbsa <- whole_cbsa[!pos,]

remove_cbsa <- function(line, cbsaCode)
    trim(gsub(as.character(cbsaCode), "", line))
for(i in 1:nrow(whole_cbsa)) whole_cbsa[i,1] <- remove_cbsa(whole_cbsa[i,1],whole_cbsa[i,2])

# Separating msa rows from cbsa rows
msa <- whole_cbsa[wchar(whole_cbsa[,1]) < max( wchar( whole_cbsa[, 1])), ]

cbsa <- whole_cbsa[wchar(whole_cbsa[,1]) == max( wchar( whole_cbsa[, 1])), ]
	
cbsa$fips_st <- as.numeric(substr(cbsa[,1], 71, 72))
# New York didn't go over
msa[nrow(msa)+1,] <- cbsa[is.na(cbsa$fips_st),c(1:2)]
cbsa <- cbsa[-826,]
cbsa$fips_place <- as.numeric(substr(cbsa[,1], 76, 80))
cbsa <- cbsa[,-1]

# trimming msa names
for (i in 1:nrow(msa)){
	msa[i,1] <- trim(gsub(" Metropolitan Statistical Area", "", msa[i,1]))
	msa[i,1] <- trim(gsub(" Micropolitan Statistical Area", "", msa[i,1]))
}
names(msa) <- c("msa_name", "cbsa")	

# merging cbsa codes with msa names
cbsa <- merge(msa, cbsa, by = "cbsa")

# merging cbsa with cities
location <- merge(location, cbsa, by = c("fips_st", "fips_place"), all.x = T)


# Source 3 (CSA codes): http://www.census.gov/population/www/metroareas/lists/2007/List6.txt

download.file("http://www.census.gov/population/www/metroareas/lists/2007/List6.txt", "csacodes2007-11.txt")
csa <- read.fwf("csacodes2007-11.txt", width = c(3,7), skip = 11, strip.white = T)
csa <- na.omit(csa)
names(csa) <- c("csa", "cbsa")
csa$csa <- as.numeric(csa$csa)
csa$cbsa <- as.numeric(csa$cbsa)

# merging csa codes to city data
location <- merge(location, csa, by = c("cbsa"), all.x = T)



# Source 4 (fips county codes): http://www.census.gov/population/www/metroareas/lists/2007/List4.txt

# and http://www.census.gov/population/www/metroareas/lists/2007/List5.txt

download.file("http://www.census.gov/population/www/metroareas/lists/2007/List4.txt", "metrocounties2007-11.csv")
download.file("http://www.census.gov/population/www/metroareas/lists/2007/List5.txt", "microcounties2007-11.csv")

metro <- read.csv("metrocounties2007-11.csv", sep = "@", skip = 11, header = F)
micro <- read.csv("microcounties2007-11.csv", sep = "@", skip = 11, header = F)

# extracting codes
micro$cbsa <- as.numeric(substr(micro[,1], 1,5))
micro$fips_st <- as.numeric(substr(micro[,1], 9, 10))
micro$fips_county <- as.numeric(substr(micro[,1], 11, 13))
micro <- micro[!is.na(micro$fips_st),]

metro$cbsa <- as.numeric(substr(metro[,1], 1,5))
metro <- metro[-c(1562:1565),]
metro$fips_st <- as.numeric(substr(metro[,1], 17, 18))
metro$fips_county <- as.numeric(substr(metro[,1], 19, 21))
metro <- metro[!is.na(metro$fips_st),]

# removing spanish names that confuse R
metro <-metro[metro$cbsa != 10380,]
metro <-metro[metro$cbsa != 32420,]
metro <-metro[metro$cbsa != 38660,]
metro <-metro[metro$cbsa != 41900,]
metro <-metro[metro$cbsa != 41980,]
metro <-metro[metro$cbsa != 49500,]

# isolating counties
for (i in 1:nrow(micro)){
	micro[i,1] <- trim(gsub(micro$cbsa[i], "", micro[i,1]))
	micro[i,1] <- trim(gsub(micro$fips_county[i], "", micro[i,1]))
	micro[i,1] <- trim(gsub(micro$fips_st[i], "", micro[i,1]))
	micro[i,1] <- trim(gsub("0", "", micro[i,1]))
	micro[i,1] <- strsplit(micro[i,1], ", ")[[1]][1]
	micro[i,1] <- strsplit(micro[i,1], " Parish")[[1]][1]
	micro[i,1] <- strsplit(micro[i,1], " County")[[1]][1]
}
names(micro)[1] <- "county"

metro$remove <- substr(metro[,1], 1, 21)
for (i in 1:nrow(metro)){
	metro[i,1] <- trim(gsub(metro$remove[i], "", metro[i,1]))
	metro[i,1] <- strsplit(metro[i,1], ", ")[[1]][1]
	metro[i,1] <- strsplit(metro[i,1], " Parish")[[1]][1]
	metro[i,1] <- strsplit(metro[i,1], " County")[[1]][1]
}
names(metro)[1] <- "county"
metro$remove <- NULL

# combining metropolitan and micropolitan data bases
county_code <- rbind(metro, micro)

# adding county codes to location database

location <- merge(location, county_code, by = c("fips_st", "cbsa"), all.x = T)

# consolidating county name data
for (i in 1:nrow(location)){
	if (!is.na(location$county.x)) {
		location$county <- location$county.x
	} else {
		location$county <- location$county.y
	}
}

# making the data pretty
location <-location[,c(4,5,13, 6,7,3,12,2,10,1,9)]
names(location)[8:9] <- c("fips_cbsa", "fips_csa")
location <- unique(location)

# saving database
write.table(location, "location_database2007.csv", sep =",", row = F)
