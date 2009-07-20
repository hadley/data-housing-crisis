# http://www.census.gov/geo/www/tiger/latlng.txt

# Downloading text file
download.file("http://www.census.gov/geo/www/tiger/latlng.txt","latlng.txt")

# changed all "," to "-" using find and replace
options(stringsAsFactors = FALSE)
a <- read.csv("latlng.txt",skip = 2, header = FALSE)

# pulling out variable names
namesA <- strsplit(a[1,]," ")[[1]]
namesA <- namesA[nchar(namesA) > 0]
a <- as.data.frame(a[-1,])


# creating columns

library(R.oo)
latlng <- data.frame(a = 1, b = 2, c= 3, d = 4, e = 5, f = 6)
names(latlng) <- namesA

for (i in 2:nrow(a)){
	latlng[i-1,] <- data.frame(ST = as.numeric(substr(a[i,],2,3)), PLACE = as.numeric(substr(a[i,],5,9)), ANPSADPI = trim(substr(a[i,], 11,36)), AB = substr(a[i,], 37,38), INTPTLAT = as.numeric(substr(a[i,], 41, 48)) , INTPTLNG = -1 * as.numeric(substr(a[i,], 51,59)))
	}

# removing city/town/CDP designations
for (i in 1:nrow(latlng)){
	latlng[i,3] <- paste(trim(strsplit(latlng[i,3]," city")[[1]]),sep="",collapse="")
	latlng[i,3] <- paste(trim(strsplit(latlng[i,3]," town")[[1]]),sep="",collapse="")
	latlng[i,3] <- paste(trim(strsplit(latlng[i,3]," CDP")[[1]]),sep="",collapse="")
	}
	
# change names
names(latlng) <-c("fips_st", "fips_place", "city", "state", "latitude", "longitude")

# making implied decimals explicit
latlng <- within(latlng, {
	latitude <- latitude / 1000000
	longitude <- longitude / 1000000
	})

# save file
save(latlng, file = "latlng.rdata")
write.table(latlng, file = "latlng.csv", sep = ",", row = F)