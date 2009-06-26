# consolidating useful information out of the CBA-monthly dataset for the following cbsa's

# Vacation Cities (hot)
# orlando = 36740
# myrtle beach = 34820
# miami = 33100
# las vegas = 29820
# honolulu = 26180

# Not Vacation Cities (not)
# Oklahoma City = 36420
# Sacramento = 40900
# Scranton = 42540
# Worcester = 79600
# Amarillo = 11000

hot_cbsa <- c(36740, 34820, 33100, 29820, 26180)
not_cbsa <- c(36420, 40900, 42540, 79600, 11000)

library(plyr)

# grab information from each separate month's data set
find_data <- function (df){
	if(df$gtcbsa[1] %in% hot_cbsa){
		dest <- "vacation"
	}else dest <- "not_vacation"
	good_stuff <- data.frame(
				total = nrow(df),
				retired = nrow(df[df$pemlr == 5,]),
				leisure = nrow(df[df$prmjind1 == 11,]),
				accomodations = nrow(df[df$prdtind1 == 45,]),
				food = nrow(df[df$prdtind1 == 46,]),
				arts = nrow(df[df$prdtind1 == 44,]),
				destination = dest)
	good_stuff
}

# telling R where to look and to retrieve the data
make_all <- function (df) {
	path <- paste("clean/", df$year, "-", sprintf("%02i", df$month), ".csv.gz", sep = "")
	
	message("Processing ", path)
	df <- read.csv(gzfile(path), header = T)
	closeAllConnections()
	
	new <- df[df$gtcbsa %in% c(hot_cbsa, not_cbsa),]
	the_data <- ddply(new, .(gtcbsa), find_data)
	the_data
}

# the months we want data for (i.e months that use gtcbsa)
all_months <- expand.grid(
  year = 2004:2009,
  month = 1:12
)[-c(1, 7, 13, 19, 30, 36, 42, 48, 54, 60, 66, 72),]


# consolidating the data
cbs_data <- ddply(all_months, .(year, month), make_all)

key <- data.frame( city = c("Orlando", "Myrtle Beach", "Miami", "Las Vegas", "Honolulu", "Oklahoma City", "Sacramento", "Scranton", "Worcester", "Amarillo"), gtcbsa = c(hot_cbsa, not_cbsa))

cbs_data <- merge(key, cbs_data, by = "gtcbsa")


# Merced Query
# _______________________________________________

make_merced <- function (df) {
	path <- paste("clean/", df$year, "-", sprintf("%02i", df$month), ".csv.gz", sep = "")
	
	message("Processing ", path)
	df <- read.csv(gzfile(path), header = T)
	closeAllConnections()
	
	new <- df[df$gtcbsa == 32900,]
	new
}

all_months <- expand.grid(
  year = 2004:2009,
  month = 1:12
)[-c(1, 7, 13, 19, 30, 36, 42, 48, 54, 60, 66, 72),]

merced <- ddply(all_months, .(year, month), make_merced)

write.table(merced, "merced_data.csv", sep = ",", row = F)