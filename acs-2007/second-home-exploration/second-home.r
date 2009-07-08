# getting 2nd house data from acs
# manually download housing data from http://factfinder.census.gov/home/en/acs_pums_2000.html
# manually change name of 'c2sshus.csv' to 'ss00hus.csv'

library(plyr)

get_info <- function(df)
	c(owner_occ = sum( c(sum(df$tenure == "1"), sum(df$tenure == "2"))), total = nrow(df))

get_data <- function(year) {
	path <- paste("../csv_hus/ss", sprintf("%02i", year), "hus.csv", sep = "")
	
	vars <- read.csv(path, nrows = 1)
	sh_vars <- c("ST", "PUMA", "TEN")
	of_interest <- names(vars) %in% sh_vars
	col_classes <- c("NULL", NA)[of_interest + 1]

	sh <- read.csv(path, colClasses = col_classes)
	sh <- subset(sh, !is.na(TEN))
	sh <- sh[,c(2,1,3)]
	names(sh) <- c("fips_st", "fips_puma", "tenure")

	shomes <- ddply(sh, .(fips_st,fips_puma), get_info, .progress = "text")
	rm(sh)
	shomes$per_owner <- with(shomes, owner_occ / total * 100)
	shomes$year <- as.numeric(paste(20, sprintf("%02i", year), sep = ""))

	shomes
}

get_data2 <- function(year) {
	path1 <- paste("../csv_hus/ss", sprintf("%02i", year), "husa.csv", sep = "")
	path2 <- paste("../csv_hus/ss", sprintf("%02i", year), "husb.csv", sep = "")
	
	vars <- read.csv(path1, nrows = 1)
	sh_vars <- c("ST", "PUMA", "TEN")
	of_interest <- names(vars) %in% sh_vars
	col_classes <- c("NULL", NA)[of_interest + 1]

	sh_a <- read.csv(path1, colClasses = col_classes)
	sh_a <- subset(sh_a, !is.na(TEN))
	sh_b <- read.csv(path2, colClasses = col_classes)
	sh_b <- subset(sh_b, !is.na(TEN))

	sh <- rbind(sh_a, sh_b)[sh_vars]
	names(sh) <- c("fips_st", "fips_puma", "tenure")
	rm(sh_a)
	rm(sh_b)

	shomes <- ddply(sh, .(fips_st,fips_puma), get_info, .progress = "text")
	rm(sh)
	shomes$per_owner <- with(shomes, owner_occ / total * 100)
	shomes$year <- as.numeric(paste(20, sprintf("%02i", year), sep = ""))

	shomes
}


# NOTE: Before 2005 ACS data is only available at the State level
# getting data for 2005 - 2007
years <- vector("list", 3)
years[[1]] <- get_data(5)
years[[2]] <- get_data(6)
years[[3]] <- get_data(7)

	
# combining data
shomes <- do.call("rbind", years)

# save data
write.table(shomes, "data/shomes.csv", row = F, sep = ",")