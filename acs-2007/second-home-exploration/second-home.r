# getting 2nd house data from acs
# manually download housing data from http://factfinder.census.gov/home/en/acs_pums_2000.html

# Read in only the variables of interest - this should keep memory usage down

vars <- read.csv("../csv_hus/ss07husa.csv", nrows = 1)
sh_vars <- c("ST", "PUMA", "TEN")
of_interest <- names(vars) %in% sh_vars
col_classes <- c("NULL", NA)[of_interest + 1]

sh_a <- read.csv("../csv_hus/ss07husa.csv", colClasses = col_classes)
sh_a <- subset(sh_a, !is.na(TEN))
sh_b <- read.csv("../csv_hus/ss07husb.csv", colClasses = col_classes)
sh_b <- subset(sh_b, !is.na(TEN))

sh <- rbind(sh_a, sh_b)[sh_vars]
names(sh) <- c("fips_st", "fips_puma", "vac_status", "tenure")
rm(sh_a)
rm(sh_b)

get_info <- function(df)
	c(owner_occ = sum( c(sum(df$tenure == "1"), sum(df$tenure == "2"))), total = nrow(df))

shomes <- ddply(sh, .(fips_st,fips_puma), get_info, .progress = "text")
rm(sh)
shomes$per_owner <- with(shomes, owner_occ / total * 100)


# save data
write.table(shomes, "data/shomes2007.csv", row = F, sep = ",")