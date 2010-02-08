library(reshape)
library(stringr)

all <- read.csv("allgmp.csv")
all_m <- melt(all, m = paste("X", 2001:2008, sep = ""))

# Convert variable to numeric year
all_m$year <- as.numeric(str_replace(all_m$variable, "X", ""))
all_m$variable <- NULL

gdp <- all_m[c("FIPS", "industry_id", "year", "value")]
names(gdp) <- c("fips", "indust", "year", "gdp")

# Remove header and footer rows
gdp <- gdp[!gdp$fips %in% c("FIPS", ""), ]

# Remove special values from gdp and store in separate variable
gdp$code <- ifelse(gdp$gdp %in% c("n/a", "(D)", "(L)"), 
  as.character(gdp$gdp), NA)
gdp$gdp[!is.na(gdp$code)] <- NA
gdp$gdp <- as.numeric(gdp$gdp)

gdp$fips <- as.numeric(as.character(gdp$fips))
gdp$indust <- as.numeric(as.character(gdp$indust))

write.table(gdp, "gdp-metro.csv", sep = ",", row = F)