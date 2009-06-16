# Source: http://www.census.gov/popest/housing/HU-EST2007-CO.html

library(ggplot2)

a <- read.csv("hu-2000-2007.csv")

names(a)[1:6] <- c("SumLev", "State_Fips", "County_Fips", "County", "HUCENSUS_2000", "HUESTBASE_2000")

HousingUnitEst <- NULL
for(i in 7:14)
	HousingUnitEst <- c(HousingUnitEst, a[,i])
	
Year <- rep(2000:2007, each = nrow(a))

Orig <- a[,c(1:6)]

Orig <- rbind(Orig,Orig,Orig,Orig,Orig,Orig,Orig,Orig)

new <- cbind(Orig, Year, HousingUnitEst)

write.table(new, "HUnit-00-07Clean.csv", sep = ",", row = FALSE)