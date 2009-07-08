library(ggplot2)

pop <- read.csv("CO-EST2008-ALLDATA.csv", stringsAsFactors = FALSE)
names(pop) <- tolower(names(pop))
pop <- rename(pop, c("gqestimatesbase2000" = "gqestimates2000"))

# Remove non-yearly variables 
pop$census2000pop <- NULL
pop$estimatesbase2000 <- NULL


# Melt data and separate variable name from year
library(reshape)
popm <- melt(pop, id = 1:7)

nc <- nchar(as.character(popm$variable))
popm$year <- as.numeric(substr(popm$variable, nc - 3, nc))
popm$variable <- substr(popm$variable, 1, nc - 4)

# Remove 2001 because numbers are only for April-July
popm <- subset(popm, year != 2000)

# Cast into desired output and save as csv
clean <- cast(popm, state + county + year ~ variable)
write.table(clean, "census-population.csv", sep = ",", row = FALSE)


########################################################################
########################################################################
########################################################################


met <- read.csv("CBSA-EST2008-ALLDATA.csv", skip = 4, stringsAsFactors = FALSE)
names(met) <- tolower(strsplit(readLines("CBSA-EST2008-ALLDATA.csv")[1], ",")[[1]])

# Remove non-yearly variables 
met$census2000pop <- NULL
met$estimatesbase2000 <- NULL


# Melt data and separate variable name from year
library(reshape)
metm <- melt(met, id = 1:5)

nc <- nchar(as.character(metm$variable))
metm$year <- as.numeric(substr(metm$variable, nc - 3, nc))
metm$variable <- substr(metm$variable, 1, nc - 4)

# Remove 2001 because numbers are only for April-July
metm <- subset(metm, year != 2000)

# Cast into desired output and save as csv
cleanMet <- cast(metm, year + cbsa + mdiv + stcou + name + lsad ~ variable)
write.table(cleanMet, "census-population-by-metro.csv", sep = ",", row = FALSE)




