library(ggplot2)
options(stringsAsFactors = FALSE)

pop <- read.csv("raw-county.csv", stringsAsFactors = FALSE)
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
write.table(clean, "population-county.csv", sep = ",", row = FALSE)


########################################################################
########################################################################
########################################################################

met <- read.csv("raw-cbsa.csv", skip = 4, stringsAsFactors = FALSE)
met <- met[ -((nrow(met) - 1):nrow(met)), ]
names(met) <- tolower(strsplit(readLines("raw-cbsa.csv", 1), ",")[[1]])

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

# Focus on just MSAs
msa <- subset(metm, lsad == "Metropolitan Statistical Area")
msa$lsad <- NULL
msa$stcou <- NULL
msa$mdiv <- NULL

msa2 <- cast(msa, name + year ~ variable)

msa_codes <- read.csv("../msa-changes/msa-codes.csv")
msa2$city <- gsub(",| /1", "", msa2$name)
msa2$name <- NULL
msa3 <- merge(msa2, msa_codes, by = "city", all.x = TRUE)

write.table(msa3, "population-msa.csv", sep = ",", row = FALSE)

