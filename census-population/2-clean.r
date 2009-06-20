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