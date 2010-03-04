library(stringr)
library(plyr)

gdp <- read.csv("../../data/gdp-metro/gdp-metro.csv")

indust <- read.csv("../../data/gdp-metro/indust-dictionary.csv")
fips <-  read.csv("../../data/gdp-metro/fips-dictionary.csv")

top_indust <- subset(gdp, indust %in% c(11, 3, 10, 12, 36, 45, 58, 63, 62, 66, 78, 67, 55, 12, 71, 50, 6, 74, 100, 104))

gdp <- merge(top_indust, indust, by = "indust")
gdp <- merge(gdp, fips, by = "fips")

# Make industry and city labels small enough to plot

gdp$industry <- abbreviate(gdp$Industry, 8)
gdp$Industry <- NULL

gdp$metro <- str_replace(gdp$Metropolitan.Area, " \\(MSA\\)", "")
gdp$metro <- abbreviate(gdp$metro, 15)
gdp$Metropolitan.Area <- NULL

gdp <- gdp[!is.na(gdp$gdp), ]

# Merge with population, index gdp by population

pop <- read.csv("../../data/census-population/population-msa.csv")
names(pop)[12] <- "fips"

gdp <- merge(gdp, pop[c("fips","year","popestimate")], 
  by = c("fips","year"))
gdp$index.gdp <- gdp$gdp / gdp$popestimate

write.table(gdp, "gdp-selected.csv", sep = ",", row = F)

# Calculate and save summary statistics

growth <- ddply(gdp, c("industry", "indust", "fips"), function(df) {
  exp(coef(lm(log(gdp) ~ I(year - 2001), data = df)))
}, .progress = "text")
names(growth)[4:5] <- c("start", "growth")

write.table(growth, "gdp-growth.csv", sep = ",", row = F)
