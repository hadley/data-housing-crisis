library(stringr)
library(plyr)

gdp <- read.csv("../../data/gdp-metro/gdp-metro.csv")
indust <- read.csv("../../data/gdp-metro/indust-dictionary.csv")
fips <-  read.csv("../../data/gdp-metro/fips-dictionary.csv")

top_indust <- subset(gdp, indust %in% c(11, 3, 10, 12, 36, 45, 58, 63, 62, 66, 78, 67, 55, 12, 71, 50, 6, 74, 100, 104))
# Remove code because non of the top industries have missing values caused
# by omission due to small size
top_indust$code <- NULL

gdp <- merge(top_indust, indust, by = "indust")
gdp <- merge(gdp, fips, by = "fips")

# Make industry and city labels small enough to plot
gdp$industry <- abbreviate(gdp$Industry, 8)
gdp$Industry <- NULL

gdp$metro <- str_replace(gdp$Metropolitan.Area, " \\(MSA\\)", "")
gdp$metro <- abbreviate(gdp$metro, 15)
gdp$Metropolitan.Area <- NULL

gdp <- gdp[!is.na(gdp$gdp), ]

# Compute useful transformations of gdp --------------------------------------

# Merge with population, to create per capita gdp
pop <- read.csv("../../data/census-population/population-msa.csv")
names(pop)[12] <- "fips"

gdp <- merge(gdp, pop[c("fips","year","popestimate")], 
  by = c("fips","year"))
gdp$gdp.pc <- gdp$gdp / gdp$popestimate * 1e6
gdp$popestimate <- NULL

# Divide by total gdp to compute total proportion of gdp

gdp <- ddply(gdp, c("fips", "year"), transform, 
  gdp.prop = gdp / sum(gdp, na.rm = T), .progress = "text")

# Compute year-to-year changes

gdp <- gdp[with(gdp, order(fips, indust, year)), ]
rownames(gdp) <- NULL

gdp <- ddply(gdp, c("fips", "indust"), transform, 
  gdp.delta = c(diff(gdp) / diff(year), NA) / gdp,
  .progress = "text")

write.table(gdp, "gdp-summary.csv", sep = ",", row = F)

# Calculate and save summary statistics

growth <- ddply(gdp, c("industry", "indust", "fips"), function(df) {
  exp(coef(lm(log(gdp) ~ I(year - 2001), data = df)))
}, .progress = "text")
names(growth)[4:5] <- c("start", "growth")

write.table(growth, "gdp-growth.csv", sep = ",", row = F)
