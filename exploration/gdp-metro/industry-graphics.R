library(stringr)
library(ggplot2)

gdp <- read.csv("../../data/gdp-metro/gdp-metro.csv")

selected <- subset(gdp, fips %in% c(35620, 31100,26420, 37980,38060, 41700, 41740, 19100, 41940, 19820, 26900, 27260, 41860, 18140, 12420, 32820, 12580) & indust %in% c(11, 3, 10, 12, 36, 45, 58, 63, 62, 66, 78, 67, 55, 12, 71, 50, 6, 74, 100, 104))

# Match with industry and city labels

indust <- read.csv("../../data/gdp-metro/indust-dictionary.csv")
fips <-  read.csv("../../data/gdp-metro/fips-dictionary.csv")

selected <- merge(selected, indust, by = "indust")
selected <- merge(selected, fips, by = "fips")

# Make industry and city labels small enough to plot
selected$industry <- abbreviate(selected$Industry, 8)
selected$Industry <- NULL

selected$metro <- str_replace(selected$Metropolitan.Area, " \\(MSA\\)", "")
selected$metro <- abbreviate(selected$metro, 15)
selected$Metropolitan.Area <- NULL

selected <- selected[!is.na(selected$gdp), ]


qplot(year, gdp, data = selected, colour = industry, geom="line", facets=~ metro, log = "y")
qplot(year, gdp, data = selected, colour = metro, geom="line", facets=~ industry, log = "y")

# need to group by the industry as well as color it by industry.

one <- subset(selected, fips == 12420 & indust == 36)
lm(log(gdp) ~ I(year - 2001), data = one)

growth <- ddply(selected, c("industry", "metro"), function(df) {
  exp(coef(lm(log(gdp) ~ I(year - 2001), data = df)))
})
names(growth)[3:4] <- c("start", "growth")

qplot(growth, industry, data = growth) + 
  geom_vline(xintercept = 1, colour = "grey50")
qplot(growth, metro, data = growth) + 
  geom_vline(xintercept = 1, colour = "grey50")
  
qplot(start, growth, data = growth)

# Next: remove suspiciously low starting values
# Merge with population, index by populatio
pop <- read.csv("../../data/census-population/population-msa.csv")
names(pop)[12] <- "fips"

withpop <- merge(selected, pop[c("fips","year", "popestimate")], 
  by = c("fips","year"))
withpop$index.gdp <- withpop$gdp / withpop$popestimate

#decided to look at only construction

construction <- subset(withpop, industry== "Cnstrctn")

qplot(year, gdp, data = construction, geom="line", facets=~ metro)
qplot(year, index.gdp, data = construction, geom="line", facets=~ metro)