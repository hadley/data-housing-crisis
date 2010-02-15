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

# Merge with population, index by population
pop <- read.csv("../../data/census-population/population-msa.csv")
names(pop)[12] <- "fips"

withpop <- merge(selected, pop[c("fips","year","popestimate")], 
  by = c("fips","year"))
withpop$index.gdp <- withpop$gdp / withpop$popestimate


qplot(year, gdp, data = selected, colour = industry, geom="line", facets=~ metro, log = "y")
qplot(year, index.gdp, data = withpop, colour = industry, geom="line", facets=~ metro, log = "y")

# Compare industries across cities ------------------------------------------
# Get rid of all city-industry combinations with < 3 points
selected2 <- ddply(selected, c("industry", "metro"), transform, 
  n = length(metro))
selected2 <- subset(selected2, n >= 4)

qplot(year, gdp, data = selected2, group = metro, geom="line", facets=~ industry, log = "y")
# Industries do seem share common patterns across cities, but it's difficult
# to compare within an industry because the geometric growth dominates smaller
# deviations

# Look at just geometric growth 

growth <- ddply(withpop, c("industry", "metro"), function(df) {
  exp(coef(lm(log(gdp) ~ I(year - 2001), data = df)))
})
names(growth)[3:4] <- c("start", "growth")

growth <- subset(growth, !is.na(growth))

qplot(growth, reorder(industry, growth), data = growth) + 
  geom_vline(xintercept = 1, colour = "grey50")
qplot(growth, metro, data = growth) + 
  geom_vline(xintercept = 1, colour = "grey50")
  
qplot(start, growth, data = growth)

# Remove geometric growth and just look at deviations

deviations <- ddply(selected2, c("industry", "metro"), function(df) {
  mod <- lm(log(gdp) ~ I(year - 2001), data = df)
  d <- resid(mod)
  
  data.frame(year = df$year, dev = d)
})
qplot(year, exp(dev), data = deviations, group = metro, geom="line", 
  facets = ~ industry)
qplot(year, exp(dev), data = deviations, group = metro, geom="line", 
  facets = ~ industry) + ylim(0.8, 1.2) + geom_smooth(aes(group = 1))
