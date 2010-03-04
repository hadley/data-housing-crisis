library(stringr)
library(ggplot2)

gdp <- read.csv("gdp-selected.csv")


qplot(year, gdp, data = gdp, colour = industry, geom="line", facets=~ metro, log = "y")
qplot(year, index.gdp, data = gdp, colour = industry, geom="line", facets=~ metro, log = "y")

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

growth <- ddply(selected, c("industry", "metro"), function(df) {
  exp(coef(lm(log(gdp) ~ I(year - 2001), data = df)))
}, .progress = "text")

names(growth)[3:4] <- c("start", "growth")

growth <- subset(growth, !is.na(growth))
write.table(growth, "gdp-growth.csv", sep = ",", row = F)


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
