library(ggplot2)

gdp <- read.csv("gdp-summary.csv")

# Add in population
pop <- read.csv("../../data/census-population/population-msa.csv")
names(pop)[12] <- "fips"

gdp <- merge(gdp, pop[c("fips", "year", "popestimate")])

# Do cities of different sizes have different industry profiles? -------------
qplot(log10(popestimate), gdp.prop, data = subset(gdp, year == 2008)) + 
  facet_wrap(~ industry) + 
  geom_smooth(method = "lm")
  
qplot(log10(popestimate), gdp.pc, data = subset(gdp, year == 2008)) + 
  facet_wrap(~ industry, scale = "free_y") + 
  geom_smooth(method = "lm")
  
# Some trends, but probably not worth breaking into groups

# Calculate average gdp.prop for each industry -------------------------------

overall <- ddply(gdp, c("industry", "year"), summarise, 
  overall.prop = weighted.mean(gdp.prop, popestimate))
  
qplot(year, overall.prop, data = overall, geom = "line") + 
  facet_wrap(~ industry)
  
gdp <- merge(gdp, overall, by = c("industry", "year"))

# Calculate diversity statistics ---------------------------------------------
hpi_peaks <- read.csv("hpi-peaks.csv") 

diversity <- ddply(subset(gdp, year == 2006), c("fips", "year"), summarise, 
  d1 = sum((gdp.prop - overall.prop)^2 / overall.prop),
  d2 = sd(gdp.prop),
  d3 = sum((gdp.prop - 1 / 19) ^ 2),
  biggest.gdp = max(gdp.prop),
  n.big = sum(gdp.prop > 0.1)
)
diversity <- merge(diversity, hpi_peaks, by = "fips")

qplot(d1, data = diversity, binwidth = 0.1)
qplot(log10(d1), data = diversity, binwidth = 0.1)  

qplot(d2, data = diversity, binwidth = 0.01)  
qplot(d1, d2, data = diversity)  

# Compare to HPI data --------------------------------------------------------

qplot(d1, prop_change, data = diversity)
qplot(d2, prop_change, data = diversity)
qplot(d3, prop_change, data = diversity)
qplot(biggest.gdp, prop_change, data = diversity)

qplot(d1, yearly_change, data = diversity)
qplot(d2, yearly_change, data = diversity)
qplot(d3, yearly_change, data = diversity, xlab= "Industrial Diversity", ylab= "HPI Annual Growth Rate")
ggsave("diversity.pdf", width = 10, height = 6)
qplot(biggest.gdp, yearly_change, data = diversity)
