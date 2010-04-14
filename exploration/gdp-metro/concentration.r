# Spending a large proportion of total GDP in a single industry appears
# (in general) to be protective.  The only exception is construction.

library(ggplot2)

# Load data
gdp <- read.csv("gdp-summary.csv")
hpi_peaks <- read.csv("hpi-peaks.csv") 
gdp_hpi <- merge(hpi_peaks, gdp, by = "fips", all.x = TRUE)

# Calculate proportion of spending in each industry on average
gdp_overall <- ddply(subset(gdp, year >= 2006), c("fips", "industry"), summarise,
  gdp.prop = weighted.mean(gdp.prop, gdp))
gdp_overall <- merge(gdp_overall, hpi_peaks, by = "fips")

qplot(gdp.prop, yearly_change, data = gdp_overall) +
  facet_wrap(~ industry)
qplot(gdp.prop, yearly_change, data = gdp_overall) +
  facet_wrap(~ industry, scale = "free_x")



# Select industries where concentration is most protective
industries <- c("Edctnahs" = "Education", "Govrnmnt" = "Government", "Mining" = "Mining", "Mnfctrng" = "Manufacturing")

selected <- subset(gdp_overall, industry %in% names(industries))
selected$industry <- industries[as.character(selected$industry)]

ggplot(selected, aes(gdp.prop, yearly_change)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_point() +
  facet_wrap(~ industry) +
  xlab("Average percent of spending (2006-2008)") +
  ylab("Average annual growth rate of HPI (2006-2009)")

ggsave("concentration.pdf", width = 10, height = 6)