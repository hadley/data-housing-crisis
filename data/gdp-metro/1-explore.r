# Basic exploration and data checking

gdp <- read.csv("gdp-metro.csv")

library(ggplot2)
qplot(gdp, data = gdp)
qplot(gdp, data = gdp) + xlim(0, 1000)

qplot(log10(gdp), data = gdp, binwidth = 0.1)

qplot(industry, weight = gdp, data = gdp)