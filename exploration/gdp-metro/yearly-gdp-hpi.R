library(ggplot2)
library(stringr)
library(plyr)

gdp<-read.csv("gdp-summary.csv")
hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")


#calculating yearly change: data is in quaters to took the yearly data from quater 4 only
hpi.select<-subset(hpi,quarter==4) 

#I got this code from gdp.delta which is the yearly change, but I am a little confused what diff does. The code works- I just don't know if it gives me what I want
hpi.select <- ddply(hpi.select, c("fips_msa"), transform, 
  hpi.delta = c(diff(hpi) / diff(year), NA) / hpi,
  .progress = "text")


#??
hpi.gdp <- merge(gdp, hpi.select, by.x="fips", by.y="fips_msa")

#??
qplot(hpi.delta, gdp.delta, data= subset(hpi.gdp, indust == 3)) + 	facet_wrap(~year)