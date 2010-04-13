library(plyr)
options(stringsAsFactors = FALSE)

hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")
hpi$time <- hpi$year + (hpi$quarter - 1) / 4

hpi$city_state <- paste(hpi$city, hpi$state, sep= ", ")

# Summarise each MSA with a few values that represent the pattern of 
# change over time
hpi_peaks <- ddply(hpi, .(fips_msa), summarise, 
  peak_time = time[which.max(hpi)],
  peak_hpi = max(hpi),
  prop_change = max(hpi) / hpi[time == 2009],
  yearly_change = (hpi[time == 2009] / hpi[time == 2006]) ^ (1 / 3) - 1
)

names(hpi_peaks)[1] <- "fips"

write.table(hpi_peaks, "hpi-peaks.csv", sep = ",", row = F)