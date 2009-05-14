# source: http://www.huduser.org/datasets/usps.html
# see download.r for script to get all

library(foreign)
vac <- read.dbf("vacancy-2009-01.dbf")
names(vac) <- tolower(names(vac))

vac$fips <- substr(vac$geoid, 0, 5)

length(table(vac$fips))
# 3219 = 3141 USA counties + 78 Pueto Rico Municipios

# Only interested in residental data

res <- vac[, c("fips", "ams_res", "res_vac", "avg_vac_r", "nostat_res", "avg_ns_res")]
names(res) <- c("fips", "total", "vac","vac_avg", "nostat", "nostat_avg")

# Convert average days to total number of days so can aggregate into fips
res <- transform(res, 
  vac_days  =   round(vac * vac_avg),
  nostat_days = round(nostat * nostat_avg),
  vac_avg = NULL,
  nostat_avg = NULL
)


library(plyr)
county <- ddply(res, .(fips), numcolwise(sum), .progress = "text")

write.table(county, "vacancy-2009.csv", sep = ",", row = F)