# source: http://www.huduser.org/datasets/usps.html
# see download.r for script to get all

# This is a good strategy to follow when the data are large (so keeping in git
# is a pain), and there is little connection between the file name and name 
# with a sensible naming convention

library(foreign)
library(plyr)

dir.create("cleaned")
dir.create("cleaned/by-quarter")
dir.create("cleaned/by-year")

data_names <- c("vacancy-2008-01","vacancy-2008-02","vacancy-2008-03","vacancy-2008-04","vacancy-2009-01")

for(i in 1:length(data_names))
{

  vac <- read.dbf(paste("original/", data_names[i], ".dbf", sep="", collapse=""))
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
  
  
  county <- ddply(res, .(fips), numcolwise(sum), .progress = "text")
  
  write.table(county, paste("cleaned/by-quarter/", data_names[i], ".csv", sep="", collapse=""), sep = ",", row = F)
}



make_year_csv <- function(newName, namesData)
{
  data_year <- NULL
  
  for(i in 1:length(namesData))
  {
    quarter <- read.csv(paste("cleaned/by-quarter/", namesData[i], ".csv", sep="", collapse = ""))
    data_year <- rbind(data_year, quarter)
    
  }

  write.table(data_year, paste("cleaned/by-year/", newName, ".csv", sep = "", collapse=""), sep = ",", row = F)
  
}


data_names_2008 <- c("vacancy-2008-01","vacancy-2008-02","vacancy-2008-03","vacancy-2008-04")
make_year_csv("vacancy-2008",data_names_2008)
make_year_csv("vacancy-2009","vacancy-2009-01")
