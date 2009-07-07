# source: http://www.huduser.org/datasets/usps.html
# see download.r for script to get all

# This is a good strategy to follow when the data are large (so keeping in git
# is a pain), and there is little connection between the file name and name 
# with a sensible naming convention

library(foreign)
library(plyr)

options(stringsAsFactors = FALSE)

clean_file <- function(fileName, lessThan2008 = FALSE)
{
  
  vac <- read.dbf(fileName)
  names(vac) <- tolower(names(vac))
  
  
  # Only interested in residental data
  #print(head(vac))
  d <- NULL
  if(!lessThan2008)
  {
    #print("greaterThan")
    res <- vac[, c("geoid", "ams_res", "res_vac", "avg_vac_r", "nostat_res", "avg_ns_res")]
    bus <- vac[, c("geoid", "ams_bus", "bus_vac", "avg_vac_b", "nostat_bus", "avg_ns_bus")]
    names(res) <- c("geoid", "total", "vac","vac_avg", "nostat", "nostat_avg")
    names(bus) <- c("geoid", "total", "vac","vac_avg", "nostat", "nostat_avg")
    
    
    all <- list(
      geoid = vac[, "geoid"] , 
      total = res[,"total"] + res[,"total"], 
      vac = res[,"vac"]+ bus[,"vac"],
      vac_avg = (res[,"vac_avg"] * res[,"total"] + bus[,"vac_avg"] * bus[,"total"]) / (res[,"total"] + bus[,"total"]),
      nostat = res[,"nostat"] + bus[,"nostat"] ,
      nostat_avg = (res[,"nostat_avg"] * res[,"total"] + bus[,"nostat_avg"] * bus[,"total"]) / (res[,"total"] + bus[,"total"])
    )
    #print(head(all))    
#    d <- rbind(res, bus, all)
    d <- rbind(all)
      #print(head(d))
  }
  else
  {
    #print("greaterThan")
    d <- vac[, c("geoid","ams","vac","avg_vac","nostat","avg_nostat")]  
    names(d) <- c("geoid","total","vac","vac_avg","nostat","nostat_avg")

  }
  #print("done making d")
  d <- d[,c("geoid", "total", "vac", "vac_avg", "nostat", "nostat_avg")]
  
  # Convert average days to total number of days so can aggregate into fips
  #print("start transform")
  d <- transform(d, 
    vac_days  =   round(vac * vac_avg),
    nostat_days = round(nostat * nostat_avg),
    vac_avg = NULL,
    nostat_avg = NULL
  )
  #print("end transform")  
  
  fips <- substr(as.character(d[,"geoid"]), 0, 5)
  print(head(fips))
  statefips <- substr(fips,1,2)
  countyfips <- substr(fips,3,5)
  fips <- d[,"geoid"] <- NULL
  d <- cbind(countyfips, statefips, d[,colnames(d) != "geoid"])
  #print(head(d))
  
  county <- ddply( d, .(statefips, countyfips), numcolwise(sum), .progress = "text")
#print(head(county))  

  county
}


clean_qtr <- function(qtr, year, lessThan2008)
{
  path <- paste("original/vacancy-", year,"-",qtr, ".dbf", sep="", collapse="")
  if(!file.exists(path)) 
    return()
  
  d <- NULL  
  d <- clean_file(path, lessThan2008)
  
  quarter <- rep(as.numeric(qtr), nrow(d))
  
  d <- cbind(quarter, d)
  
  d
}

clean_year <- function(yr)
{
  cat("\nYear = ", yr,"\n")
  quarter<- c("01", "02", "03", "04")

  d <- ldply(quarter, year = yr, lessThan2008 = yr < 2008, clean_qtr, .progress = "text")

  year <- rep(yr, nrow(d))
  d <- cbind(year, d)
  d
}


all <- ldply(2006:2009, clean_year)

colnames(all) <- tolower(colnames(all))
#print(head(all))  

all <- all[,c("year", "quarter", "statefips", "countyfips", colnames(all)[!colnames(all) %in% c("year", "quarter", "statefips", "countyfips", "fips", "geoid")])]

#all <- apply(all, MARGIN = 2, FUN = as.numeric)
all <- as.data.frame(all)
cat("\nALL\n")
print(head(all))

write.table(all, gzfile("new-vacancy.csv.gz"), sep = ",", row = F)
#write.table(all, "new-vacancy.txt", sep = ",", row = F)

