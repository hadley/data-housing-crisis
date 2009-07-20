library(mgcv)
library(ggplot2)
options(stringsAsFactors = FALSE)

data <- read.csv(gzfile("construction-housing-units.csv.gz"))
closeAllConnections()
data$size <- c("1" = "single", "2" = "multi", "3-4" = "multi", "5-Inf" = "multi")[data$units]
data$units <- c("1" = "house", "2" = "duplex", "3-4" = "townhouse", "5-Inf" = "apts")[data$units]

#print(unique(data[,"state"]))
#print(str(data))

#data[,"month"] <- factor(data[,"month"], levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") ) 

#print(str(data))
#print(head(data))

time <- data[,"year"] + as.numeric(data[,"month"]) / 12 - 1/24

data <- cbind(time, data)

print(head(data))

States <- c("CA","CA MSA","FL","FL MSA","NV","NV MSA","NY","NY MSA","AZ","AZ MSA","OR","OR MSA")
stat <- States[nchar(States) > 0]
dataTmp <-  data[data[,"state"] %in% stat, ]
dataTmp[,"state"] <- substr(dataTmp[,"state"], 1, 2)

print(head(dataTmp))


  dataTotal <- ddply(data, c("state","city", "time"), summarise, n = sum(housing_units), value = sum(valuation), .progress = "text")

 write.csv(dataTotal, gzfile("Total Contruction.csv.gz"), row.names = FALSE)
 closeAllConnections()

  dataTotal <- read.csv(gzfile("Total Contruction.csv.gz"))
  closeAllConnections()
  

  merc <- dataTotal[dataTotal$city == "Merced", ]
  head(merc)
  smoothNew <- function(var, date)
    predict(gam(var ~ s(date)))
    
  print(head(merc))
  merc$n_sm <- smoothNew(merc$n, merc$time)
  merc$value_sm <- smoothNew(merc$value, merc$time)
  print(head(merc))

mercedFive <- data[data$city == "Merced" & data$units == "apts",]
mercedFive$hu_smo <- smoothNew(mercedFive$housing_units, mercedFive$time)
mercedFive$value_smo <- smoothNew(mercedFive$valuation, mercedFive$time)


MercedFive <- qplot(time, housing_units, data = mercedFive, geom = "line", main = "Merced, CA: Five or More Housing Units", ylab = "Housing Units", xlab = "Time") + 
  geom_line(aes(y = hu_smo)) + 
  geom_vline(aes(xintercept = 2006.375), colour = I("red"), size = 2)




GoodCities <- c()
uniCities <- unique(dataTmp[,"city"])
for(i in uniCities)
{
  a <- range(dataTmp[dataTmp[,"city"] == i, "time"])
  if(a[1] < 2001 & a[2] > 2008)
    GoodCities <- c(GoodCities, TRUE)
  else
    GoodCities <- c(GoodCities, FALSE) 
}


dataCitySelect <- dataTmp[dataTmp[,"city"] %in% uniCities[GoodCities], ]


#p <- qplot(time, housing-units, data = dataTmp, group = city, geom = "line") + facet_grid(units ~ state, scales = "free")
#p <- qplot(time, housing-units, data = dataTmp, group = city, geom = "line") + facet_grid(units ~ state, scales = "free")
#p <- qplot(time, housing-units, data = dataTmp, group = city, geom = "line") + facet_grid(units ~ state, scales = "free")
#p <- qplot(time, housing-units, data = dataTmp, group = city, geom = "line") + facet_grid(units ~ state, scales = "free")

#p <- p + scale_y_log10()

cat("printing\n")
#print(p)

  hpiTrouble <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line", main = "States with HPI Trouble") + facet_grid(units ~ state, scales = "free")
  con0508 <- qplot(time, n, data = dataTotal , group = city, geom = "line") + facet_wrap( ~ state, scales = "free")
  florida <- qplot(time, housing_units, data = dataCitySelect[dataCitySelect[,"state"] == "FL", ], main = "Florida", group = city, geom = "line", colour = city) + facet_wrap(~ units , scales = "free")
  majorstatesgoodcities <- qplot(time, housing_units, data = dataCitySelect, group = city, geom = "line", colour = city) + facet_grid(units ~ state, scales = "free")
  
  Merced <- qplot(time, n, data = merc, geom = "line", xlab = "Time", ylab = "Housing Units", main = "Merced, CA") + geom_line(aes(y = n_sm)) + geom_vline(aes(xintercept = 2006.375), colour = I("red"), size = 2)

  
  
if(TRUE)
{
  print("printing")

  pdf("exports/Six States with HPI Trouble.pdf", width = 8, height = 6)
    print(hpiTrouble)
  dev.off()

  pdf("exports/Construction of MSA of 49 States.pdf", width = 8, height = 6)
    print(con0508)
  dev.off()

  pdf("exports/Trend of FL.pdf", width = 8, height = 6)
    print(florida)
  dev.off()

  pdf("exports/Trend of AZ, CA, FL, NY, OR.pdf", width = 8, height = 6)
    print(majorstatesgoodcities)
  dev.off()
 
  pdf("exports/Merced.pdf", width = 8, height = 6)
    print(Merced)
  dev.off()
  
  pdf("exports/Merced Five Units.pdf", width = 8, height = 6)
    print(MercedFive)
  dev.off()
}