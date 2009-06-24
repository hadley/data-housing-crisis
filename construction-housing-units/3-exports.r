library(ggplot2)
options(stringsAsFactors = FALSE)

data <- read.csv(gzfile("new-construction.csv.gz"))
closeAllConnections()

data[,"month"] <- factor(data[,"month"], levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") ) 

#print(str(data))
#print(head(data))

time <- data[,"year"] + (as.numeric(data[,"month"]) - .5/12) / 12

data <- cbind(time, data)

#print(head(data))

States <- c("CA","CA MSA","FL","FL MSA","NV","NV MSA","NY","NY MSA","AZ","AZ MSA","OR","OR MSA")
stat <- States[nchar(States) > 0]
dataTmp <-  data[data[,"state"] %in% stat, ]
dataTmp[,"state"] <- substr(dataTmp[,"state"], 1, 2)





#p <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line", colour = state) + facet_grid(bedrooms ~ ., scales = "free")



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



#p <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line") + facet_grid(bedrooms ~ state, scales = "free")
#p <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line") + facet_grid(bedrooms ~ state, scales = "free")
#p <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line") + facet_grid(bedrooms ~ state, scales = "free")
#p <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line") + facet_grid(bedrooms ~ state, scales = "free")

#p <- p + scale_y_log10()

cat("printing\n")
#print(p)



if(TRUE)
{
  
  hpiTrouble <- qplot(time, housing_units, data = dataTmp, group = city, geom = "line", main = "States with HPI Trouble") + facet_grid(bedrooms ~ state, scales = "free")
  pdf("exports/Six States with HPI Trouble.pdf", width = 10, height = 8)
    print(hpiTrouble)
  dev.off()


  con0508 <- qplot(time, housing_units, data = data[data[,"bedrooms"] == "Total" & nchar(data[,"state"]) < 5, ] , group = city, geom = "line") + facet_wrap( ~ state, scales = "free")
  pdf("exports/Construction of MSA of 49 States.pdf", width = 10, height = 8)
    print(con0508)
  dev.off()

  florida <- qplot(time, housing_units, data = dataCitySelect[dataCitySelect[,"state"] == "FL", ], main = "Florida", group = city, geom = "line", colour = city) + facet_wrap(~ bedrooms , scales = "free")
  pdf("exports/Trend of FL.pdf", width = 10, height = 8)
    print(florida)
  dev.off()

  majorstatesgoodcities <- qplot(time, housing_units, data = dataCitySelect, group = city, geom = "line", colour = city) + facet_grid(bedrooms ~ state, scales = "free")
  pdf("exports/Trend of AZ, CA, FL, NY, OR.pdf", width = 10, height = 8)
    print(majorstatesgoodcities)
  dev.off()
}