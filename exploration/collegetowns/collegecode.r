library(ggplot2)
library(R.oo)

college<- read.csv("Collegetown.csv")

college$university <- trim(college$university)
head(college)

location <-read.csv("../city-location/location_database.csv")

collegetown<-merge(college,location,by=c("city","state"))
new <- collegetown[,c(1,2,3, 9)]
new <- unique(new)
names(new)[4]<- "fips_msa"
hpi<- read.csv("../fhfa-house-price-index/fhfa-house-price-index-msa.csv")

head(new)
both<- merge(new,hpi,by=c("fips_msa"))
head(both)

names(both)[2]<- "city"
names(both)[3]<- "state"
both$city.y <- both$state.y <- NULL

both <- both[ ! both$city %in% c("CA", "IN"), ]


head(both)

both$time <-both[,"year"] + (both[,"quarter"] - 1) / 4


#for(i in names(both)[7:9])
#	both <- both[!is.na(both[,i]),]

both$city_state<- paste(both$city,both$state, sep=", " )
print(head(both))
unique(both$city_state)
write.table(both, "college-hpi.csv", sep = ",", row = FALSE)