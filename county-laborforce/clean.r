# Source:http://www.bls.gov/lau/#table (county data)
# See readme.md to see how original files were modified before loaded into R

library(ggplot2)
new<-read.csv("raw/laborforce00.csv")
names(new)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce00", "employ00", "Unemploy00", "rate00")
new2<-new[,-c(1,4,5)]

lf<-read.csv("raw/laborforce01.csv")
names(lf)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce01", "employ01", "Unemploy01", "rate01")
lf01<-lf[,-c(1,4,5)]
both<-merge(new2,lf01,by=c("state","county"),all=T)

lf2<-read.csv("raw/laborforce02.csv")
names(lf2)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce02", "employ02", "Unemploy02", "rate02")
lf02<-lf2[,-c(1,4,5)]
both2<-merge(both,lf02,by=c("state","county"),all=T)

lf3<-read.csv("raw/laborforce03.csv")
names(lf3)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce03", "employ03", "Unemploy03", "rate03")
lf03<-lf3[,-c(1,4,5)]
both3<-merge(both2,lf03,by=c("state","county"),all=T)

lf4<-read.csv("raw/laborforce04.csv")
names(lf4)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce04", "employ04", "Unemploy04", "rate04")
lf04<-lf4[,-c(1,4,5)]
both4<-merge(both3,lf04,by=c("state","county"),all=T)

lf5<-read.csv("raw/laborforce05.csv")
names(lf5)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce05", "employ05", "Unemploy05", "rate05")
lf05<-lf5[,-c(1,4,5)]
both5<-merge(both4,lf05,by=c("state","county"),all=T)

lf6<-read.csv("raw/laborforce06.csv")
names(lf6)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce06", "employ06", "Unemploy06", "rate06")
lf06<-lf6[,-c(1,4,5)]
both6<-merge(both5,lf06,by=c("state","county"),all=T)

lf7<-read.csv("raw/laborforce07.csv")
names(lf7)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce07", "employ07", "Unemploy07", "rate07")
lf07<-lf7[,-c(1,4,5)]
both7<-merge(both6,lf07,by=c("state","county"),all=T)

lf8<-read.csv("raw/laborforce08.csv")
names(lf8)[1:9]<-c("laus", "state", "county", "place", "Year","laborforce08", "employ08", "Unemploy08", "rate08")
lf08<-lf8[,-c(1,4,5)]
bothall<-merge(both7,lf08,by=c("state","county"),all=T)


save(bothall,file="laborforce00-08.rdata")
load(file="laborforce00-08.rdata")

write.table(bothall, "county-laborforce.csv", sep = ",", row = FALSE)
