a <- read.csv("original/county-pop-00-08.dbf")
library(ggplot2)

popEst <- NULL
for(i in 10:18)
	popEst <- c(popEst, a[,i])

NumPopChange <- NULL
for(i in 19:27)
	NumPopChange <- c(NumPopChange, a[,i])
	
Births <- NULL
for(i in 28:36)
	Births <- c(Births, a[,i])

Deaths <- NULL
for(i in 37:45)
	Deaths <- c(Deaths, a[,i])

NatIncline <- NULL
for(i in 46:54)
	NatIncline <- c(NatIncline, a[,i])
	
InternationalMig <- NULL
for(i in 55:63)
	InternationalMig <- c(InternationalMig, a[,i])

DomesticMig <- NULL
for(i in 64:72)
	DomesticMig <- c(DomesticMig, a[,i])

NetMig <- NULL
for(i in 73:81)
	NetMig <- c(NetMig, a[,i])
	
Residual <- NULL
for(i in 82:90)
	Residual <- c(Residual, a[,i])
	
GQEstimate <- NULL
for(i in 92:100)
	GQEstimate <- c(GQEstimate, a[,i])
	
BirthRate <- rep(NA,nrow(a))
for(i in 101:108)
	BirthRate <- c(BirthRate, a[,i])
	
DeathRate <- rep(NA,nrow(a))
for(i in 109:116)
	DeathRate <- c(DeathRate, a[,i])
	
NatIncreaseRate <- rep(NA,nrow(a))
for(i in 117:124)
	NatIncreaseRate <- c(NatIncreaseRate, a[,i])
	
InternMigRate <- rep(NA,nrow(a))
for(i in 125:132)
	InternMigRate <- c(InternMigRate, a[,i])	

DomMigRate <- rep(NA,nrow(a))
for(i in 133:140)
	DomMigRate <- c(DomMigRate, a[,i])	

NetMigRate <- rep(NA,nrow(a))
for(i in 141:148)
	NetMigRate <- c(NetMigRate, a[,i]) 
	

Year <- rep(2000:2008, each = nrow(a))


Orig <- a[,c(1:9,91)]
Orig <- rbind(Orig,Orig,Orig,Orig,Orig,Orig,Orig,Orig,Orig)

newData <- cbind(Orig, Year, popEst, NumPopChange,Births, Deaths, NatIncline,  InternationalMig, DomesticMig, NetMig, Residual, GQEstimate, BirthRate, DeathRate, NatIncreaseRate, InternMigRate, DomMigRate, NetMigRate)