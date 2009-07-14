setwd(choose.dir())
clean <- read.csv("gdp-metro.csv")
options(StringsAsFactors = FALSE)

# dim(subset(clean, indust == 1))
# 2184 5
# sum(is.na(subset(clean, indust == 11)))

library(ggplot2)
sumNA <- function(x) sum(is.na(x))
ddply(clean, .(indust), sumNA) -> naCount
nacount2 <- cbind(naCount, naCount$V1/2184)
names(nacount2) <- c("indust", "count", "perc")

qplot(y = indust, x = perc, data = nacount2) + xlim(
c(0,1)) + geom_segment(aes(xend = 0, yend = indust))+geom_line(x=1, color = "red", linetype = "dashed")

# initial graph showing percent of missing data by industry

indust <- read.csv("indust_dictionary.csv")
nacount3 <- merge(indust, nacount2)

# interesting industries: construction (11), retail trade (35), rail trans (38)
# truck trans(40), finance+insurance (50), real estate (55), accom+food service(74)
# financial activities(102), prof+busi servies(103), leisure+hostipitality(105)
# all have less that ~20 percent n/a, some significantly less

interest <- c(1,2,11,35,38,40,50,55,74,102,103,105)
nacount4 <- subset(nacount3, nacount3$indust %in% interest)
qplot(y=indust, x = perc, data= nacount4, color = "red")+xlim(c(0,1))

# picking out some large metro areas (note 00998 US Metropolitan Portion seems to be a sum total)
clean1 <- subset(clean, indust == 1 & year == 2001)
fips <- read.csv("fips_dictionary.csv")
clean2 <- merge(clean1,fips)
clean2 <- clean2[order(clean2$gdp, decreasing = TRUE),]
head(clean2,n=20)
cities <- clean2$fips[2:11]

clean3 <- subset(clean, cities %in% fips)

ddply(clean3, .(indust), sumNA) -> naCount5
nacount5 <- cbind(naCount5, naCount5$V1/2184,stringsAsFactors=T)
names(nacount5) <- c("indust", "count", "perc")
nacount5 <- subset(nacount5, nacount5$indust %in% interest)
qplot(y=indust, x = perc, data= nacount5, color = "red")+xlim(c(0,1))

# as expected, b/c using only these very large cities the n/a percentage is almost null for the
# interesting industries. Making another category of other.

clean4 <- subset(clean3, clean3$indust %in% interest)

clean5 <- subset(clean4, !indust == 2)
clean5 <- subset(clean5, !indust == 1)
a <- data.frame(NULL)
d <- data.frame(NULL)
for (i in 0:(length(clean5$gdp)/10-1)){
a <- rbind(a , sum(clean5$gdp[c((1+i*10):(10+i*10))],na.rm = T))
d <- rbind(d,clean5$fips[c(1+i*10)])
}
names(a) <- c("sumGDP")
names(d) <- c("fips")

year <- rep(2001:2006, 10)
#this line is wrong. forgot what i was doing so fix later

a <- cbind(d,a,year)
b <- subset(clean4, indust ==1)
c <- merge(a,b)
c$X <- NULL
c$indust = 999
c$gdp2 = (c$gdp - c$sumGDP)
c$sumGDP = NULL
c$gdp = c$gdp2
c$gdp2 = NULL

clean6 <- merge(clean4,c, all = T)
clean6$X = NULL
write.csv(clean6, file = "data-exploration/ten_cities.csv")

# Trying to find the useful cities that don't have too much missin data

cities2 <- clean2$fips
rank <- data.frame(rank = c(1:364))
rank <- cbind(rank,cities2)
names(rank) <- c("rank","fips")
naperc <- function(x){
sum(is.na(x))/6}
ddply(clean, .(indust, fips), naperc) -> percna
cm <- merge(percna,rank)
names(cm) <- c("fips", "indust", "na", "rank")

naperc2 <- subset(cm,indust==103)
qplot(x=rank,y=na,data=naperc2)
qplot(x=rank,y=na,data=cm,facets=~indust)

#potentially lower na industries
#c(1,2,4,11,12,35,38,40,50,45,38,55,68,,71,77,,78,79,80,81,99,102,103,104,105,106,108,109)
indust[indust$indust %in% c(1,2,4,11,12,35,38,40,45,50,55,68,71,77,78,79,80,81,99,102,103,104,105,106,108,109),]

comp <- clean[clean$indust %in% c(1,2,4,11,12,35,38,40,45,50,55,68,71,77,78,79,80,81,99,102,103,104,105,106,108,109),]
comp <- merge(comp, fips)

fourty <- comp[comp$indust == 40, "gdp"]
eleven <- comp[comp$indust == 11, "gdp"]

qplot(x = year, y = log(gdp), data = comp[ comp$indust %in% c(55, 11), ] , geom = "line", facets = ~Metropolitan.Area, colour = as.factor(indust), group = indust, ylab = "GDP (log)", xlab = "Time") + scale_colour_manual(name = "Industry", value = c("11" = "red", "55" = "blue"), breaks = c(11,55), labels = c("Construction", "Real Estate"))-> bob
ggsave(bob, file = "gdp_maybe.pdf", width = 24, height = 18)

# attempting to index data to make above graph interesting

comp2 <- comp[order(comp$indust),]
comp2 <- comp2[order(comp2$fips),]

#code from garret
# 2001 comp

# isolate 2001
comp2001 <- comp[comp$year == 2001,]

# extract gdps
gdp2001 <- comp2001$gdp

# expand vector by repeating each six times
gdp2001 <- gdp2001[rep(1:length(gdp2001), each = 6)]

# add it back to the original
comp2 <- cbind(comp2, gdp2001)
comp2 <- merge(comp2,fips)
comp2$index <- comp2$gdp/comp2$gdp2001
fourty <- comp2[comp2$indust == 40, "gdp"]
eleven <- comp2[comp2$indust == 11, "gdp"]
qplot(x = year, y = index, data = comp2[comp2$indust %in% c(55, 11), ] , geom = "line", facets = ~Metropolitan.Area, colour = as.factor(indust), group = indust, ylab = "GDP (log)", xlab = "Time") + scale_colour_manual(name = "Industry", value = c("11" = "red", "55" = "blue"), breaks = c(11,55), labels = c("Construction", "Real Estate"))-> bob
ggsave(bob, file = "gdp_maybe.pdf", width = 72, height = 54)

#back to categorizing cities.

comp3 <- comp2[comp2$year==2001,]
fourty <- comp3[comp3$indust == 4, "gdp"]
eleven <- comp3[comp3$indust == 45, "gdp"]
qplot(x=log10(eleven),y=log10(fourty))

#results uninteresting. except for 80 which looks dual modal. will try comparing % of total gdp of industries to see if this adds clarity

comp32001 <- comp3[comp3$indust == 1,]

# extract gdps
gdp32001 <- comp32001$gdp

# expand vector by repeating each six times
gdp32001 <- gdp32001[rep(1:length(gdp32001), each = 26)]
comp3 <- cbind(comp3, gdp32001)
comp3$percgdp <- comp3$gdp/comp3$gdp32001
fourty <- comp3[comp3$indust == 45, "percgdp"]
eleven <- comp3[comp3$indust == 11, "percgdp"]
qplot(x=(eleven)^(1/2),y=(fourty)^(1/2))

#intersting results.. finally. 4&45 50&12 12&102 45&11



#next phase: choose a year, and compare indust to indust accross fips to look for city defining ratio/curve

comp <- clean[clean$indust %in% c(1,2,4,11,12,35,38,40,45,50,55,68,71,77,78,79,80,81,99,102,103,104,105,106,108,109),]
comp <- comp[comp$year == c(2001),]
comp <- merge(comp,rank)
comp$X <- NULL
comp <- comp[! comp$fips == 998,]

# data is cleaned and ten cities represented with idust = 999 as an "other" category for the gdp
# not in the interest category. note: ten largest cities.

# Graphical analysis of ten largest cities.

rm(list=ls())
city <- read.csv("data-exploration/ten_cities.csv")

# graph(x) yields a comparison between industry "x" and industry 1, or total gdp

graph <- function(x){
a <- subset(city, indust == 1)
b <- subset(city, indust == x)
c <- data.frame(x=1:60)
c <- cbind(c,a$gdp,b$gdp)
c$x <- NULL
names(c) <- c("x","y")
qplot(x,y,data=c)
}

# with limits
# qplot(x,y,data=c)+xlim(c(0,max(a,na.rm=T)))+ylim(c(0,max(b,na.rm=T)))


