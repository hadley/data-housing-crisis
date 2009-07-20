library(ggplot2)
library(MASS)
library(mgcv)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

source("helper.r")

returnMaxCon <- function(d, maxcolumn)
	unique(d[d[,maxcolumn] == max(d[,maxcolumn]), c(maxcolumn,"time")])

returnMinTimeCon <- function(d, column)
  d[d$time == min(d$time), c(column, "time")]

#containsAllTime <- function(d)
#  all(seq(from = 2000 + 1/8, to =2009, by = 1/4) %in% d$time)
getTimeLen <- function(d) 
  length(unique(d$time))

hasAllTime <- function(d) 
  hasPercentTime(d, 1)

hasPercentTime <- function(d, percent)
  (getTimeLen(d) / max(getTimeLen(con))) >= percent
  
getAllCities <- function(d)
  rep(paste(unique(d), sep ="", collapse = "\n"), length(d))



conAll <- read.csv(gzfile("../../construction-housing-units/construction-housing-units.csv.gz"))
hpi <- read.csv("../../fhfa-house-price-index/fhfa-house-price-index-msa.csv")


closeAllConnections()

hpi$time <- hpi[,"year"] + (hpi[,"quarter"] - .5) / 4
hpi$city_state <- paste(hpi$city, hpi$state, sep = ", ")
colnames(hpi)[colnames(hpi) == "fips_msa"] <- "msa_code"

hpi <- ddply(hpi, c("msa_code"), transform, stripName = getAllCities(city_state), .progress = "text")


conAll$size <- c("1" = "single", "2" = "multi", "3-4" = "multi", "5-Inf" = "multi", "Total" = "Total")[conAll$units]

colnames(conAll)[colnames(conAll) == "housing_units"] <- "n"
colnames(conAll)[colnames(conAll) == "valuation"] <- "value"

#conAll$month <- (conAll$time %% 1 + 1/24) * 12
conAll$time <- conAll$year + conAll$month / 12 - 1/24
conAll$month <- conAll$year <- NULL
#print(head(conAll))



con <- conAll[conAll$size == "Total", ]
msa <- read.csv("../../msa-name-over-time/msa-codes.csv")
#print(head(con))
con$name <- paste(con$city, ", ", con$state, sep = "")

con <- merge(con, msa)#, all.y = TRUE)
con$city_state <- con$name
con$name <- NULL
#print(head(con))

allTimeCon <- ddply(con, .(msa_code) , hasAllTime, .progress = "text")
colnames(allTimeCon) <- c("msa_code", "good")
#print(head(allTimeCon))

con50 <- ddply(con, .(msa_code) , hasPercentTime, percent = .50, .progress = "text")
colnames(con50) <- c("msa_code", "good")
con50 <- merge(con, con50)
con50 <- con50[con50$good, colnames(con50) != "good"]



conGood <- merge(con, allTimeCon)
conGood <- conGood[conGood$good, colnames(conGood) != "good"]

conGood$month <- (conGood$time %% 1 + 1/24) * 12
#conGood <- ddply(conGood, c("city_state"), transform, n_ds = qdeseas(n, (time %% 1 + 1/24) *12), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, stripName = getAllCities(city_state), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_ds = qdeseas(n, month), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_log_ds = log_qdeseas(n, month), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_sm = smooth(n, time), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_log = log_d(n), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_log_sm = log_smooth(n, time), .progress = "text")
conGood$month <- NULL


#cat("\n\nconGood\n")
#print(head(conGood))


con50 <- ddply(con50, c("msa_code"), transform, stripName = getAllCities(city_state), .progress = "text")
con50 <- ddply(con50, c("msa_code"), transform, n_sm = smooth(n, time), .progress = "text")
con50 <- ddply(con50, c("msa_code"), transform, n_log = log_d(n), .progress = "text")





#cat("\n\ncon50\n")
#print(head(con50))
con50RawAndSmooth <- qplot(time, n_log, data = conGood, geom = "line") + geom_line(aes(y = n_log_sm), colour = ("blue")) + facet_wrap( ~ stripName) + opts(legend.position = "none")
savePlot(con50RawAndSmooth)



construction <- conGood[,c("msa_code", "stripName", "time", "n")]
#construction$type <- rep("construction", nrow(construction))
hpindex <- hpi[, c("msa_code", "time", "hpi")]
#hpindex$type <- rep("hpi", nrow(hpindex))


both <- merge(construction, hpindex)
both <- ddply(both, c("stripName"), transform, n_x = index_with_time(n, time), .progress = "text")
both <- ddply(both, c("stripName"), transform, n_sm = smooth(n, time), .progress = "text")
both <- ddply(both, c("stripName"), transform, n_smx = index_with_time(n_sm, time), .progress = "text")
both$hpi_log <- log_d(both$hpi)
both$n_smx_log <- log_d(both$n_smx)
both <- ddply(both, c("stripName"), transform, above150 = mean(n) > 150, .progress = "text")


pop <- read.csv("../../census-population/census-population-by-metro.csv")

pop <- pop[, c("cbsa", "year", "popestimate")]
pop$time <- pop$year + 0.125
pop$msa_code <- pop$cbsa
pop$cbsa <- pop$year <- NULL
pop <- ddply(pop, c("msa_code", "time"), transform, pop = mean(popestimate), .progress = "text")
pop$estimate <- NULL
pop <- ddply(pop, c("msa_code"), transform, popx = index_with_time(pop, time), .progress = "text")

tri <- merge(both, pop)

plotTri <- function(data)
{
  p <- ggplot(data = data , aes(x = time)) + 
      geom_line(aes(y = n_smx, colour = "Construction (Smoothed and Indexed)"))  + 
      geom_line(aes(y = hpi, colour = "Housing Price Index")) + 
      geom_line(aes(y = popx, colour = "Population Estimate Index")) + 
      facet_wrap(~stripName) +   
      scale_colour_manual( name = "Line Type", 
        values = c("Housing Price Index" = "blue", "Construction (Smoothed and Indexed)" = "red", "Population Estimate Index" = "green") 
      ) + 
      labs(x = "Time", y = "Index Value")
  p
}



Merced <- tri[tri$msa_code == "32900", ]
MercedPlot <- plotTri(Merced)
savePlot(MercedPlot)

bothPlotLog <- ggplot(data = both[both$above150,] , aes(x = time)) + geom_line(aes(y = n_smx_log, colour = "Housing Units (smooth)")) + geom_line(aes(y = hpi_log, colour = "HPI")) + facet_wrap(~stripName) +   scale_colour_manual( name = "Line Type", values = c("HPI" = "blue", "Housing Units (smooth)" = "red") )
savePlot(bothPlotLog)

bothPlot <- ggplot(data = both[both$above150,] , aes(x = time)) + geom_line(aes(y = n_smx, colour = "Housing Units (smooth)")) + geom_line(aes(y = hpi, colour = "HPI")) + facet_wrap(~stripName) +   scale_colour_manual( name = "Line Type", values = c("HPI" = "blue", "Housing Units (smooth)" = "red") )
savePlot(bothPlot)

bothPlotRaw <- ggplot(data = both[both$above150,] , aes(x = time)) + geom_line(aes(y = n, colour = "Housing Units (smooth)")) + geom_line(aes(y = hpi, colour = "HPI")) + facet_wrap(~stripName, scales = "free") +   scale_colour_manual( name = "Line Type", values = c("HPI" = "blue", "Housing Units (smooth)" = "red") )
savePlot(bothPlotRaw)


# Removed due to having low housing unit construction
#Pittsfield <- tri[tri$stripName == "Pittsfield, MA", ]
#PittsfieldPlot <- plotTri(Pittsfield)
#PittsfieldPlot

Missoula <- both[both$stripName == "Missoula, MT", ]
MissoulaPlot <- ggplot(data = Missoula, aes(x = time)) + geom_line(aes(y = n, colour = "Housing Units (smooth)")) + geom_line(aes(y = hpi, colour = "HPI")) + facet_wrap(~stripName) +   scale_colour_manual( name = "Line Type", values = c("HPI" = "blue", "Housing Units (smooth)" = "red") )
savePlot(MissoulaPlot)



triPlot <- plotTri(tri[tri$above150,]) + opts(title = "Larger Towns' Indexes")
savePlot(triPlot)
# When wanting to combine data sets, HPI, Construction and Population was a logical choice.  We wanted to see if there was any correlation between them.
# Figure X is a combination of the FHFA Housing Price Index, Census Construction, and Census Population data sets.  It shows that population does not change, while the Construction seems to be the magnet for the Housing Price Index.


mix1 <- con[,c("msa_code", "time", "n")]
colnames(mix1)[3] <- "value"
mix1$type <- rep("Construction", nrow(mix1))


mix2 <- hpi[, c("msa_code", "time", "hpi")]
colnames(mix2)[3] <- "value"
mix2$type <- rep("HPI", nrow(mix2))

mix3 <- pop[, c("msa_code", "time", "pop")]
colnames(mix3)[3] <- "value"
mix3$type <- rep("Population", nrow(mix3))

mix <- rbind(mix1, mix2, mix3)
mix$value_log <- log_d(mix$value)
mix <- merge(msa, mix)
mix <- ddply(mix, c("msa_code"), transform, stripName = getAllCities(name), .progress = "text")
mix$name <- NULL

bigMSA <- unique(tri[tri$above150, "msa_code"])


mixPlot <- qplot(x = time, y = value_log, data = mix[mix$msa_code %in% bigMSA & mix$type != "Population", ], geom = "line", facets = ~stripName, colour = type) +
    scale_colour_manual( name = "Line Type", 
        values = c("HPI" = "blue", "Construction" = "red", "Population" = "green") 
      ) + 
      labs(x = "Time", y = "Value")
      
mixPlotSelect <- qplot(x = time, y = value_log, data = mix[mix$msa_code %in% bigMSA & mix$type != "Population", ], geom = "line", colour = type) + facet_wrap(~stripName, scales = "free") +
    scale_colour_manual( name = "Line Type", 
        values = c("HPI" = "blue", "Construction" = "red", "Population" = "green") 
      ) + 
      labs(x = "Time", y = "Value")
# the above "mix" stuff didn't pan out how I wanted it to.  I wanted to show a more realistic raw with more time points, but did didn't work.

triPlotRawLog <-  ggplot(data = tri[tri$above150,], aes(x = time)) + 
      geom_line(aes(y = log_d(n), colour = "Housing Units (Smoothed Index)"))  + 
      geom_line(aes(y = log_d(hpi), colour = "Housing Price Index")) + 
      geom_line(aes(y = log_d(pop), colour = "Population Estimate Index")) + 
      facet_wrap(~stripName) +   
      scale_colour_manual( name = "Line Type", 
        values = c("Housing Price Index" = "blue", "Housing Units (Smoothed Index)" = "red", "Population Estimate Index" = "green") 
      ) + 
      labs(x = "Time", y = "Index Value")
savePlot(triPlotRawLog)







stop()



#maxCon <- ddply(con, .(state,city) , returnMaxCon, maxcolumn = "n", .progress = "text")
#colnames(maxCon) <- c("state", "city", "max_n", "max_time")
#print(head(maxCon))
#minTimeCon <- ddply(con, .(state,city) , returnMinTimeCon, column = "n", .progress = "text")
#colnames(minTimeCon) <- c("state", "city", "n_2000", "time_2000")
#print(head(minTimeCon))
#conMax <- merge(maxCon, merge(minTimeCon, allTimeCon))
#print(head(conMax))
#conMax <- conMax[conMax$good, - c((ncol(conMax)-1):ncol(conMax)) ]
#conMax$percent_change <- conMax$max_n / conMax$n_2000 * 100
#print(head(conMax))
#ca <- conMax[conMax$state == "CA", ]
#qplot(max_time, max_n, data= ca, geom = "text", label = city)
#qplot(max_time, max_n, data= conMax, geom = "text", label = city)
#qplot(max_time, data= conMax, geom = "histogram")
#qplot(max_time, percent_change, data= conMax, geom = "text", label = city)
#qplot(max_time, percent_change, data= conMax, geom = "text", label = city, log = "y")


qplot(time, n, data= con[con$city == "Bismarck", ], geom = "line")
Bismark <- qplot(time, n, data= conGood[conGood$city == "Bismarck", ], geom = "line", main = "Bismark, ND") + geom_line(aes(y=n_sm), colour = I("red")) + geom_line(aes(y=n_ds), colour = I("blue"))
Bismark
savePlot(Bismark)

florence <- conGood[conGood$city == "Florence", ]
qplot(time, n, data= florence, geom = "line")
Florence <- qplot(time, n, data= florence, geom = "line", main = "Florence, SC") + geom_line(aes(y=n_sm), colour = I("red")) + geom_line(aes(y=n_ds), colour = I("blue"))
Florence
savePlot(Florence)

Deseas_by_State <- qplot(time, n_ds, data = conGood, geom = "line", colour = msa_code) + facet_wrap( ~ state, scales = "free") + opts(legend.position = "none")
savePlot(Deseas_by_State)


Deseas_by_MSA_Log <- qplot(time, n_log_ds, data = conGood, geom = "line") + facet_wrap( ~ msa_code) + opts(legend.position = "none")
savePlot(Deseas_by_MSA_Log)

Deseas_by_State_Log <- qplot(time, n_log_ds, data = conGood, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(Deseas_by_State_Log)


Smooth_by_State <- qplot(time, n_sm, data = conGood, geom = "line", colour = msa_code) + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(Smooth_by_State)

Smooth_by_MSA <- qplot(time, n_sm, data = conGood, geom = "line") + facet_wrap( ~ msa_code) + opts(legend.position = "none")
savePlot(Smooth_by_MSA)

Smooth_by_State_Log <- qplot(time, n_log_sm, data = conGood, geom = "line", colour = msa_code) + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(Smooth_by_State_Log)

Smooth_by_MSA_Log <- qplot(time, n_log_sm, data = conGood, geom = "line") + facet_wrap( ~ msa_code) + opts(legend.position = "none")
savePlot(Smooth_by_MSA_Log)

Raw_by_MSA_Log <- qplot(time, n_log, data = conGood, geom = "line") + facet_wrap( ~ msa_code) + opts(legend.position = "none")
savePlot(Raw_by_MSA_Log)

Raw_and_Smooth_by_MSA_Log <- qplot(time, n_log, data = conGood, geom = "line") + geom_line(aes(y = n_log_sm), colour = ("blue")) + facet_wrap( ~ msa_code) + opts(legend.position = "none")
Raw_and_Smooth_by_MSA_Log2 <- qplot(time, n_log, data = conGood, geom = "line") + geom_line(aes(y = n_log_sm), colour = ("blue")) + facet_wrap( ~ stripName) + opts(legend.position = "none")
savePlot(Raw_and_Smooth_by_MSA_Log)





qplot(time, n, data = con[con$state %in% c("NJ","NY","PA"),], colour = city, geom = "line") + facet_wrap(~state, scales = "free") + opts(legend.position = "none")
qplot(time, n, data = con[con$state %in% c("NJ","NY","PA"),], colour = city, geom = "line") + facet_wrap(~state, scales = "free")
qplot(time, n, data = con[con$state %in% c("NJ","NY","PA") & con$n > 1000,], colour = city, geom = "line") + facet_wrap(~state, scales = "free")
#found that there was a HUGE spike in 2008 in Long Island NY/PA/NJ
# the data in each state is the same






