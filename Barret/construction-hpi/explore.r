library(ggplot2)
library(MASS)
library(mgcv)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)


# Helper functions ----------------------------------------------------------
savePlot <- function(..., plot= TRUE, big = TRUE)
{
  cat("\nPrinting plot ", substitute(...),".pdf in folder 'exports'", sep ="")
  if(plot)
    if(big)
      ggsave(..., file=paste("exports/", substitute(...),"_BIG.pdf",sep = "", collapse = ""), width=20, height=15)    
    else
      ggsave(..., file=paste("exports/", substitute(...),".pdf",sep = "", collapse = ""), width=8, height=6)
  else
    cat("\nJust Kidding!!!\n")
  cat("\n")
}
index <- function(x) x / x[1]


deseas <- function(var, month) {
  # var - ave(var, month, mean, na.rm = TRUE) + mean(var, na.rm = TRUE)
  resid(rlm(var ~ factor(month))) + mean(var, na.rm = TRUE)
}
qdeseas <-  failwith(NA, deseas, quiet =T)

smooth <- function(var, date)
  predict(gam(var ~ s(date)))

log_d <- function(var)
{
  var[var==0] <- 1
  log(var)
}

log_smooth <- function(var, date)
  smooth(log_d(var), date)
log_deseas <- function(var, date)
  smooth(log_d(var), date)

returnMaxCon <- function(d, maxcolumn)
	unique(d[d[,maxcolumn] == max(d[,maxcolumn]), c(maxcolumn,"time")])

returnMinTimeCon <- function(d, column)
  d[d$time == min(d$time), c(column, "time")]

#containsAllTime <- function(d)
#  all(seq(from = 2000 + 1/8, to =2009, by = 1/4) %in% d$time)
getTimeLen <- function(d) 
  length(unique(d$time))

hasAllTime <- function(d) 
  getTimeLen(d) == max(getTimeLen(con))




conAll <- read.csv(gzfile("../../construction-housing-units/construction-housing-units.csv.gz"))
#hpiMax <- read.csv(gzfile("../../fhfa-house-price-index/Max HPI.csv.gz"))

closeAllConnections()

conAll$size <- c("1" = "single", "2" = "multi", "3-4" = "multi", "5-Inf" = "multi", "Total" = "Total")[conAll$units]

colnames(conAll)[colnames(conAll) == "housing_units"] <- "n"
colnames(conAll)[colnames(conAll) == "valuation"] <- "value"

#conAll$month <- (conAll$time %% 1 + 1/24) * 12
conAll$time <- conAll$year + conAll$month / 12 - 1/24
#print(head(conAll))



con <- conAll[conAll$size == "Total", ]
msa <- read.csv("../../msa-name-over-time/msa_codes.csv")
#print(head(con))
con$name <- paste(con$city, ", ", con$state, sep = "")

con <- merge(con, msa)#, all.y = TRUE)
con$city_state <- con$name
con$name <- NULL
#print(head(con))

allTimeCon <- ddply(con, .(msa_code) , hasAllTime, .progress = "text")
colnames(allTimeCon) <- c("msa_code", "good")
#print(head(allTimeCon))




conGood <- merge(con, allTimeCon)
conGood <- conGood[conGood$good, colnames(conGood) != "good"]

#conGood <- ddply(conGood, c("city_state"), transform, n_ds = qdeseas(n, (time %% 1 + 1/24) *12), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_ds = qdeseas(n, month), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_log_ds = log_deseas(n, month), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_sm = smooth(n, time), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_log = log_d(n), .progress = "text")
conGood <- ddply(conGood, c("msa_code"), transform, n_log_sm = log_smooth(n, time), .progress = "text")


getAllCities <- function(d)
  rep(paste(unique(d), sep ="", collapse = "\n"), length(d))

conGood <- ddply(conGood, c("msa_code"), transform, stripName = getAllCities(city_state), .progress = "text")
cat("\n\nconGood\n")
print(head(conGood))


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






