library(ggplot2)
library(MASS)
library(mgcv)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

# Helper functions ----------------------------------------------------------
index <- function(x) x / x[1]


deseas <- function(var, month) {
  # var - ave(var, month, mean, na.rm = TRUE) + mean(var, na.rm = TRUE)
  resid(rlm(var ~ factor(month))) + mean(var, na.rm = TRUE)
}

smooth <- function(var, date)
  predict(gam(var ~ s(date)))

returnMaxCon <- function(d, maxcolumn)
	unique(d[d[,maxcolumn] == max(d[,maxcolumn]), c(maxcolumn,"time")])

returnMinTimeCon <- function(d, column)
  d[d$time == min(d$time), c(column, "time")]

containsAllTime <- function(d)
  all(seq(from = 2000 + 1/8, to =2009, by = 1/4) %in% d$time)




con <- read.csv(gzfile("construction-housing-units/Total Contruction.csv.gz"))
hpiMax <- read.csv(gzfile("house-price-index/Max HPI.csv.gz"))

closeAllConnections()

con$state <- toupper(con$state)

list <- ddply(list, c("msa", transform, sales_ds = deseas(sales, month))


maxCon <- ddply(con, .(state,city) , returnMaxCon, maxcolumn = "n", .progress = "text")
colnames(maxCon) <- c("state", "city", "max_n", "max_time")
print(head(maxCon))


minTimeCon <- ddply(con, .(state,city) , returnMinTimeCon, column = "n", .progress = "text")
colnames(minTimeCon) <- c("state", "city", "n_2000", "time_2000")
print(head(minTimeCon))


allTimeCon <- ddply(con, .(state,city) , containsAllTime, .progress = "text")
colnames(allTimeCon) <- c("state", "city", "good")
print(head(allTimeCon))


conMax <- merge(maxCon, merge(minTimeCon, allTimeCon))
print(head(conMax))
conMax <- conMax[conMax$good, - c((ncol(conMax)-1):ncol(conMax)) ]



conMax$percent_change <- conMax$max_n / conMax$n_2000 * 100
print(head(conMax))


ca <- conMax[conMax$state == "CA", ]
qplot(max_time, max_n, data= ca, geom = "line")
qplot(max_time, max_n, data= ca, geom = "text", label = city)
qplot(max_time, max_n, data= conMax, geom = "text", label = city)
qplot(max_time, data= conMax, geom = "histogram")
qplot(max_time, percent_change, data= conMax, geom = "text", label = city)
qplot(max_time, percent_change, data= conMax, geom = "text", label = city, log = "y")
qplot(time, n, data= con[con$city == "Bismarck", ], geom = "line")







