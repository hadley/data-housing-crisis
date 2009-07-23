library(ggplot2)
library(MASS)
library(mgcv)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

source("../helper.r")

  
getAllCities <- function(d)
  rep(paste(unique(d), sep ="", collapse = "\n"), length(d))



conAll <- read.csv(gzfile("../../data/construction-housing-units/construction-housing-units.csv.gz"))
hpi <- read.csv("../../data/fhfa-house-price-index/fhfa-house-price-index-msa.csv")
msa <- read.csv("../../data/msa-name-over-time/msa-codes.csv")


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
#print(head(con))
con$name <- paste(con$city, ", ", con$state, sep = "")

con <- merge(con, msa)#, all.y = TRUE)
con$city_state <- con$name
con$name <- NULL
#print(head(con))
con <- ddply(con, c("msa_code"), transform, stripName = getAllCities(city_state), .progress = "text")
con <- ddply(con, c("stripName"), transform, n_sm = qsmooth(n, time), .progress = "text")

con <- ddply(con, c("stripName"), transform, n_sm_idx = index_with_time(n_sm, time), .progress = "text")

#con <- ddply(con, c("stripName"), transform, n_ds = qdeseas(n, (time %% 1 + 1/24) * 12), .progress = "text")
#con <- ddply(con, c("stripName"), transform, n_log_ds = log_qdeseas(n, (time %% 1 + 1/24) * 12), .progress = "text")
#con <- ddply(con, c("stripName"), transform, n_log = log_d(n), .progress = "text")
#con <- ddply(con, c("stripName"), transform, n_log_sm = log_smooth(n, time), .progress = "text")

#
conOriginal <- con


con$value <- con$units <- con$size <- NULL
hpi$year <- hpi$quarter <- hpi$error <- NULL

# move hpi to value
hpi$value <- hpi$hpi
hpi$hpi <- NULL

#con$value <- con$n
con$value <- con$n_sm_idx
con$n <- NULL

type <- rep("Total Construction", nrow(con))
con <- cbind(con[, c("city", "state", "msa_code", "city_state", "stripName", "time", "value")], type)

type <- rep("HPI", nrow(hpi))
hpi <- cbind(hpi[, c("city", "state", "msa_code", "city_state", "stripName", "time", "value")], type)









both <- rbind(con, hpi)



# Merced MSA == 32900

merc <- both[both$msa_code == 32900, ]

mercPlot <- qplot(x = time, y = value, data = merc, colour = type, geom = "line", xlab = "Time", ylab = "Value", main = unique(merc$stripName)) + scale_colour_manual("Legend", value = c("Total Construction" = "red", "HPI" = "blue"))
#print(mercPlot)


makePlot <- function(Data)
{
  
  p <- qplot(x = time, y = value, data = Data, colour = type, geom = "line", xlab = "Time", ylab = "Value", main = unique(Data$stripName)) + scale_colour_manual("Legend", value = c("Total Construction" = "red", "HPI" = "blue"))
  
  p
  
}

# 10420 10580 10500 10740 10780 11020
plots <- list()

plots$one <- makePlot( both[both$msa_code == 32900, ])

plots$two <- makePlot( both[both$msa_code == 10420, ])
plots$three <- makePlot( both[both$msa_code == 10580, ])
plots$four <- makePlot( both[both$msa_code == 10500, ])
plots$five <- makePlot( both[both$msa_code == 10740, ])
plots$six <- makePlot( both[both$msa_code == 10780, ])
plots$seven <- makePlot( both[both$msa_code == 11020, ])


dir.create("exports")
for(i in unique(both$msa_code))
{
#  p <- makePlot( both[both$msa_code == i, ] )
#  ggsave(p , file = paste("exports/", i , ".pdf", sep = "", collapse = ""), height = 6, width = 8)
}







Data <- merc
  p <- qplot(x = time, y = log_d(value), data = Data, colour = type, geom = "line", xlab = "Time", ylab = "Value", main = unique(Data$stripName)) + scale_colour_manual("Legend", value = c("Total Construction" = "red", "HPI" = "blue"))
print(p)