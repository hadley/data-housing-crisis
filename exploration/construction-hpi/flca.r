library(ggplot2)
library(MASS)
library(mgcv)
library(R.oo)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

source("helper.r")

conAll <- read.csv(gzfile("../../construction-housing-units/construction-housing-units.csv.gz"))
hpi <- read.csv("../../fhfa-house-price-index/fhfa-house-price-index-msa.csv")
msa <- read.csv("../../msa-name-over-time/msa-codes.csv")
closeAllConnections()

msa$city_state <- msa$name
msa$name <- NULL

hpi$time <- hpi[,"year"] + (hpi[,"quarter"] - .5) / 4
hpi$city_state <- paste(hpi$city, hpi$state, sep = ", ")
colnames(hpi)[colnames(hpi) == "fips_msa"] <- "msa_code"


conAll$size <- c("1" = "single", "2" = "multi", "3-4" = "multi", "5-Inf" = "multi", "Total" = "Total")[conAll$units]
colnames(conAll)[colnames(conAll) == "housing_units"] <- "n"
colnames(conAll)[colnames(conAll) == "valuation"] <- "value"

#conAll$month <- (conAll$time %% 1 + 1/24) * 12
conAll$time <- conAll$year + conAll$month / 12 - 1/24
conAll$month <- conAll$year <- NULL
#print(head(conAll))

con <- conAll[conAll$size == "Total", ]
#print(head(con))
con$city_state <- paste(con$city, ", ", con$state, sep = "")

msacitystate <- strsplit(msa$city_state, ",")
for(i in 1:nrow(msa))
{
  msa$city[i] <- trim(msacitystate[[i]][1])
  msa$state[i] <- trim(msacitystate[[i]][2])
}


msaCAFL <- msa[msa$state %in% c("CA", "FL"), ]


conCAFL <- merge(msaCAFL, con)
hpiCAFL <- merge(msaCAFL, hpi)

hpiCAFL$year <- hpiCAFL$quarter <- hpiCAFL$error <- NULL
conCAFL$units <- conCAFL$size <- NULL

qplot(x = time, y = n, data = conCAFL, geom = "line", facets = ~name)

conCAFLm <- melt(conCAFL, id = c("city", "state", "msa_code", "city_state", "time"))
hpiCAFLm <- melt(hpiCAFL, id = c("city", "state", "msa_code", "city_state", "time"))

all <- rbind(conCAFLm, hpiCAFLm)
all <- ddply(all, c("msa_code"), transform, stripName = getAllCities(city_state), .progress = "text")

qplot(time, value, data = all[all$variable %in% c("hpi", "n"), ] , geom = "line", colour = variable) + facet_wrap(~stripName, scales = "free")

