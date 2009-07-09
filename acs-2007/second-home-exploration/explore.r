library(ggplot2)
library(MASS)
library(mgcv)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

# Helper functions ----------------------------------------------------------
savePlot <- function(..., plot= TRUE, big = TRUE)
{
nameOfPlot <- substitute(...)
nameOfPlot <- gsub("_", "-", nameOfPlot)
cat("\nPrinting plot ", nameOfPlot,".pdf in folder 'exports'", sep ="")
if(plot)
if(big)
ggsave(..., file=paste("exports/", nameOfPlot,"_BIG.pdf",sep = "", collapse = ""), width=20, height=15) 
else
ggsave(..., file=paste("exports/", nameOfPlot,".pdf",sep = "", collapse = ""), width=8, height=6)
else
cat("\nJust Kidding!!!\n")
cat("\n")
}

index <- function(x) x / x[1]




shomes <- read.csv("data/shomes.csv", header = T)
st_shomes <- read.csv("data/shomes_by_state.csv", header = T)


# shomes_by-state_free_scales <- qplot(year, owner_occ, data = st_shomes, geom = "line") + facet_wrap( ~ state, scales = "free") + opts(legend.position = "none")

# shomes_by_state <- qplot(year, owner_occ, data = st_shomes, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
# savePlot(shomes_by_state)

by_year <- ddply(st_shomes, .(year), function(df) sum(df$total))
st_shomes <- merge(st_shomes, by_year, by = "year", all.x = T)
names(st_shomes)[7] <- "ACS_size"
st_shomes$prop <- st_shomes$total/st_shomes$ACS_size

ACS_proportions <- qplot(year, prop, data = st_shomes, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(ACS_proportions)
# size of data set doubled in 2005
# ACS decided to add the most additional respondents in FL, CA, and TX
# suggests looking into population

per_shomes_by_state <- qplot(year, per_owner, data = st_shomes, geom = "line") + facet_wrap( ~ state) + opts(legend.position = "none")
savePlot(per_homes_by_state)
# DC has unusually low home ownership rates

per_shomes_by_state_free_scales <- qplot(year, per_owner, data = st_shomes, geom = "line") + facet_wrap( ~ state, scales = "free") + opts(legend.position = "none")
savePlot(per_shomes_by_state_free_scales)

# st_shomes <- ddply(st_shomes, "state", transform, index = index(prop))
# qplot(year, index, data = try, geom = "line", colour = state)
# + facet_wrap( ~ state) + opts(legend.position = "none")

# looking at florida
florida <- shomes[shomes$year == 2007 & shomes$state == "FL",]
qplot(year, per_owner, data = florida, geom = "text", label = fips_puma) 
savePlot(florida)
# the percent of second homes fluctuates among pumas.  The highest rates occur in and near downtown Miami (4009, 4010) and downtown Orlando (2204). Implying that our measure of second homes measures rental properties more than vacation homes.


