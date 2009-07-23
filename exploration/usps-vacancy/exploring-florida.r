# exploring vacancy data
library(plyr)
library(ggplot2)
library(R.oo)
options(stringsAsFactors = F)
savePlot <- function(..., plot= TRUE, big = FALSE)
{
nameOfPlot <- substitute(...)
nameOfPlot <- gsub("_", "-", nameOfPlot)
cat("\nPrinting plot ", nameOfPlot,".pdf in folder 'exports'", sep ="")
if(plot)
if(big)
ggsave(..., file=paste("exports/", nameOfPlot,"-BIG.pdf",sep = "", collapse = ""), width=20, height=15) 
else
ggsave(..., file=paste("exports/", nameOfPlot,".pdf",sep = "", collapse = ""), width=8, height=6)
else
cat("\nJust Kidding!!!\n")
cat("\n")
}

all <- read.csv(gzfile("../../data/usps-vacancy/usps-vacancy.csv.gz"))

# states with big bubbles have larger amounts of vacancies
vacancy_by_state <- qplot(year + quarter/4, vac + nostat, data = all, facets = ~ statefips, main = "Number of vacant addresses")
vacancy_by_state
savePlot(vacancy_by_state, big = T)

per_vacancy_by_state <- qplot(year + quarter/4, (vac + nostat)/total, data = all, facets = ~ statefips, main = "Percent of addresses vacant")
per_vacancy_by_state
savePlot(per_vacancy_by_state, big = T)

florida <- all[all$statefips == 12,]
index <- function(x) x / x[1]
florida_all <- ddply(florida, .(countyfips), transform, vac_ind = index(vac))
florida_all <- ddply(florida_all, .(countyfips), transform, nostat_ind = index(nostat))


# vacancies by county (Broward/Ft. Lauderdale, Palm Beach, and Miami-Dade lead the way)
#_______________________________________________________________
qplot(year + quarter/4, vac, data = florida, geom = "line", group = countyfips, colour=countyfips, main = "Vacancies in Florida Counties")
# Broward County (Ft. Lauderdale leads the pack)

# when indexed Flagler county stands out
#________________________________________
FL_indexed <- qplot(year + quarter/4, vac_ind, data = florida_all, geom = "line", group = countyfips, colour=countyfips, main = "Vacancies in Florida Counties")
FL_indexed
savePlot(FL_indexed)

florida_all$tot_vac <- with(florida_all, vac + nostat)
florida_all <- ddply(florida, .(countyfips), transform, tot_ind = index(total))


FL_tot_indexed <- qplot(year + quarter/4, tot_ind, data = florida_all, geom = "line", group = countyfips, colour=countyfips, main = "Total addresses in Florida Counties")
FL_tot_indexed

florida_all <- ddply(florida, .(countyfips), transform, tot_vac_ind = index(tot_vac))

FL_total_vac_indexed <- qplot(year + quarter/4, tot_vac_ind, data = florida_all, geom = "line", group = countyfips, colour=countyfips, main = "Vacancies and nostats in Florida Counties")

qplot(year + quarter/4, tot_vac, data = florida_all, geom = "line", group = countyfips, colour=countyfips, main = "Vacancies and nostats in Florida Counties")

# of vacancy days sharply increases
# _____________________________________
quartz()
qplot(year + quarter/4, vac_days, data = florida, geom = "line", group = countyfips, colour=countyfips, main = "Number of vacancy days in Florida Counties")



# nostat unindexed
qplot(year + quarter/4, nostat, data = florida_all, geom = "line", group = countyfips, colour=countyfips, main = "Vacancies in Florida Counties")

# nostat indexed all over the place
#_____________________________
qplot(year + quarter/4, nostat_ind, data = florida_all, geom = "line", group = countyfips, colour=countyfips, main = "Vacancies in Florida Counties")



# of addresses jumped up in 2008
quartz()
qplot(year + quarter/4, total, data = florida, geom = "line", group=countyfips, main = "Total addresses in Florida Counties")