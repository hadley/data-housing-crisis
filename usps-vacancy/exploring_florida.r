# exploring vacancy data
library(ggplot2)
quartz()
options(na.action = "na.exclude")

all <- read.csv(gzfile("new-vacancy.csv.gz"))

# states with big bubbles have larger amounts of vacancies
qplot(year + quarter/4, vac + nostat, data = all, facets = ~ statefips, main = "Number of vacant addresses")
ggsave("all_states.pdf")

florida <- all[all$statefips == 12,]
index <- function(x) x / x[1]
florida_all <- ddply(florida, .(countyfips), transform, vac_ind = index(vac))
florida_all <- ddply(florida_all, .(countyfips), transform, nostat_ind = index(nostat))


# vacancies by county (Broward/Ft. Lauderdale, Palm Beach, and Miami-Dade lead the way)
#_______________________________________________________________
qplot(year + quarter/4, vac, data = florida, geom = "line", colour=countyfips, main = "Vacancies in Florida Counties")
# Broward County (Ft. Lauderdale leads the pack)

# when indexed Flagler county stands out
#________________________________________
qplot(year + quarter/4, vac_ind, data = florida_all, geom = "line", colour=countyfips, main = "Vacancies in Florida Counties")



# of vacancy days sharply increases
# _____________________________________
quartz()
qplot(year + quarter/4, vac_days, data = florida, geom = "line", colour=countyfips, main = "Total addresses in Florida Counties")


# nostat unindexed
qplot(year + quarter/4, nostat, data = florida_all, geom = "line", colour=countyfips, main = "Vacancies in Florida Counties")

# nostat indexed all over the place
#_____________________________
qplot(year + quarter/4, nostat_ind, data = florida_all, geom = "line", colour=countyfips, main = "Vacancies in Florida Counties")



# of addresses jumped up in 2008
quartz()
qplot(year + quarter/4, total, data = florida, geom = "line", group=countyfips, main = "Total addresses in Florida Counties")