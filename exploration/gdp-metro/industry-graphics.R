library(ggplot2)

gdp <- read.csv("../../data/gdp-metro/gdp-metro.csv")

topcities <- subset(gdp, fips %in% c("35620", "31100","26420", "37980","38060", "41700", "41740", "19100", "41940",  "19820",  "26900",  "27260", "41860", "18140",  "12420", "32820", "12580"))

industries_cities <- subset(topcities, indust %in% c("11", "3", "10", "12", "36", "45", "58", "63", "62", "66", "78", "67", "55", "12","71", "50", "6", "74", "100","104"))

industries_cities <- industries_cities[!is.na(industries_cities$gdp),]

p <- qplot(x= year,y= gdp, data= industries_cities, colour = indust, group = indust, geom="line", facets=~ fips)
# need to group by the industry as well as color it by industry.

print(p)