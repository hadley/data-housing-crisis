library(ggplot2)
options(stringsAsFactors = FALSE)


# Helper functions ----------------------------------------------------------
savePlot <- function(..., plot= TRUE, big = TRUE)
{
nameOfPlot <- substitute(...)
nameOfPlot <- gsub("_", "-", nameOfPlot)

  dir.create("exports/", showWarnings = FALSE)

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



#-----------------------------------------------------------------------------

hpi<- read.csv("../fhfa-house-price-index/fhfa-house-price-index-msa.csv")
both<- read.csv("college-hpi.csv")

hpi$time <- hpi$year + (hpi$quarter - 1) / 4
hpi$city_state <- paste(hpi$city, hpi$state, sep = ", ")

hpi_by_citystate <- qplot(x=time, y=hpi,data=both, facets=~city_state, geom="line")
savePlot(hpi_by_citystate)

# I'd suggest plotting all cities facetted by state, with an overlaid average for each state (so you can see if they were better or worse off)

hpi <- ddply(hpi, c("state", "time"), transform, state_mean = mean(hpi), .progress = "text")


both <- merge(both, unique(hpi[, c("state", "time", "state_mean") ]))



hpi_by_state <- qplot(x=time, y=hpi,data=both, group = city_state, facets=~state, geom="line") + geom_line(aes(y = state_mean), colour = I("red"))

hpiCA <- qplot(x=time, y=hpi,data=hpi[hpi$state == "CA", ], group = city_state, facets=~state, geom="line") + geom_line(aes(y = state_mean), colour = I("red")) + geom_line(data = both[both$state == "CA", ] , colour = I("blue"))

#Makes a plot of each state with the cities, mean, and college towns.
#pdf("exports/all-states.pdf", width = 16, height = 12)
#  for(i in unique(both$state))
#    print(qplot(x=time, y=hpi,data=hpi[hpi$state == i, ], group = city_state, facets=~state, geom="line") + geom_line(aes(y = state_mean), colour = I("red")) + geom_line(data = both[both$state == i, ] , colour = I("blue")))
#dev.off()


# Make a super data set that contains the state mean, each city's hpi and the college towns.  Fun!
types <- c("Cities", "College Towns", "Mean of All Cities")

d <- hpi[nchar(hpi$state) < 3 & hpi$state %in% both$state , ]

m <- d[, c("state", "time", "state_mean", "city_state")]
m$Type <- rep(types[3], nrow(m))
m$city_state <- paste("b",m$state, sep = "") # helps with order (2nd)
names(m) <- c("state", "time", "hpi", "city_state", "Type")
m <- unique(m)


d <- d[, c("state", "time", "hpi", "city_state") ]
d$Type <- rep(types[1], nrow(d))
d$city_state <- paste("a",d$city_state, sep = "") # helps with order (1st)


b <- both[, c("state", "time", "hpi", "city_state") ]
b$Type <- rep(types[2], nrow(both))
b$city_state <- paste("c",b$city_state, sep = "") # helps with order (3rd)

data <- rbind( d, m, b)

data$Type <- factor(data$Type, levels = types)
  
hpi_All_States <- qplot(
    x = time, 
    y = hpi, 
    data = data, 
    colour = Type, 
    group = city_state, 
    facets = ~ state, 
    geom = "line", 
    log = "y",
    main = "Housing Price Index\nComparison for College Towns",
    ylab = "log(Housing Price Index)",
    xlab = "Time"
  ) + 
  scale_colour_manual( 
    breaks = types, 
    values = c(
      "Cities" = "black", 
      "College Towns" = "green", 
      "Mean of All Cities" = "red"
    )
  ) 
  
savePlot(hpi_All_States)




