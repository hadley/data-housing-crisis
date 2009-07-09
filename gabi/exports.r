library(ggplot2)
options(stringsAsFactors = FALSE)


# Helper functions ----------------------------------------------------------
savePlot <- function(..., plot= TRUE, big = TRUE)
{
nameOfPlot <- substitue(...)
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



#-----------------------------------------------------------------------------

hpi<- read.csv("../fhfa-house-price-index/fhfa-house-price-index-msa.csv")
both<- read.csv("college-hpi.csv")

hpi$time <- hpi$year + (hpi$quarter - 1) / 4
hpi$city_state <- paste(hpi$city, hpi$state, sep = ", ")

hpi_by_city_state <- qplot(x=time, y=hpi,data=both, facets=~city_state, geom="line")

# I'd suggest plotting all cities facetted by state, with an overlaid average for each state (so you can see if they were better or worse off)

hpi <- ddply(hpi, c("state", "time"), transform, state_mean = mean(hpi), .progress = "text")


both <- merge(both, unique(hpi[, c("state", "time", "state_mean") ]))



hpi_by_state <- qplot(x=time, y=hpi,data=both, group = city_state, facets=~state, geom="line") + geom_line(aes(y = state_mean), colour = I("red"))

hpiCA <- qplot(x=time, y=hpi,data=hpi[hpi$state == "CA", ], group = city_state, facets=~state, geom="line") + geom_line(aes(y = state_mean), colour = I("red")) + geom_line(data = hpi[hpi$state == "CA" & hpi$city %in% both$city, ] , colour = I("blue"))


