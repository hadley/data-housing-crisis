library(ggplot2)
library(MASS)
library(mgcv)
options(na.action = "na.exclude")
options(stringsAsFactors = FALSE)

# Helper functions ----------------------------------------------------------
savePlot <- function(..., plot= FALSE, big = TRUE)
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




shomes <- read.csv("data/shomes.csv", header = T)



qplot(owner_occ, year, data = shomes, geom = "line") + facet_wrap( ~ fips_st, scales = "free") + opts(legend.position = "none")