#Source: http://www.fhfa.gov/Default.aspx?Page=87

library(ggplot2)

a <- read.csv("Original/1q09hpistspo.csv")

new <- a[a[,"yr"] %in% 2000:2009, ]


#Check to make sure that yr contains the numbers i want

unique(new [,"yr"])

names(new)[2:5] <- c("year","quarter","HPI_not_seasonally_adjusted","HPI_seasonally_adjusted")

write.table(new, "HPI-State00-09.csv", sep = ",", row = FALSE)