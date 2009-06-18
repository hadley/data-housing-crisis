#Source: http://www.fhfa.gov/Default.aspx?Page=87

library(ggplot2)

a <- read.csv("Original/1q09hpistspo.csv")

new <- a[a[,"yr"] %in% 2000:2009, ]


#Check to make sure that yr contains the numbers i want
print(unique(new [,"yr"]))

names(new)[2:5] <- c("year","quarter","HPI_not_seasonally_adjusted","HPI_seasonally_adjusted")

write.table(new, "HPI-State00-09.csv", sep = ",", row = FALSE)


##########################

# remove all """ "(" ")" 
# change all ",-,-" to ",NA,NA" ; ", " to ","
# add "city, state, fips, year, quarter, hpi, error" for column titles
# rename to 1q09hpi_cbsa_Ver2.csv


a <- read.csv("Original/1q09hpi_cbsa_Ver2.csv")
names(a) <- c("city", "state", "fips_msa", "year", "quarter", "hpi", "error")

new <- a[a[,"year"] %in% 2000:2009, ]
#Check to make sure that yr contains the numbers i want
print(unique(new [,"year"]))


write.table(new, "HPI-metro-areas.csv", sep = ",", row = FALSE)





