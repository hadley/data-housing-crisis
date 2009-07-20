clean <- read.csv(file.choose(), header = T)
# remove X column
clean <- clean[,-1]

# putting each industry code into its own column
library(reshape)
melted <- melt(clean, id = c("fips", "year", "indust"))
new <- cast(melted, fips + year ~ indust)

# unfortunately R doesn't handle numbers as column names very well.  So if you want to manipulate with a column name, you have to change it first
names(new)[3] <- "total" 

# to rank them pick a year to do the rankings with and subset the data
new2006 <- new[new$year == 2006,]

# then rank by total
new2006 <- new2006[order(new2006$total),]

# you can read much more on the reshape package at:http://www.jstatsoft.org/v21/i12

# let me know if you'd like me to print out a copy for you