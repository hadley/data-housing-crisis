# Source: http://www.macromarkets.com/csi_housing/data_download.asp
# http://www.macromarkets.com/csi_housing/data_downloads/SPCSI.xls

# The first thing I did was to save the file as csv from excel.

# Load the csv file into R
# skip = 1 means skip the first line
spcsi <- read.csv("SPCSI.csv", skip = 1)

# look at the data
head(spcsi)
# fix the name of the first column
names(spcsi)[1] <- "date"
# make all column names lowercase
names(spcsi) <- tolower(names(spcsi))

# Delete the composite measures
# You use $ to access a variable (e.g. csxr) inside a data frame (eg. spcsi)
spcsi$csxr <- NULL
spcsi$spcs20r <- NULL

# Instead of having each city in it's own column, it's easier to work with the
# data when all the index values are in a single column, and another column
# identifies which city belongs to which index
library(reshape)
spcsim <- melt(spcsi, id = "date", na.rm = TRUE)
# give more informative variable names
names(spcsim) <- c("date", "abbr", "spcsi")

# We also need to put the date into a format that R can understand.  Currently
# it's just treating it as a categorical variable (or factor).  You can see
# this by looking at the output of str
str(spcsim)

# The syntax to convert this to a proper date is complex.  The important
# things to note are that we first add a day (the middle of the month) to 
# make a complete date, and then use the date specification %d %B %Y, which 
# tells R to look for a day, then a spelled-out month and then a year.  See
# ?strptime for a list of all possible formats.
spcsim$date <- paste("15", spcsim$date)
spcsim$date <- as.POSIXct(strptime(spcsim$date, "%d %B %Y"))

# It's a good idea to check that our data looks ok with some plots:
library(ggplot2)
qplot(date, spcsi, data = spcsim, geom = "line", group = abbr)
qplot(date, spcsi, data = spcsim, geom = "line", facets = ~ abbr)

# We might also want to create new columns for the name of the city 
# and the state.  The easiest way to add this information is to create
# another data source that has the columns abbreviation, city and state.
# For this number of cities, it's easiest to just create this table by hand
# in Excel.  This is abbr-lookup.csv
abbr <- read.csv("abbr-lookup.csv")
abbr$abbr <- tolower(abbr$abbr)

# Combine with full dataset using merge
spcsim <- merge(spcsim, abbr, by = "abbr")

# Now do a plot using those new columns
qplot(date, spcsi, data = spcsim, geom = "line", group = city, facets = ~ state)

# Finally we save out the cleaned file to a fresh csv file
write.table(spcsim, "spcsim-clean.csv", sep = ",", row = FALSE)

# You should alway check that you code is fully reproducible by exiting R
# and then running source("clean.r").  This is like copying and pasting the 
# contents 
