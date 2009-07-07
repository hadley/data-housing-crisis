# Data downloaded from http://www.huduser.org/datasets/50per.html
# Original data stored in original directory

# When you download multiple files like this, they are normally named
# inconsistently.  It's much easier to work with them if they're named
# sensibly so it's a good idea to that as you convert them to csv.
# csv files stored in raw directory

# This is also a good time to compare the data from year to year and identify
# potential problems.  For this data you'll notice that 2006-2009 seems pretty
# standard but 03, 04, and 05 are all a little different.  We'll just ignore
# those years for now.

# County data ---------------------------------------------------------------

# We'll begin by loading in the county data.
files <- c("raw/county-2006.csv", "raw/county-2007.csv", "raw/county-2008.csv", "raw/county-2009.csv")
names(files) <- 2006:2009

# If the files all have pretty much the same column names we can load them 
# all in in one fell swoop
library(plyr)
county <- ldply(files, read.csv)
names(county) <- tolower(names(county))
# ldply automatically adds a column from the names of the files vector.  It
# calls it .id, but we know it really represents the year.
names(county)[1] <- "year"

head(county)
# First we'll get rid of all the columns we don't need.  Since the fips code
# uniquely identifies a county, we'll use that instead of the various names.  
# This will make it easier to match up with other data sources.  Similarly
# we don't need the population data.  When you're deleting multiple columns, 
# it's often easier to just select the columns you want to keep:

county <- county[, c("year", "fips", "state", "county", "rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4")]

# See http://en.wikipedia.org/wiki/FIPS_county_code for more about fips

# Just like in the previous example, the data is easier to work with if 
# the prices for different numbers of bedrooms is stored in a long, not wide
# format
library(reshape)
countym <- melt(county, id = c("year", "fips", "state", "county"))

# Again we rename the columns
names(countym) <- c("year", "fips", "state", "county", "bedrooms", "rent")
# and then convert the number of bedrooms column to a number.  We'll do this
# by removing "Rent50_" and then converting the string to a number:
countym$bedrooms <- as.numeric(gsub("rent50_", "", countym$bedrooms))

# And again do some quick plots.  
library(ggplot2)
qplot(year, rent, data = countym, geom = "line", colour = bedrooms, facets = ~ state)

# Finally we save out the cleaned file to a fresh csv file
write.table(countym, "market-rents-county.csv", sep = ",", row = FALSE)


# Area data ------------------------------------------------------------------

# Load the area data in a similar way
files <- c("raw/area-2006.csv", "raw/area-2007.csv", "raw/area-2008.csv", "raw/area-2009.csv")
names(files) <- 2006:2009
area <- ldply(files, read.csv)
names(area) <- tolower(names(area))
names(area)[1] <- "year"

# What's cbsasub mean?  A bit of googling leads to :
# http://en.wikipedia.org/wiki/Core_Based_Statistical_Area

area <- area[, c("year", "cbsasub", "state", "rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4")]

# And fix the number of bedrooms in a similar way
aream <- melt(area, id = c("year", "cbsasub", "state"))
names(aream) <- c("year", "cbsasub", "state", "bedrooms", "rent")
aream$bedrooms <- as.numeric(gsub("rent50_", "", aream$bedrooms))

write.table(aream, "market-rents-area.csv", sep = ",", row = FALSE)
