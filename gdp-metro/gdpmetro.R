# set up
library(ggplot2)
library(plyr)
source(file.choose(), chdir=T)

clean_file <- function(df) {
df[df == "n/a" | df == "(D)" | df == "(L)"] <- NA
df$Metropolitan.Area <- NULL
df$Component <- NULL
df$Industry <- NULL
names(df) <- c("fips", "indcode", "comcode", "y2001", "y2002", 
"y2003", "y2004", "y2005", "y2006")
df <- subset(df, !(df$fips %in% c("Source: Bureau of Economic Analysis")))
df <- subset(df, !(df$fips %in% c("FIPS")))}

path <- dir(pattern = "\\.csv") 
all_data <- llply(path, read.csv, stringsAsFactors = FALSE)
clean_data <- ldply(all_data, clean_file)
