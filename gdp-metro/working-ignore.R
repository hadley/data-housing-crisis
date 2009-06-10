# set up
library(ggplot2)
library(plyr)
source(file.choose('c:/Users/Administrator/housing-crisis/gdp-metro/GDP.R'), chwd=T)

path <- dir(pattern = "\\.csv") 

library(plyr)
all_data <- llply(path, read.csv, stringsAsFactors = FALSE)
clean_data <- ldply(all_data, clean_file)

table(clean_data$fips, clean_data$indcode)

{m <- read.csv(path)
m[m == "n/a" | m == "(L)" | m == "(D)"] <- NA
m$Metropolitan.Area <- NULL
m$Component <- NULL
m$Industry <- NULL
names(m) <- c("fips", "indcode", "comcode", "y01", "y02", 
"y03", "y04", "y05", "y06")
m <- subset(m, !(m$fips %in% c("Source: Bureau of Economic Analysis")) & !(m$fips %in% c("FIPS")))
n <- rbind(n , m)}}

# checks
dim(n)
364*352
# failed to get count right; keep getting 127969 instead of the expected 128128

# debug loop, will capture names of files with less than 352 end lines
n <- c()
debug <- data.frame("file" = c(), "dim" = c())
for (i in 990:1000){
path <- gsub("([0-9])([0-9])([0-9])([0-9])([0-9])","\\1\\2\\3\\4\\5\\.csv",formatC(i, width = 5, flag = 0))
if(file.access(path, mode = 0) == 0)
{m <- read.csv(path)
m[m == "n/a" | m == "(L)" | m == "(D)"] <- NA
m$Metropolitan.Area <- NULL
m$Component <- NULL
m$Industry <- NULL
m <- subset(m, !(m$fips %in% c("Source: Bureau of Economic Analysis")) & !(m$fips %in% c("FIPS")))
if(!dim(m) == 352 && 9)
{debug <- rbind(debug, c(path, dim(m)))}}}

clean_file <- function(m1) {

}

clean_file(m1)

files <-

# original loop to extract names
y <- c()
for (i in 10000:11000){
w <- formatC(i, width = 5, flag = 0)
path <- gsub("([0-9])([0-9])([0-9])([0-9])([0-9])","\\1\\2\\3\\4\\5\\.csv",w)
x <- file.access(path, mode = 0)
x <- x == 0
if(x == TRUE)
y <- c(y , x)}
# check for consistancy; originally 364 files
sum(y)
y <- dput(names(y))
# save names
write.csv(y, file = ('filenames'))

clean_file <- function(df) {
df[df == "n/a"] <- NA
df[df == "(L)"] <- NA
df[df == "(D)"] <- NA
df$Metropolitan.Area <- NULL
df$Component <- NULL
df$Industry <- NULL
names(df) <- c("fips", "indcode", "comcode", "y2001", "y2002", 
"y2003", "y2004", "y2005", "y2006")
df <- subset(df, !(df$fips %in% c("Source: Bureau of Economic Analysis")))
df <- subset(df, !(df$fips %in% c("FIPS")))
df
}




# original data clean up script
m1 <- read.csv('10180.csv')
m2 <- m1
# need to change n/a syntax to NA
m2[m2 == "n/a"] <- NA
# also converting (D) and (L) into NA
m2[m2 == "(L)"] <- NA
m2[m2 == "(D)"] <- NA
m3 <- m2
# removing repeated data
m3$Metropolitan.Area <- NULL
m3$Component <- NULL
m3$Industry <- NULL
# renaming
names(m3) <- c("fips", "indcode", "comcode", "01", "02", 
"03", "04", "05", "06")
# removing the census bureau tag
m3 <- subset(m3, !(m3$fips %in% c("Source: Bureau of Economic Analysis")))
# more stuff in the middle to remove
m3 <- subset(m3, !(m3$fips %in% c("FIPS")))
# unsure how to reset the numbering on the left
# saved with "A" tag (later discarded this)
write.csv(m3, file = ('10180A.csv'))
# note each data frame ends up with 352 rows and 9 collumns
# cleaning
rm(m1, m2, m3)
