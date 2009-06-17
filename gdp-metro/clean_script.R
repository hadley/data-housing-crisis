# source http://www.bea.gov/regional/gdpmetro/
# choose clean_file R code
source(file.choose(), chwd=T)
library(ggplot2)
library(plyr)
options(stringsAsFactors = FALSE)
clean_file_2 <- function(df){
	df <- subset(df, Component.Code %in% c(200))
	df[df == "n/a" | df == "(D)" | df == "(L)"] <- NA
	df$Metropolitan.Area <- NULL
	df$Component <- NULL
	df$Component.Code <- NULL
	df$Industry <- NULL
	pos <- 1:ncol(df)
	pos <- pos[names(df) %in% "X2001"]
	gdp <- NULL
	for(i in pos:(pos+5))
		gdp <- c(gdp,as.numeric(df[,i]))
	indust <- NULL
	for (i in 1:6)
		indust <- rbind(indust,df[,1:2])
	year <- rep(2001:2006, each = nrow(df))
	df <- as.data.frame(cbind(indust,year,gdp))}

clean_file <- function(df) {
df <- subset(df, Component.Code %in% c(200))
df[df == "n/a" | df == "(D)" | df == "(L)"] <- NA
df$Metropolitan.Area <- NULL
df$Component <- NULL
df$Component.Code <- NULL
df$Industry <- NULL
names(df) <- c("fips", "indcode", "2001", "2002", 
"2003", "2004", "2005", "2006")

}

path <- dir(pattern = "\\.csv") 
all_data <- llply(path, read.csv, stringsAsFactors = FALSE)
clean_data <- ldply(all_data, clean_file)

subset(df, Component.Code %in% c(200))