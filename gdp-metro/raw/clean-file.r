clean_file <- function(df){
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
	df <- as.data.frame(cbind(indust,year,gdp))
	names(df) <- c("fips", "indust", "year", "gdp")
	df
}