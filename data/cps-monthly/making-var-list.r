# making variables.txt for cps-monthly

# cleaning data dictionaries
library(R.oo)
vars <- c(
  name = 10,
  other = 80
)

dictionary <- function(file_path){
	data <- read.fwf(file_path, vars,
		strip.white = TRUE, stringsAsFactors = FALSE)
	trim_data(data)	
}

trim_data <- function(data){	
	data <- na.omit(data[data[,1] != "",])
	for (i in 1:nrow(data)){
		line <- trim(strsplit(data[i,2], "  ")[[1]])
		data$length[i] <- line[1]
		range <- line[length(line)]
		data$start[i] <- trim(strsplit(range, "-")[[1]])
		test <- substr(data$start[i], 1, 1)
		if(test == "("){ 
			data$start[i] <- substr(data$start[i], 2,5)
		}
		test2 <-substr(data$V1[i], 1, 1)
		if(test2 == "\f") {
			data$V1[i] <- substr(data$V1[i], 3, 10)
		}
	}
	data <- data[nchar(data[,3]) <=4 & nchar(data[,4]) <= 4,] 
	
	data[, c(1,3,4)]
}		

jan03dd <- dictionary("data-dictionaries/trimmed/jan03dd.txt")
may04dd <- dictionary("data-dictionaries/trimmed/may04dd.txt")
aug05dd <- dictionary("data-dictionaries/trimmed/augnov05dd.txt")
jan07dd <- dictionary("data-dictionaries/trimmed/jan07dd.txt")
jan09dd <- dictionary("data-dictionaries/trimmed/jan09dd.txt")

# creating variables.csv
make_dates <- function(years, months){
	expanded <- years[rep(1:length(years), each = length(months))]
	combined <- cbind(expanded, rep(months, length(years)))
	
	dates <- as.data.frame(combined)
	names(dates) <- c("year", "month")
	dates
}

# Cataloguing the dates for which each data dictionary applies
jan03dates <- make_dates(c(2003:2004), c(1:12))[-c(17:24),]
may04dates <- make_dates(c(2004:2005), c(1:12))[-c(1:4, 20:24),]
aug05dates <- make_dates(c(2005:2006), c(1:12))[-c(1:7),]
jan07dates <- make_dates(c(2007, 2008), c(1:12))
jan09dates <- make_dates(2009, c(1:4))

add_dates <- function(dictionary, dates){
	expanded <- dictionary[rep(1:nrow(dictionary), each = nrow(dates)),]
	as.data.frame(cbind(year = rep(dates$year, nrow(dictionary)),
		month = rep(dates$month, nrow(dictionary)),expanded))
}

part1 <- add_dates(jan03dd, jan03dates)
part2 <- add_dates(may04dd, may04dates)
part3 <- add_dates(aug05dd, aug05dates)
part4 <- add_dates(jan07dd, jan07dates)
part5 <- add_dates(jan09dd, jan09dates)

variables <- rbind(part1, part2, part3, part4, part5)
names(variables) <- c("year", "month", "name", "length", "start")

write.table(variables, file = "variables.csv", sep = ",", row = F)
