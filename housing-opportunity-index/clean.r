options(stringsAsFactors = FALSE)
hoi <- read.csv("raw/wellsfargoHOI.csv", header = T)

# removing blank rows
hoi <- hoi[hoi$NAME != "",]



library(plyr) 
add_place <- function(df){
	place <- strsplit(df[1,1], ", ")
	df$city <- place[[1]][1]
	df$state <- place[[1]][2]
	df[-1,c(71,72,1:70)]
	}

# create city and state	variables
hoi <- ddply(hoi, .(msa_fip), add_place)

library(reshape)
# putting years in columns	
melt_hoi <- melt(hoi, id = c("city", "state","NAME", "msa_fip", "flag"))

# separating years and quarters
new <- cbind(melt_hoi[,-c(5, 6)], colsplit(melt_hoi$variable, split = "_", names = c("quarter", "year")))

# putting each measure into its own column
cast_new <- cast(new,  ... ~ NAME )

# fixing quarters
cast_new$quarter <- as.numeric(substr(cast_new$quarter, 2, 2))

# fixing years
for (i in 1:length(cast_new$year)){
	if (as.numeric(cast_new$year[i]) < 10){
		cast_new$year[i] <- paste("200", cast_new$year[i], sep ="")
		} else {
			cast_new$year[i] <- paste("19", cast_new$year[i], sep ="")
			}
	}

cast_new$year <- as.numeric(cast_new$year)

# changing names
names(cast_new) <- c( names(cast_new)[1:5], "count", "hoi", "int_rate", "med_inc", "med_price", "national_rank", "regional_rank")

# saving
write.table(cast_new, "housing-opportunity-index.csv", sep = ",", row = FALSE)