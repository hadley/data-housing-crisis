vars_that_I_want <- c("PEMLR", "GTCBSA", "PRDTIND1", "PRMJIND1")

all_vars <- read.csv("variables.csv", header = TRUE, stringsAsFactors = FALSE)

to_extract <- all_vars[all_vars$abbr %in% vars_that_I_want,]

parse_line <- function(line, fields) {
  substring(line, fields$start, fields$start + fields$length - 1)  
}

parse_month <- function(year, month) {
	path <- paste("raw/", year, "-", sprintf("%02i", month), ".txt.gz", sep = "")
	out_path <- paste("clean/", year, "-", sprintf("%02i", month), ".csv.gz", sep = "")
	
	message("Processing ", path)
	lines <- readLines(gzfile(path))
	closeAllConnections()

	to_find <- to_extract[to_extract$year == year & to_extract$month == month,]
	
	parsed <- llply(lines, parse_line, fields = to_find, .progress = "text")

	  # Convert to data frame
	out <- do.call("rbind", parsed)
	outdf <- as.data.frame(alply(out, 2, type.convert))
	names(outdf) <- tolower(to_find$abbr)
	outdf$year <- year
	outdf$month <- month
	
	# Save as compressed csv file
	write.table(outdf, gzfile(out_path), sep = ",", row = F)
	closeAllConnections()
}

	



all_months <- expand.grid(
  year = 2000:2009,
  month = 1:12
)[-c(50, 60, 70, 80, 90, 100, 110, 120),]
m_ply(all_months, parse_month)