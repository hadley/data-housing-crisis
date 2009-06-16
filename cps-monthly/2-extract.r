vars <- c(
  abbr = 13,
  length = 2,
  desc = 44,
  start = 4,
  end = 4
)
to_extract <- read.fwf("variables.txt", vars, 
  strip.white = TRUE, stringsAsFactors = FALSE)
names(to_extract) <- names(vars)

parse_line <- function(line, fields) {
  substring(line, fields$start, fields$start + fields$length - 1)  
}

parse_year <- function(year, month) {
  path <- paste("raw/", year, "-", sprintf("%02i", month), ".txt.gz", sep = "")
  if (!file.exists(path)) return() # Skip if file doesn't exist
  
  message("Processing ", path)
  lines <- readLines(gzfile(path))
  closeAllConnections()
  
  parsed <- llply(lines, parse_line, fields = to_extract, .progress = "text")

  # Convert to data frame
  out <- do.call("rbind", parsed)
  outdf <- as.data.frame(alply(out, 2, type.convert))
  names(outdf) <- tolower(to_extract$abbr)
  outdf$year <- year
  outdf$month <- month

  # Save as compressed csv file
  out_path <- paste("clean/", year, "-", sprintf("%02i", month), ".csv.gz", sep = "")
  write.table(outdf, gzfile(out_path), sep = ",", row = F)
  closeAllConnections()
}

library(plyr)

all_months <- expand.grid(
  year = 2000:2009,
  month = 1:12
)
m_ply(all_months, parse_year)