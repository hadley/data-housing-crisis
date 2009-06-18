# Load variable specification from vars-constant and vars-variable.

vars <- c(
  year = 5,
  code = 2,
  abbr = 12,
  length = 3,
  start = 5, 
  range = 20
)

vari <- read.fwf("vars-variable.txt", vars, stringsAsFactors = FALSE, strip.white = TRUE)
names(vari) <- names(vars)

const <- read.fwf("vars-constant.txt", vars[-1], stringsAsFactors = FALSE, strip.white = TRUE)
names(const) <- names(vars)[-1]
years <- 2000:2008

const_year <- data.frame(
  year = rep(years, each = nrow(const)),
  const
)

all <- rbind(vari, const_year)
field_desc <- all[order(all$year, all$start), c("year", "start", "length", "abbr")]

# Manually parse and load into R.  
# Not using read.fwf because the specification is annoying (need to load 
# all fields and then discard) and because it doesn't seem to work with 
# the gzfile connection.

parse_line <- function(line, fields) {
  substring(line, fields$start, fields$start + fields$length - 1)  
}

parse_year <- function(year) {
  path <- paste("raw/", year, ".txt.gz", sep = "")
  message("Loading raw data")
  lines <- readLines(gzfile(path))
  closeAllConnections()
  print(head(lines))
  message("Extracting fields")
  fields <- field_desc[field_desc$year == year, ]
  parsed <- llply(lines, parse_line, fields = fields, .progress = "text")

  # Convert to data frame
  message("Converting to data frame")
  out <- do.call("rbind", parsed)
  outdf <- as.data.frame(alply(out, 2, type.convert))
  names(outdf) <- tolower(fields$abbr)
  outdf$year <- year

  # Save as compressed csv file
  message("Saving output")
  out_path <- paste("clean/", year, ".csv.gz", sep = "")
  write.table(outdf, gzfile(out_path), sep = ",", row = F)
  closeAllConnections()
}

dir.create("clean") 
l_ply(2001:2008, parse_year)
