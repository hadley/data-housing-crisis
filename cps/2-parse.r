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
all <- all[order(all$year, all$start), c("year", "start", "length", "abbr")]

# Manually parse and load into R.  
# Not using read.fwf because the specification is annoying (need to load 
# all fields and then discard) and because it doesn't seem to work with 
# the gzfile connection.

m2001 <- gzfile("march-supplement/2001.txt.gz")
lines <- readLines(m2001, n = 10000)

parse_line <- function(line, fields) {
  substring(line, fields$start, fields$start + fields$length - 1)  
}

parsed <- llply(lines, parse_line, fields = one, .progress = "text")

out <- do.call("rbind", parsed)
outdf <- as.data.frame(alply(out, 2, type.convert))
names(outdf) <- tolower(one$abbr)
outdf$year <- 2001

write.table(outdf, gzfile("migration/2001.csv.gz"), sep = ",", row = F)