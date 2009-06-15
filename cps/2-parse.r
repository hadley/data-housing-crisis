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

# Need to turn this into a format that read.fwf can deal with
one <- subset(all, year == 2001)
one$end <- one$start + one$length

pos <- as.vector(t(one[c("start", "end")]))
widths <- c(pos[1], diff(pos))

con <- gzfile("march-supplement/2001.txt.gz")
read.fwf(con, widths)

con <- gzfile("march-supplement/2001.txt.gz")
fwf <- read.fwf("march-supplement/2003.txt", widths, stringsAsFactors = FALSE, n= 500)[, seq(2, length(widths), by = 2)]
names(fwf) <- tolower(one$abbr)