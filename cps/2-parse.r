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

