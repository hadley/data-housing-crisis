# Source: http://quickfacts.census.gov/qfd/download_data.html
options(stringsAsFactors = FALSE)

qf <- read.csv("quickfacts.csv")

# Give nicer abbreviations
dd <- read.csv("data-dictionary.csv", sep = ";")
names(qf) <- dd$abbr[match(names(qf), dd$field)]

# Extract state and county fips codes
codes <- formatC(qf$statecounty, width = 5, flag = "0")
qf$state <- as.numeric(substr(codes, 1, 2))
qf$county <- as.numeric(substr(codes, 3, 5))
qf$statecounty <- NULL

# Remove state and overall aggregates
qf2 <- subset(qf, county != 0)

write.table(qf2, "census-quickfacts-clean.csv", sep = ",", row = FALSE)
