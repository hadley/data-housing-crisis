library(plyr)
options(stringsAsFactors = FALSE)

load_file <- function(path) {
  lines <- readLines(path, encoding = "Latin 1")
  n <- length(lines)
  
  last <- which(lines[-n] == "" & lines[-1] == "")

  data <- lines[grepl("Statistical Area", lines)]
  data <- gsub(" (Metropolitan|Micropolitan) Statistical Area", "", data)
  data <- gsub(",", "", data)
  
  pieces <- strsplit(data, " {2,}")
  pieces <- pieces[sapply(pieces, length) == 2]
  
  df <- as.data.frame(do.call("rbind", pieces))
  names(df) <- c("msa_code", "city")

  df
}

files <- dir("original/", "^2", full.names = TRUE)
data <- ldply(files, load_file)
data <- unique(data[order(data$msa_code), ])
write.table(data, "msa-codes.csv", row = FALSE, sep = ",")

# Output the most recent year, with major city extracted.  Use this for
# labelling plots/tables etc.
codes08 <- load_file("original/2008.txt")

divider <- as.numeric(regexpr(" [A-Z-]+$", codes08$city))
city <- substr(codes08$city, 1, divider - 1)
state <- substr(codes08$city, divider + 1, 100)

major_city <- sapply(strsplit(city, "-"), "[", 1)
major_state <- sapply(strsplit(state, "-"), "[", 1)

major <- data.frame(
  msa_code = codes08$msa_code,
  city = major_city,
  state = major_state,
  label = paste(abbreviate(major_city, 6), major_state, sep = "")
)
write.table(major, "msa-major.csv", row = FALSE, sep = ",")
