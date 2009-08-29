library(ggplot2)
library(R.oo)
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


# Separate states from city names and store with one record per state.
msa <- read.csv("msa-codes.csv")

divider <- as.numeric(regexpr(" [A-Z-]+$", msa$city))
city <- substr(msa$city, 1, divider - 1)
states <- strsplit(substr(msa$city, divider + 1, 100), "-")

lengths <- sapply(states, length)
rep <- rep(1:nrow(msa), lengths)
citystate <- data.frame(
  city = city[rep], 
  state = unlist(states), 
  msa_code = msa$msa_code[rep]
)

write.table(citystate, "msa-states.csv", row = FALSE, sep = ",")
