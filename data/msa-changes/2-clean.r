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
