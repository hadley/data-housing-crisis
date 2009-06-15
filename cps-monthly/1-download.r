urls <- scan("0-urls.txt", "character")
files <- basename(urls)

month <- factor(substr(files, 1, 3), levels = c("jan", "feb", "mar", 
  "apr", "may", "jun", "jul", "aug",  "sep", "oct", "nov", "dec"))

year2 <- as.numeric(substr(files, 4, 5))
year <- ifelse(year2 < 20, 2000 + year2, 1900 + year2)

df <- data.frame(month, year, url = urls, stringsAsFactors = FALSE)
df$path <- with(df, 
  paste("raw/", year, "-", sprintf("%02i", month), ".txt.gz", sep = ""))
recent <- subset(df, year >= 2000)


library(plyr)

download_if_necessary <- function(path, url) {
  if (!file.exists(path)) {
    download.file(url, path)
  }
}

m_ply(recent[c("path", "url")], download_if_necessary)
