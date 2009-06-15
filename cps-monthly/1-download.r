urls <- scan("0-urls.txt", "character")
files <- basename(urls)

month <- factor(substr(files, 1, 3), levels = c("jan", "feb", "mar", 
  "apr", "may", "jun", "jul", "aug",  "sep", "oct", "nov", "dec"))

year2 <- as.numeric(substr(files, 4, 5))
year <- ifelse(year2 < 20, 2000 + year2, 1900 + year2)

df <- data.frame(month, year, url = urls, stringsAsFactors = FALSE)
recent <- subset(df, year >= 2000)

df$path <- with(df, 
  paste("raw/", year, "-", sprintf("%02i", month), ".txt.gz", sep = ""))

library(plyr)
m_ply(cbind(url = df$url, destfile = df$path), download.file)