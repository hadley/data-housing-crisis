options(stringsAsFactors = FALSE)
library(ggplot2)
library(R.oo)

load_file <- function(path) {
  raw <- readLines(path)
  # Find first line of data: the second line which has at least one character
  # and a character in the second column
  first <- which(nchar(raw) > 1 & substr(raw, 2, 2) != " ")[2]
  # First the last line of data: the first line after the start which has less
  # than two characters
  last <- which(nchar(raw[-seq_len(first)]) < 2)[1] + first - 1
  
  lines <- raw[first:last]
  # Remove first column, if blank
  n <- length(lines)
  
  # Split each data line into name and data component
  divider <- as.vector(regexpr("[0-9]", lines))
  divider[divider == -1] <- 100
  names <- trim(substr(lines, 1, divider - 1))
  data_str <- substr(lines, divider, 1000)
  
  # Figure out which lines are continued city names
  continued <- divider == 100
  group <- cumsum(c(FALSE, !continued))[-n]

  # Combine city names and throw away empty data
  cities <- as.character(tapply(names, group, paste, collapse = " "))
  city_data <- trim(data_str[!continued])
  
  # Convert data strings into numeric matrix
  data_pieces <- strsplit(city_data, " +")
  # Keep only columns for 1, 2, 3-4 and 5+ units
  num_pieces <- lapply(data_pieces, function(x) as.numeric(x[2:5]))
  data <- do.call("rbind", num_pieces)
  
  month <- data.frame(cities, data)
  names(month) <- c("city", "1", "2", "3-4", "5+")
  
  month
}

months <- c("jan", "feb", "mar", "apr", "may", "jun", 
            "jul", "aug", "sep", "oct", "nov", "dec")

clean_month <- function(type, year, month) {
  path <- paste("original/", type,"/", year, months[month], ".txt", sep = "")
  if (!file.exists(path)) return()
  
  data <- load_file(path)
  
  molten <- melt(data, "city")
  names(molten)[2:3] <- c("size", type)
  molten$month <- month
  molten$year <- year
  
  molten
}

clean_year <- function(type, year) {
  ldply(1:12, year = year, type = type, clean_month)
}

clean_both <- function(year) {
  units <- clean_year("units", year)
  valuation <- clean_year("valuation", year)
  
  all <- merge(units, valuation, all = T,
    by = c("city", "year", "month", "size"))
  is.na(all$valuation) <- all$housting
  all[with(all, order(city, year, month)), ]
}


# all <- ldply(2000:2009, clean_both, .progress = "text")
fix_names <- function(name) {
  
  name <- trim(name)
  name <- gsub("[*,]", " ", name)
  name <- gsub(" (CMSA|MSA|PMSA|P MSA|PM SA|PMS|PMS A)", "", name)
  name <- gsub("--|- | -", "-", name)
  name <- gsub(" {2,}", " ", name)

  # Random fixes
  name <- gsub("Bea ch", "Beach", name)
  name <- gsub("Bernar dino", "Bernardino", name)
  
  name
}

all$city2 <- fix_names(all$city)

msa <- read.csv("../msa-changes/msa-codes.csv")
msa$name <- gsub(",", "", msa$name)
labelled <- merge(all, msa, by.x = "city2", by.y = "name", all.x = TRUE)


table(labelled$city2[is.na(labelled$msa_code)])

# write.table(all, "construction-housing-units.csv", sep = ",", row = F)
