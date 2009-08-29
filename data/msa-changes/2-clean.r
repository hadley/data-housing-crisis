library(ggplot2)
library(R.oo)
options(stringsAsFactors = FALSE)


get_file <- function(fileName) 
{
  
  linePos <- 1
  rows <- readLines(fileName)

  while(TRUE)
  {
    
    numbs <- as.character(1:10)
#    print(numbs)

#print(strsplit(row,"")[[1]][1])    
    if(strsplit(rows[linePos],"")[[1]][1] %in% numbs)
      break()
    
    linePos <- linePos + 1
#    print(linePos)
  }
  
  lineStart <- linePos
  cat("\nLine Start");print(lineStart)
  
  while(TRUE)
  {
    
    numbs <- as.character(1:10)
#    print(numbs)

#print(strsplit(row,"")[[1]][1])    
    if(! strsplit(rows[linePos],"")[[1]][1] %in% c(numbs, " ", NA))
      break()
    
    linePos <- linePos + 1
#    print(linePos)
  }
  
  lineEnd <- linePos-1
  
  cat("\nLine End");print(lineEnd)
  
  file <- rows[lineStart:lineEnd]
  
#  colnames(file) <- c("cbsa", "bad", "name")
  
#  file[,-2]
  
  return(file)
}

clean_file <- function(fileName)
{
  file <- get_file(fileName)
  
  file <- file[file != ""]
  
  
  goodRows <- rep(FALSE, length(file))
  for(i in 1:length(file))
    if(substr(file[i], 25, 25)[[1]] != " ")
      goodRows[i] <- TRUE
      
  
  file <- file[goodRows]
  
  msa_code <- substr(file,1,5)
  
  name <- c()
  for(i in 1:length(file))
    name[i] <- substr(file[i], 25, nchar(file[i], type = "width"))
  
  name <- gsub("Micropolitan Statistical Area", "", name)
  name <- gsub("Metropolitan Statistical Area", "", name)
  name <- trim(name)
  
  year <- c()
  fileName2 <- fileName
  fileName2 <- gsub("original/", "", fileName2)
  fileName2 <- gsub(".txt", "", fileName2)

  if(length(strsplit(fileName2, "-")[[1]]) > 1)
    year <- rep(as.numeric(strsplit(fileName2, "-")[[1]][1]) + (as.numeric(strsplit(fileName2, "-")[[1]][2]) - .5)/12, length(file))
  else
    year <- rep(as.numeric(fileName2), length(file))

  Final <- data.frame(msa_code = msa_code, name = name, year = year)
  
  Final <- Final[!is.na(Final$name), ] # remove all weird names, 
  #i.e. "10380                   Aguadilla-Isabela-San Sebasti\xe1n, PR Metropolitan Statistical Area"
  
  Final
}


a <- clean_file("original/2003.txt")
b <- clean_file("original/2003-12.txt")

d <- rbind(a,b)
head(d[order(d[,1]), ])

fileNames <- paste("original/", c("2003", "2003-12", as.character(2004:2007)), ".txt", sep = "")

data <- NULL
for(i in fileNames)
  data <- rbind(data, clean_file(i))


data$all_years <- rep(FALSE, nrow(data))
for(i in unique(data$msa_code))
{
  tmp <- data[data$msa_code == i, ]
  if(length(unique(tmp$year)) == length(fileNames))
    data[data$msa_code == i, "all_years"] <- TRUE
  if(length(unique(tmp$name)) == 1)
    data[data$msa_code == i, "year"] <- -1
}

data <- unique(data)
data <- data[order(data$msa_code), ]

data$tmp <- paste(data$msa_code, data$name)
data <- data[!duplicated(data$tmp), ]
data$tmp <- NULL

data <- data[,c("msa_code", "name")]


write.csv(data, "msa-codes.csv", row.names= FALSE)


