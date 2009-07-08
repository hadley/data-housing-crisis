


get_file <- function(fileName)
{
  
  linePos <- 1
  while(TRUE)
  {
    
    numbs <- as.character(1:10)
#    print(numbs)

    row <- readLines(fileName)[linePos]
#print(strsplit(row,"")[[1]][1])    
    if(strsplit(row,"")[[1]][1] %in% numbs)
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

    row <- readLines(fileName)[linePos]
#print(strsplit(row,"")[[1]][1])    
    if(! strsplit(row,"")[[1]][1] %in% c(numbs, " ", NA))
      break()
    
    linePos <- linePos + 1
#    print(linePos)
  }
  
  lineEnd <- linePos-1
  
  cat("\nLine End");print(lineEnd)
  
  file <- readLines(fileName)[lineStart:lineEnd]
  
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
        
  
  year <- c()
  fileName2 <- fileName
  fileName2 <- gsub("original/", "", fileName2)
  fileName2 <- gsub(".txt", "", fileName2)
  if(length(strsplit(fileName2, "-")[[1]]) > 1)
    year <- rep(as.numeric(strsplit(fileName2, "-")[[1]][1]) + (as.numeric(strsplit(fileName2, "-")[[1]][2]) - .5)/12, length(file))
  else
    year <- rep(as.numeric(fileName2), length(file))        


  Final <- data.frame(msa_code = msa_code, name = name, year = year)
  
  
  
  Final
}


d <- rbind(a,b)
head(d[order(d[,1]), ])





a <- clean_file("original/2003.txt")
b <- clean_file("original/2003-12.txt")

