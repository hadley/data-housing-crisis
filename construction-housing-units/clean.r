
library(plyr)
library(R.oo)

make_num_vector<- function(numstring)
{
  strsplit(numstring," ")[[1]] -> b
  d <- c()
  for(i in 1:length(b)) if(nchar(b[i]) >= 1) d <- c(d,as.numeric(b[i]))
  d
}



clean_file <- function(fileName, columnNames) 
{
  lineNum <- 2
  line <- ""
  dataSet <- NULL
  numVect <- c("1","2","3","4","5","6","7","8","9","0")
  
  ## loop to find the first line of the data
  while(TRUE)
  {
    line <- readLines(fileName, n = lineNum)[lineNum]
    
    if(nchar(line) > 1 & substr(line,2,2) != " ")
      break()
      
    lineNum <- lineNum + 1
  }
  
  ## loop to go through all data
  while(TRUE)
  {
    ## read the lines of data in and only keep the last one
    ## essentially only reading one line at a time
    #prevLine <- readLines(fileName, n = lineNum-1)[lineNum -1]
    line <- readLines(fileName, n = lineNum)[lineNum]
    
    ## end of data, so stop
    if(nchar(line) < 2)
      break()
    
    if(substr(line, 1, 3) != "   ")
    {
    #print(lineNum)
    #print(line)
    #print(nchar(line))
    
    ## if the line 'wraps' then attach the next line to it
      if(nchar(trim(line)) < 60)
      {
        Repeat <- TRUE
        while(Repeat)
        {
          lineNum <- lineNum + 1
          line2 <- trim(readLines(fileName, n = lineNum)[lineNum])
     
          line <- paste(trim(line)," ", line2, sep = "", collapse = "")

          if(nchar(trim(line)) > 70 )
            Repeat <- FALSE
          
        }
      }
      
      ## find when the numbers start
      pos <- 1
      while(TRUE)
      {
        if(substr(line,pos,pos) %in% numVect )
          break()
        else
          pos <- pos + 1
        
      }
      
      
      cityState <- trim(substr(line,1,pos-1))
      
      # remove "*" replace with space
      cityState <- paste(strsplit(cityState,"\\*")[[1]], sep = "", collapse = " ")

      city <- NULL
      state <- NULL
      skipLine <- FALSE
      
      # clean the city and state
      if(length(strsplit(cityState, ",")[[1]]) > 1)
      {
        cs <- trim(strsplit(cityState, ",")[[1]])
        city <- cs[1]
        state <- cs[2]
      }
      else
      {
        if(length(strsplit(cityState, "  ")[[1]]) == 2)
        {
          cs <- trim(strsplit(cityState, "  ")[[1]])
          city <- cs[1]
          state <- cs[2]
        }
        else
        {
          cs <- strsplit(cityState, " ")[[1]]
          
          if(length(cs) < 3)
          {
            skipLine <- TRUE
          }
          else
          {
            city <- paste(cs[1:(length(cs) - 2)], sep = "", collapse = " ")
            state <- paste(cs[(length(cs) - 2):length(cs)], sep = "", collapse = " ")
          }
        }
      }
          
          
      if(!skipLine)
      {
        #cat(".")

        numbers <- make_num_vector( substr( line, pos, nchar(line))) 
        
        ## collect the info
        vect <- c(  city, state , numbers)
        if(any(is.na(vect)))
        {
          cat("\n")
          print(vect)
          stop("\nVect\nLine:\t", lineNum)
        }
        
        if( length(dataSet) > 2)
          if(length(vect) != ncol(dataSet))
          {
            cat("\n")
            print(vect)
            stop("\nLength\nLine:\t",lineNum)
          }

        ## store and repeat
        dataSet <- rbind(dataSet,vect)
        if(any(is.na(dataSet)))
        {
          print(vect)
          stop("\nDataSet\nLine:\t", lineNum)
        }
        
        
      }
    } # end if != "   "
    
    
    lineNum <- lineNum + 1
  } # end looking for data
  
  # change row names to prevent errors
  rownames(dataSet) <- 1:nrow(dataSet)
  
  # make into a data frame to allow for multi type data
  dataSetNew <- as.data.frame(dataSet)
  colnames(dataSetNew) <- columnNames[1:ncol(dataSetNew)]
  
  # make all the number columns into numbers and not strings
  for(i in 3:ncol(dataSetNew))
    dataSetNew[,i] <- as.numeric(as.character(dataSetNew[,i]))
  
  # return the data set
  dataSetNew
  
}


#clean_and_save <- function(fileName, saveName)
#{
#  Names <- c("City", "State", "Total", "Units_1", "Units_2", "Units_3-4", "Units_5-Inf", "Units_Stucture_5-Inf", "Monthly_Coverage_Percent") 
#    
#  cat("\n",saveName)
#
#  write.csv( clean_file(fileName, Names), saveName, row.names = FALSE)
#}



#file_names <- c(
#                paste(
#                  rep(2000:2008,each = 12) ,
#                  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
#                  sep=""
#                ),
#                paste(
#                  "2009",
#                  c("jan", "feb", "mar", "apr"), 
#                  sep=""
#                )
#              )
              
options(stringsAsFactors = FALSE)

#dir.create("cleaned")
#dir.create("cleaned/by_year")
#dir.create("cleaned/by_month")
#dir.create("cleaned/by_year/valuation")
#dir.create("cleaned/by_year/housing_units")
#dir.create("cleaned/by_month/valuation")
#dir.create("cleaned/by_month/housing_units")
#
#
#for( folder in c("housing_units/", "valuation/"))
#for(i in 1:length(file_names))
#{
#  clean_and_save(paste("original/", folder, file_names[i], ".txt", sep = "", collapse = ""), paste("cleaned/by_month/", folder, file_names[i], ".csv", sep = "", collapse = "") )
#}
#
#cat("\n")
#dataNew <- NULL
#for(folder in c("valuation/","housing_units/"))
#for( i in 1:(9*12))
#{
#  cat(i%%12,", ") 
#  month <- read.csv( paste("cleaned/by_month/", folder, file_names[i], ".csv", sep = "", collapse = ""))
#  Month <- NULL
#  if(i %% 12 == 0)
#    Month <- rep(12, nrow(month))
#  else
#    Month <- rep(i%%12,nrow(month))
#    
#  dataNew <- rbind(dataNew, cbind(month[,1:7],Month))
#  if(i %% 12 == 0)
#  {
#    fileName <- paste("cleaned/by_year/", folder,substr(file_names[i], 1, 4),".csv", sep = "", collapse = "")
#    cat("File Saved: ",fileName,"\n")
#    write.csv(dataNew, fileName, row.names = FALSE)
#    dataNew <- NULL
#  }
#}
#
#dataNew <- NULL
#for(folder in c("valuation/", "housing_units/"))
#{
#  for( i in 1:4 + 9*12)
#  {
#    cat(i%%12,", ") 
#    month <- read.csv( paste("cleaned/by_month/", folder, file_names[i], ".csv", sep = "", collapse = ""))
#    Month <- NULL
#    if(i %% 12 == 0)
#      Month <- rep(12, nrow(month))
#    else
#      Month <- rep(i%%12,nrow(month))
#
#    dataNew <- rbind(dataNew, cbind(month[,1:7], Month))
#    
#  }
#  fileName <- paste("cleaned/by_year/", folder,substr(file_names[length(file_names)], 1, 4),".csv", sep = "", collapse = "")
#  cat("File Saved: ",fileName,"\n")
#  write.csv(dataNew, fileName, row.names = FALSE)
#}
#
#
#for(folder in c("valuation", "housing_units"))
#{
#  dataNew <- NULL
#  for(i in 2000:2009)
#  {
#    cat(i,", ")
#    year <- read.csv( paste("cleaned/by_year/", folder,"/", i, ".csv", sep = "", collapse = ""))
#    Year <- rep(i, nrow(year))
#    dataTmp <- cbind(year, Year)
#    #print(head(dataTmp))
#    dataNew <- rbind(dataNew, dataTmp)
#  }
#  cat("File Saved: ",folder,".csv\n")
#  write.csv(dataNew, paste(folder, ".csv", sep = "", collapse = ""), row.names = FALSE)
#  
#}
#








clean_month <- function(month, year, type)
{

    Names <- c("City", "State", "Total", "Units_1", "Units_2", "Units_3-4", "Units_5-Inf", "Units_Stucture_5-Inf", "Monthly_Coverage_Percent") 
    
    ret <- NULL
    path <- paste("original/", type,"/", year,month, ".txt", sep = "", collapse = "")
    
    if(!file.exists(path)) 
      return()
    
    d <- clean_file(path, Names)
    month <- rep(month, nrow(d))
    d <- cbind(month, d[,1:7])
    d
}

clean_year <- function(year, type)
{
  cat("\nYear = ", year,":   ",type,"\n")
  months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  d <- melt(ldply(months, year = year, type = type, clean_month, .progress = "text"), id = c("month", "City", "State"))
  year <- rep(year, nrow(d))
  d <- cbind(year, d)
  colnames(d)[colnames(d) == "variable"] <- "bedrooms"
  colnames(d)[colnames(d) == "value"] <- type
  d
}

clean_all <- function(year)
{
  house <- clean_year(year, "housing_units")
  val <- clean_year(year, "valuation")

  merge(house, val, all = TRUE)
}

all <- ldply(2000:2009, clean_all)
colnames(all) <- tolower(colnames(all))
all <- all[,c("year","month","city","state","bedrooms","housing_units","valuation")]

write.table(all, gzfile("all_data.csv.gz"), sep = ",", row = F)
closeAllConnections()





