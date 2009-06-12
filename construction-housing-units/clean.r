

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
    prevLine <- readLines(fileName, n = lineNum-1)[lineNum -1]
    line <- readLines(fileName, n = lineNum)[lineNum]
    
    ## end of data, so stop
    if(nchar(line) < 2)
      break()
    
    if(substr(line, 1, 3) != "   " & substr(prevLine, 1, 3) != "   ")
    {
    cat(".")
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

          if(nchar(trim(line2)) > 60 )
            Repeat <- FALSE
          
        }
      }
    #print(line)
    #print(nchar(line))
      
      ## find when the numbers start
      pos <- 1
      while(TRUE)
      {
        if(substr(line,pos,pos) %in% numVect )
          break()
        else
          pos <- pos + 1
        
      }
      
      ## get and split the city and state and trim both of them
      cityState <- trim(substr(line,1,pos-1))

      # change all * to " "
      cityState <- paste(strsplit(cityState,"\\,\\*")[[1]],collapse=",")

      # change all * to " "
      cityState <- paste(strsplit(cityState,"\\*")[[1]],collapse=" ")

      # change all ",  " to ","
      cityState <- paste(strsplit(cityState,"\\,  ")[[1]],collapse=",")

          
      # change all "  " to ", "
      cityState <- paste(strsplit(cityState,"  ")[[1]],collapse=",")
          
      # split according to "," and trim
      cityState <- trim(strsplit(cityState,"\\,")[[1]])     
      #print(cityState)
      if(length(cityState) == 1)
      {
        cityStateSplit <- strsplit(cityState," ")[[1]]
        print(lineNum)
        print(cityStateSplit)
        city <- paste(cityStateSplit[1:(length(cityStateSplit)-2)], sep = "", collapse=" ")
        state <- paste(cityStateSplit[(length(cityStateSplit)-2):length(cityStateSplit)],sep="",collapse=" ")
      }
      
      if(length(cityState) > 2)
      {
      #  stop("File:\t",fileName,"\n\tLine:\t", lineNum,"\n\tthere were more than one city - state separated by ','.\n\n\tFix it!\n\n",line)
         cat("Line: ", lineNum," - multiple states")
         cityState <- c(cityState[1], paste(cityState[-1],sep="-",collapse=""))
      }
      numbers <- make_num_vector( substr( line, pos, nchar(line))) 
      
      #if(lineNum < 20)
      #{
      #  print(substr(line, pos, nchar(line)))
      #  print(numbers)
      #}
        
      ## collect the info
      vect <- c(  cityState , numbers)
        
      ## store and repeat
      dataSet <- rbind(dataSet,vect)

    } # end if != "   "
    
    
    lineNum <- lineNum + 1
  } # end looking for data
  
  # change row names to prevent errors
  rownames(dataSet) <- 1:nrow(dataSet)
  #print(dataSet)
  
  # make into a data frame to allow for multi type data
  dataSetNew <- as.data.frame(dataSet)
  colnames(dataSetNew) <- columnNames[1:ncol(dataSetNew)]
  
  # make all the number columns into numbers and not strings
  for(i in 3:ncol(dataSetNew))
    dataSetNew[,i] <- as.numeric(as.character(dataSetNew[,i]))
  
  #print for fun
  #print(head(dataSetNew))

  # return the data set
  dataSetNew
  
}


clean_and_save <- function(fileName, saveName)
{
  Names <- c("City", "State", "Total", "Units_1", "Units_2", "Units_3-4", "Units_5-Inf", "Units_Stucture_5-Inf", "Monthly_Coverage_Percent") 
#  Names <- c("City_State", "Total", "1_Units", "2_Units", "3-4_Units", "5-Inf_Units", "5-Inf_Units_Stucture", "Monthly_Coverage_Percent") 
    
  cat("\n",saveName)

  write.csv( clean_file(fileName, Names), saveName, row.names = FALSE)
}



file_names <- c(
                paste(
                  rep(2000:2008,each = 12) ,
                  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                  sep=""
                ),
                paste(
                  "2009",
                  c("jan", "feb", "mar", "apr"), 
                  sep=""
                )
              )
              
options(stringsAsFactors = FALSE)

dir.create("cleaned")
dir.create("cleaned/by_year")
dir.create("cleaned/by_month")
dir.create("cleaned/by_year/valuation")
dir.create("cleaned/by_year/housing_units")
dir.create("cleaned/by_month/valuation")
dir.create("cleaned/by_month/housing_units")


for( folder in c("housing_units/", "valuation/"))
for(i in 1:length(file_names))
{
  clean_and_save(paste("original/", folder, file_names[i], ".txt", sep = "", collapse = ""), paste("cleaned/by_month/", folder, file_names[i], ".csv", sep = "", collapse = "") )
}

cat("\n")
dataNew <- NULL
#for(folder in c("valuation/","housing_units/"))
#for( i in 1:(9*12))
#{
#  cat(i%%12,", ") 
#  month <- read.csv( paste("cleaned/by_month/", folder, file_names[i], ".csv", sep = "", collapse = ""))
#  print(file_names[i])
#  print(head(month))
#  dataNew <- rbind(dataNew, month[,1:8])
#  if(i %% 12 == 0)
#  {
#    fileName <- paste("cleaned/by_year/", folder,substr(file_names[i], 1, 4),".csv", sep = "", collapse = "")
#    cat("File Saved: ",fileName,"\n")
#    write.csv(dataNew, fileName, row.names = FALSE)
#    dataNew <- NULL
#  }
#}


#for( i in 1:4 + 9*12)
#{
#  cat(i%%12,", ") 
#  dataNew <- rbind(dataNew, read.csv( paste("cleaned/by_month/", folder, file_names[i], ".csv", sep = "", collapse = "")))
#  
#}
#fileName <- paste("cleaned/by_year/", folder,substr(file_names[length(file_names)], 1, 4),".csv", sep = "", collapse = "")
#cat("File Saved: ",fileName,"\n")
#write.csv(dataNew, fileName, row.names = FALSE)



#clean_and_save(paste("original/housing_units/", file_names[4], ".txt", sep = "", collapse = ""), paste("cleaned/", file_names[4], ".csv", sep = "", collapse = "") )