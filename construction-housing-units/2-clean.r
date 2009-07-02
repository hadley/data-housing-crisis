library(ggplot2)
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
     
          seper = " "
          if(substr(line, nchar(line), nchar(line)) == "-" | substr(line2, 1, 1) == "-")
            seper = ""
            
          line <- paste(trim(line), line2, sep = seper, collapse = "")

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
      cityState <- gsub("\\*", " ", cityState)
      cityState <- gsub("CMSA", "", cityState)
      cityState <- trim(gsub("MSA", "", cityState))


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
            city <- paste(cs[1:(length(cs) - 1)], sep = "", collapse = " ")
            state <- paste(cs[length(cs)], sep = "", collapse = " ")
          }
        }
      }
          
          
      state <- trim(gsub("-Texarkana", "", state))
          
      if(!skipLine)
      {
        #cat(".")

        numbers <- make_num_vector( substr( line, pos, nchar(line))) 
        
        ## collect the info
        stateSplit <- strsplit(state,"-")[[1]]
        
        for(i in 1:length(stateSplit))
        {
          
          vect <- c(  trim(city), trim(stateSplit[i]), numbers)  
  
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

              
options(stringsAsFactors = FALSE)

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
  colnames(d)[colnames(d) == "variable"] <- "units"
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
all <- all[,c("year","month","city","state","units","housing_units","valuation")]

# Convert month to a number
all$month <- as.numeric(factor(all$month, levels = c("jan", "feb", "mar", 
  "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

# Removed unneeded units prefix
all$units <- gsub("Units_", "", all$units)

# Remove totals
all <- subset(all, units != "Total")

write.table(all, gzfile("construction-housing-units.csv.gz"), sep = ",", row = F)
closeAllConnections()