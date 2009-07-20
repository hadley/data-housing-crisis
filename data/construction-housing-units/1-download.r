# http://www.census.gov/const/C40/Table3/...





dir.create("original/")
dir.create("original/valuation")
dir.create("original/housing-units")

get_txt <- function(filenum, name) {
  download.file(
    paste("http://www.census.gov/const/C40/Table3/", filenum, sep = ""),
    "temp.txt"
  )
  file.rename("temp.txt", paste("original/", name, sep = ""))
}




file_namesU <- c(
                paste(
                  "tb3u",
                  rep(c("00","01","02",as.character(2003:2008)),each = 12) ,
                  c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                  ".txt",
                  sep=""
                ),
                paste(
                  "tb3u",
                  "2009",
                  c("01", "02", "03", "04"), 
                  ".txt",
                  sep=""
                )
              )
              
file_namesU[substr(file_namesU,5,8) == "2003"][1:5] <- paste(
                  "tb3u",
                  "03",
                  c("01", "02", "03", "04","05"), 
                  ".txt",
                  sep=""
                )
                
file_namesV <- file_namesU
substr(file_namesV,4,4) <- "v"
                
 

save_names <- c(
                paste(
                  rep(2000:2008,each = 12) ,
                  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                  ".txt",
                  sep=""
                ),
                paste(
                  "2009",
                  c("jan", "feb", "mar", "apr"), 
                  ".txt",
                  sep=""
                )
              )



for(i in 1:length(file_namesU))
{
  get_txt(file_namesU[i], paste("housing-units/",save_names[i],sep="",collapse=""))
  get_txt(file_namesV[i], paste("valuation/",save_names[i],sep="",collapse=""))
}
