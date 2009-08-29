#http://www.census.gov/population/www/metroareas/lists/2003/03mfips.txt
#http://www.census.gov/population/www/metroareas/lists/2003-12/0312mfips.txt
#http://www.census.gov/population/www/metroareas/lists/2004/List1.txt
#http://www.census.gov/population/www/metroareas/lists/2005/List1.txt
#http://www.census.gov/population/www/metroareas/lists/2006/List1.txt
#http://www.census.gov/population/www/metroareas/lists/2007/List1.txt

fileNames <- c("2003/03mfips.txt", "2003-12/0312mfips.txt", "2004/List1.txt", "2005/List1.txt", "2006/List1.txt", "2007/List1.txt", "historical/99mfips.txt")


dir.create("original/")

get_txt <- function(name) {
  from <- paste("http://www.census.gov/population/www/metroareas/lists/",
     name, sep = "")
  to <- paste("original/", strsplit(name, "/")[[1]][1], ".txt", sep = "")
  
  if (file.exists(to)) return()
  download.file(from, to)
}


for( i in fileNames)
  get_txt(i)