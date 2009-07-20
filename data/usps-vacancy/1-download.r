dir.create("original")

get_dbf <- function(filenum, name) {
  download.file(
    paste("http://www.huduser.org/datasets/usps/downloads.odb?file=", filenum, sep = ""),
    "temp.zip"
  )
  dbf <- unzip("temp.zip")
  file.rename(basename(dbf), paste("original/", name, ".dbf", sep = ""))
  file.remove("temp.zip")  
}


get_dbf("032009", "vacancy-2009-01")

get_dbf("122008", "vacancy-2008-04")
get_dbf("092008", "vacancy-2008-03")
get_dbf("062008", "vacancy-2008-02")
get_dbf("032008", "vacancy-2008-01")

# qtr 4, yr 2007 had problems, noted on website
get_dbf("092007", "vacancy-2007-03")
get_dbf("062007", "vacancy-2007-02")
get_dbf("032007", "vacancy-2007-01")

get_dbf("122006", "vacancy-2006-04")
get_dbf("092006", "vacancy-2006-03")
get_dbf("062006", "vacancy-2006-02")
get_dbf("032006", "vacancy-2006-01")

get_dbf("122005", "vacancy-2005-04")

