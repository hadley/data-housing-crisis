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