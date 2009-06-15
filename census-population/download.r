# Source: http://www.census.gov/popest/datasets.html
# http://www.census.gov/popest/counties/files/CO-EST2008-ALLDATA.csv


dir.create("original")

get_csv <- function(filenum, name) {
  download.file(
    paste("http://www.census.gov/popest/counties/files/", filenum, ".csv", sep = ""),
    "temp.csv"
  )
  file.rename("temp.csv", paste("original/", name, ".dbf", sep = ""))
}


get_csv("CO-EST2008-ALLDATA", "county-pop-00-08")

