download.file(
  "http://www.huduser.org/datasets/usps/downloads.odb?file=032009",
  "temp.zip"
)
dbf <- unzip("temp.zip")
file.rename(basename(dbf), "original/vacancy-2009-01.dbf")
file.remove("temp.zip")