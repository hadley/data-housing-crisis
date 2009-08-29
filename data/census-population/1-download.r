# Source: http://www.census.gov/popest/datasets.html
# http://www.census.gov/popest/counties/files/CO-EST2008-ALLDATA.csv


url <- "http://www.census.gov/popest/counties/files/CO-EST2008-ALLDATA.csv"
download.file(url, "raw-county.csv")


# Source: http://www.census.gov/popest/datasets.html
#http://www.census.gov/popest/metro/files/2008/CBSA-EST2008-alldata.csv

url <- "http://www.census.gov/popest/metro/files/2008/CBSA-EST2008-alldata.csv"
download.file(url, "raw-cbsa.csv")
