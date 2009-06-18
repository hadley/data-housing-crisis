# source http://www.bea.gov/regional/gdpmetro/
# set working directory to gdp-metro/raw
setwd(choose.dir())
rm(list=ls())
source("clean_file.R")
library(ggplot2)
library(plyr)
options(stringsAsFactors = FALSE)
path <- dir(pattern = "\\.csv") 
all_data <- llply(path, read.csv, stringsAsFactors = FALSE)
clean_data <- ldply(all_data, clean_file)
write.csv(clean_data, file = "clean_data.csv")