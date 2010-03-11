library(plyr)
library(stringr)

# Load inflow and outflow, extracting variables of interst and naming
# consistently

inflow_files <- dir(pattern = "inflow.*\\.csv")
names(inflow_files) <- str_extract(inflow_files, "[0-9]{4}")
raw_inflow <- ldply(inflow_files, read.csv, na = "-1")
raw_inflow <- raw_inflow[, c(".id", "County_Name", 
  "State_Code_Dest", "County_Code_Dest", 
  "Return_Num", "Exmpt_Num", "Aggr_AGI"), ]
names(raw_inflow) <- c("year", "src", "state", "county", 
  "returns", "exempt", "income")
raw_inflow$dir <- "in"

outflow_files <- dir(pattern = "outflow.*\\.csv")
names(outflow_files) <- str_extract(outflow_files, "[0-9]{4}")
raw_outflow <- ldply(outflow_files, read.csv, na = "-1")
raw_outflow <- raw_outflow[, c(".id", "County_Name", 
  "State_Code_Origin", "County_Code_Origin", 
  "Return_Num", "Exmpt_Num", "Aggr_AGI"), ]
names(raw_outflow) <- c("year", "src", "state", "county", 
  "returns", "exempt", "income")
raw_outflow$dir <- "out"

raw <- rbind(raw_inflow, raw_outflow)

# Extract only summaries
sum_re <- "(Tot Mig-((Same St)|(Diff St)|Foreign))|Non-Migrants"
summary <- str_detect(raw$src, sum_re)
raw <- raw[summary, ]

raw <- subset(raw, state != 0 & county != 0) 
raw$src <- str_extract(raw$src, sum_re)
raw$county <- with(raw, str_c(state, sprintf("%03d", county)))
raw$state <- NULL

write.table(raw, "migration-county.csv", sep = ",", row = F)