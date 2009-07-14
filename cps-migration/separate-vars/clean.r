# Information by CBSA code
library(plyr)
cps05 <- read.csv(gzfile("clean/2005.csv.gz"))
cps06 <- read.csv(gzfile("clean/2006.csv.gz"))
cps07 <- read.csv(gzfile("clean/2007.csv.gz"))
cps08 <- read.csv(gzfile("clean/2008.csv.gz"))

# Reason for moving 

move <- subset(cps05, nxtres != 0 & gtcbsa != 0)
	reasons05 <- ddply(move, c("year", "gtcbsa", "nxtres"), nrow)
	
move <- subset(cps06, nxtres != 0 & gtcbsa != 0)
	reasons06 <- ddply(move, c("year", "gtcbsa", "nxtres"), nrow)
	
move <- subset(cps07, nxtres != 0 & gtcbsa != 0)
	reasons07 <- ddply(move, c("year", "gtcbsa", "nxtres"), nrow)
	
move <- subset(cps08, nxtres != 0 & gtcbsa != 0)
	reasons08 <- ddply(move, c("year", "gtcbsa", "nxtres"), nrow)
	
Reasonsall <- rbind(reasons05,reasons06, reasons07, reasons08)

dir.create("separate-vars")
write.table(Reasonsall, "separate-vars/CBSAreasonformove05-08.csv", sep = ",", row = FALSE)


# 