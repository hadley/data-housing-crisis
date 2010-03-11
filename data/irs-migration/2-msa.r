# Convert county summaries to msa summaries
library(plyr)

mig_county <- read.csv("migration-county.csv")

cbsa <- read.csv("county-cbsa.csv")
msa <- subset(cbsa, cbsatype == "Metro")[c("county", "cbsa", "afact")]

combined <- merge(mig_county, msa, by = "county")

mig_msa <- ddply(combined, c("cbsa", "src", "year", "dir"), summarise,
  returns = sum(returns * afact, na.rm = TRUE),
  exempt = sum(exempt * afact, na.rm = TRUE),
  income = sum(income * afact, na.rm = TRUE),
  .progress = "text")
  
# subset(mig_msa, cbsa == "26420")

write.table(mig_msa, "migration-msa.csv", sep = ",", row = F)