# Read in only the variables of interest - this should keep memory usage down

vars <- read.csv("ss07pusa.csv", nrows = 1)
mig_vars <- c("ST", "PUMA", "MIGSP", "MIGPUMA")
of_interest <- names(vars) %in% mig_vars
col_classes <- c("NULL", NA)[of_interest + 1]

mig_a <- read.csv("ss07pusa.csv", colClasses = col_classes)
mig_a <- subset(mig_a, !is.na(MIGSP))
mig_b <- read.csv("ss07pusb.csv", colClasses = col_classes)
mig_b <- subset(mig_b, !is.na(MIGSP))


mig <- rbind(mig_a, mig_b)[mig_vars]
names(mig) <- c("from_state", "from_puma", "to_state", "to_puma")
rm(mig_a)
rm(mig_b)

# Compute counts for each movement combinations
mig <- mig[do.call("order", mig), ]
mig$first <- !duplicated(mig)
mig$last <- c(mig$first[-1], TRUE)
mig$group <- c(0, cumsum(mig$last[-nrow(mig)]))

counts <- subset(mig, first)[1:4]
counts$count <- rle(mig$group)$lengths

write.table(counts, "migration.csv", row = F, sep = ",")