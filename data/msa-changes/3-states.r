# Separate states from city names and store with one record per state.
msa <- read.csv("msa-codes.csv")

divider <- as.numeric(regexpr(" [A-Z-]+$", msa$city))
city <- substr(msa$city, 1, divider - 1)
states <- strsplit(substr(msa$city, divider + 1, 100), "-")

lengths <- sapply(states, length)
rep <- rep(1:nrow(msa), lengths)
citystate <- data.frame(
  city = city[rep], 
  state = unlist(states), 
  msa_code = msa$msa_code[rep]
)

write.table(citystate, "msa-states.csv", row = FALSE, sep = ",")
