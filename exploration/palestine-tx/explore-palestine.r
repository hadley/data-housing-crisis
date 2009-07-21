library(ggplot2)
source("explore-helper.r")

# Explore housing trends within Houston, 2000+
palestine <- subset(read.csv("texas-listings.csv"), msa == 410)
palestine$date <- palestine$year + (palestine$month - 1) / 12

qplot(date, sales, data = palestine, geom = "line")
ggsave("palestine-tx.pdf", width = 8, height = 4)