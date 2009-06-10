mort <- read.csv("Historical Mortgage Rates US Avg.txt", sep = "\t")

mortValues <- melt(mort, "Weekending")

N <- rep(1:nrow(mort),3)
mortValues <- cbind(mortValues,N)

ggplot(data = mortValues, aes(x = as.Date(Weekending,format = "%m/%d/%Y"))) + 
  geom_line(aes(y = value, color = variable)) + scale_x_date() + xlab("Date")


ARM <- read.csv("Historical ARM Rates US Avg.txt", sep = "\t")

armValues <- melt(ARM,"Date")

N <- rep(1:nrow(ARM), ncol(ARM) - 1)
armValues <- cbind(armValues, N)

ggplot(data = armValues, aes(x = as.Date(Date,format = "%m/%d/%Y"))) + 
  geom_line(aes(y = value,color = variable)) + scale_x_date() + xlab("Date")

