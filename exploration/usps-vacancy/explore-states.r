a <- read.csv(gzfile("new-vacancy.csv.gz"))
closeAllConnections()
dir.create("states")

for(state in unique(a$statefips))
{
  stateData <- a[a$statefips == state, ]

  pdf(paste("states/",state, ".pdf",sep = "", collapse = ""), width =8, height = 6)
  for(i in unique(stateData$countyfips))
    print(qplot(year + quarter / 4, vac, data = stateData[stateData$countyfips == i, ], colour = type, main = paste("vac in county = ", i, ", state = ", state, sep = "", collapse = "")))
  dev.off()

}
