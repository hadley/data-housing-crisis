

both<- read.csv("college-hpi.csv")

qplot(x=time, y=hpi,data=both, facets=~city_state, geom="line")