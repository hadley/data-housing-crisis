gdp <- read.csv("gdp-metro.csv")
pop <- read.csv("data/census-population/population-msa.csv")
names(pop)[12] <- "fips"

withpop <- merge(gdp, pop, by = c("fips","year"))

index.gdp <- withpop[,"gdp"] / withpop[,"popestimate"]

gdp2 <- cbind(withpop,index.gdp)