library(plyr)
library(ggplot2)
library(MASS)
options(stringsAsFactors = FALSE)

msa <- read.csv("../../data/msa-changes/msa-states.csv")
tx_msa <- subset(msa, state == "TX")
tx_msa$state <- NULL
tx_codes <- tx_msa$msa_code

msa_labels <- read.csv("../../data/msa-changes/msa-major.csv")

# Load population data
pop <- read.csv("../../data/census-population/population-msa.csv")
pop <- subset(pop, msa_code %in% tx_codes)
popmin <- pop[c("msa_code", "year", "npopchg_", "popestimate", "domesticmig")]
names(popmin)[3:4] <- c("pop_change", "pop")

# Load construction data
cons <- read.csv("../../data/construction/construction-housing-units.csv")
cons <- subset(cons, msa_code %in% tx_codes)
cons <- cons[with(cons, order(city, year, month, size)), ]
cons$city <- NULL

# Load multiple listing service data
listings <- read.csv("../../data/texas-msa-sales/texas-listings.csv")
listings <- subset(listings, year >= 2000)
recenter_msa <- read.csv("../../data/texas-msa-sales/msa-names.csv")

listings_raw$city <- NULL
listings_raw <- merge(listings, recenter_msa, by = "msa")

listings <- ddply(listings_raw, c("msa_code", "year", "month"), summarise,
  volume = sum(volume, na.rm = T), 
  listings = sum(listings, na.rm = T), 
  sales = sum(sales, na.rm = T))
is.na(listings$sales) <- listings$sales == 0 

listings <- transform(listings, 
  avg_price = volume / sales,
  pct_sold = sales / listings,
  date = year + month / 12)


listings <- merge(listings, msa_labels, by = "msa_code", all.x = T)
listings <- listings[with(listings, order(msa_code, year, month)), ]

# Explore sales standardised by population -----------------------------------
xdate <- scale_x_continuous("Date", breaks = c(2001, 2003, 2005, 2007, 2009), 
 labels = c("01", "", "05", "", "09"))

listpop <- merge(listings, popmin, by = c("msa_code", "year"))

qplot(date, sales, data = listpop, geom = "line", group = city)
qplot(date, sales, data = listpop, geom = "line") + 
  facet_wrap(~ city) + xdate
ggsave("sales.pdf", width = 8, height = 6)

qplot(date, sales / pop * 1e4, data = listpop, geom = "line") + 
  facet_wrap(~ city) + xdate + ylim(2, 22)
ggsave("sales-per-person.pdf", width = 8, height = 6)

deseas <- function(var, month) {
  resid(rlm(var ~ factor(month), na.action = "na.exclude")) + 
    mean(var, na.rm = TRUE)
}

listpop <- ddply(listpop, "msa_code", transform,
  sales_adj = deseas(sales / pop * 1e4, month)
)

qplot(date, sales_adj, data = listpop, geom = "line") + 
  facet_wrap(~ city) + xdate + ylim(2, 22)
ggsave("sales-adj.pdf", width = 8, height = 6)

list2008 <- ddply(subset(listpop, year == 2008), "city", summarise,
  pop = pop[length(pop)], 
  sales = sum(sales),
  avg_sales_per_10000 = sum(sales) / pop[length(pop)] / 12 * 1e4
)
qplot(pop / 1e6, avg_sales_per_10000, data = list2008, 
  xlab = "Population (millions)", 
  ylab = "Average monthly sales (per 10000 people)") + 
  geom_text(aes(label = city), size = 4, hjust = -0.1, angle = 45)
ggsave("sales-pop.pdf", width = 8, height = 6)

qplot(pop, avg_sales_per_10000, data = list2008, log = "xy")
qplot(pop, avg_sales_per_10000, label = city, data = list2008, geom = "text", 
  log = "x")
  

# Compare indices for population, sales, price, and new construction ---------
single <- subset(cons, size == 1)
single$size <- NULL
all <- merge(listpop, single, by = c("msa_code", "year", "month"), all = T)
all <- all[with(all, order(msa_code, year, month)), ]
all <- subset(all, !is.na(city))

index <- function(x) x / x[1]
smooth <- function(var, data) {
  try_default(predict(mgcv::gam(var ~ s(data), na.action = na.exclude)), NA)
}

smoothes <- ddply(all, "city", summarise,
  date = date,
  construction_s = smooth(units / pop, date) * 1e4,
  population_s = smooth(pop, date),
  sales_s = smooth(sales / pop, date) * 1e4,
  sold_value_s = smooth(avg_price, date),
  new_value_s = smooth(valuation / units, date)
)

ggplot(smoothes, aes(date)) + 
  geom_line(aes(y = sales_s, colour = "sold")) + 
  geom_line(aes(y = construction_s, colour = "built")) + 
  labs(colour = "Type", y = "Houses (per 10000 people)") + 
  facet_wrap(~ city) + 
  xdate
ggsave("new-vs-old-n.pdf", width = 8, height = 6)

ggplot(smoothes, aes(date)) + 
  geom_line(aes(y = sold_value_s / 1e3, colour = "sold")) + 
  geom_line(aes(y = new_value_s, colour = "built")) + 
  labs(colour = "Type", y = "Average price ($000)") + 
  facet_wrap(~ city) + 
  xdate
ggsave("new-vs-old-price.pdf", width = 8, height = 6)
