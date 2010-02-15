# Try and find industries that are protective against the housing crisis
# 
# Pick out top and bottom 10 cities based on hpi change.
# Compare gdp for each.

# Load hpi data
# For each msa, calculate max change & date of max
# Add ranks use rank(-max_change, ties.method = "first")
# subset(,  rank <= 10 | rank > 900)