#########################
# Market Basket Analysis
#########################


library(arules)
data(AdultUCI)
dim(AdultUCI)

# drop useless columns
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0])

# convert all numerical varibles into ordered factors since Association rule cannot take continuous variables
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0, 25, 40, 60, 168)), labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
# -Inf ~ 0 = None, 1 ~ 7298 = Low, 7299 ~ Inf = High 
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]], 
                                          c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]), Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]], 
                                          c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]), Inf)), labels = c("None", "Low", "High"))

# transform into transactions such that we have all the items bought together in one row.
Adult = as(AdultUCI,"transactions")
Adult

# encode
aa = as(Adult, "matrix")

# see the frequency *cex.name = font size
itemFrequencyPlot(Adult[, itemFrequency(Adult) > 0.2], cex.names = 0.8)

# default with minimum support of 0.1, minimum confidence of 0.8, but custom parapeter using list
# support: (A+B) / total : Fraction of transactions that contain both A and B
# condidence: (A+B) / A : Fraction of transactions that B appear in transactions that contain A only
rules <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))
rules

summary(rules)
# rhs = {A}, lhs = {B}
# lift = increase in expectation that someone will buy {B}, when we know that they bought {A}

# %in% : select itemsets matching any given item
# select all rules with item "income=small" in the rhs and lift > 1.2
rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeSmall

# display association & transaction in readable form
inspect(sort(rulesIncomeSmall, by = "confidence")[1:3])

rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)
rulesIncomeLarge
inspect(sort(rulesIncomeLarge, by = "confidence")[1:3])





