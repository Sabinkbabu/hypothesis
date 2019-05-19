# we 'll use the insectsprays dataset
str(InsectSprays)
InsectSprays
# Optional test for normality
library(lattice)
histogram(~count | spray, data = InsectSprays)

with(InsectSprays, tapply(count, spray, shapiro.test))

# we want to model the means of variable count
# as a function of the variable spray
aov_model <- aov(count ~ spray, data = InsectSprays)
aov_model

summary(aov_model)

# p < .0001 provides the evdience that the 6 sprays
# are not all the same

# models.table function allows us to examines the individual levels
# of factors. creates 2 tables
model.tables(aov_model, type = 'effects')
# spray E on average had 6 bugs fewer than the average over all fields
# on fields where A was used, farmers found on average
# 5 bugs more compared to the overall mean

# test the pairwise differences between the sprays 
# pairwise comparison test can be used to determine which group
# differences are statistically significant

comparisons <- TukeyHSD(aov_model)
# model now contains list of where each element is named
# after one factor in the model
comparisons$spray['D-C',]
comparisons$spray['F-C',]
comparisons$spray['F-E',]

plot(comparisons, las = 1)
