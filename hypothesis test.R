str(beaver2)
# The beaver dataset contains data on body temp of 4 beavers
# every 10 mins for a day for demo purposes
# we want to examine the diffrence in average body temp
#during periods of activity to evaluate whether 
# body temp is affected by activity

# activ should be a factor
# temp is numerical

# use the transform function to
# change the activ to a factor
transformed_beaver_data <- transform(beaver2, activ = factor(activ, labels = c("no", "yes")))
str(transformed_beaver_data)
# before selecting the appropraite test, we need to check
# whether the data is normally distributed or not
# notes on balckboard for info

library("lattice")

# the histogram uses a 1 sided formula so we dont specify
# anything on the left side of ~
# and on the right side we specify which varibale is in the histogram

histogram(~temp | activ, data = transformed_beaver_data)

# Quantile-quantile plot allows us to 
# examine if data is distributed normally
# compare the quantiles of both samples
# we use the square brackets to select the cases we want

with(transformed_beaver_data, 
     qqplot(temp[activ == "yes"], 
            temp[activ == "no"],
            main = "Comparing two samples",
            xlab = "active temp = yes",
            ylab = "activ temp = no"))

# using  a qq plot to check for normality
# qqnorm functions plots the sample
# against a normal distrubution
with(transformed_beaver_data, {
  qqnorm(temp[activ == "no"],
         main = "inactive data")
  qqline(temp[activ == "no"])
  })

# Formal test of normality
# provided through the Shapiro-wilks test
normality_test <- shapiro.test(transformed_beaver_data$temp)
normality_test$p.value

# p value tells us the chnaces the sample 
# comes from a normal distrubtion
# in this example, p value is less than 0.05
# so it is not normally distributed

# we can also check the normality of each variables
# using the tapply() function
with(transformed_beaver_data, tapply(temp, activ, shapiro.test))


t.test(temp~activ, data = transformed_beaver_data)


