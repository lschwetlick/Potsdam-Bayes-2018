## linear regression 

data(attitude)

## Traditional multiple regression analysis
lmObj = lm(rating ~ ., data = attitude)
summary(lmObj)
bf = regressionBF(rating ~ ., data = attitude)

#  compare best 5 with best
bf2 = head(bf) / max(bf)
bf2
plot(bf2)


#  all single effects with intercept only
bf = regressionBF(rating ~ ., data = attitude, whichModels = "bottom")
plot(bf)

#  all single effects with full model
bf = regressionBF(rating ~ ., data = attitude, whichModels = "top")
plot(bf)


# Compare the two models
complaintsOnlyBf = lmBF(rating ~ complaints, data = attitude) 
complaintsLearningBf = lmBF(rating ~ complaints + learning, data = attitude) 
complaintsOnlyBf / complaintsLearningBf
