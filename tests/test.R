library(rpart)
library(rpart.LAD)


# test 1
mystate <- data.frame(state.x77, region = state.region)
names(mystate) <- casefold(names(mystate)) #remove mixed case

# Rsqured fit
fit0 <- rpart(murder ~ ., data = mystate, minsplit = 10)
print(summary(fit0))
plot(fit0); text(fit0)
xerror0 <- colMeans((xpred.rpart(fit0) - mystate$murder)^2)
print(xerror0)

#LAD fit
fit1 <- rpart(murder ~ ., data = mystate, method = LAD, minsplit = 10)
print(summary(fit1))
plot(fit1); text(fit1)
xerror1 <- colMeans((xpred.rpart(fit1) - mystate$murder)^2)
print(xerror1)

xerror1b <- colMeans


# test 2: example from book
#library(MASS)
#fit3 <- rpart(medv ~ ., data = Boston, method = LAD)
#plot(fit3)
#text(fit3)
# unfortunately, it does not reproduce the plot entirely... reasons?

#LAD fit large
#mystate2 <- do.call(rbind, lapply(1:3000, function(i) mystate))
#print(system.time(fit1 <- rpart(murder ~ ., data = mystate2, method = LAD, minsplit = 10)))
