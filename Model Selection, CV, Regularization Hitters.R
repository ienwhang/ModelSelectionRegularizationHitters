## Model/Variable Selection & Cross Validation Analysis

# Read in data and load packages
library(ISLR)
library(leaps)
library(MASS)
library(boot)
library(car)
library(glmnet)
hitters <- read.csv("hitters.csv", header = T)

# Find VIF 
mod <- lm(Salary ~ ., data = hitters)
summaree <- summary(mod)
vif(mod)

# All possible regressions
all <- regsubsets(Salary ~ ., data = hitters, nvmax = 19)
allSumm <- summary(all)

# Find the optimal number of explanatory variables
max_idx <- which.max(allSumm$adjr2)
print(paste("Optimal number of explanatory variables:", max_idx))

# Plot the Adjusted R^2 for each of these 
{plot(allSumm$adjr2, type = "l", xlab = "Number of Explanatory Variables", 
      ylab = bquote("Adjusted R"^2), main = "Optimal Model Order")
  points(allSumm$adjr2, pch = 16)
  points(x = max_idx, y = allSumm$adjr2[max_idx], pch = 16, col = "red")}

# Find the optimum combination of 11 variables to include
allSumm$which[max_idx,]
modelAllOptimum <- lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + 
                        CWalks + League + Division + PutOuts + Assists, 
                      data = hitters)

# Set up parameters
sm <- lm(Salary ~ 1, data = hitters)
lg <- lm(Salary ~ ., data = hitters)

# Forward selecion
forwardSteps <- stepAIC(object = sm, scope = list(upper = lg, lower = sm),
                        direction = "forward")
forward <- stepAIC(object = sm, scope = list(upper = lg, lower = sm), 
                   direction = "forward", trace = 0)

# Backward selection
backwardSteps <- stepAIC(object = lg, scope = list(upper = lg, lower = sm),
                        direction = "backward")
backward <- stepAIC(object = lg, scope = list(upper = lg, lower = sm), 
                    direction = "backward", trace = 0)

# Hybrid selection
hybridSteps <- stepAIC(object = lg, scope = list(upper = lg, lower = sm),
                       direction = "both")
hybrid <- stepAIC(object = sm, scope = list(upper = lg, lower = sm), 
                  direction = "both", trace = 0)

# Perform cross validation
# Get RMSE
n <- dim(hitters)[1]
trn <- sample(x = c(rep(TRUE, round(0.8*n)), rep(FALSE, n-round(0.8*n))), 
              size = n, replace = FALSE)
train <- hitters[trn,]
tst <- !trn 
test <- hitters[tst,]

# Function that returns RMSE
# Takes in model and test distribution as input
getRMSE <- function(mod, test) {
  pred <- predict(object = mod, newdata = test)
  RMSE <- sqrt(mean((test$Salary - pred)^2))
  return (RMSE)
}

# Full model 
mod <- lm(Salary ~ ., data = train)
RMSEfull <- getRMSE(mod, test)

# Optimal all-possible regressions model
mod <- lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + 
            CWalks + League + Division + PutOuts + Assists, 
          data = train)
RMSEoptimum <- getRMSE(mod, test)

# Best stepwise-selection model
sm <- lm(Salary ~ 1, data = train)
lg <- lm(Salary ~ ., data = train)

forward <- stepAIC(object = sm, scope = list(upper = lg, lower = sm), 
                   direction = "forward", trace = 0)
RMSEforward <- getRMSE(forward, test)
summary(forward)

backward <- stepAIC(object = lg, scope = list(upper = lg, lower = sm), 
                    direction = "backward", trace = 0)
RMSEbackward <- getRMSE(backward, test)
summary(backward)

hybrid <- stepAIC(object = sm, scope = list(upper = lg, lower = sm), 
                  direction = "both", trace = 0)
RMSEhybrid <- getRMSE(hybrid, test)
summary(hybrid)

# Get K-fold RMSE
# Full model 
mod <- glm(Salary ~ ., data = hitters)
RMSEfull_K <- sqrt((cv.glm(hitters, mod, K = 10)$delta)[1])

# Optimal all-possible regressions model
mod <- glm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + 
             CWalks + League + Division + PutOuts + Assists, 
           data = hitters)
RMSEoptimum_K <- sqrt((cv.glm(hitters, mod, K = 10)$delta)[1])

# Best stepwise-selection model
sm <- lm(Salary ~ 1, data = hitters)
lg <- lm(Salary ~ ., data = hitters)

forward <- stepAIC(object = sm, scope = list(upper = lg, lower = sm), 
                   direction = "forward", trace = 0)
mod <- glm(forward)
RMSEforward_K <- sqrt((cv.glm(hitters, mod, K = 10)$delta)[1])

backward <- stepAIC(object = lg, scope = list(upper = lg, lower = sm), 
                    direction = "backward", trace = 0)
mod <- glm(backward)
RMSEbackward_K <- sqrt((cv.glm(hitters, mod, K = 10)$delta)[1])

hybrid <- stepAIC(object = sm, scope = list(upper = lg, lower = sm), 
                  direction = "both", trace = 0)
mod <- glm(hybrid)
RMSEhybrid_K <- sqrt((cv.glm(hitters, mod, K = 10)$delta)[1])

## Shrinkage/Regularization Methods
## Ridge and LASSO
X <- model.matrix(Salary ~ ., hitters)[,-1]
y <- hitters$Salary

grid <- 10^seq(10, -3, length=1000)  # Set grid

# RIDGE
ridge.mod <- glmnet(X, y, alpha=0, lambda=grid)
plot(ridge.mod, xvar = "lambda", label = TRUE)  # Plot param ests against lambda

# LASSO
lasso.mod <- glmnet(X, y, alpha=1, lambda=grid)
plot(lasso.mod, xvar = "lambda", label = TRUE)  # Plot param ests against lambda

# Set seed and partition training and test set
set.seed(1)  
train <- sample(1:nrow(X), 210)
test <- (-train)
y.test <- y[test]

# RIDGE
cv.out <- cv.glmnet(X[train,], y[train], alpha=0)  # Find best lambda
plot(cv.out)
bestlam.r <- cv.out$lambda.min
predict(ridge.mod, s=bestlam.r, type = "coefficients")[1:20,]  # Best ridge regression model

# LASSO
cv.out <- cv.glmnet(X[train,], y[train], alpha=1)  # Find best lambda
plot(cv.out)
bestlam.l <- cv.out$lambda.min
predict(lasso.mod, s=bestlam.l, type = "coefficients")[1:20,]  # Best LASSO regression model

trn <- hitters[train,]
tst <- hitters[-train,] # Held-out test set

# Get RMSE of different models
stepwise <- lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + 
                 CRBI + CWalks + Division + PutOuts + Assists, data = trn)
step.pred <- predict(stepwise, tst)
RMSEs <- sqrt(mean((step.pred - y.test)^2))  # Predictive RMSE of best stepwise model

ridge.pred <- predict(ridge.mod, s=bestlam.r, newx=X[test,])
RMSEr <- sqrt(mean((ridge.pred - y.test)^2))  # Predictive RMSE of best ridge model

lasso.pred <- predict(lasso.mod, s=bestlam.l, newx=X[test,])
RMSEl <- sqrt(mean((lasso.pred - y.test)^2))  # Predictive RMSE of best LASSO model
