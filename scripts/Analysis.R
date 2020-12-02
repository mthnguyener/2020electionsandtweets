library(tidyverse)
library(tree)
training <- read_csv("sent_set.csv")

# Data Clean-up:

training%>%
  mutate(candidate=as.factor(case_when(candidate == "Biden" ~ 1 , candidate == "Trump" ~ 0)))%>%
  select(candidate, anticipation,sadness,fear,joy,positive,surprise,trust,anger,disgust,negative)->full.set

attach(full.set)

head(full.set)

# Variable selection

# Best subset selection

library(leaps)
reg.fit = regsubsets(candidate~.,nvmax=10, data=full.set)
summary(reg.fit)

plot( summary(reg.fit)$cp, main="Cp"); lines( summary(reg.fit)$cp )
plot( summary(reg.fit)$bic, main="BIC"); lines( summary(reg.fit)$bic )
plot( summary(reg.fit)$adjr2, main="ADJUSTED R-SQUARE"); lines( summary(reg.fit)$adjr2 )

which.min( summary(reg.fit)$cp )
which.min( summary(reg.fit)$bic )
which.max( summary(reg.fit)$adjr2 ) #If we look at the plot after 9 curve flattens. So with best subset model we can choose either 9 or 10 variables.  Lets look at other variable selection methods:

# Forward and Backward
reg.fit.fwd <- regsubsets(candidate~.,data = full.set,nvmax=10,method = "forward")

summary(reg.fit.fwd)

plot(reg.fit.fwd)
plot(reg.fit.fwd, scale = "adjr2")
plot(reg.fit.fwd, scale = "cp")

# Backward:

reg.fit.bwd <- regsubsets(candidate~.,data = full.set,nvmax=20, method = "backward")

summary(reg.fit.bwd)

plot(reg.fit.bwd)
plot(reg.fit.bwd, scale = "adjr2")

# Running correlation between variables to check for any multicollinearity: 
corr <- full.set%>%
  select(anticipation:negative)%>%
  cor()

corrplot(cor(corr), method="color", type = "upper", tl.col="black",tl.srt=40, addCoef.col = "gray8", diag = T, number.cex = 0.65)


# GLM binomial Sentiment:

set.seed(1)
n= length(candidate)

Z = sample(1:nrow(full.set), 0.7*nrow(full.set))
glm.fit <- glm(candidate ~ ., data=full.set[Z,],family=binomial) 

summary(glm.fit)

# dev.Rsq <- (null.dev - res.dev)/null.dev
# dev.Rsq <- (27100 -25706)/27100
# dev.Rsq

Probability = predict(glm.fit,full.set[-Z,], type="response")
Predicted.Direction = rep("0",length(Probability))
Predicted.Direction[ Probability > 0.5 ] = "1"

table( candidate[-Z], Predicted.Direction )


mean( candidate[-Z] == Predicted.Direction ) #We correctly classified 2599 for Biden and 2410 for Trump. overall our correct classification rate is 59% and error rate is 40% - a little better than coin toss. 

# Classification Tree

library(tree)
class.tree <- tree(candidate ~., data=full.set, mindev=0.005)

summary(class.tree)

plot(class.tree,type ="uniform")
text(class.tree, pretty=0)

# Process above may produce good predictions on training data but is likely to overfit because we might grow a large tree. In this case reduce it by eliminating the least important nodes.


# estimate the correct classification rate by cross validation. 

set.seed(1)

n= length(candidate)

Z = sample(1:nrow(full.set), 0.7*nrow(full.set))

train.tree = tree(candidate ~ .,data=full.set[Z,])

candidate.predict = predict(train.tree,full.set, type = "class")
table(candidate.predict[-Z], candidate[-Z])
mean(candidate.predict[-Z]!=candidate[-Z])


# Using cross validation to determine the optimal complexity of a tree and the number of terminal nodes that minimizes the deviance. 
cv = cv.tree(train.tree)
cv
plot(cv)


# instead of optimizing by the smallest deviance, optimize the complexity and the number of terminal nodes by the smallest mis-classification error
cv = cv.tree(train.tree, FUN = prune.misclass)
cv
plot(cv)

#prune the tree to the optimal size which is 3 obtained above
tree.pruned = prune.misclass(train.tree, best = 3)

plot(tree.pruned)
text(tree.pruned, pretty=0)

# Classification tree returned : positive as the most important predictor - surprise being the next important predictor. When the tweet message is  positive and it is greater than 2.5 - we will be predicting the message is more for Biden. 

# When message is positive but less than 2.5 and greater than 0.5 surprise ( in another word, when the message is not very positive but has some surprise elements in it than we classify the message as more support for Biden.)

# Again, this is great for interpretation but if we look at the test MSE = 0.4, we are still doing a just a little better than tossing a coin. classification tree returned similar results as the logistic regression. 



# Above 2 methods provided great intrepretability but prediction accuracy was low. our goal was to have a high prediction accuracy. Therefore,we opted run some models where we lose some intrepretability but we will gain some prediction accuracy

# Random Forrest and Bagging

library(randomForest)
set.seed(1)
train <- sample(1:nrow(full.set), 0.7*nrow(full.set)) 
# Bagging 500 trees (default), with 10 predictors each, with variable importance statistics
bag.election <- randomForest(candidate~., data=full.set[train,], mtry=10, importance=T)
# mtry =30 uses all 30 predictors; ntree = is number of trees to fit (default is 500 trees)
bag.election # Check it out
varImpPlot(bag.election) # Variable importance plots
importance(bag.election)

# Bagging Tree Predictions
bag.pred <- predict(bag.election, newdata=full.set[-train,]) # Predict with the train model and test data
plot(bag.pred, full.set$candidate[-train], xlab="Predicted", ylab="Actual") # Plot predicted vs. actual
abline(0,1) # Draw a 45 degree line (intercept=0; slope=1)
mean((bag.pred-full.set$candidate[-train])^2) # Get the mean squared error -> 0.28


# Random Forest
set.seed(1)
train <- sample(1:nrow(full.set), 0.7*nrow(full.set))
# Bagging 500 trees (default), with 6 predictors each, with variable importance statistics
rf.election <- randomForest(candidate ~ . , data=full.set[-train,], mtry=10, importance=T)
plot(rf.election) # Looks like the MSE error flattens after about 80 trees

rf.election # Check out the tree
varImpPlot(rf.election) # We can also plot the results
importance(rf.election) # To view the importance of each variable

rf.pred <- predict(rf.election,newdata=full.set[-train,]) # Predict with the train model and test data
plot(rf.pred , full.set$candidate[-train], xlab="Predicted", ylab="Actual") # Plot predicted vs. actual 
abline(0,1) # Draw a 45 degree line  (intercept=0; slope=1)
mean((rf.pred-full.set$candidate[-train])^2) # Mean squared error same as bagging -> 0.286