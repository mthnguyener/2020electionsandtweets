# Read Dataset
full.set <- read.csv("data/sent_training.csv")

model.set <- full.set %>%
  rename(c("biden"="candidate")) %>%
  mutate(biden = ifelse(biden=="Biden", 1, 0)) %>%
  select(-date, -name, -sources, -isretweeted, -length)

# Split training and testing sets - set.seed(1)

# Bagging
library(randomForest) # Contains the randomForest() function
library(MASS) # Contains the Boston housing data set
set.seed(1)
train <- sample(1:nrow(model.set), 0.7*nrow(model.set)) 
# Bagging 500 trees (default), with 13 predictors each, with variable importance statistics
bag.election <- randomForest(biden~., data=model.set[train,], mtry=30, importance=T)
# mtry =30 uses all 30 predictors; ntree = is number of trees to fit (default is 500 trees)
bag.election # Check it out
varImpPlot(bag.election) # Variable importance plots
importance(bag.election)

# Bagging Tree Predictions
bag.pred <- predict(bag.election, newdata=model.set[-train,]) # Predict with the train model and test data
plot(bag.pred, model.set$biden[-train], xlab="Predicted", ylab="Actual") # Plot predicted vs. actual
abline(0,1) # Draw a 45 degree line (intercept=0; slope=1)
mean((bag.pred-model.set$biden[-train])^2) # Get the mean squared error -> 0.1373493


# Random Forest
# Bagging 500 trees (default), with 6 predictors each, with variable importance statistics
rf.election <- randomForest(biden ~ . , data=model.set[-train,], mtry=6, importance=T)
plot(rf.election) # Looks like the MSE error flattens after about 80 trees

rf.election # Check out the tree
varImpPlot(rf.election) # We can also plot the results
importance(rf.election) # To view the importance of each variable

rf.pred <- predict(rf.election,newdata=model.set[-train,]) # Predict with the train model and test data
plot(rf.pred , model.set$biden[-train], xlab="Predicted", ylab="Actual") # Plot predicted vs. actual 
abline(0,1) # Draw a 45 degree line  (intercept=0; slope=1)
mean((rf.pred-model.set$biden[-train])^2) # Mean squared error is much lower than Bagging -> 0.1093216
