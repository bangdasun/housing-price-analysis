library(tree)
tree1 = tree(price ~. -date-zipcode, data = training)
summary(tree1)
# Visualize the tree
plot(tree1)
text(tree1)
# Text description of tree
tree1

cv_tree1 = cv.tree(tree1)
library(ggplot2)
ggplot() + 
  geom_line(mapping = aes(x = cv_tree1$size, y = cv_tree1$dev)) + 
  geom_point(mapping = aes(x = cv_tree1$size, y = cv_tree1$dev), size = 3) + 
  labs(x = "size of tree", y = "dev")

predOfTree1 = predict(tree1, newdata = testing)
rmse1 = sqrt(mean((predOfTree1 - testing$price)^2))
rmse1
ggplot() +
  geom_point(mapping = aes(x = predOfTree1, y = testing$price), alpha = .2, size = 2) +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "prediction", y = "actual value")
ggplot() + 
  geom_line(mapping = aes(x = 1:length(predOfTree1), y = predOfTree1 - testing$price)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_abline(slope = 0, intercept = rmse1, linetype = "dashed", color = "red") + 
  geom_abline(slope = 0, intercept = -rmse1, linetype = "dashed", color = "red") + 
  labs(x = "observation", y = "price")

# Another package to build decision tree
library(rpart)
tree2 = rpart(price ~. -id-zipcode-date, data = training)
summary(tree2)
tree2
printcp(tree2)
# Get a simpler but less accurate tree by change cp
tree3 = rpart(price ~. -id-zipcode-date, data = training, cp = .1)
printcp(tree3)
plot(tree3)
text(tree3, cex = .7)
# Get a complexr tree
tree4 = rpart(price ~. -id-zipcode-date, data = training, cp = .005)
printcp(tree4)
plot(tree4)
text(tree4, cex = .7)

library(randomForest)
set.seed(1)
randForest = randomForest(I(log(price)) ~. -id-zipcode-date, data = training, mtry = 5,
                          importance = TRUE)
randForest
predOfrandForest = predict(randForest, newdata = testing)
rmse2 = sqrt(mean((testing$price - exp(predOfrandForest))^2))
rmse2
ggplot() +
  geom_point(mapping = aes(x = exp(predOfrandForest), y = testing$price), alpha = .2, size = 2) +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "prediction", y = "actual value")
ggplot() + 
  geom_line(mapping = aes(x = 1:length(predOfrandForest), y = exp(predOfrandForest) - testing$price)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_abline(slope = 0, intercept = rmse2, linetype = "dashed", color = "red") + 
  geom_abline(slope = 0, intercept = -rmse2, linetype = "dashed", color = "red") + 
  labs(x = "observation", y = "price")

importance(randForest)
varImpPlot(randForest)
