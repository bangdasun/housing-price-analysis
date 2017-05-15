# Data import 
setwd("C://Users//Bangda//Desktop//project-housing price analysis")
training = read.csv("training.csv", header = TRUE)
testing  = read.csv("test.csv", header = TRUE)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(ggplot2)
ggplot(training) + 
  geom_point(mapping = aes(x = sqft_living, y = price), alpha = .1, size = 2)

simple_model1 = lm(price ~ sqft_living, data = training)
summary(simple_model1)
# Logarithm transform
simple_model2 = lm(I(log(price)) ~ I(log(sqft_living)), data = training)
summary(simple_model2)

predOfModel1 = predict(simple_model1, newdata = testing)
rmse1 = sqrt(mean((predOfModel1 - testing$price)^2))
predOfModel2 = predict(simple_model2, newdata = testing)
rmse2 = sqrt(mean((exp(predOfModel2) - testing$price)^2))
rmseVec = c(rmse1, rmse2)
rmseVec

findMinRMSE = function(predictor) {
  # @param predictor
  # @return RMSE of linear regression model
  if (!predictor%in%c("sqft_living", "sqft_lot")) {
    reg   = formula(paste("I(log(price)) ~ ", predictor))
    model = lm(reg, data = training)
    pred  = predict(model, newdata = testing)
    rmse  = sqrt(mean((testing$price - exp(pred))^2))
  }
  else {
    reg   = formula(paste("I(log(price)) ~ ", paste("I(log(", predictor, "))", sep = "")))
    model = lm(reg, data = training)
    pred  = predict(model, newdata = testing)
    rmse  = sqrt(mean((testing$price - exp(pred))^2))
  }
  return(rmse)
}
# Check the function
findMinRMSE(predictor = "sqft_living")
# Find the best predictor
predictorName = c("bedrooms", "bathrooms", "sqft_living",
                  "sqft_lot", "floors"   , "waterfront",
                  "view"    , "condition", "grade",
                  "yr_built", "lat"      , "long")
predictorMatrix = matrix(predictorName, nrow = length(predictorName))
rmseVec = apply(predictorMatrix, MARGIN = 1, findMinRMSE)
rmseVec
rmsedf = data.frame(predictor = predictorName,
                    rmse = rmseVec)
ggplot(rmsedf, mapping = aes(x = predictor, y = rmseVec)) +
  geom_bar(stat = "identity")

multiple_model1 = lm(I(log(price)) ~ I(log(sqft_living)) + 
                       bedrooms + bathrooms + grade + waterfront, 
                     data = training)
summary(multiple_model1)

predOfModel3 = predict(multiple_model1, newdata = testing)
rmse3 = sqrt(mean((testing$price - exp(predOfModel3))^2))
rmse3

residOfMultiModel1 = residuals(multiple_model1)
residdf = data.frame(residOfMultiModel1 = residOfMultiModel1,
                     logsqft_living     = log(training[, "sqft_living"]),
                     bedrooms           = training[, "bedrooms"],
                     bathrooms          = training[, "bathrooms"],
                     grade              = training[, "grade"],
                     waterfront         = training[, "waterfront"])
p1 = ggplot(residdf) + 
  geom_point(mapping = aes(x = logsqft_living, y = residOfMultiModel1), alpha = .1)
p2 = ggplot(residdf) + 
  geom_point(mapping = aes(x = bedrooms, y = residOfMultiModel1), alpha = .1)
p3 = ggplot(residdf) + 
  geom_point(mapping = aes(x = bathrooms, y = residOfMultiModel1), alpha = .1)
p4 = ggplot(residdf, aes(factor(grade), residOfMultiModel1)) + geom_boxplot()
p5 = ggplot(residdf, aes(factor(waterfront), residOfMultiModel1)) + geom_boxplot()
multiplot(p1, p2, p3, p4, p5, cols = 3)

multiple_model2 = lm(I(log(price)) ~ I(log(sqft_living)) + 
                       bedrooms + bathrooms + grade + waterfront + yr_built + lat + long,
                     data = training)
summary(multiple_model2)

predOfModel4 = predict(multiple_model2, newdata = testing)
rmse4 = sqrt(mean((testing$price - exp(predOfModel4))^2))
rmse4

ggplot(training, aes(factor(zipcode), price)) + geom_boxplot() + coord_flip()

multiple_model3 = lm(I(log(price)) ~ I(log(sqft_living)) + 
                       bedrooms + bathrooms + grade + waterfront + yr_built + lat + long + factor(zipcode),
                     data = training)
summary(multiple_model3)

predOfModel5 = predict(multiple_model3, newdata = testing)
rmse5 = sqrt(mean((testing$price - exp(predOfModel5))^2))
rmse5

multiple_model4 = lm(I(log(price)) ~ I(log(sqft_living)) + 
                       bedrooms * bathrooms * grade + floors + waterfront + yr_built + 
                       lat + long + factor(view) + factor(zipcode) + factor(condition),
                     data = training)
summary(multiple_model4)
predOfModel6 = predict(multiple_model4, newdata = testing)
rmse6 = sqrt(mean((testing$price - exp(predOfModel6))^2))
rmse6

