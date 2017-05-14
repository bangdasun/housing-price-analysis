setwd("C://Users//Bangda//Desktop//project-housing price analysis")
training = read.csv("training.csv", header = TRUE)
testing  = read.csv("test.csv", header = TRUE)

# Visualize the relationship between predictors and price (response)
library(ggplot2)
p1 = ggplot(training, aes(factor(floors), price)) + geom_boxplot() + 
  labs(x = "floors", y = "price", title = "Different floors")
p2 = ggplot(training, aes(factor(waterfront), price)) + geom_boxplot() + 
  labs(x = "waterfront", y = "price", title = "Whether has waterfront")
p3 = ggplot(training, aes(factor(view), price)) + geom_boxplot() +
  labs(x = "view", y = "price", title = "Different views")
p4 = ggplot(training, aes(factor(condition), price)) + geom_boxplot() + 
  labs(x = "condition", y = "price", title = "Different conditions")
p5 = ggplot(training, aes(factor(grade), price)) + geom_boxplot() + 
  labs(x = "grade", y = "price", title = "Different grades")
multiplot(p1, p2, p3, p4 ,p5, cols = 3)

# NOTE: The following is from cookbook - R
#  
#   http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/  
# 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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

p6 = ggplot(training) + 
  geom_point(mapping = aes(x = bedrooms, y = price), alpha = .1, size = 1.5)
p7 = ggplot(training) + 
  geom_point(mapping = aes(x = bathrooms, y = price), alpha = .1, size = 1.5)
p8 = ggplot(training) + 
  geom_point(mapping = aes(x = sqft_living, y = price), alpha = .1, size = 1.5)
p9 = ggplot(training) + 
  geom_point(mapping = aes(x = sqft_lot, y = price), alpha = .1, size = 1.5)
p10 = ggplot(training) + 
  geom_point(mapping = aes(x = yr_built, y = price), alpha = .1, size = 1.5)
p11 = ggplot(training) + 
  geom_point(mapping = aes(x = lat, y = price), alpha = .1, size = 1.5)
p12 = ggplot(training) + 
  geom_point(mapping = aes(x = long, y = price), alpha = .1, size = 1.5)
multiplot(p6, p7, p8, p9, p10, p11, p12, cols = 4)

# Visualize location
ggplot(training) + 
  geom_point(mapping = aes(x = long, y = lat, color = zipcode), alpha = .5)

library(maps)
all_states = map_data("state")
washington = all_states[all_states$region == "washington", ]
p = ggplot() + 
  geom_polygon(data = washington, aes(x = long, y = lat, group = group), color = "white", fill = "grey")
p13 = ggplot() + 
  geom_polygon(data = all_states, aes(x = long, y = lat, group = group), color = "white", fill = "grey") + 
  geom_point(data = training, mapping = aes(x = long, y = lat, color = zipcode), alpha = .5) + 
  labs(title = "location of all the houses with different ")
p13
p14 = p + 
  geom_point(data = training, mapping = aes(x = long, y = lat, color = zipcode), alpha = .5) + 
  coord_cartesian(ylim = c(47.1, 47.8), xlim = c(-122.5, -121.5))
p14
multiplot(p13, p14, cols = 2)

# More visualizations
ggplot(training, aes(x = factor(waterfront), y = price, fill = factor(grade))) + 
  geom_bar(stat = "identity", position = "dodge") +  
  scale_fill_brewer(palette="Spectral") + 
  labs(x = "whether has waterfront", title = "Distribution of grade based on whether the house has waterfront")

ggplot(training, aes(x = factor(waterfront), y = price, fill = factor(grade))) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ factor(view)) + 
  scale_fill_brewer(palette="Spectral") + 
  labs(x = "whether has waterfront", title = "Distribution of grade/view based on whether the house has waterfront")

ggplot(training, aes(x = factor(waterfront), y = price, fill = factor(grade))) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(factor(view) ~ factor(condition)) + 
  scale_fill_brewer(palette="Spectral") + 
  labs(x = "whether has waterfront", title = "Distribution of grade/view/condition based on whether the house has waterfront")

# Correlation matrix
variable_set <- training[c("price", "sqft_living", "bedrooms", "bathrooms",
                           "floors", "waterfront", "view", "grade")]
corMatrix = round(cor(variable_set), digits = 3)
heatmap(corMatrix, Rowv = NA, Colv = NA)
