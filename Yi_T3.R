library(tree)
set.seed(1)
data = read.csv("/Users/Yi/Desktop/DMP/Tree/3.csv")
dim(data)
# [1] 10000    31
train = sample(1:nrow(data), nrow(data) / 2)
training_data = data[train,]
dim(training_data)
# [1] 5000   31
summary(training_data)
# 70% of training data
smp_size <- floor(0.7 * nrow(training_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(training_data)), size = smp_size)
training <- training_data[train_ind, ]
dim(training)
# [1] 3500   31
testing_data = data[-train,]
dim(testing_data)
# [1] 5000   31
summary(testing_data)
# 30% of testing data
smp_size1 <- floor(0.3 * nrow(testing_data))
set.seed(123)
test_ind <- sample(seq_len(nrow(testing_data)), size = smp_size1)
testing <- training_data[test_ind, ]
dim(testing)
# [1] 1500   31

# Model 1: Only for quantitative features
tree_model1 = tree(is_exciting ~ donation_total+total_price_excluding_optional_support+total_price_including_optional_support,data=training)
tree_model1
plot(tree_model1)
text(tree_model1, pretty = 0)
summary(tree_model1)
# Classification tree:
#   tree(formula = is_exciting ~ donation_total + total_price_excluding_optional_support + 
#          total_price_including_optional_support, data = training)
# Number of terminal nodes:  6 
# Residual mean deviance:  0.7071 = 2471 / 3494 
# Misclassification error rate: 0.1214 = 425 / 3500 
tree_predict1 = predict(tree_model1, testing)
summary(tree_predict1)
# f                t          
# Min.   :0.3600   Min.   :0.06298  
# 1st Qu.:0.8633   1st Qu.:0.06298  
# Median :0.8633   Median :0.13666  
# Mean   :0.8845   Mean   :0.11551  
# 3rd Qu.:0.9370   3rd Qu.:0.13666  
# Max.   :0.9370   Max.   :0.64000 
## Cross Validation
cv_tree1 = cv.tree(tree_model1)
plot(cv_tree1$size, cv_tree1$dev, type = "b")


# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
tree <- rpart(is_exciting ~ donation_to_project+donation_optional_support+donation_total+total_price_excluding_optional_support+total_price_including_optional_support, data=training)
tree
## Display complexity parameter table
printcp(tree)
# Classification tree:
#   rpart(formula = is_exciting ~ donation_to_project + donation_optional_support + 
#           donation_total + total_price_excluding_optional_support + 
#           total_price_including_optional_support, data = training)
# 
# Variables actually used in tree construction:
#   [1] donation_optional_support              total_price_excluding_optional_support
# [3] total_price_including_optional_support
# 
# Root node error: 455/3500 = 0.13
# 
# n= 3500 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.017033      0   1.00000 1.00000 0.043727
# 2 0.016117      4   0.93187 0.95824 0.042938
# 3 0.010000     10   0.82637 0.84615 0.040683
plotcp(tree)
library(rpart.plot)
rpart.plot(tree,extra = 1)

# Model 2: Donation + poverty_level + total
tree_model2 = tree(is_exciting ~ donation_to_project+donation_optional_support+donation_total+poverty_level, data=training)
tree_model2
plot(tree_model2)
text(tree_model2, pretty = 0)
summary(tree_model2)
# Classification tree:
#   tree(formula = is_exciting ~ donation_to_project + donation_optional_support + 
#          donation_total + poverty_level, data = training)
# Variables actually used in tree construction:
#   [1] "donation_optional_support" "poverty_level"            
# Number of terminal nodes:  3 
# Residual mean deviance:  0.7425 = 2596 / 3497 
# Misclassification error rate: 0.13 = 455 / 3500 
tree_predict2 = predict(tree_model2, testing)
summary(tree_predict)
# f                t          
# Min.   :0.8345   Min.   :0.00000  
# 1st Qu.:0.8345   1st Qu.:0.09199  
# Median :0.8345   Median :0.16546  
# Mean   :0.8718   Mean   :0.12822  
# 3rd Qu.:0.9080   3rd Qu.:0.16546  
# Max.   :1.0000   Max.   :0.16546 

## Cross Validation
cv_tree2 = cv.tree(tree_model2)
cv_tree2
plot(cv_tree2$size, cv_tree2$dev, type = "b")

# rplot
tree2 <- rpart(is_exciting ~donation_to_project+donation_optional_support+donation_total+poverty_level, data=training)
tree2
plotcp(tree2)
rpart.plot(tree2,extra = 1)

# Model 3: Everything except donation and price
tree_model3 = tree(is_exciting ~.-donation_to_project -donation_optional_support -donation_total -total_price_excluding_optional_support -total_price_including_optional_support, data=training)
tree_model3
plot(tree_model3)
text(tree_model3, pretty = 0)
summary(tree_model3)
tree_predict3 = predict(tree_model3, testing)
summary(tree_predict3)
# f                t         
# Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:1.0000   1st Qu.:0.0000  
# Median :1.0000   Median :0.0000  
# Mean   :0.8687   Mean   :0.1313  
# 3rd Qu.:1.0000   3rd Qu.:0.0000  
# Max.   :1.0000   Max.   :1.0000  

## Cross Validation
cv_tree3 = cv.tree(tree_model3)
plot(cv_tree3$size, cv_tree3$dev, type = "b")

# rplot
tree3 <- rpart(is_exciting ~.-donation_to_project -donation_optional_support -donation_total -total_price_excluding_optional_support -total_price_including_optional_support, data=training)
tree3
plotcp(tree3)
rpart.plot(tree3,extra = 1)

