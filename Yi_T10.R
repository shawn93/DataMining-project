library(tree)
set.seed(1)
data = read.csv("/Users/Yi/Desktop/DMP/Tree/10.csv")
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
# Variables actually used in tree construction:
#   [1] "donation_total"
# Number of terminal nodes:  3 
# Residual mean deviance:  0.7495 = 2621 / 3497 
# Misclassification error rate: 0.1309 = 458 / 3500 
# > 
tree_predict1 = predict(tree_model1, testing)
summary(tree_predict1)
# f                t          
# Min.   :0.7224   Min.   :0.07556  
# 1st Qu.:0.8584   1st Qu.:0.07556  
# Median :0.8584   Median :0.14162  
# Mean   :0.8678   Mean   :0.13224  
# 3rd Qu.:0.9244   3rd Qu.:0.14162  
# Max.   :0.9244   Max.   :0.27762  
## Cross Validation
cv_tree1 = cv.tree(tree_model1)
plot(cv_tree1$size, cv_tree1$dev, type = "b")


# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
tree <- rpart(is_exciting ~ donation_to_project+donation_optional_support+donation_total+total_price_excluding_optional_support+total_price_including_optional_support, data=training)
tree
summary(tree)
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
#   [1] "donation_to_project" "poverty_level"       "donation_total"     
# Number of terminal nodes:  4 
# Residual mean deviance:  0.7391 = 2584 / 3496 
# Misclassification error rate: 0.1269 = 444 / 3500 
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
# Classification tree:
#   tree(formula = is_exciting ~ . - donation_to_project - donation_optional_support - 
#          donation_total - total_price_excluding_optional_support - 
#          total_price_including_optional_support, data = training)
# Variables actually used in tree construction:
#   [1] "at_least_1_teacher_referred_donor"             
# [2] "great_chat"                                    
# [3] "fully_funded"                                  
# [4] "three_or_more_non_teacher_referred_donors"     
# [5] "one_non_teacher_referred_donor_giving_100_plus"
# Number of terminal nodes:  6 
# Residual mean deviance:  0.004036 = 14.1 / 3494 
# Misclassification error rate: 0.0002857 = 1 / 3500 
tree_predict3 = predict(tree_model3, testing)
summary(tree_predict3)
# f                t         
# Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:1.0000   1st Qu.:0.0000  
# Median :1.0000   Median :0.0000  
# Mean   :0.8716   Mean   :0.1284  
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

