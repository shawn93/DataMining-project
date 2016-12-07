library(tree)
set.seed(1)
data = read.csv("/Users/Yi/Desktop/DMP/Tree/8.csv")
dim(data)
# [1] 10000    31
train = sample(1:nrow(data), nrow(data) / 2)
training_data = data[train,]
dim(training_data)
# [1] 5000   31
testing_data = data[-train,]
dim(testing_data)
# [1] 5000   31

tree_model = tree(is_exciting ~ ., data=training_data)
tree_model
# node), split, n, deviance, yval, (yprob)
# * denotes terminal node
# 
# 1) root 5000 4118.000 f ( 0.856200 0.143800 )  
# 2) at_least_1_teacher_referred_donor: f 3366    0.000 f ( 1.000000 0.000000 ) *
#   3) at_least_1_teacher_referred_donor: t 1634 2242.000 f ( 0.559976 0.440024 )  
# 6) great_chat: f 820    0.000 f ( 1.000000 0.000000 ) *
#   7) great_chat: t 814  586.600 t ( 0.116708 0.883292 )  
# 14) fully_funded: f 74    0.000 f ( 1.000000 0.000000 ) *
#   15) fully_funded: t 740  191.000 t ( 0.028378 0.971622 )  
# 30) three_or_more_non_teacher_referred_donors: f 61   70.200 t ( 0.262295 0.737705 )  
# 60) one_non_teacher_referred_donor_giving_100_plus: f 15    0.000 f ( 1.000000 0.000000 ) *
#   61) one_non_teacher_referred_donor_giving_100_plus: t 46    9.635 t ( 0.021739 0.978261 ) *
#   31) three_or_more_non_teacher_referred_donors: t 679   59.070 t ( 0.007364 0.992636 )  
# 62) at_least_1_green_donation: f 5    0.000 f ( 1.000000 0.000000 ) *
#   63) at_least_1_green_donation: t 674    0.000 t ( 0.000000 1.000000 ) *
plot(tree_model)
text(tree_model, pretty = 0)
summary(tree_model)
# Classification tree:
#   tree(formula = is_exciting ~ ., data = training_data)
# Variables actually used in tree construction:
#   [1] "at_least_1_teacher_referred_donor"             
# [2] "great_chat"                                    
# [3] "fully_funded"                                  
# [4] "three_or_more_non_teacher_referred_donors"     
# [5] "one_non_teacher_referred_donor_giving_100_plus"
# [6] "at_least_1_green_donation"                     
# Number of terminal nodes:  7 
# Residual mean deviance:  0.00193 = 9.635 / 4993 
# Misclassification error rate: 2e-04 = 1 / 5000 

tree_predict = predict(tree_model, testing_data)
summary(tree_predict)
# f                t         
# Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:1.0000   1st Qu.:0.0000  
# Median :1.0000   Median :0.0000  
# Mean   :0.8544   Mean   :0.1456  
# 3rd Qu.:1.0000   3rd Qu.:0.0000  
# Max.   :1.0000   Max.   :1.0000

## Cross Validation
set.seed(3)
cv_tree = cv.tree(tree_model)
plot(cv_tree$size, cv_tree$dev, type = "b")

# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
tree <- rpart(is_exciting ~ ., data=training_data)
tree
# n= 5000 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 5000 719 f (0.85620000 0.14380000)  
# 2) at_least_1_teacher_referred_donor=f 3366   0 f (1.00000000 0.00000000) *
#   3) at_least_1_teacher_referred_donor=t 1634 719 f (0.55997552 0.44002448)  
#   6) great_chat=f 820   0 f (1.00000000 0.00000000) *
#     7) great_chat=t 814  95 t (0.11670762 0.88329238)  
#     14) fully_funded=f 74   0 f (1.00000000 0.00000000) *
#       15) fully_funded=t 740  21 t (0.02837838 0.97162162)  
#       30) three_or_more_non_teacher_referred_donors=f 61  16 t (0.26229508 0.73770492)  
#       60) one_non_teacher_referred_donor_giving_100_plus=f 15   0 f (1.00000000 0.00000000) *
#         61) one_non_teacher_referred_donor_giving_100_plus=t 46   1 t (0.02173913 0.97826087) *
#           31) three_or_more_non_teacher_referred_donors=t 679   5 t (0.00736377 0.99263623) *

## Display complexity parameter table
printcp(tree)
# Classification tree:
#   rpart(formula = is_exciting ~ ., data = training_data)
# 
# Variables actually used in tree construction:
#   [1] at_least_1_teacher_referred_donor             
# [2] fully_funded                                  
# [3] great_chat                                    
# [4] one_non_teacher_referred_donor_giving_100_plus
# [5] three_or_more_non_teacher_referred_donors     
# 
# Root node error: 719/5000 = 0.1438
# 
# n= 5000 
# 
# CP nsplit rel error   xerror      xstd
# 1 0.433936      0 1.0000000 1.000000 0.0345083
# 2 0.102921      2 0.1321280 0.132128 0.0134266
# 3 0.010431      3 0.0292072 0.029207 0.0063601
# 4 0.010000      5 0.0083449 0.025035 0.0058901
plotcp(tree)
library(rpart.plot)
rpart.plot(tree,extra = 1)

