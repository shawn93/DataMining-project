library(tree)
set.seed(1)
data = read.csv("/Users/Yi/Desktop/DMP/Tree/9.csv")
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
# 1) root 5000 3909.00 f ( 0.867600 0.132400 )  
# 2) at_least_1_teacher_referred_donor: f 3562    0.00 f ( 1.000000 0.000000 ) *
#   3) at_least_1_teacher_referred_donor: t 1438 1984.00 f ( 0.539638 0.460362 )  
# 6) great_chat: f 674    0.00 f ( 1.000000 0.000000 ) *
#   7) great_chat: t 764  600.50 t ( 0.133508 0.866492 )  
# 14) fully_funded: f 55    0.00 f ( 1.000000 0.000000 ) *
#   15) fully_funded: t 709  345.90 t ( 0.066291 0.933709 )  
# 30) three_or_more_non_teacher_referred_donors: f 103  140.00 t ( 0.417476 0.582524 )  
# 60) one_non_teacher_referred_donor_giving_100_plus: f 40    0.00 f ( 1.000000 0.000000 ) *
#   61) one_non_teacher_referred_donor_giving_100_plus: t 63   24.12 t ( 0.047619 0.952381 ) *
#   31) three_or_more_non_teacher_referred_donors: t 606   48.14 t ( 0.006601 0.993399 ) *
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
# Number of terminal nodes:  6 
# Residual mean deviance:  0.01447 = 72.26 / 4994 
# Misclassification error rate: 0.0014 = 7 / 5000 

tree_predict = predict(tree_model, testing_data)
summary(tree_predict)
# f                  t         
# Min.   :0.006601   Min.   :0.0000  
# 1st Qu.:1.000000   1st Qu.:0.0000  
# Median :1.000000   Median :0.0000  
# Mean   :0.862047   Mean   :0.1380  
# 3rd Qu.:1.000000   3rd Qu.:0.0000  
# Max.   :1.000000   Max.   :0.9934 

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
# 1) root 5000 662 f (0.86760000 0.13240000)  
# 2) at_least_1_teacher_referred_donor=f 3562   0 f (1.00000000 0.00000000) *
#   3) at_least_1_teacher_referred_donor=t 1438 662 f (0.53963839 0.46036161)  
#   6) great_chat=f 674   0 f (1.00000000 0.00000000) *
#     7) great_chat=t 764 102 t (0.13350785 0.86649215)  
#     14) fully_funded=f 55   0 f (1.00000000 0.00000000) *
#       15) fully_funded=t 709  47 t (0.06629055 0.93370945)  
#       30) three_or_more_non_teacher_referred_donors=f 103  43 t (0.41747573 0.58252427)  
#       60) one_non_teacher_referred_donor_giving_100_plus=f 40   0 f (1.00000000 0.00000000) *
#         61) one_non_teacher_referred_donor_giving_100_plus=t 63   3 t (0.04761905 0.95238095) *
#           31) three_or_more_non_teacher_referred_donors=t 606   4 t (0.00660066 0.99339934) *

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
# Root node error: 662/5000 = 0.1324
# 
# n= 5000 
# 
# CP nsplit rel error   xerror      xstd
# 1 0.422961      0  1.000000 1.000000 0.0362019
# 2 0.083082      2  0.154079 0.154079 0.0150996
# 3 0.030211      3  0.070997 0.070997 0.0103072
# 4 0.010000      5  0.010574 0.010574 0.0039938
plotcp(tree)
library(rpart.plot)
rpart.plot(tree,extra = 1)

