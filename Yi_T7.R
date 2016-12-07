library(tree)
set.seed(1)
data = read.csv("/Users/Yi/Desktop/DMP/Tree/7.csv")
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
# 1) root 5000 3641.00 f ( 0.88140 0.11860 )  
# 2) at_least_1_teacher_referred_donor: f 3440    0.00 f ( 1.00000 0.00000 ) *
#   3) at_least_1_teacher_referred_donor: t 1560 2072.00 f ( 0.61987 0.38013 )  
# 6) great_chat: f 899    0.00 f ( 1.00000 0.00000 ) *
#   7) great_chat: t 661  438.00 t ( 0.10287 0.89713 )  
# 14) fully_funded: f 45    0.00 f ( 1.00000 0.00000 ) *
#   15) fully_funded: t 616  196.40 t ( 0.03734 0.96266 )  
# 30) three_or_more_non_teacher_referred_donors: f 65   81.79 t ( 0.32308 0.67692 )  
# 60) one_non_teacher_referred_donor_giving_100_plus: f 24   18.08 f ( 0.87500 0.12500 ) *
#   61) one_non_teacher_referred_donor_giving_100_plus: t 41    0.00 t ( 0.00000 1.00000 ) *
#   31) three_or_more_non_teacher_referred_donors: t 551   26.47 t ( 0.00363 0.99637 ) *
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
# Residual mean deviance:  0.008921 = 44.55 / 4994 
# Misclassification error rate: 0.001 = 5 / 5000 

tree_predict = predict(tree_model, testing_data)
summary(tree_predict)
# f                t         
# Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:1.0000   1st Qu.:0.0000  
# Median :1.0000   Median :0.0000  
# Mean   :0.8787   Mean   :0.1213  
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
# 1) root 5000 593 f (0.881400000 0.118600000)  
# 2) at_least_1_teacher_referred_donor=f 3440   0 f (1.000000000 0.000000000) *
#   3) at_least_1_teacher_referred_donor=t 1560 593 f (0.619871795 0.380128205)  
#   6) great_chat=f 899   0 f (1.000000000 0.000000000) *
#     7) great_chat=t 661  68 t (0.102874433 0.897125567)  
#     14) fully_funded=f 45   0 f (1.000000000 0.000000000) *
#       15) fully_funded=t 616  23 t (0.037337662 0.962662338)  
#       30) three_or_more_non_teacher_referred_donors=f 65  21 t (0.323076923 0.676923077)  
#       60) one_non_teacher_referred_donor_giving_100_plus=f 24   3 f (0.875000000 0.125000000) *
#         61) one_non_teacher_referred_donor_giving_100_plus=t 41   0 t (0.000000000 1.000000000) *
#           31) three_or_more_non_teacher_referred_donors=t 551   2 t (0.003629764 0.996370236) *

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
# Root node error: 593/5000 = 0.1186
# 
# n= 5000 
# 
# CP nsplit rel error   xerror      xstd
# 1 0.442664      0 1.0000000 1.000000 0.0385531
# 2 0.075885      2 0.1146712 0.114671 0.0138110
# 3 0.015177      3 0.0387858 0.038786 0.0080688
# 4 0.010000      5 0.0084317 0.016863 0.0053273
plotcp(tree)
library(rpart.plot)
rpart.plot(tree,extra = 1)

