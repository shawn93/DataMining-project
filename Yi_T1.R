library(tree)
set.seed(1)
data = read.csv("/Users/Yi/Desktop/DMP/Tree/1.csv")
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

# Logistic Regression
# Test feature importance
# For donations and prices
glm.fit = glm(is_exciting ~ donation_to_project, data=training, family=binomial)
summary(glm.fit)
glm.fit = glm(is_exciting ~ donation_optional_support, data=training, family=binomial)
summary(glm.fit)
glm.fit = glm(is_exciting ~ donation_total, data=training, family=binomial)
summary(glm.fit)
# High P-Value
glm.fit = glm(is_exciting ~ total_price_excluding_optional_support, data=training, family=binomial)
summary(glm.fit)
# High P-Value
glm.fit = glm(is_exciting ~ total_price_including_optional_support, data=training, family=binomial)
summary(glm.fit)

# For other features
glm_feature <- glm(is_exciting ~ is_teacher_acct+donation_included_optional_support+payment_included_acct_credit+payment_included_campaign_gift_card+payment_included_web_purchased_gift_card+payment_was_promo_matched+via_giving_page+for_honoree+three_or_more_non_teacher_referred_donors+one_non_teacher_referred_donor_giving_100_plus+donation_from_thoughtful_donor+school_metro+school_magnet+school_year_round+school_nlns+school_kipp+school_charter_ready_promise+teacher_teach_for_america+poverty_level+eligible_double_your_impact_match+eligible_almost_home_match, data = training, family = binomial)
summary(glm_feature)
# Low P-Value: is_teacher_acctt, payment_included_acct_creditt, payment_included_campaign_gift_cardt,
# payment_was_promo_matchedt, via_giving_paget, three_or_more_non_teacher_referred_donorst,
# school_metrosuburban, school_metrourban, teacher_teach_for_americat, poverty_levelhighest poverty,
# eligible_double_your_impact_matcht

# High P-Value:
glm.fit <- glm(is_exciting ~ great_chat, data = training, family = binomial)
summary(glm.fit)
glm.fit <- glm(is_exciting ~ at_least_1_teacher_referred_donor, data = training, family = binomial)
summary(glm.fit)
glm.fit <- glm(is_exciting ~ fully_funded, data = training, family = binomial)
summary(glm.fit)
glm.fit <- glm(is_exciting ~ at_least_1_teacher_referred_donor, data = training, family = binomial)
summary(glm.fit)
glm.fit <- glm(is_exciting ~ at_least_1_green_donation, data = training, family = binomial)
summary(glm.fit)

glm.fit <- glm(is_exciting ~ three_or_more_non_teacher_referred_donors+one_non_teacher_referred_donor_giving_100_plus+donation_from_thoughtful_donor+school_metro+school_magnet+school_year_round+school_nlns+school_kipp+school_charter_ready_promise+teacher_teach_for_america+poverty_level+eligible_double_your_impact_match+eligible_almost_home_match, data = training, family = binomial)
summary(glm.fit)
# Low P-Value: three_or_more_non_teacher_referred_donorst, school_metrosuburban, school_metrourban,
# teacher_teach_for_americat, poverty_levelhighest poverty

# Model 1: Only for quantitative features
hist(training$donation_total)
summary(training$donation_total)
# highs = subset(training$donation_total, training$donation_total > 200)
# lows = subset(training$donation_total, training$donation_total < 200)
# Problem!!!
# oversampled_highs = highs[sample(nrow(highs), 900, replace = TRUE),  ]
# final_dataset = merge(oversampled_highs, lows, all.x = TRUE, all.y = TRUE)
hist(training$donation_to_project)
hist(training$donation_optional_support)

tree_model1 = tree(is_exciting ~ donation_total+total_price_excluding_optional_support+total_price_including_optional_support,data=training)
tree_model1
plot(tree_model1)
text(tree_model1, pretty = 0)
summary(tree_model1)
# Classification tree:
#   tree(formula = is_exciting ~ donation_total + donation_optional_support + 
#          donation_total + total_price_excluding_optional_support + 
#          total_price_including_optional_support, data = training)
# Variables actually used in tree construction:
#   [1] "donation_total"                         "donation_optional_support"             
# [3] "total_price_excluding_optional_support"
# Number of terminal nodes:  4 
# Residual mean deviance:  0.7275 = 2543 / 3496 
# Misclassification error rate: 0.1277 = 447 / 3500 

tree_predict1 = predict(tree_model1, testing)
summary(tree_predict1)
# f                t         
# Min.   :0.8369   Min.   :0.0000  
# 1st Qu.:0.8369   1st Qu.:0.1053  
# Median :0.8369   Median :0.1631  
# Mean   :0.8730   Mean   :0.1270  
# 3rd Qu.:0.8947   3rd Qu.:0.1631  
# Max.   :1.0000   Max.   :0.1631 

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
#   [1] donation_to_project                    total_price_excluding_optional_support
# [3] total_price_including_optional_support
# 
# Root node error: 191/1500 = 0.12733
# 
# n= 1500 
# 
# CP nsplit rel error xerror     xstd
# 1 0.013962      0   1.00000 1.0000 0.067594
# 2 0.013089      3   0.95812 1.0157 0.068045
# 3 0.010000      7   0.90576 1.0052 0.067745
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
#          donation_total + poverty_level, data = train)
# Variables actually used in tree construction:
#   [1] "donation_to_project"
# Number of terminal nodes:  3 
# Residual mean deviance:  0.7371 = 1103 / 1497 
# Misclassification error rate: 0.1273 = 191 / 1500 
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


# Model 3: Everything except donation and price
tree_model3 = tree(is_exciting ~.-donation_to_project -donation_optional_support -donation_total -total_price_excluding_optional_support -total_price_including_optional_support, data=training)
tree_model3
plot(tree_model3)
text(tree_model3, pretty = 0)
summary(tree_model3)
tree_predict3 = predict(tree_model3, testing)
summary(tree_predict3)
# f                t         
# Min.   :0.8369   Min.   :0.0000  
# 1st Qu.:0.8369   1st Qu.:0.1047  
# Median :0.8369   Median :0.1631  
# Mean   :0.8732   Mean   :0.1268  
# 3rd Qu.:0.8953   3rd Qu.:0.1631  
# Max.   :1.0000   Max.   :0.1631  

## Cross Validation
cv_tree3 = cv.tree(tree_model3)
plot(cv_tree3$size, cv_tree3$dev, type = "b")

# rplot
tree3 <- rpart(is_exciting ~.-donation_to_project -donation_optional_support -donation_total -total_price_excluding_optional_support -total_price_including_optional_support, data=training)
tree3
plotcp(tree3)
rpart.plot(tree3,extra = 1)

