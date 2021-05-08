
# Modeling - Random Forest

# Libraries
library(ROCR)
library(ranger)
library(tidyverse)
library(caret)
library(vip)
library(gridExtra)

# Source files
source('doe.preprocess.R')

# Re-save sourced data to modeling data
doe.modeling <- doe.all 

## Recode multi-level variables to binary, and change all binary variables to factor
doe.modeling <- doe.modeling %>% 
  mutate(bplace = as.factor(ifelse(bplace <= 88, 0, 1)), # domestic (0) vs. foreign (1)
         age.diff = as.factor(ifelse(age.diff == 0, 0, 1)), # entered freshman yr at age 14 (0) vs. at any other age (1)
         hlang = as.factor(ifelse(hlang == 'NO', 0, 1)), # home language is English (0) vs. not (1) 
         days.miss = as.factor(ifelse(mn.days.miss >= 10, 1, 0)),   ## these binaries are for the caret model
         num.sus = as.factor(ifelse(mn.num.sus > 0, 1, 0)),
         num.sch = as.factor(ifelse(num.schools > 1, 1, 0)),
         eth.bin = as.factor(ifelse(eth == 3 | eth == 4, 1, 0)),  # if ethnicity is black or hispanic (1)
         graduate = as.factor(graduate),
         gen = as.factor(gen),
         any.pov = as.factor(any.pov),
         any.shlt = as.factor(any.shlt),
         any.iep = as.factor(any.iep),
         any.ell = as.factor(any.ell),
         any.mvd = as.factor(any.mvd),
         any.repeats = as.factor(any.repeats),
         college = as.factor(college))

# Create training and test sets
doe.train <- doe.modeling %>% 
  filter(frsh < 2015) 

doe.test <- doe.modeling %>% 
  filter(frsh == 2015) 


## ===== Fit RF Model ======================================================================

# Fit random forest model - graduated high school (with impurity-based variable importance)
rf.mod.hs <- ranger::ranger(graduate ~ days.miss + num.sus + num.sch + hlang + bplace + gen + 
                              eth.bin + any.pov + any.shlt + any.iep + any.ell + age.diff + any.repeats, 
                            num.trees = 250, 
                            mtry = 2,
                            min.node.size = 10,
                            data = doe.train, 
                            respect.unordered.factors = T, 
                            probability = T, 
                            importance = "impurity") 

# Fit random forest model - graduated high school (with permutation-based variable importance)
rf.mod.hs.perm <- ranger::ranger(graduate ~ days.miss + num.sus + num.sch + hlang + bplace + gen + 
                                  eth.bin + any.pov + any.shlt + any.iep + any.ell + age.diff + any.repeats, 
                                num.trees = 250, 
                                mtry = 2,
                                min.node.size = 10,
                                data = doe.train, 
                                respect.unordered.factors = T, 
                                probability = T, 
                                importance = "permutation") 

# Tune model using `caret`
tgrid <- expand.grid(
  .mtry = 2:13,
  .splitrule = "gini",
  .min.node.size = c(5, 10)
)

caret.mod.hs <- train(graduate ~ days.miss + num.sus + num.sch + hlang + bplace + gen + 
                        eth.bin + any.pov + any.shlt + any.iep + any.ell + age.diff + any.repeats, 
                      data = doe.train,
                      method = "ranger",
                      tuneGrid = tgrid,
                      num.trees = 250,
                      importance = "permutation")

# Variable importance of model
sort(rf.mod.hs$variable.importance, decreasing = T) # impurity
sort(rf.mod.hs.perm$variable.importance, decreasing = T) # permutation


# Variable importance of tuning models (exploratory only)
varImp(caret.mod.hs)


# Predict tuned model on test set 
caret.pred <- predict(caret.mod.hs, doe.test)
# Confusion matrix of prediction from tuned model
confusionMatrix(caret.pred, doe.test$graduate)


# Out-of-bag (OOB) error for HS graduate model
sqrt(rf.mod.hs$prediction.error) # but could be overestimated


# Predict on test data - high school (IMPURITY)
doe.test$pred.prob.hs <- predict(rf.mod.hs, data = doe.test, type = 'response')$predictions[,2]

# AUC 
test.pred <- prediction(doe.test$pred.prob.hs, doe.test$graduate)
test.perf <- performance(test.pred, "auc")
test.perf@y.values[[1]]  # 0.7261382


# Predict on test data - high school (PERMUTATION)
doe.test$pred.prob.hs.p <- predict(rf.mod.hs.perm, data = doe.test, type = 'response')$predictions[,2]

# AUC 
test.pred.perm <- prediction(doe.test$pred.prob.hs.p, doe.test$graduate)
test.perf.perm <- performance(test.pred.perm, "auc")
test.perf.perm@y.values[[1]]  # 0.7257417


##================================================================================================

# Plot variable importance
p1 <- vip::vip(rf.mod.hs, num_features = 13, bar = TRUE) + # impurity
  ggtitle("Impurity-Based Variable Importance") +
  theme_bw()
p2 <- vip::vip(rf.mod.hs.perm, num_features = 13, bar = TRUE) + # permutation
  ggtitle("Permutation-Based Variable Importance") +
  theme_bw()

gridExtra::grid.arrange(p1, p2, nrow = 1)


################################################################################
################## College enrollment model - NOT USED FOR PAPER ###############
################################################################################
## NOTE: This model was used to predict whether students enrolled in college, and was 
## initially included in our analyses, but ultimately we decided to take it out because
## it was out of scope, and instead decided discuss future work that could focus on this topic.

# Fit random forest model - went to college
rf.mod.col <- ranger::ranger(college ~ days.miss + num.sus + num.sch + hlang + bplace + gen + 
                               eth.bin + any.pov + any.shlt + any.iep + any.ell + age.diff + any.repeats, 
                             num.trees = 250, 
                             mtry = 2,
                             min.node.size = 10,
                             data = doe.train, 
                             respect.unordered.factors = T, 
                             probability = T, 
                             importance = "impurity")

# Tune model using `caret`
tgrid.col <- expand.grid(
  .mtry = 2:13,
  .splitrule = "gini",
  .min.node.size = c(5, 10)
)

caret.mod.col <- train(college ~ days.miss + num.sus + num.sch + hlang + bplace + gen + 
                        eth.bin + any.pov + any.shlt + any.iep + any.ell + age.diff + any.repeats,
                      data = doe.train,
                      method = "ranger",
                      tuneGrid = tgrid.col,
                      num.trees = 250,
                      importance = "permutation")

# Variable importance of model
sort(rf.mod.col$variable.importance, decreasing = T) # highest one is the most relevant to the prediction


# Predict tuned model on test set 
caret.pred.col <- predict(caret.mod.col, doe.test)
# Confusion matrix of prediction from tuned model
confusionMatrix(caret.pred.col, doe.test$college)


# Out-of-bag (OOB) error for COLLEGE model
sqrt(rf.mod.col$prediction.error)

# Predict on test data - college
doe.test$pred.prob.col <- predict(rf.mod.col, data = doe.test, type = 'response')$predictions[,2]

# AUC 
test.pred.col <- prediction(doe.test$pred.prob.col, doe.test$college)
test.perf.col <- performance(test.pred.col, "auc")
test.perf.col@y.values[[1]]  # 0.7821873

