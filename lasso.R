# load libraries
library(tidyverse)
library(glmnet)
library(Matrix)
library(pROC)
library(caret)
library(plotmo)
library(MASS)
library(ROCR)
library(glmmLasso)
library(corrplot)

### 2. Student-Level Model --------------------------------------------------------

# organize data for the individual level models
doe.glm <- doe.simp %>%
  ungroup() %>%
  dplyr::select(-id, -final.sch, -comp.grades, - final.status, -grade, -year, -mod.sch, - mn.days.miss) %>%
  na.omit()

# change language and birthplace variables to binary, US or not, rather than a character list
doe.glm <- doe.glm %>%
  mutate(hlang = ifelse(hlang == "NO", 0, 1),
         bplace = ifelse(bplace <= 88, 0, 1))

# create training and testing subsets
glm.train <- doe.glm %>%
  filter(frsh == 2013 | frsh == 2014 ) %>%
  dplyr::select(-frsh)

glm.test <- doe.glm %>%
  filter(frsh == 2015) %>%
  dplyr::select(-frsh)

# save outcome to vector and delete
train.outcome <- glm.train$graduate
test.outcome <- glm.test$graduate

# eliminate outcome column in training and testing dataset
glm.train <- glm.train %>%
  dplyr::select(-graduate)

glm.test <- glm.test %>%
  dplyr::select(-graduate)

# run lasso model for individual variables, formatted as a matrix for the function
lasso <- glmnet(data.matrix(glm.train), as.matrix(train.outcome), alpha=1, family = "binomial")

# create lasso plot
lasso.plot <- plot_glmnet(lasso, xvar = "lambda", label=5)

# run cross-validation lasso model for individual variables, formatted as a matrix for 
# the function
cv.lasso <- cv.glmnet(data.matrix(glm.train), as.matrix(train.outcome), alpha=1, family="binomial")

# plot cross-validated lasso model
cv.lasso.plot <- plot(cv.lasso, xvar = "lambda", label=T)

# save default lambda value
lambda <- cv.lasso$lambda.1se 

# save coefficients
coefs <-  as.matrix(coef(cv.lasso)) 
pos.coef <- which(abs(coefs[,1]) > 0)
coefs[pos.coef, 1, drop=FALSE]

# save CV lasso probability that each student will graduate 
glm.test$cv.lass.pred <- predict(cv.lasso, newx = data.matrix(glm.test), type='response',  s = lambda)

# find AUC of LASSO model on student data from 2015 first-years
cv.pr <- prediction(glm.test$cv.lass.pred, test.outcome)
cv.perf <- performance(cv.pr, "auc")
cv.AUC <- cv.perf@y.values[[1]]

# plot predicted probabilities
glm.test <- glm.test %>%
  mutate(att = 1 - av.per.miss)

ggplot(data = glm.test, aes(y = as.numeric(cv.lass.pred), x = as.numeric(att), 
                       color = as.numeric(cv.lass.pred), shape = as.factor(test.outcome))) +
  geom_point() +
  labs(title ="LASSO Graduation Predictions for Individual NYC Homeless Students",
       subtitle = "includes those who started 9th grade in a public school between 2013-2015",
       y = "Predictive Probability of Graduation\nwith Eight Demographic Features",
       x = "Individual Student Attendance Rates",
       shape = "Who Actually Graduated?") +
  scale_shape_manual(values=c(4, 19),
                     breaks=c("0", "1"),
                     labels=c("Didn't Finish", "Graduated"))+
  scale_x_continuous(labels = scales::percent) +
  scale_color_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"), guide = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = "serif"), 
        axis.text = element_text(size=11), 
        axis.title = element_text(size=11, face="bold"), 
        legend.title = element_text(size=8, face="bold"),
        plot.title = element_text(size=12, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size=8, face = "italic", hjust = 0.5),
        panel.grid.minor = element_line(colour = "gray", linetype = 'solid', size = .2))

# save the plot
ggsave("lasso.pred.png", width = 8, height = 3)

### 2. Student and School-Level Model --------------------------------------------------------

# organize data for the individual and school level model
doe.sch.glm <- data.frame(doe.sch) %>%
  ungroup() %>%
  dplyr::select(-id, -final.sch, -comp.grades, -final.status, -grade, -year, 
                -mn.days.miss, -sch.name) %>%
  mutate(mod.sch = as.factor(mod.sch),
         sch.princ.yrs = as.numeric(sch.princ.yrs),
         sch.princ.yrs = scale(sch.princ.yrs),
         sch.ela = scale(sch.ela),
         sch.mth = scale(sch.mth),
         sch.pop = scale(sch.pop),
         sch.rig.instr = as.numeric(sch.rig.instr), 
         sch.coll.teach = as.numeric(sch.coll.teach),
         sch.support.env = as.numeric(sch.support.env),
         sch.eff.lead = as.numeric(sch.eff.lead),
         sch.fam.inv = as.numeric(sch.fam.inv),
         sch.trust = as.numeric(sch.trust), 
         sch.teach.3plus = as.numeric(sch.teach.3plus),
         sch.st.att = as.numeric(sch.st.att),
         sch.abs.18plus = as.numeric(sch.abs.18plus),
         sch.tch.att = as.numeric(sch.tch.att),
         sch.coll.3sem = as.numeric(sch.coll.3sem),
         sch.grad = as.numeric(sch.grad)) %>%
  na.omit()

# change hlang and bplace to binary variables
doe.sch.glm <- doe.sch.glm %>%
  mutate(hlang = ifelse(hlang == "NO", 0, 1),
         bplace = ifelse(bplace <= 88, 0, 1))

# create training and testing subsets for model with school-level variables
glm.sch.train <- doe.sch.glm %>%
  filter(frsh == 2013 | frsh == 2014 ) %>%
  dplyr::select(-frsh)

glm.sch.test <- doe.sch.glm %>%
  filter(frsh == 2015) %>%
  dplyr::select(-frsh)

test.sch.outcome <-  data.frame(glm.sch.test$graduate)

# eliminate outcome column in testing dataset with school-level variables
glm.sch.test <- glm.sch.test %>%
  dplyr::select(-graduate)

# run lasso model for school and individual variables, formatted as a matrix for the function
lasso.sch40 <- glmmLasso(fix = graduate ~ av.per.miss + mn.num.sus + num.schools + 
                         hlang + gen + eth + any.pov + any.shlt + any.iep + any.ell + any.mvd + 
                         age.diff + any.repeats + sch.pop + sch.rig.instr + sch.coll.teach + 
                         sch.support.env + sch.eff.lead + sch.fam.inv + sch.trust + sch.ela + 
                         sch.mth + sch.ell + sch.iep + sch.contained + sch.ENI + sch.tmp.hous + 
                         sch.HRA + sch.eth.aapi + sch.eth.bl + sch.eth.his + sch.eth.wh + 
                         sch.princ.yrs + sch.teach.3plus + sch.st.att + sch.abs.18plus + 
                         sch.tch.att + sch.coll.3sem + sch.grad, rnd = list(mod.sch=~1),
                      data = glm.sch.train, lambda = 40, family = binomial())

# save coefficients of the model with school-level variables
coef.sch <- coef(lasso.sch40)

# save school and student level lasso probability that each student will graduate 
glm.sch.test$cv.lass.40 <- predict(lasso.sch40, glm.sch.test)

# find AUC of LASSO model on student and school-level data from 2015 first-years
pr.40 <- prediction(glm.sch.test$cv.lass.40, test.sch.outcome)
perf.40 <- performance(pr.40, "auc")
AUC.40 <- perf.44@y.values[[1]]
