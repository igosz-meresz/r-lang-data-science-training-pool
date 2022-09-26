setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# --------------------- PROBLEM STATEMENT --------------------------------------

# Direct marketing either through mail, email or phone is a common tactic to
# acquire customers. Because targeting a customer requires resources and
# cost we want to target only those who are most likely to be interested
# in the offer. Targeting only the potential customers will protect 
# the brand image by not spamming customers who don't wish to be contacted.

# Identifying the potential customers based on demographic information
# and past beheaviours' is a valuable machine learning problem for most
# organizations.

# The goal is to predict if a given client will subscribe to a term deposit - 
# variable Y

# The data is related with direct marketing campaigns (phone calls) of
# a Portugese banking institution.


# First, I want to get an understanding of which variables are going to be useful
# in predicting the Y variable. The Y variable in this case is:
# -- Whether a customer has subscribed to a banking product or not

# --------------------- LOADING THE DATA AND LIBRARIES -------------------------

library(dplyr)
library(magrittr)
library(caret)
data <- read.csv('https://bit.ly/3f2y5Lg', sep = ';')
# https://raw.githubusercontent.com/selva86/datasets/master/bank-full.csv

# --------------------- GETTING TO KNOW THE DATA -------------------------------


# Inspect top 6 rows
head(data)

# Num unique values in each column
sapply(data, function(x){unique(x) %>% NROW()})


# Most variables are categorical, excep for age, duration, campaign, pdays, previous.
# We know from data description that emp.var.rate, cons.price.idx, cons.conf.idx,
# euribor3m, nr.employed are numeric though some contain few unique values


# Let's check distribution of the Y variable

# Check Freq Count of Y
table(data$y)
# no   yes 
# 36548  4640 

# Seems the distribution is largely negative. Most of the customers decided 
# not to subscribe.

table(data$y)/NROW(data$y)
# no       yes 
# 0.8873458 0.1126542 

# Nearly 90% of the data are non-responders. So, the data is quite imbalanced.


# Distribution of other variables
table(data$job) # most people are in 'admin', 'blue-collar', followed by 'technician'
table(data$marital) # married people dominate the dataset
table(data$education) # university degree and high school are the most common
table(data$default) # Has credit in default? Mostly no
table(data$housing) # Has housing loan? About 50% have
table(data$loan) # Has personal loan? About 70% don't have
table(data$contact) # Nearly 2/3rd were contacted on the phone
table(data$month) # last contact month of the year: May, July and Aug dominate
table(data$day_of_week) # last contact day of the week - evenly distributed
table(data$previous) # no. of contacts before this campaign: Majority have not been contacted
table(data$poutcome) # outcome of previous marketing. Mostly nonexistent.


# Next, I'll conduct statistical tests to determine which of the X variables
# are going to be useful in predicting the Y variable. Since Y variable
# has only two classes I can perform a Chi-Square Test to determine the 
# usefulness of the variable.

# --------------------- STATISTICAL TESTS --------------------------------------

# Chi-sq test: (for categorical vs categorical)
# Those values, where p-value < 0.05 are statistically significant and therefore,
# likely to be useful in predicting the Y itself.

options(scipen = 999)

chisq.test(y = data$y, x = data$job)
chisq.test(y = data$y, x = data$marital)
chisq.test(y = data$y, x = data$education)
chisq.test(y = data$y, x = data$default)
chisq.test(y = data$y, x = data$housing) # NOT SIGNIFICANT
chisq.test(y = data$y, x = data$loan) # NOT SIGNIFICANT
chisq.test(y = data$y, x = data$contact)
chisq.test(y = data$y, x = data$month)
chisq.test(y = data$y, x = data$day_of_week)
chisq.test(y = data$y, x = data$previous)
chisq.test(y = data$y, x = data$poutcome)


# ANOVA (for categorical vs numeric)
# Great for predicting the usefulness of continuous variables.
# Just like with the previous test, I'm looking for p-value of < 0.05

summary(aov(age ~ y, data = data)) # age of client
summary(aov(duration ~ y, data = data)) # last contact duration in secs
summary(aov(pdays ~ y, data = data)) # number of days passed since contacted
summary(aov(campaign ~ y, data = data)) # no. of contacts during this campaign
summary(aov(previous ~ y, data = data)) # no. of contacts prior to this campaign

# Each continuous variable is statistically significant.


# Box plots, Histograms and Density by categories
boxplot(age ~ y, data = data, main = 'Age ~ Y')
# 'no' category falls into 'yes' category - rather not a significant predictor
boxplot(duration ~ y, data = data, main = 'Duration ~ Y') # last contact duration in seconds
# there is a clear shift in the position of the boxes along the Y axis
# this means that the chance of 'duration' being a more useful predictor
# than 'age' is going to be slightly higher
boxplot(pdays ~ y, data = data, main = 'pdays ~ Y') # number of days passed since last contacted
# not vety informative
boxplot(campaign ~ y, data = data, main = 'campaigns ~ Y') # no. of contacts during this campaign
boxplot(previous ~ y, data = data, main = 'previous ~ Y') # no. of contacts prior to this campaign


# --------------------- FIXING DATATYPE AND CREATING TEST & TRAIN SETS ---------


# Filter 1500 rows at random
# Set Class of columns
numeric_columns <- c('age', 'duration', 'campaign', 'pdays', 'previous',
                     'emp.var.rate', 'cons.price.idx', 'cons.conf.idx',
                     'euribor3m', 'nr.employed')

# Converting numeric variables to numeric class
for(column in colnames(data)){
  feature_col <- data[, column]
  if(!column %in% numeric_columns){
    levels_cat <- sort(unique(feature_col))
    data[, column] <- factor(tolower(as.character(data[, column])),
                             levels = tolower(as.character(levels_cat)))
  }else{
    data[, column] <- as.numeric(data[, column])
  }
}

# Put 'no' as first level
data$y <- relevel(data$y, 'no')

# Check
str(data)

# Now, there are only factor and numeric variables


# Create the training and test datasets
set.seed(100)
trainRowNums <- createDataPartition(data$y, p = 0.8, list = FALSE)
df_train <- data[trainRowNums, ]
df_test <- data[-trainRowNums, ]

write.csv(df_train, 'bank_marketing_train.csv', row.names = F)
write.csv(df_test, 'bank_marketing_test.csv', row.names = F)


# --------------------- BUILDING LOGISTIC REGRESSION MODEL ---------------------

# I'm including all the X variables, apart from y from df_train dataset
logit_model <- glm(y ~ ., data = df_train, family = binomial('logit'))
summary(logit_model) #  AIC: 13836 -- the lower the AIC the better it is

# If you look at this output, some of the variables are not significant,
# for example, 'age'. Considering that the model, behind the scenes, broke up
# categories such as job or education into subcategories I can see, that some
# job factors and education factors are statistically significant, whereas some
# are not. However, since 'job' has at least one statistically significant 
# variable, the entire category of 'job' is statistically significant. 

# 'marital' as a whole is not significant, because none of the subcategories
# are statistically significant. 

# In the 'education' category one of the factors is significant, so the entire
# category is statistically significant. 

# 'Housing' - not significant

# 'loan' has NA missing value - suspicious

# 'contact' is significant

# in 'month' many values are significant, so is 'day_of_week'

# 'duration' and 'campaign' is significant, 'previous' is not

# 'cons.conf.idx' and 'nr.employed' is insignificant


# Let's build a model with only significant variables
formula = y ~ pdays + duration + campaign + job + education + contact +
  poutcome + emp.var.rate + cons.price.idx + euribor3m + day_of_week +
  month + default

logit_model <- glm(formula, data = df_train, family = binomial('logit'))
summary(logit_model) # AIC: 13823 -- lower than the previous model


# --------------------- MCFADDEN'S R-SQUARED -----------------------------------

# If you're predicting the Y variable which is a continuous, real variable
# then you can use the R-Squared to measure the goodness of fit of the model.
# In this case it's better however, to use the McFadden's Pseudo R-Squared.

null_dev <- logit_model$null.deviance # deviance of intercept onlly model (-logloss)
dev <- logit_model$deviance

1 - (dev/null_dev) # 0.4078843


# --------------------- CONFUSION MATRIX AND EVALUATION METRICS ----------------

# Once I've built the logistic regression model I need to check the model in
# a dataset that it has not yet seen. That will be the df_test for me.
# I need smart evaluation metrics to judge the performance of this model.

# Prediction
pred_prob <- predict(logit_model, df_test, type = 'response')
pred <- factor(ifelse(pred_prob > 0.5, 'yes', 'no'), levels = c('no', 'yes'))

# Compute the confusion matrix -------------------------------------------------
confusionMatrix(reference = df_test$y, data = pred,
                mode = 'everything', positive = 'yes')

#              Reference
# Prediction       no    yes
#              no  7126  522
#             yes  183  406

# rows have the actual or true values of the variable
# columns have the predicted values
# 7126 were predicted as no and were truly no
# 406 were predicted as yes and were truly yes


# Evaluation metrics -----------------------------------------------------------
# A number of meaningful evaluation metrics can be computed from the confusion matrix

# Sensitivity or Recall: The percentage of total events (1's) correctly predicted
# A / (A + C)
# 406 / (522 + 406)

# Specificity: The percentage of total non-events (0's) correctly predicted
# D / (B + D)
# 7126 / 7126 + 183

# Precision: Of all the predicted events what percentage was correct?
# A / (A + B)
# 406 / 183 + 406

# Precision and Recall are typically monitored as a pair. Combine it as F1 Score
# F1: (2 * precision * recall) / (precision + recall)

# Prevalence: The proportion of actual events in data. As the name suggests it
# reflects the general prevalence of the event
# (522 + 406) / 522 + 183 + 406 + 7126

# Detection Prevalence: The proportion of predicted events in data
# (406 + 183) / (522 + 183 + 406 + 183)

# Detection Rate: The proportion of true positive by the total number of samples
# 406 / total samples

# Balanced Accuracy = (sensitivity + specificity)/2

# Kappa = (observed accuracy - expected accuracy)(1 - expected accuracy)
# https://stats.stackexchange.com/questions/8216/cohens-kappa-in-plain-english


# --------------------- PRECISION RECALL CURVE ---------------------------------

# Precision Recall Chart
library(grid)
library(ROCR)
library(DMwR)
DMwR::PRcurve(pred_prob, df_test$y, main = 'Precision Recall Chart')

DMwR::CRchart(pred_prob, df_test$y, main = 'Cumulative Recall Chart')

# there is a plot.performance error here, dunno how to fix it yet.


# --------------------- ROC CURVE ----------------------------------------------
# install.packages('ROCit')
library(ROCit)
ROCit_obj <- rocit(score = pred_prob, class = df_test$y)
plot(ROCit_obj)

# ROC score
ROCit_obj$AUC # 0.9408419

# The more area under the ROC curve the better the model. 

# What is the optimal cutoff score?
# The optimal cutoff score is the point where the Youden Index is the highest.
# Youden Index = sensiticity + specificity - 1


# --------------------- KS STATISTIC AND GAIN CURVE ----------------------------

# The KS Statistic and the Gain Curve are very useful for Marketing Campaigns
# the Buyer Propensity Model. From the business perspective it helps
# the marketing teams to decide on choosing the best population to target
# in order to get the best returns. 

# Gain Table and KS-Stat -------------------------------------------------------
# install.packages('InformationValue')
library(ggplot2)
library(InformationValue)
require(ggplot2)
InformationValue:::ks_table(df_test$y, pred_prob)
InformationValue::ks_stat(df_test$y, pred_prob)

# KS Statistic is the maximum distance between the responders gain and
# the non-responders gain for proportion of population targeted
# (along the X axis). In order to compute the statistic, the Gain chart
# should have cumulative capture rate of both responders (1's) as well as
# non-responders (0's). Very useful in marketing campaigns, buyer propensity models.

# Advantages:
# 1. Spend less on marketing expense by focusing only on the most likely respondents.
# 2. You will not bother those who are unlikely to respond. They might consider
#    your campaign as spam, which could form a negative brand image.


# Gains Chart with KS Plot Function --------------------------------------------
ks_table <- InformationValue:::ks_table
ks_plot <- function (actuals, predictedScores) {
  rank <- 0:10
  ks_table_out <- ks_table(actuals = actuals, predictedScores = predictedScores)
  perc_events <- c(0, ks_table_out$cum_perc_responders) * 100
  perc_nonevents <- c(0, ks_table_out$cum_perc_non_responders) * 100
  random_prediction <- seq(0, 100, 10)
  df <- data.frame(rank, random_prediction, perc_events, perc_nonevents)
  df_stack <- stack(df, c(random_prediction, perc_events, perc_nonevents))
  df_stack$rank <- rep(rank, 3)
  df_stack$delta <- df_stack$values[12:22] - df_stack$values[1:11]
  values <- df_stack$values
  ind <- df_stack$ind
  
  rowmax <- which.max(ks_table_out$difference)
  l_start <- ks_table_out[rowmax, "cum_perc_non_responders"]
  l_end <- ks_table_out[rowmax, "cum_perc_responders"]
  
  print(ggplot2::ggplot(df_stack, aes(x = rank, y = values, 
                                      colour = ind, label = paste0(round(values, 2), "%"))) + 
          geom_line(size = 1.00) + 
          labs(x = "rank", y = "Percentage events (1's) & non-events (0's) Captured", 
               title = "KS Plot", subtitle=paste("KS Statistic: ", InformationValue::ks_stat(actuals, predictedScores))) + 
          theme(plot.title = element_text(size = 20, 
                                          face = "bold")) + 
          geom_text(aes(y = values + 4), size=2) + 
          scale_x_continuous(breaks=0:10, labels=0:10) + 
          geom_segment(x = rowmax, y = l_start*100, xend = rowmax, yend = l_end*100, 
                       col="red", arrow = arrow(length = unit(0.05, "npc"), 
                                                ends="both"), linetype = "dashed", lwd=.5)) + 
    scale_color_discrete(name="")  + theme(legend.justification=c(.99,0.01), legend.position=c(.99,0.01))
}

ks_plot(df_test$y, pred_prob)


# green line is the cum % of responders
# blue line is the cum % of non-responders
# red line in the middle is the random model
# so, if we target 30% random people we get ~ 27% of responders
# however, if we target 30% of people from the model (green line) we get 96% responders


# --------------------- CONCORDANT AND DISCORDANT PAIRS ------------------------
# Predicted probability score of a positive case should be higher than
# predicted probability score of a negative case. That is what is called a 
# concordant pair. If it is otherwise, that pair becomes discordant.

# library(InformationValue)
preds <- predict(logit_model, df_test)
actuals <- as.numeric(df_test$y) - 1
out <- Concordance(actuals, preds)

out$Concordance # 94%
out$Discordance # 6%

# For a good binary classification model, the probability score of events (1's)
# should be greater than the probability score of non-events (0's). This, however,
# may not always be the case, even when a high accuracy is attained. We use
# this concept to judge the goodness of the classification model.


# --------------------- HANDLING CLASS IMBALANCE -------------------------------

# Cost Sensitive Learning ------------------------------------------------------
# library(dplyr)

# The logit model has been built
formula = y ~ pdays + duration + campaign + job + education + contact + poutcome +
  emp.var.rate + cons.price.idx + euribor3m + day_of_week + month + default

logit_model <- glm(formula, data = df_train, family = binomial('logit'))

# McFadden's Pseudo R-Squared
null_dev <- logit_model$null.deviance
dev <- logit_model$deviance
(null_dev - dev)/null_dev
# the model has explained 40.8% of variance in Y

# Predict
pred <- predict(logit_model, df_test, type = 'response')
y_pred <- factor(ifelse(pred > 0.5, 'yes', 'no'), levels = c('no', 'yes'))

# Capture Rates
actuals <- ifelse(df_test$y == 'yes', 1, 0)
df_probs <- data.frame(Act = actuals,
                       Prob = pred,
                       Pred_Decile = cut(pred,
                                         breaks = quantile(pred, probs = seq(0, 1, .1), na.rm = T),
                                         include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred * 10))

out <- df_probs %>% group_by(Pred_Decile) %>%
  summarise(n_obs = length(Act), n_pos = sum(Act)) %>%
  arrange(desc(Pred_Decile)) %>%
  mutate(caputre_rate = round(cumsum(n_pos)/sum(n_pos), 3) * 100)

out

# First row from the right represents the top 10 %tile predicted probability scores.
# Maximum numbers of 'yes' responders are in the first bucket (row)
# By targeting the 520 of 824 marketing team will be able to capture 56% of 'yes' responders.
# By targeting the first 30%tile the business will be able to capture 96% of 'yes's


caret::confusionMatrix(reference = df_test$y, data = y_pred, mode = 'everything', positive = 'yes')
# Sensitivity: 43.75%
# F1: 54.52


# Cost Sensitive Learning ------------------------------------------------------
# Build logit model
table(df_train$y)/NROW(df_train$y)
wts_map <- as.numeric(table(df_train$y)/NROW(df_train$y))
wts <- ifelse(df_train$y == 'yes', wts_map[1], wts_map[2])

logit_model <- glm(formula, weights = wts, data = df_train, family = binomial('logit'))
summary(logit_model)


# McFadden's Pseudo R-Square
null_dev <- logit_model$null.deviance
dev <- logit_model$deviance
(null_dev - dev)/null_dev
# 52.85%
# The accuracy has improved by 10%

# Predict
pred <- predict(logit_model, df_test, type = 'response')
y_pred <- factor(ifelse(pred > 0.5, 'yes', 'no'), levels = c('no', 'yes'))

# Capture Rates
actuals <- ifelse(df_test$y == 'yes', 1, 0)
df_probs <- data.frame(Act = actuals,
                       Prob = pred,
                       Pred_Decile = cut(pred,
                                         breaks = quantile(pred, probs = seq(0, 1, .1), na.rm = T),
                                         include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred * 10))

out <- df_probs %>% group_by(Pred_Decile) %>%
  summarise(n_obs = length(Act), n_pos = sum(Act)) %>%
  arrange(desc(Pred_Decile)) %>%
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3) * 100)

out
# Previously, the regular logit_model was able to capture 56% exactly.
# Now, we're able to capture 56.8%. There is a drop in the second bucket
# but we make up for it in the third bucket. There is a 1% increase in the 
# number of positive cases, or responders the model is able to capture
# by targeting the fist 30%tage of the values. 

# Compute the sensitive F1 score
caret::confusionMatrix(reference = df_test$y, data = y_pred, mode = 'everything',
                       positive = 'yes')
# Sensitivity: 2x increase - this is the true power of Cost Sensitive model.
# Increase from 43% to 88.57%. It is now able to correctly assign 88.57% of
# responders. 
# F1: .5973 - 6% increase.


# Oversampling -----------------------------------------------------------------

# Freq Distribution
table(df_train$y)

# There's a significant class imbalance here

# Oversample the minority class
# Picking a larger number of minority class at random so that it matches
# the number of observations. The minority class is in this case the
# non-evens. Splitting a random number of evens and non-evens into two
# datasets
df_y <- df_train[df_train$y == 'yes', ]
df_n <- df_train[df_train$y != 'yes', ]


set.seed(100)
df_y <- df_y[sample(1:NROW(df_y), size = NROW(df_n), replace = T), ]
df_train <- rbind(df_y, df_n)
table(df_train$y)
# now, both yes and no are of equal proportions.


# Build Logit Model
formula = y ~ pdays + duration + campaign + job + education + contact +
  poutcome + emp.var.rate + cons.price.idx + euribor3m + day_of_week +
  month + default

logit_model <- glm(formula, data = df_train, family = binomial('logit'))
summary(logit_model)


# McFadden's Pseudo R-square
null_dev <- logit_model$null.deviance
dev <- logit_model$deviance
(null_dev - dev)/null_dev # 0.5287241

# the original McFadden's Pseudo R-Squared was around 43%. We have almost 10%
# increase.


# Predict
pred <- predict(logit_model, df_test, type = 'response')
y_pred <- factor(ifelse(pred > 0.5, 'yes', 'no'), levels = c('no', 'yes'))


# Deciles Table
actuals <- ifelse(df_test$y == 'yes', 1, 0)

df_probs <- data.frame(Act = actuals,
                       Prob = pred,
                       Pred_Decile = cut(pred,
                                         breaks = quantile(pred, probs = seq(0, 1, .1), na.rm = T),
                                         include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred * 10))

out <- df_probs %>% group_by(Pred_Decile) %>%
  summarise(n_obs = length(Act), n_pos = sum(Act)) %>%
  arrange(desc(Pred_Decile)) %>%
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3) * 100)

out

# FROM ORIGINAL MODEL
# Pred_Decile n_obs n_pos caputre_rate
# <fct>         <int>   <dbl>       <dbl>
#   1 10          824   520         56  
# 2 9             824   271         85.2
# 3 8             823   101         96.1
# 4 7             824    23         98.6
# 5 6             823     6         99.2
# 6 5             824     4         99.7
# 7 4             824     2         99.9
# 8 3             823     0         99.9
# 9 2             824     0         99.9
# 10 1            824     1         100 

# -----------------------------------------

# FROM NEW MODEL
# Pred_Decile n_obs n_pos capture_rate
# <fct>         <int>   <dbl>       <dbl>
#   1 10          824   527         56.8 <-- increase by 0.8%
# 2 9             824   267         85.6 <-- increase by 0.4%
# 3 8             823   109         97.3 <-- increase by 1.2%
# 4 7             824    14         98.8
# 5 6             823     6         99.5
# 6 5             824     2         99.7
# 7 4             824     2         99.9
# 8 3             823     0         99.9
# 9 2             824     1         100  
# 10 1            824     0         100 


caret::confusionMatrix(reference = df_test$y, data = y_pred, mode = 'everything',
                       positive = 'yes')

# Sensitivity : 0.88470 <-- improved 2x
# F1 : 0.59687 <-- siginificant improvement as well

# FROM ORIGINAL MODEL
# ------------------------
# Sensitivity : 0.43750              
# Specificity : 0.97496              
# Pos Pred Value : 0.68930              
# Neg Pred Value : 0.93175              
# Precision : 0.68930              
# Recall : 0.43750              
# F1 : 0.53527              
# Prevalence : 0.11266              
# Detection Rate : 0.04929              
# Detection Prevalence : 0.07151              
# Balanced Accuracy : 0.70623  


# Hybrid Sampling with SMOTE ---------------------------------------------------
# library(DMwR)

# What SMOTE does is that it creates new samples of minority class that
# are very similar to the data that is already present in the training set.
# It also undersamples the majority class so that the dataset has a good balance
# of the majority and minority class. 
# Perc.over and perc.under how much over and undersampling has to be done.
library(DMwR)
df_train_new <- DMwR::SMOTE(y ~ . ,data = df_train, perc.over = 400, perc.under = 100)
table(df_train_new$y)

# Why is this command getting stuck in the console?


# Build Logit Model
formula = y ~ pdays + duration + campaign + job + education + contact +
  poutcome + emp.var.rate + cons.price.idx + euribor3m + day_of_week +
  month + default

logit_model <- glm(formula, data = df_train_new, family = binomial('logit'))
summary(logit_model)


# McFadden's Pseudo R-Square
null_dev <- logit_model$null.deviance
dev <- logit_model$deviance
(null_dev - dev)/null_dev


# Predict
pred <- predict(logit_model, df_test, type = 'response')
y_pred <- factor(ifelse(pred > 0.5, 'yes', 'no'), levels = c('no', 'yes'))


# Caputre Rate
actuals <- ifelse(df_test$y == 'yes', 1, 0)

df_probs <- data.frame(Act = actuals,
                       Prob = pred,
                       Pred_Decile = cut(pred,
                                         breaks = quantile(pred, probs = seq(0, 1, .1), na.rm = T),
                                         include.lowest = T, labels = 1:10),
                       Natural_Decile = ceiling(pred * 10))

out <- df_probs %>% group_by(Pred_Decile) %>%
  summarise(n_obs = length(Act), n_pos = sum(Act)) %>%
  arrange(desc(Pred_Decile)) %>%
  mutate(capture_rate = round(cumsum(n_pos)/sum(n_pos), 3) * 100)

out


caret::confusionMatrix(reference = df_test$y, data = y_pred, mode = 'everything',
                       positive = 'yes')

InformationValue::Concordance(as.numeric(df_test$y)-1, pred)
