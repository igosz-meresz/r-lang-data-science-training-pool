setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# --------------------- REGRESSION MODEL WORKFLOW ------------------------------
# 1. Data Preparation
#       - missing value and outlier treatment
# --------------------- 
# 2. Exploratory Data Analysis
#       - graphical analysis
#       - statistical tests
# --------------------- 
# 3. Training, Validation & Testing Split
# --------------------- 
# 4. Model Building
# --------------------- 
# 5. Variable Transformations and Interactions
# --------------------- 
# 6. Evaluating Model Performance
# --------------------- 
# 7. Variance Inflation Factor
# --------------------- 
# 8. Heteroscedasticity
# --------------------- 
# 9. Influential Points
# --------------------- 
# 10. Model Search Approaches
# --------------------- 

# install.packages(c('mice', 'DMwR', 'Hmisc'))

library(Hmisc)
options(scipen = 999) 
# ^ enable any decimal points will be shown as decimal points 
#   instead of scientific notation


# -------------- READ DATA -----------------------------------------------------
df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')
df_orig <- df # backup the original version
View(df)

# California Standardized Texting and Reporting (STAR) dataset
# contains data on test performance, school charachteristics
# and student demographic backgrounds.
# Source: California Department of Education (www.cde.ca.gov)

# DATA DESCRIPTION
# DIST_CODE :    DISTRICT CODE;
# READ_SCR  :    AVG READING  SCORE;
# MATH_SCR  :    AVG MATH SCORE;
# COUNTY    :    COUNTY;
# DISTRICT  :    DISTRICT;
# GR_SPAN   :    GRADE SPAN OF DISTRICT;
# ENRL_TOT  :    TOTAL ENROLLMENT;
# TEACHERS  :    NUMBER OF TEACHERS;
# COMPUTER  :    NUMBER OF COMPUTERS;
# TESTSCR   :    AVG TEST SCORE (= (READ_SCR+MATH_SCR)/2);
# COMP_STU  :    COMPUTERS PER STUDENT (= COMPUTER/ENRL_TOT);
# EXPN_STU  :    EXPENTITURES PER STUDENT ($);
# STR       :    STUDENT TEACHER RATIO (ENRL_TOT/TEACHERS);
# EL_PCT    :    PERCENT OF ENGLISH LEARNERS;
# MEAL_PCT  :    PERCENT QUALIFYING FOR REDUCED-PRICE LUNCH;
# CALW_PCT  :    PERCENT QUALIFYING FOR CALWORKS;
# AVGINC    :    DISTRICT AVERAGE INCOME (IN $1000'S);


# -------------- GROUP VARS ----------------------------------------------------
numeric_vars <- c('enrl_tot', 'calw_pct', 'teachers', 'meal_pct', 'computer',
                  'comp_stu', 'expn_stu', 'str', 'avginc', 'el_pct')

category_vars <- c('county', 'gr_span')

y_var <- 'testscr' # this is the variable I'm going to predict
 
drop_vars <- c('observation_number', 'dist_cod', 'district') 
# ^ not useful variables


# -------------- INPUTTING MISSING VALUES (for learning purpose) ---------------
set.seed(100)
n_missing <- 15
for(i in 1:n_missing){
  df[sample(1:NROW(df))[1], sample(c('enrl_tot', 'calw_pct', 'teachers'))[1]] <- NA
}
df[, numeric_vars]


# -------------- IMPUTE (replace) MISSING VALUES -------------------------------
# Approach 1: Hmisc: Mean, Median Imputation
# library(Hmisc)
impute(df$enrl_tot, mean) # replace with mean
impute(df$calw_pct, mean)
impute(df$teachers, mean)


# Median imputation
impute(df$enrl_tot, median) # replace with median
impute(df$calw_pct, median)
impute(df$teachers, median)

# of course, you to actually replace data in the df you need to assign it:
# df$teachers <- impute(df$teachers, median) etc.

# to run following commands:
# install.packages(c("zoo","xts","quantmod", "abind")) 
# those packages fix dependency issues on the next comman
# install.packages("C:\\Users\\igorr\\Documents\\R\\win-library\\4.1\\DMwR_0.4.1.tar.gz", repos = NULL, type = "source")
# to fix the issue of deprecated package I had to manually install the file
# in the directory, downloadedfrom DMwR archive in CRAN.
library(zoo)
library(xts)
library(quantmod)
library(abind)
library(DMwR)


# Evaluate replacement ---------------------------------------------------------

actuals <- df_orig$enrl_tot[is.na(df$enrl_tot)]
predicteds <- rep(mean(df$enrl_tot, na.rm = T), length(actuals))
regr.eval(actuals, predicteds)


actuals <- df_orig$calw_pct[is.na(df$calw_pct)]
predicteds <- rep(mean(df$calw_pct, na.rm = T), length(actuals))
regr.eval(actuals, predicteds)


actuals <- df_orig$teachers[is.na(df$teachers)]
predicteds <- rep(mean(df$teachers, na.rm = T), length(actuals))
regr.eval(actuals, predicteds)


# Approach 2: mice
library(mice)

# perform mice imputation, based on random forests
miceMod <- mice(df[, !names(df) %in% 'testscr'], method = 'rf') # exclude testscr column
df_out <- complete(miceMod) # generate the completed data

# Evaluate mice
actuals <- df_orig$teachers[is.na(df$teachers)]
predicteds <- df_out$teachers[is.na(df$teachers)]
regr.eval(actuals, predicteds)


actuals <- df_orig$calw_pct[is.na(df$calw_pct)]
predicteds <- df_out$calw_pct[is.na(df$calw_pct)]
regr.eval(actuals, predicteds)


actuals <- df_orig$enrl_tot[is.na(df$enrl_tot)]
predicteds <- df_out$enrl_tot[is.na(df$enrl_tot)]
regr.eval(actuals, predicteds)


# -------------- OUTLIER ANALYSIS ----------------------------------------------
# The boxplot will reveal outliers as dots outside the whiskers
boxplot(df_orig$teachers)
boxplot.stats(df_orig$teachers)

boxplot(df_orig$enrl_tot)
boxplot.stats(df_orig$enrl_tot)

boxplot(df_orig$calw_pct)
boxplot.stats(df_orig$calw_pct)


# Mini-challenge
# From caschool dataset, find the outliers for 'avginc' as per the boxplot.
# What are the cutoff values for the outliers?

df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')


# Solution:
boxplot.stats(df$avginc)

# cutoffs
boxplot.stats(df$avginc)$stats[c(1,5)]

# outliers
boxplot.stats(df$avginc)$out


### ------------------- GRAPHICAL AND STATISCIAL ANALYSIS -------------------###

library(dplyr)
library(magrittr)
library(Hmisc)
options(scipen = 999)

# -------------- READ DATA -----------------------------------------------------
df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')
df_orig <- df # backup the original version
View(df)


# -------------- GROUP VARS ----------------------------------------------------
numeric_vars <- c('enrl_tot', 'calw_pct', 'teachers', 'meal_pct', 'computer',
                  'comp_stu', 'expn_stu', 'str', 'avginc', 'el_pct')

category_vars <- c('county', 'gr_span')

y_var <- 'testscr' # this is the variable I'm going to predict

drop_vars <- c('observation_number', 'dist_cod', 'district') 
# ^ not useful variables

# Strucutre of data ------------------------------------------------------------
# First step is to conduct an uni variate analysis of various columns in df

Hmisc::describe(df)
# For continuous variables, the number of distinct values should be close
# to the total observations.
# Note the number of distinct values. Small number of distinct values may be 
# an indication that it is a categorical variable.
# Notice the Range of values.

summary(df)


# Pair plots
library(psych)
pairs.panels(df[, c(y_var, numeric_vars)],
             method = 'pearson', # correlation method
             hist.col = '#00AFBB',
             density = TRUE, # show density plots
             ellipses = TRUE) # show correlation ellipses

# corelations are on the top
# I can see that teachers, computers or enrl_tot has weak correlation with
# testscr - the object of my prediction. Doens't necessary mean they are 
# not important.
# Each steeply increasing or decreasing scatterplots are interesting. 
# In this case, meal_pct, avginc, el_pct are of interess or comp_stu
# have the potential to explain the Y (testscr). 



# Graphical Analysis -----------------------------------------------------------
par(dev.off()) # reset plot device


# Histogram and density
df$testscr %>% hist(prob = T, main = 'Test Score') %$%
  lines(density(df$testscr))
# Inference: Bell shaped curve
# This is probably a normally distributed value. 


df$enrl_tot %>% hist(prob = T, main = 'Total Enrollment') %$%
  lines(density(df$enrl_tot))
# Inference: Skewed to the left

df$avginc %>% hist(prob = T, main = 'Average Income ($1000s') %$%
  lines(density(df$avginc))
# Inference: Skewed to the left

df$str %>% hist(prob = T, main = 'Student Teacher Ratio') %$%
  lines(density(df$str))
# Inference: Bell shaped curve

df$el_pct %>% hist(prob = T, main = 'Percentage of English Learners') %$%
  lines(density(df$el_pct))
# Skewed to the left

# -- ! Do similar analysis for remaingin variables


# Scatter plot -----------------------------------------------------------------
# Correlation test
correlation_test <- function(x = NULL, y = df$score){
  ct <- cor.test(y = y, x = x)
  return(round(c(pvalue = ct$p.value, ct$estimate), 4))
}

# 1. AVGINC
plot(df$avginc, df$testscr,
     main = paste('Scatterplot of Score ~ Avg Inc \n Correlation: ',
                  round(cor(df$avginc, df$testscr), 2)),
     xlab = 'Average Income of District',
     ylab = 'Score', pch = 16)
abline(lm(testscr ~ avginc, data = df), col = 'red')


# COR.TEST
correlation_test(df$avginc, df$testscr)
# Strong +ve correlation. Growth rate decreases with higher values


# LOG of AVGINC
plot(log(df$avginc), df$testscr,
     main = paste('Scatterplot of Score ~ Avg Inc \n Correlation: ',
                  round(cor(log(df$avginc), df$testscr), 2)),
     xlab = 'Average Income of District',
     ylab = 'Score', pch = 16)
abline(lm(testscr ~ log(avginc), data = df), col = 'red')

# COR.TEST
correlation_test(log(df$avginc), df$testscr)
# Strong +ve Correlation. Significant. 

# -- ! Do similar analysis for remaining variables


# Correlation test on all columns at once
sapply(df[, numeric_vars], correlation_test)



# Interaction Plots ------------------------------------------------------------
plot(df$avginc, df$testscr,
     main = paste('Scatterplot of Score ~ Average Income \n Correlation:',
                  round(cor(df$avginc, df$testscr), 2)),
     xlab = 'Average Income',
     ylab = 'Score', pch = 16,
     col = colors()[as.numeric(factor(df$county)) + 20])

legend(50, 700, legend = unique(factor(df$county)),
       col = colors()[as.numeric(factor(df$county))], lty = 1, cex = 0.5,
       title = 'Line types', text.font = 4, bg = 'lightblue')

abline(lm(testscr ~ avginc, data = df), col = 'red')

# There is a clear difference in how the test score is distributed and how it
# varies with average income.


# Box Plot ---------------------------------------------------------------------
boxplot(testscr ~ gr_span, data = df, main = 'Test Score ~ Gr Span')


# ANOVA (Analysis of Variance)
# This test checks whether there are statistically significat differences
# between three or more samples. 
anova_grspan <- aov(testscr ~ gr_span, data = df)
summary(anova_grspan) # significant


# Mini-challenge
# 1. Does County have a statistically significant relationship with Test Score?
# 2. How about number of computers (`computer`)?

# Solution:
# 1.
# County
boxplot(testscr ~ county, data = df, main = 'Test Score ~ County')
anova_county <- aov(testscr ~ county, data = df)
summary(anova_county) # significant

# 2.
# Computers
boxplot(testscr ~ computer, data = df, main = 'Test Score ~ Computers')
anova_computers <- aov(testscr ~ computer, data = df)
summary(anova_computers) # significant



### ------------------- BUILDING LINEAR REGRESSION MODEL --------------------###

# df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')
# library(DMwR)
# options(scipen = 999)
# 
# numeric_vars <- c('enrl_tot', 'calw_pct', 'teachers', 'meal_pct', 'computer',
#                   'comp_stu', 'expn_stu', 'str', 'avginc', 'el_pct')
# 
# category_vars <- c('county', 'gr_span')
# 
# y_var <- 'testscr' # this is the variable I'm going to predict
# 
# drop_vars <- c('observation_number', 'dist_cod', 'district') 
# # ^ not useful variables



# # --- SIDENOTE: --------------------------------------------------------------
# # Before I build a linear regression model I have to split into training and
# # test dataset. It is because the model is built on the training data.
# # The model only sees the data that is represented in df_train. Once the model
# # is built the parameters used to predict the Y variable (testscr) in the test
# # data. Once the Y variable is predicted I'll also know the actual value of the
# # Y variable in the test data. These two are compared to know how well
# # the model has learned the relationship between the X and the Y in the 
# # training data.This is a way of validating the model and checking if it 
# # performs as good as in the training data. Ideally, the amount of error I get
# # in the training and the test data should be close to each other.
# 
# # Common ratios that are used to split are:
# # 70:30
# # 80:20
# # 75:25

# Train Test Split -------------------------------------------------------------
# I'm splitting in the ratio of 75:25
set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]

# check no. of rows in both datasets
NROW(df_train)
NROW(df_test)

# Simple Linear Regression -----------------------------------------------------
linear_model <- lm(testscr ~ meal_pct, data = df_train)
summary(linear_model)

#
# https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
#

# Check the sign of the coeffs, p-value
cor(df[, c(y_var, numeric_vars)]) # the signs match, they're negative
# # What this means is, as the number of students who're eligible for subsidized
# # lunch increases, the test score decreases.

# Next step is to predict on the test data using the model that I've just built.
preds <- predict(linear_model, df_test)
actuals <- df_test$testscr

# The numbers are quite close to the actuals. 

# Evaluate error metrics
DMwR::regr.eval(actuals, preds)


# Let's improve the model
# Multiple Linear Regression ---------------------------------------------------
# I'm including one more variable, that is student-teacher ratio into the formula.

linear_model <- lm(testscr ~ str + meal_pct, data = df_train)
summary(linear_model)

# str also has a negative coefficient, meaning as there are more students per
# teacher the test score also drops. Both variables are statistically significant.

# What if I build a model that contains all the variables in the dataset?
linear_model <- lm(testscr ~ enrl_tot + calw_pct + teachers + meal_pct + computer +
                     comp_stu + expn_stu + str + avginc + el_pct + county +
                     gr_span, data = df_train)
summary(linear_model)

# # From the exploratory analysis I know that many of the variables came out to be
# # statistically significant because they had high correlation and the respective
# # correlation test (ANOVA test) gave statistically significant relationships.
# # However, when I build the model including all variables that may not be
# # the case. Only some variables come out to be statistically significant,
# # for example avginc, el_pct.

# Let's instead build a model that uses only statistically significant
# variables:
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc + el_pct +
                   gr_span, data = df_train)
summary(linear_model)

# Tips to find good models:
# Check variables that came out to be useful and statistically significant
# in the exploratory analysis. Avoid Variables where signs did not match,
# whose p-values were too high.


### ------------------- A GOOD MODEL ----------------------------------------###
# The process of finding a good model is not quite a trial-and-error.

# Start with a full model and progressively remove variables that have 
# incorrect sign and very high p-values. I also want to have variables
# that are easy to explain and intuitive for the business to understand.

# The variables that are insignificant in one model can turn out to be
# statistically significant in the presence of other variables.
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + enrl_tot +
                     computer + el_pct + gr_span, data = df_train)
summary(linear_model)
# in this model comp_stu, computer and enrl_tot are not statistically significant

# replace enrl_tot and computer with avginc
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc
                   + el_pct + gr_span, data = df_train)
summary(linear_model)
# in this model comp_stu is significant. 

# Predict
preds <- predict(linear_model, df_test)
actuals <- df_test$testscr

# Evaluate
DMwR::regr.eval(actuals, preds)


### ------------------- CROSS VALIDATION APPROACHES -------------------------###

# Build Model
form <- formula(testscr ~ meal_pct + comp_stu + expn_stu + avginc +
                  el_pct + gr_span)
linear_model <- lm(form, data = df_train)
summary(linear_model)

# K Fold Cross Validation
# install.packages('DAAG')
library(DAAG)

# CVlm builds the linear regression model and validates it by itself.
cvResults <- suppressWarnings(CVlm(data = df_train,
                                   form.lm = form,
                                   m = 5,
                                   dots = FALSE,
                                   seed = 29,
                                   legend.pos = 'topleft',
                                   printit = FALSE,
                                   main = 'Small symbols are predicted values while bigger ones are actuals'));

# the lines in this model are on top of each other so it's a highly stable model.

# Mean Squared Error
attr(cvResults, 'ms') # 67.87967


# Leave On Out Cross Validation
# install.packages('caret')
library(caret)

train.control <- trainControl(method = 'LOOCV')

# Train the model
model <- train(form, data = df_train, method = 'lm', trControl = train.control)

# Summarize the results
print(model)
summary(model)


# Repeated K-Fold Cross Validation
train.control <- trainControl(method = 'repeatedcv', repeats = 5)

# Train the model
model <- train(form, data = df_train, method = 'lm', trControl = train.control)

# Summarize the results
print(model)
summary(model)


### ------------------- VARIABLE TRANSFORMATIONS AND INTERACTIONS -----------###

# Train Test Split -------------------------------------------------------------
set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]

# Simple Linear Regression -----------------------------------------------------
linear_model <- lm(testscr ~ avginc, data = df_train)
summary(linear_model) # Adjusted R-squared:  0.4953 

# Is it possible to improve this model?
# Log of X
linear_model2 <- lm(testscr ~ log(avginc), data = df_train)
summary(linear_model2) # Adjusted R-squared:  0.5498 - that's better

# Square Root of X
linear_model2 <- lm(testscr ~ I(avginc^0.5), data = df_train)
summary(linear_model2) # Adjusted R-squared:  0.5341 - worse than log of X

# Squared of X
linear_model2 <- lm(testscr ~ I(avginc^2), data = df_train)
summary(linear_model2) # Adjusted R-squared:  0.3766 - that's much worse


# How did I find what can improve the model?
# If you look at plot of log of variables from 1 to 1000 it is increasing 
# rapidly initially and as the values of X variable increase the log
# starts to flatten out more.
plot(log(1:1000), main = 'Log of 1 to 1000')

# This is very similar to shape of avginc
plot(df_train$avginc, df_train$testscr, main = 'Avginc')

# As average income increases the rate of increase on testscr decelerates
# after a certain point. 

# This gives a clue that log would be appropriate transformation for this 
# variable. 


# How to know what variable transformation to make? ----------------------------
par(dev.off())
plot(df$avginc, df$testscr,
     main = paste('Scataterplot of Score ~ Avg Inc \n Correlation: ',
                  round(cor(df$avginc, df$testscr), 2)),
     xlab = 'Average Income of District', ylab = 'Score', pch = 16)
abline(lm(testscr ~ avginc, data = df), col = 'red')

# LOG of AVGINC
plot(log(df$avginc), df$testscr,
     main = paste('Scataterplot of Score ~ Avg Inc \n Correlation: ',
                  round(cor(log(df$avginc), df$testscr), 2)),
     xlab = 'Average Inome of District', ylab = 'Score', pch = 16)
abline(lm(testscr ~ log(avginc), data = df), col = 'red')

# SQUAREROOT OF AVGINC
plot(sqrt(df$avginc), df$testscr,
     main = paste('Scatterplot of Score ~ SQRT Avg Inc \n Correlation: ',
                  round(cor(sqrt(df$avginc), df$testscr), 2)),
     xlab = 'Average Income of District', ylab = 'Score', pch = 16) 
abline(lm(testscr ~ sqrt(avginc), data = df), col = 'red')

# SQUARE OF AVGINC
plot(df$avginc^2, df$testscr,
     main = paste('Scatterplot of Score ~ Avg Inc ^2 \n Correlation: ',
                  round(cor(df$avginc^2, df$testscr), 2)),
     xlab = 'Average Income of District', ylab = 'Score', pch = 16) 
abline(lm(testscr ~ I(avginc^2), data = df), col = 'red')


# I can see that squaring the variable is taking it away from linear relationship.
# It's getting cluttered towards the left. 


# Interaction Variables --------------------------------------------------------
# Sometimes two or more variables interact to produce a better model.
linear_model <- lm(testscr ~ meal_pct + comp_stu + avginc + el_pct +
                     gr_span, data  = df_train)
summary(linear_model) # Adjusted R-squared:  0.8008 
AIC(linear_model) # 2228.709


linear_model <- lm(testscr ~ meal_pct + comp_stu + avginc + avginc:meal_pct +
                     el_pct + gr_span, data  = df_train)
summary(linear_model) # Adjusted R-squared:  0.803 
AIC(linear_model) # 2226.212
# this model gives a slightly improved AIC value compared to other models.

# How to find out possible interactions ----------------------------------------
# Visualize Variable Interaction (Intuition, Trial and Error)
# Testscr changes with Slope of the curve + Size of the points
library(ggplot2)
ggplot(aes(x = meal_pct, y = testscr), data = df_train) +
  geom_point(aes(size = avginc)) +
  labs(title = 'Test Score ~ Pct Eligible for Meal. Size of points: avginc')

ggplot(aes(x = meal_pct, y = testscr), data = df_train) +
  geom_point(aes(size = str)) +
  labs(title = 'Test Score ~ Pct Eligible for Meal. Size of points: str')


### ------------------- VARIANCE INFLATION FACTOR ---------------------------###
# install.packages('car')
library(car)
library(DMwR)

# Train Test Split -------------------------------------------------------------
set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]

# Simple Linear Regression -----------------------------------------------------
linear_model <- lm(testscr ~ enrl_tot + calw_pct + teachers + meal_pct + computer
                   + comp_stu + expn_stu + str + avginc + el_pct, data = df_train)
summary(linear_model)
# Many of the variables are not significatn and have incorrect signs.
# If I look at the correlations I will find out that signs of the correlation
# and coefficients for some of the X variables do not match.

cor(df[, c(y_var, numeric_vars)])[, 1]

# First, I will remove those variables.
# Removing STR -> wrong sign
# Removing Teachers and Computers -> counter intuitive correlation sign
linear_model <- lm(testscr ~ calw_pct + meal_pct + 
                     comp_stu + expn_stu + avginc + el_pct, data = df_train)
summary(linear_model)
# In the new model only calw_pct is insignificant. Rest is OK. 
car::vif(linear_model)

# VIF > 4 is considered to be very high. In this particular case, meal_pct
# has a VIF of more than 6. However, only calw_pct is not significant in this model.
# Let's remove that.
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc + el_pct,
                   data = df_train)
summary(linear_model)
car::vif(linear_model)

# VIF should be not so high that it affect the sign of the coefficients
# and cause cancelling of effects. All VIF is less than 4. 
# Less than 2 would have been ideal, but since the sign of the coefficients
# are not affected, let's stop. 

# Now, I'll compute the Cook's Distance and see if I can improve the model
# by removing the extreme observations.

### ------------------- INFLUENCE POINTS ------------------------------------###

# Train Test Split -------------------------------------------------------------
set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]

# rename rownames
rownames(df_train) <- 1:NROW(df_train)

# Model for Test SCores
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc + el_pct +
                     gr_span, data = df_train)
summary(linear_model) # Adjusted R-squared:  0.8022 

# Computing evaluation metrics
# Predict on Test Data
preds <- predict(linear_model, df_test)
actuals <- df_test$testscr

DMwR::regr.eval(actuals, preds)
# mae         mse        rmse        mape 
# 6.78025379 79.56020823  8.91965292  0.01047233 
# mapge is very small but there is a certain amount of error in the dataset.

# Influence Points -------------------------------------------------------------
influenceIndexPlot(linear_model)
# I am interested in the top plot. There are certain observations that have 
# very high Cooks distance. 

# Cooks Distance
cd <- cooks.distance(linear_model)

# Finding Influential Points
ips <- cd[cd > 4 * mean(cd)]
top_rows <- as.numeric(names(ips))

# view the influential observations (row names not observations number column)
df_train[top_rows, ]

# Rebuild the model after removing the top influential points
# Build model
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc +
                     el_pct + gr_span, data = df_train[-top_rows, ])
summary(linear_model) # Adjusted R-squared:  0.8465 

# Predict on Test Data
preds <- predict(linear_model, df_test)
actuals <- df_test$testscr

DMwR::regr.eval(actuals, preds)
# mae         mse        rmse        mape 
# 6.73573252 77.82531445  8.82186570  0.01039907
# we have a small improvement, even though it was almost 99% (1 - mape)


### ------------------- BOX COX TRANSFORMATION ------------------------------###
library(dplyr)
library(magrittr)
library(foreign)
library(car)
library(Hmisc)
library(DMwR)
options(scipen = 999)

set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]

# Yeo Johnson Transformation
# The data has to be passed as a data.frame, hence drop = F
yjModel <- caret::preProcess(df_train[, 'avginc', drop = F], method = 'YeoJohnson')
df_train$avgincyj <- predict(yjModel, df_train[, 'avginc', drop = F])[, 1]


# BoxCox Transform
bcmod <- caret::BoxCoxTrans(df_train$avginc)
df_train$avgincbc <- predict(bcmod, df_train$avginc)

# Computations are done, now it's time for visualization.

# Histogram and Density - on Raw Variable
df_train$avginc %>% hist(prob = T, main = 'Average Income') %$%
  lines(density(df_train$avginc))
# Inference: Skewed Curve

# Histogram and Density - on Box Cox Transform
df_train$avgincbc %>% hist(prob = T, main = 'Average Income - Box Cox') %$%
  lines(density(df_train$avgincbc))
# Inference: Bell shaped curve

# Histogram and Density - on Yeo Johnson Transform
df_train$avgincyj %>% hist(prob = T, main = 'Average Income - Yeo Johnson') %$%
  lines(density(df_train$avgincyj))
# Inference: Bell shaped curve. Density has increased in the mean region.

# Box Cox Transformations can be useful to improve model fit and overcome
# heteroscedasticity in some cases.


### ------------------- HETEROSCEDASTICITY -- RESIDUAL ANALYSIS -------------###
library(car)
library(DMwR)
options(scipen = 999)

# To understand heteroscedasticity clearly, let's use the food dataset.
# Then apply the principle to caschool dataset.

# Read data
food <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/food.csv')

# Scatter Plot
par(dev.off())
plot(food$income, food$food_exp)
abline(lm(food_exp ~ income, data = food))

# It is clear, that as income increases the expenditure also increases, however,
# points are sort of spread out. 

# Demonstration of Heteroscedasticity
lin_model <- lm(food_exp ~ income, data = food)
summary(lin_model)
# Both Intercept and income are statistically significant and we have
# the estimates as well. The r-squared is about 36%, however, the model
# is significant. 

# Detecting Heteroscedasticity

# 1. Visual Detection
# Residual plot
# turn off device and split plotting scren into two rows and two columns
par(dev.off()); par(mfrow = c(2, 2));
plot(lin_model)

# Plot of residuals against X and Y
plot(food$income, lin_model$residuals)    # X
plot(food$food_exp, lin_model$residuals)  # Y


# 2. Breush Pagan Test
# H0: Homoscedasticity of residuals (constant variance of residuals)
car::ncvTest(lin_model)
lmtest::bptest(lin_model, studentize = F)
# if p-value is less than 0.05 we reject the null hypothesis. 
# In this case definitely less than 0.05 so I can fairly assume
# that there is some amount of heteroscedasticity in this model.


### ------------------- OVERCOMING HETEROSCEDASTICITY -----------------------###

# Methods to rectify heteroscedasticity:
#   1. Variable Transformation: log, sqrt, powerm box-cox, yeoJohnson, scaling
#   2. Change the specificaation of the model.
#   3. Remove highly influential points and re-fit the model.
#   4. Discover the stuitable explanatory variable.


# Approach 1: Box Cox Transform
bcMod <- caret::BoxCoxTrans(food[, 'income'])
food[, 'incomebc'] <- predict(bcMod, food[, 'income'])

# Check
l1 <- lm(food_exp ~ incomebc, data = food)
summary(l1)
par(mfrow = c(2,2))
plot(l1)
ncvTest(l1)


# Approach 2: YeoJohnson Transform
yjMod <- preProcess(food['income'], method = 'YeoJohnson')
food[, 'incomeyj'] <- predict(yjMod, food['income'])[1]

# Check
l2 <- lm(food_exp ~ incomeyj, data = food)
summary(l2)
par(mfrow = c(2,2))
plot(l2)
ncvTest(l2)


# Approach 3: Scale X variable
l3 <- lm(food_exp ~ I(scale(income)), data = food)
summary(l3)
par(mfrow = c(2,2)); plot(l3)
ncvTest(l3)


# Approach 4: Remove influential points with high cook's distance

# Linear Regression
lin_model <- lm(food_exp ~ income, data = food)

# Influence Plot
influenceIndexPlot(lin_model)

l4 <- lm(food_exp ~ income, data = food[-c(31, 38, 33, 39, 36), ])
summary(l4)
par(mfrow = c(2, 2)); plot(l4)
ncvTest(l4)

# R-squared has improved drastically. 
# There is evidence that there is some heteroscedasticity remaining in this model.

# Cook's Distance
cooks.distance(lin_model) %>% sort() %>% desc()


# Mini-Challenge ---------------------------------------------------------------
# Check for heteroscedasticity for the following regression model in
# the caschool dataset. Visually check the regression plots for heteroscedasticity.

df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')

formula <- testscr ~ meal_pct + comp_stu + expn_stu + avginc + el_pct + gr_span

# Perform Breush Pagan Test to confirm

# Solution:
# Midel for Test Scores
linear_model <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc +
                     el_pct + gr_span, data = df_test)
summary(linear_model)

# Plot
par(dev.off())
par(mfrow = c(2, 2))
plot(linear_model)

# Breush Pagan Test
car::ncvTest(linear_model)

# There is no heteroscedasticity, the p-value is not significant. 


### ------------------- STEPWISE REGRESSION  --------------------------------###
# algorithm that helps find the best regression model
library(dplyr)
library(magrittr)
library(DMwR)
options(scipen = 999)

# Import data
df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')

# Retain numerical and categorical
numeric_vars <- c('enrl_tot', 'calw_pct', 'teachers', 'meal_pct', 'computer',
                  'comp_stu', 'expn_stu', 'str', 'avginc', 'el_pct')

category_vars <- c('county', 'gr_span')

y_var <- 'testscr' # this is the variable I'm going to predict

drop_vars <- c('observation_number', 'dist_cod', 'district') 
# ^ not useful variables
df <- df[, c(y_var, numeric_vars, category_vars)]


# Train Test
set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]


# Forward Stepwise
base.mod <- lm(testscr ~ 1, data = df_train) # base intercept only model
all.mod <- lm(testscr ~ ., data = df_train) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod),
                direction = 'forward', trace = 1, steps = 1000)
summary(stepMod)


# Backward
backstepMod <- step(all.mod, direction = 'backward', trace = 1, steps = 1000)
summary(backstepMod)


# Both Directions
bothstepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod),
                    direction = 'both', trace = 1, steps = 1000)
summary(bothstepMod)


### ------------------- BEST SUBSETS  ---------------------------------------###
# algorithm that helps find the best regression model

# install.packages('olsrr')
# install.packages('leaps')
library(car)
library(olsrr)
library(leaps)
library(DMwR)
options(scipen = 999)

# Import data
df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/caschool.csv')

# Retain numerical and categorical
numeric_vars <- c('enrl_tot', 'calw_pct', 'teachers', 'meal_pct', 'computer',
                  'comp_stu', 'expn_stu', 'str', 'avginc', 'el_pct')

category_vars <- c('county', 'gr_span')

y_var <- 'testscr' # this is the variable I'm going to predict

drop_vars <- c('observation_number', 'dist_cod', 'district') 
# ^ not useful variables
df <- df[, c(y_var, numeric_vars, category_vars)]
df$avginclog <- log(df$avginc)

# Train Test
set.seed(100)
train_rows <- sample(1:NROW(df), size = .75*NROW(df))
df_train <- df[train_rows, ]
df_test <- df[-train_rows, ]

# Best Subsets Regression
# First establish a base search model
linear_model <- lm(testscr ~ enrl_tot + calw_pct + teachers + meal_pct + computer +
                     comp_stu + expn_stu + avginc + el_pct + gr_span, data = df_train)
# linear_model <- lm(testscr ~ teachers + meal_pct + computer + comp_stu + expn_stu +
#                      avginc, data = df_train)'
out <- olsrr::ols_step_all_possible(linear_model)
out
plot(out)

# Higher Adj-R Squared and Mallow's CP is better

# Best Models ------------------------------------------------------------------
# Bets model by adj R-sq
out[which.max(out$adjr), ]

model1 <- lm(testscr ~ calw_pct + meal_pct + comp_stu + expn_stu + avginc + 
               el_pct + gr_span, data = df_train)
summary(model1)
par(mfrow = c(2, 2)); plot(model1)

# Breusch Pagan Test for Homoscedasticity
car::ncvTest(model1) # H0 = Homoscedastic variance

pred1 <- predict(model1, df_test)
# p-value not significant - no problem
regr.eval(df_test$testscr, pred1)
# mae         mse        rmse        mape 
# 6.69462517 78.91630302  8.88348485  0.01033816 

# Best model by AIC
out[which.min(out$aic), ]

model2 <- lm(testscr ~ meal_pct + comp_stu + expn_stu + avginc + el_pct + gr_span,
             data = df_train)
summary(model2)
# all vars are statistically significant
par(mfrow = c(2,2)); plot(model2)


# Breusch Pagan Test for Homoscedasticity
car::ncvTest(model2) # H0 = Homoscedastic variance
# p-value not significant - no problem


pred2 <- predict(model2, df_test)
regr.eval(df_test$testscr, pred2)
# mae         mse        rmse        mape 
# 6.78025379 79.56020823  8.91965292  0.01047233 