setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# ---------------- ALL AVAILABLE MODELS ----------------------------------------
# https://topepo.github.io/caret/available-models.html

# Training the Model
# see available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse = ',  ')
modelnames

# Model lookup
modelLookup('earth')


# ---------------- MISSING VALUES TREATMENT ------------------------------------


# import
df_train <- read.csv('carest_data/intermediate/df_train.csv')
df_test <- read.csv('carest_data/intermediate/df_test.csv')


# check
library(skimr)
skimmed <- skim(df_train)
View(skimmed)


# missing pattern in row wise
library(mice)
mice::md.pattern(df_train)


# Methods ----------------------------------------------------------------------
# 1. knnImpute
# 2. Bag Impute
# 3. Mice

# The missing values can be filled in by predicting it.
# The column you want to predict the missing values for is treated as
# the Y and the remaining are the X. This can be done using
# 'preProcess()' and setting the correct 'method'.

library(caret)

# Impute Options in Caret: knnImpute, bagImpute, medianImpute


# 1. knnImpute: Automatically scales the data because scaling is necessary
#    for knn algorithm.
knn_missing_data_model <- preProcess(df_train, method = 'knnImpute')
knn_missing_data_model

# Predict
# install.packages("RANN")
library(RANN)
df_train_knn <- predict(knn_missing_data_model, newdata = df_train)
anyNA(df_train_knn)


# 2. Bagging: Create the knn imputation model on the training data [USE THIS]
missing_data_model <- preProcess(df_train, method = 'bagImpute')
missing_data_model

# Predict
# Use the imputation model to predict the values of missing data points
df_train <- predict(missing_data_model, newdata = df_train)
df_test <- predict(missing_data_model, newdata = df_test)
anyNA(df_train)
anyNA(df_test)

write.csv(df_train, 'carest_data/intermediate/df_train_nomiss.csv', row.names = F)
write.csv(df_test, 'carest_data/intermediate/df_test_nomiss.csv', row.names = F)


# 3. mice: More options with mice package. Not caret based
micemod <- mice(df_train, method = 'rf') # random forest, check ?mice for different methods
df_train_mice <- complete(micemod)
anyNA(df_train_mice)


# ---------------- ONE HOT ENCODING AND SCALING --------------------------------

# One hot encoding is the process to convert a categorical variable with
# more than 2 categories to as many numeric variables, one per category.

# import
df_train <- read.csv('carest_data/intermediate/df_train.csv')
df_test <- read.csv('carest_data/intermediate/df_test.csv')

# for pipe syntax
library(dplyr)

# Before One Hot Encoding, save the Y separately
y_train <- df_train$Revenue
y_test <- df_test$Revenue

# One Hot Encoding: 
dummies_model <- dummyVars(Revenue ~ ., data = df_train)

# Create the variables using predict. The Y variable will not be present in df_train_mat
df_train <- predict(dummies_model, newdata = df_train) %>% data.frame
df_test <- predict(dummies_model, newdata = df_test) %>% data.frame

# See the strucutre of the newdataset. No Y variable
str(df_test)

# Append the Y variable
df_train$Revenue <- y_train
df_test$Revenue <- y_test


# Range: Preprocess all colkumns to be range bound between 0 and 1
preProcess_range_model <- preProcess(df_train, method = 'range', rangeBounds = c(0, 1))
df_train_scaled <- predict(preProcess_range_model, newdata = df_train)
df_test_scaled <- predict(preProcess_range_model, newdata = df_test)

Hmisc::describe(df_train_scaled)


# ---------------- FEATURE PLOTS -----------------------------------------------
# library(caret)

# import
df_train <- read.csv('carest_data/intermediate/df_train_transformed.csv')
df_test <- read.csv('carest_data/intermediate/df_test_transformed.csv')

# Box Plot
featurePlot(x = df_train[, c(1:10, 21:24, 29, 31)],
            y = factor(df_train$Revenue),
            plot = 'box',
            strip = strip.custom(par.strip.text = list(cex = .7)),
            scales = list(x = list(relation = 'free'),
                          y = list(relation = 'free')))

# Differenes in pattern visible in ProductRelataed, WeekendTRUE, ExitRates, 
# BounceRates, PageValue, Administrative, Informational, etc. 

# Density Plot
featurePlot(x = df_train[, c(1:10, 21:24, 29, 31)],
            y = factor(df_train$Revenue),
            plot = 'density',
            strip = strip.custom(par.strip.text = list(cex = .7)),
            scales = list(x = list(relation = 'free'),
                          y = list(relation = 'free')))


# ---------------- BUILDING ML MODEL WITH TRAIN FUNCTION -----------------------

# The train function is the main work horse of the caret package.
# Through this you can build all the machine learning models available.

# Not just build, you can experiment with things like cross-validation, 
# perform hyper parameter tuning and compare the model performances.

# library(caret)

# import
df_train <- read.csv('carest_data/intermediate/df_train_transformed.csv')
df_test <- read.csv('carest_data/intermediate/df_test_transformed.csv')

# Sample first 1000 for quicker training
df_train <- df_train[1:1000, ]

# `train()` function accepts both formula as weel as x and y parameters.
# Pass in the training data and specify the ML algorithm in the method argument.

# Train the model using randomForest and predict on the trainig data itself.
set.seed(100)
model_rf <- caret::train(Revenue ~ ., data = df_train, method = 'rf')
model_rf

model_rf$finalModel

# The train function does not do a simple model build. It cross validates
# the model using bootstrap cross validation by default.

# Bootstrap cross validation means the training dataset is prepared to be
# of the same size as input dataset, but observations are randomly sampled
# with replacement. As a result some rows get picked more than once.
# The rows that were not picked form the test dataset.

# According to the author, this method of cross validation provides more
# reliable estimates.

# The train function accepts any parameter used by the respective ML algorithm
# and uses it while training. To train a random forest with 1000 trees
# instead of the default 500 you can set it like this

model_rf <- caret::train(Revenue ~ ., data=df_train, method = 'rf', ntree = 1000)
model_rf

# Though the caret output contains more information besides the model itself,
# on calling the predict function, it is intelligent enough to pick the model
# object from the caret output and use that to predict on new data.

# In addition, any preprocessing done on the data through the train function
# will also be implemented on the new dataset before computing predictions.

# If you use this `model_rf$finalModel` the preprocessing will not be performed
# on new data.

# If no new data is passed, it makes the predictions on the training data iself.
# In that case the predictions are called fitted values

fitted <- predict(model_rf)

# Model accuracies
plot(model_rf, main = 'Model Accuracies with Random Forests')

# Variable importance
varimp_rf <- varImp(model_rf)
varimp_rf

# Plot variable importance
plot(varimp_rf, main = 'Variable Importance with Random Forests')

# Predict on df_test
predicted_rf <- predicted(model_rf, df_test)

# Class 