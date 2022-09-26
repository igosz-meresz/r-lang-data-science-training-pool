setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# install.packages(c('dplyr','coefplot','psych'))
library(dplyr)
library(ggplot2)
library(coefplot)

df <- mtcars
head(df)

# Graphical Analysis
# Wt
plot(df$wt, df$mpg, main = 'Scatterplot of Wt vs Mpg',
     xlab = 'Car Weight', ylab = 'Mileage')

# Cyl
plot(df$cyl, df$mpg, main = 'Scatterplot of Cyl vs Mpg',
     xlab = 'Cyl', ylab = 'Mileage')

# Disp
plot(df$disp, df$mpg, main = 'Scatterplot of Disp vs Mpg',
     xlab = 'Disp', ylab = 'Mileage')


# Pair plot
psych::pairs.panels(df[, c('mpg', 'wt', 'disp', 'cyl', 'hp')],
                    method = 'pearson', # correlation method
                    hist.col = '#00AFBB', 
                    density = TRUE, # show density plot
                    ellipses = TRUE) # show correlation ellipses


# Correlations -----------------------------------------------------------------
cor(df) %>% round(2)


# Linear Regression ------------------------------------------------------------
model <- lm(mpg ~ wt + hp, data = df)
summary(model)

# https://medium.com/towards-data-science/understanding-linear-regression-output-in-r-7a9cbda948b3


# Measures of Goodness of Fit --------------------------------------------------
df <- mtcars
head(df)


# LRM
model <- lm(mpg ~ wt + hp, data = df)
model_summary <- summary(model)
model_summary
AIC(model)
BIC(model)


set.seed(100)
model <- lm(mpg ~ wt + hp + runif(NROW(df)), data = df)
model_summary <- summary(model)
model_summary

model_summary$r.squared
model_summary$adj.r.squared


# Mini Challenge
# Which amongst the following models is better in terms of:
# R-Squared, Adj R-Squared, AIC and BIC
source('https://bit.ly/2YhVMcw')

# full code
model1 <- lm(mpg ~ wt + hp, data = mtcars )
model2 <- lm(mpg ~ wt + cyl, data = mtcars)
model3 <- lm(mpg ~ qsec + hp, data = mtcars)

# Solution:
s1 <- summary(model1)
s2 <- summary(model2)
s3 <- summary(model3)

cat(s1$r.squared, s1$adj.r.squared, AIC(model1), BIC(model1))
cat(s2$r.squared, s2$adj.r.squared, AIC(model2), BIC(model2))
cat(s3$r.squared, s3$adj.r.squared, AIC(model3), BIC(model3))
