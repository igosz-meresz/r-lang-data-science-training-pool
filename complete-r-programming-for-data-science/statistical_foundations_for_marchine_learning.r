setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# Univariate Analysis (under section of Statistical Inference)
mean(mtcars$mpg)

median(mtcars$mpg)

hist(mtcars$mpg, col = 'orange')

# for numerical variables
summary(mtcars$mpg) # statistical summary of mtcars

sd(mtcars$mpg) # standard deviation

var(mtcars$mpg) # variance

range(mtcars$mpg) # range

quantile(mtcars$mpg, c(.1, .25, 0.5, 0.75, .9)) # quantiles

sd(mtcars$mpg)/mean(mtcars$mpg) # coefficient of variation

IQR(mtcars$mpg) # Interquartile range - which is the difference between 75th and 25th %ile

# for categorical variables
table(mtcars$cyl) # frequency of different categorical variables in column 'cyl'

hist(mtcars$cyl, col = 'orange') # frequency shown in histogram


# ---------- Law of large numbers ----------------------------------------------
set <- c(1,2,3,4,5,6)

all_outputs <- c()
for (i in 1:100){
  out <- sample(set, 100, replace = T)
  all_outputs <- c(all_outputs, mean(out))
}

# expected value
mean(all_outputs)

# plot
plot(all_outputs, type = 'l')


# ---------- Central Limit Theorem ---------------------------------------------
# Uniform distribution
base_distrib <- runif(10000)

# Plot
hist(base_distrib)
plot(density(base_distrib))

# Compute Sample Means
sample_means <- c()
for(exp in 1:10000){
  samp <- sample(base_distrib, 50)
  samp_mean <- mean(samp)
  sample_means <- c(sample_means, samp_mean)
}

# Plot
hist(sample_means)
plot(density(sample_means))

# Other Distributions
base_distrib <- rchisq(1000, 10) # chi-sq
base_distrib <- rhyper(1000, 10, 7, 8) # hypergeometric


# ---------- Correlation Tests -------------------------------------------------
# Correlation
cor(mtcars$mpg, mtcars$wt)
cor(mtcars)
# cor() gives correlation score of each factor against 'mpg' in this example.
# we can see t hat variables with negative correlation (cyl, disp, hp, carb)
# have negative effect on the mileage.

# Spearman
cor(mtcars$mpg, mtcars$wt, method = 'spearman')

# Kendall's
cor(mtcars$mpg, mtcars$wt, method = 'kendall')


# Correlation Test
cor.test(mtcars$mpg, mtcars$wt, method = 'pearson')

# p-value helps us decide, if the alternative hypothesis can be rejected or not
# statistically significant has p-value of 0.05
# in this example we reject the null (alternative) hypothesis.

# mod <- lm(mpg ~ wt, data = mtcars)
# summary(mod)


# ---------- T-Test One Sample -------------------------------------------------
# Problem statement:
# A farming company wants to know if a new fertilizer has improved crop yield.
# Historic data shows the average yield of Bengal Potatoes in 20 ton per acre.
# They decide to test a new organic fertilizer on a saple of 12 different farms
# and capture the observations. Did the new fertilizer make a difference?

# One sample T-Test
x <- c(21.5, 24.5, 18.5, 17.2, 14.5, 23.2, 22.1, 20.5, 19.4, 18.1, 24.1, 18.5)

# Hypothesized mean
mu = 20.0

# Sample Mean
xbar <- mean(x)
xbar

# Standard Deviation
std <- sd(x)
std

# Standard Error
se <- std/sqrt(12) # because there are 12 observations in x
se

# T statistic
tstat <- (xbar - mu)/se
tstat

# p value
2 * pt(-1 * tstat, df = 11)

# Check by performing One Sample T-Test
t.test(x, mu = 20)

# The conclusion is the new fertilizer has no statistical relationship with
# the crop yield.


# ---------- T-Test Dependent --------------------------------------------------
# Problem statement:
# Anita is working as a Data Analyst with an e-Commerce firm that sells
# 1000's of products. She notices a strange drop in the sales volumes 
# on her employer's portal, while the overall retail industry is growing.
# She gets curious to check if the competitor is selling the products at lower
# prices. Looking at the sample data, she finds evidence of competitor selling
# the same products at lower prices, however the prices are NOT uniformly low
# across all products in sample. Anita wants statistical backing by performing
# a significance test before taking the case to the business leaders in order 
# to be considered for a full fledged initiative. Can you help her out?

# Paired t-test
a <- c(55, 35, 19, 21, 44, 61, 93, 100, 12, 51, 90) # competitor prices
b <- c(65, 43, 14, 25, 100, 99, 105, 110, 12, 48, 95) # employer prices
NROW(a)

# Difference
d <- b - a
d

# H0: Mean of the differences - 0
# H1: Mean of the differences != 0

# Mean
mu <- mean(d)
mu

# Standard Deviation
std <- sd(d)
std

# Standard Error
se <- std/sqrt(11)
se

# t statistic
tstat <- mu/se
tstat # 2.205482

# p value
2 * pt(-1*tstat, df = NROW(d)-1) # 0.05195666

# Check by performing a paired t-test
t.test(d, mu = 0)
t.test(a, b, paired = T)


# ---------- T-Test Independent ------------------------------------------------
# Sonia is working as an Information Analyst with an e-Commerce firm that sells
# retail products. She is suspicious that the rating of one of the products
# is manipulated by a group of people from a particular geography. She gathers
# two sets of the product ratings, one from a normal group and one from the 
# suspicious group. Perform a T-Test to confirm if the suspicious group indeed
# gave a significantly different rating.

# Samples
a <- c(4.5, 3.0, 4.5, 3.5, 5.0, 4.0, 3.5, 3.0, 1.0, 2.0, 4.0, 3.5, 3.0)
b <- c(4.5, 5.0, 4.0, 5.0, 4.0, 5.0, 5.0, 4.5, 4.0, 5.0, 3.5)

# Sample mean
xa <- mean(a)
xb <- mean(b)

# Sample variance
va <- var(a)
vb <- var(b)

# Number of elements
na <- length(a)
nb <- length(b)

# T Statistic
tstat = (xa - xb)/(sqrt(va/na) + (vb/nb))
tstat

# T Test
t.test(a, b)

# DOF: Welch Satterwaite Formula


# ---------- Chi-Square Test ---------------------------------------------------
# Problem statement
# An educational institution is trying to assess if students taking extra
# tuitions help in achieving 'Outstanding' grades in exams. Given data shows
# data of no. of students who attended tutitions vs. getting 'Outstanding' grades.
# Did the tuitions have an impact?

M <- as.table(rbind(c(40, 10), c(30, 40)))

dimnames(M) <- list(tuitions = c('Tuitions', 'NoTuitions'),
                    grades = c('Outstanding', 'NotOutstanding'))

Xsq <- chisq.test(M, correct = FALSE) # Prints test summary
Xsq

Xsq$observed # observed counts (same as M)
Xsq$expected # expected counts under the null
Xsq$residuals # Pearson residuals
Xsq$stdres # standardized residuals


# -------------------- ANOVA ---------------------------------------------------
# Tanaka San is experimenting with fertilizers to improve crop size. He is
# curious to know if new compound of synthetic fertilizers can affect the size
# of the tomatoes. He wants to test if the crop responds differently to any of
# the 3 fertilizers he recently produced. The size of ripened tomatoes at the
# time of harvest from samples across the 3 experiments is shown

# ANOVA computations
df <- data.frame(size = c(7,8,7,9,5,6,12,10,8,14,13,19,20,18,17),
                 farm = c('A','A','A','A','A','B','B','B','B','B','C','C','C','C','C'))

sd(c(7,8,7,9,5))
a <- c(7,8,7,9,5)
m <- mean(c(7,8,7,9,5))

sqrt(sum((a-m)^2)/4)

# Computation
x_a <- mean(df$size[df$farm == 'A']); sd_a <- sd(df$size[df$farm == 'A'])
cat(x_a, sd_a)

x_b <- mean(df$size[df$farm == 'B']); sd_b <- sd(df$size[df$farm == 'B'])
cat(x_b, sd_b)

x_c <- mean(df$size[df$farm == 'C']); sd_c <- sd(df$size[df$farm == 'C'])
cat(x_c, sd_c)

x_mean <- mean(df$size)
x_mean

# SS_b - sum of squares
ss_b <- (((x_a - x_mean)^2)*5) + (((x_b - x_mean)^2)*5) + (((x_c - x_mean)^2)*5)
ss_b

S_b <- ss_b/(3-1)
S_b
 
# SS_w
ss_w <- 4*(sd_a^2) + 4*(sd_b^2) + 4*(sd_c^2)
ss_w

# S_w
S_w <- ss_w/(15-3)
S_w

F <- S_b/S_w
F

# P value
1 - pf(F, 2, 12)


# Run ANOVA
anovamod <- aov(size ~ farm, data = df)
summary(anovamod)
# We reject the null hypothesis, that all group means are equal. 