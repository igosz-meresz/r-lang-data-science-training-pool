setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# 1. ------------------ DPLYR AND PIPE CONCEPT ---------------------------------

# Install packages
# install.packages('tidyverse') -- dplyr is part of this package
# install.packages('dplyr')
library(dplyr)
library(tidyverse)

# Data
head(mtcars)


# Pipe Example
# Objective: For hp values > 120, compute the mean of all columns by cylinder
# type and in that table, create a new column, 'kpl', which is = mpg * 0.42

# Method 1: Create in 4 steps with intermediate DFs ----------------------------
df1 <- mtcars[mtcars$hp > 120, ]
df2 <- aggregate( . ~ cyl, data = df1, FUN = mean)
df3 <- round(df2, 2)
df3$kpl <- df3$mpg * 0.42

View(df3)

# Method 2: Create in a single step --------------------------------------------
# Hard to read from inside to out
df <- round(aggregate( . ~ cyl, data = mtcars[mtcars$hp > 120, ], FUN = mean), 2)
df$kpl <- df$mpg * 0.42
df


# Alternate, easier to read method ---------------------------------------------
# library(dplyr)

output <- mtcars %>%
  filter(hp > 150) %>%
  group_by(cyl) %>%
  summarise_all(mean) %>%
  round(2) %>%
  transform(kpl = mpg * 0.42)

output

# 2. Pipe syntax ---------------------------------------------------------------
2.2452323 %>% round(3)

# round() function
round(2.24523232, 3) # same result

# Pipe: round then add
2.2452323 %>% 
  round(3) %>% 
  +2 # take number, round it and then add 2


# Mini-Challenge ---------------------------------------------------------------
# With 'mtcars' dataset, use the pipe operator to compute the correlation
# between mpg and wt when cyl = 6

# Solution:
# install.packages('magrittr')
# library(magrittr)
mtcars %>%
  filter(cyl == 8) %$%
  cor(mpg, wt)



# 3. T-Pipe operator -----------------------------------------------------------
# T Pipe operator is useful to use when there are intermediate functions that
# return nothing. Like plots.

# library(magrittr)
rnorm(10) %T>% plot(main = 'rnorm') %>% sum # T-plot
rnorm(10) %>% plot(main = 'rnorm') %>% sum # regular plot does not work



# 4. Compound assignment pipe operator -----------------------------------------
# Helps perform calculations and assignments in one step

# Compute Sqrt of mpg and assign
mtcars$mpg
mtcars$mpg %<>% sqrt
head(mtcars) # mpg contains square roots of mpg

# If you have a vector in $ and want to convert to yen without creating a new vector
payments <- c(1700, 2200, 5000, 4000, 3800)
payments %<>% `*` (110)
payments



# 5. Exposition Pipe operator --------------------------------------------------

# Helps quickly access the columns in a dataframe
mtcars %>% cor(mpg, cyl) # regular pipe gives error
mtcars %$% cor(mpg, cyl) # mtcars - df, mpg & cyl are its column names
mtcars %$% sum(mpg, disp, cyl) # use more than 2 columns


# --------------------- DATA MANIPULATION VERBS --------------------------------

# 1. Intro to Tible
# Tibble is an enhanced version of dataframe. Can be used in place of dataframes
library(dplyr)

mtcars_tbl <- as_tibble(mtcars)
mtcars_tbl

# In Tibble DF, output never runs off the screen.
# But if all the columns cant be shown in the display area the remaining cols
# are truanced.

class(mtcars_tbl) 
class(mtcars)


# 2. Convert hflights to Tibble ------------------------------------------------
library(hflights)

hf_tbl <- as_tibble(hflights)
hf_tbl

# Contains all flights departing from Houston airports IAH and HOU
# The data comes from the Research and Innovation Technology Administration
# aat the Bereau of Transportation statistics

# Glimpse function for checking structure
dplyr::glimpse(hf_tbl)


# 3. Manipulation verb 1: Filter -----------------------------------------------
# Helps filter rows based on condition
filter(hf_tbl, Month == 1, Year == 2011)

# Using with pipe
hf_tbl %>% 
  filter(Month == 1, Year == 2011)

# Combine multiple conditions
filter(hf_tbl, (Month == 1 & Year == 2011)) # AND
filter(hf_tbl, (Month == 1 | Year == 2011)) # OR


# 4. Manipulation verb 2: Slice ------------------------------------------------
# Filter out exact row numbers
slice(hf_tbl, 2:6)

# Slice is faster and more readable when used in pipes
hf_tbl %>%
  filter(Month == 1) %>%
  slice(1:5)

# Shorter format of slice
'['(hf_tbl, 1:5)

# Both filter and slice
hf_tbl %>%
  filter(Month == 1) %>%
  '['(1:5, ) # use both filter and slice


# 5. Manipulation verb 3: Select -----------------------------------------------
# Select returns the column names of the dataframe
dplyr::select(hf_tbl, Year, DayOfWeek)

# Select all columns in between
dplyr::select(hf_tbl, Year:DayOfWeek)

# Select columns 1 and 2
select(hf_tbl, 1, 2)
select(hf_tbl, c(1, 2)) # alternate

# Special functions to caputure the desired columns
select(hf_tbl, contains("Time"))
select(hf_tbl, starts_with("Day"))
select(hf_tbl, ends_with("Time"))


# 6. Manipulation Verb 4: Rename -----------------------------------------------
# Change names of specific columns
hf_tbl_new <- rename(hf_tbl, mth = Month) # newcolname = oldcolname
hf_tbl_new


# 7. Manipulation verb 5: Mutate -----------------------------------------------
# Add new column based on existing columns or computations
hf_tbl <- hf_tbl %>%
  mutate(yearmonth = paste0(Year, Month) %>%
           as.numeric())
hf_tbl


# 8. Manipulation Verb 6: Transmutate ------------------------------------------
# Similar function to mutate
# While mutate returns all columns new and old
# Transmutate return only new columns

ht_tbl_ym <- transmute(hf_tbl, yearmonth = paste0(Year, Month) %>% as.numeric)
ht_tbl_ym


# 9. Manipulation Verb 7: Arrange ----------------------------------------------
# Sort data by one or more columns

hf_tbl %>%
  arrange(Year, Month) # default is ascending order

hf_tbl %>%
  arrange(Year, desc(Month)) # Descending order for 'month'


# 10. Mini-challenge -----------------------------------------------------------
# From 'mtcars' dataframe
# - create a new column called 'carmodel' that contains names of car models
# - convert to tibble
# - create new column kpl = 0.425 * mpg, kpl_int = round(kpl)
# - sort it by kpl 
# - filter rows with vs = 1

# Solution:
data("mtcars")
mtcars_tbl <- mtcars %>%
  mutate(carmodel = rownames(mtcars)) %>%
  as_tibble() %>%
  mutate(kpl = 0.425*mpg, kpl_int = round(kpl)) %>%
  arrange(kpl) %>%
  filter(vs == 1)

mtcars_tbl



# ------------------------- GROUPING THE DPLYR WAY -----------------------------
# Get the data
# library(hflights)
head(hflights)


# Manipulation Verb 8: Group by ------------------------------------------------
# library(dplyr)
hflights_grouped <- hflights %>%
  group_by(UniqueCarrier)
View(hflights_grouped)

# grouped tbl will apear like a tibble object, but internally is of grouped_df class
class(hflights_grouped)


# Manipulatioon Verb 9: Summarize ----------------------------------------------
# Do different kinds of summarization
hflights_grouped %>% summarise(mean_dist = mean(Distance))

# Usually first do group_by(), then summarise()
# Example: Mean distance travelled by each carrier
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(mean_dist = mean(Distance))


# Useful dyplr funcctions that help with aggregation ---------------------------
unique_carrier_agg <- hflights %>% group_by(UniqueCarrier) %>%
  summarise(delay = mean(DepDelay, na.rm = T),
            num_obs = n(), # n rows
            num_distinct_obs = n_distinct(DepDelay), # unique
            first_obs = first(DepDelay), # first occurance
            second_obs = nth(DepDelay, 2), # second occurance
            last_obs = last(DepDelay)) # last occurance

unique_carrier_agg

# n(): Computes the enumber of obsercations by each group
# n_distinct: Gets the number of distinct observations in the specified variable
# first: Returns the first observation for each group
# nth: nth observation from start
# last: for the last observation


# Mini-challenge ---------------------------------------------------------------
# In hflights dataset, compute mean departure delay, number of rows, n_unique
# values, first, second and last values of departure delays grouped by 'Origin'.

# Solution:
origin_agg <- hflights %>% group_by(Origin) %>%
  summarize(delay = mean(DepDelay, na.rm = T),
            num_obs = n(),
            num_distinct_obs = n_distinct(DepDelay),
            first_obs = first(DepDelay),
            second_obs = nth(DepDelay, 2),
            last_obs = last(DepDelay))

origin_agg


# -------------------------- MERGING DATAFRAMES --------------------------------

# Create Data (Tibbles) for using joins
# Create new column
# library(dplyr)
mtcars$carname <- mtcars %>% rownames # create a new column for rownames
mtcars_tbl <- mtcars %>% as_tibble()
head(mtcars_tbl)

# 1. Create input tables -------------------------------------------------------
# Two of them having common column name
# Column carname is common amongst all the datasets except tibble3, 
# where it is renamed as car
tbl_1 <- mtcars_tbl %>% select(carname, mpg:disp) %>% slice(1:25)
tbl_2 <- mtcars_tbl %>% select(carname, wt:vs) %>% slice(20:32)
tbl_3 <- tbl_2 %>% rename(car = carname)

tbl_1
tbl_2
tbl_3

# 2. Left Join -----------------------------------------------------------------
# Retains all rows from left
# If common column names, no need to name the joining column
left_join(tbl_1, tbl_2)

# If common columns have different names, then explicitly specify
left_join(tbl_1, tbl_3, c('carname' = 'car'))


# 3. Right join ----------------------------------------------------------------
# Retain all rows from right
right_join(tbl_1, tbl_2)

# Tip: Need not really use right join, just swapping the tbl sequence
# and using left join works the same way

left_join(tbl_2, tbl_1) # shows the same result


# 4. Inner Join ----------------------------------------------------------------
# Retain only common rows
inner_join(tbl_1, tbl_2)


# 5. Full join -----------------------------------------------------------------
# Retains all rows from left to right
full_join(tbl_1, tbl_2)


# 6. Semi join -----------------------------------------------------------------
# Returns all rows from 'left' that are in common with 'right',
# but only keeps column in the left table
semi_join(tbl_1, tbl_2)


# 7. Anit join -----------------------------------------------------------------
# Returns all rows from 'left' that are NOT in common with 'right AND only keeps
# column in the left table
anti_join(tbl_1, tbl_2)


# mini-challenge:
# From df1 remove the rows that are present in df2 based on the common column a.
# Create another dataframe df3 that contains all the rows and columns from both
# datasets.
# In df2 rename column a as 'apple' in df2 and create a new dataframe named 'df4'
# that contains only the common rows in df1 and df2.
# In df1 for each unique value of column a, compute the mean, first, second last
# and second last value of column b in separate columns

# Code for the callange:
source('https://bit.ly/2VyF1IU')

# Solution:
anti_join(df1, df2)

df3 <- full_join(df1, df2)

df2_1 <- rename(df2, apple = a)
inner_join(df1, df2_1, by = c('a' = 'apple'))

df1_grouped <- group_by(df1, a)
summarise(df1_grouped,
          mean_b = round(mean(b)),
          first = first(b),
          second = nth(b, 2),
          last = last(b),
          penultimate = nth(b, -2))
