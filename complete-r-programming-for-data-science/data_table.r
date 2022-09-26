setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# 1. ------------- CREATING DATA.TABLE, FAST IMPORT & EXPORT -------------------

# How fast fread is compared to readcsv?
# Create sample file with 4 columns and 1 million rows.
vec <- 1:10^6
mat <- cbind(a = vec, b = vec, c = vec, d = vec)
head(mat)
write.csv(mat, 'mat.csv')


# Import with read.csv
system.time(mt <- read.csv('mat.csv')) # time: ~ 1.39

# Import with fread
library(data.table)
system.time(mt <- fread("mat.csv")) # time: ~ 0.03 
mt
class(mt)


# 2. ------------ CONVERT DATAFRAME TO DATA.TABLE ------------------------------

# Data.tables don't have row names. If your dataframe has meaningful rownames
# youn eed to explicitly create a new column before converting it to a datatable.

# In case df has rownames, then need to convert it to a column in datatable
head(mtcars)
mtcars$rowname <- rownames(mtcars)
df <- mtcars

# Convert dataframe to a datatable - by copying data into a new object
mtcars_dt <- data.table(df)
class(mtcars_dt)
View(mtcars_dt)

# Create from scratch
dt <- data.table(a = 1:10, b = 11:20)
dt

# Another option is to convert the dataframe to a data.table using setDT. 
# This does not involve copying of data internally, so is more efficient.

# Alternatively convert the existing df to datatable in place
setDT(df)
class(df)
df


# 3. ------------ FAST EXPORT --------------------------------------------------
fwrite(df, file = 'df.csv')


## Mini-challenge
# Import the dataset mtcars stored in the following link as data.table object.
# Convert it to a data.frame

url <- "https://bit.ly/38ZQS6F"

# Import
dt <- fread(url)

# Convert
setDF(dt)
View(dt)


# 4. ------------ UNDERSTANDING DATA.TABLE SYNTAX ------------------------------
# Create dataframe and datatable version of the same dataset
# library(data.table)
df <- mtcars
dt <- data.table(df)


# The main difference in syntax is, unlike dataframes, data.table will 
# understand the column names inside the square brackets.


# Filtering rows ---------------------------------------------------------------
# In df need to explicitly mention df$cyl inside the square brackets
df[df$cyl == 6, df$gear == 4, ]

# In datatable, table name is not needed within []
dt[cyl == 6 & gear == 4, ]


# Subsetting columns -----------------------------------------------------------
# Select columns using number positions
dt[, 1]
dt[, c(1, 2)]


# Alternate method of passing column in dt - As strings
dt[, 'mpg']
dt[, c('mpg', 'cyl')]

# Effectively datatable syntax works almost the same as dataframes when it comes
# to selecting columns. 

# The data.table way of selecting columns is using the column name directly
# instead of strings.

# Data table way of selecting columns - Using columns directly, instead of strings.
dt[, mpg]

# For multiple columns in datatable, use dot notation or list
dt[, .(mpg, cyl)]
df[, list(mpg, cyl)]


# That's a small difference but for a data scientist who writes a lot of R 
# code, these things matter.


# Using a vector for a column name ---------------------------------------------
# Suppose you are writing a for loop that iterates through selected columns.
# You may have stored the column names in separate vector.
# In such a case you want to use the vector name instead of the column name directly.

# Let's say, earlier you had stored column names in a vector for writing loops.
# Now you want to use the vector to load the columns.
myvec <- c("mpg", "cyl", "hp")
dt[, myvec] # returns error

# Correct way:
dt[, myvec, with = F]


# Create new column ------------------------------------------------------------
df <- mtcars
dt <- data.table(df)


# You can create new columns from within the square brackets using the special
# assignment operator.

# Create new column: mileage in kilometers per liter
dt[, kpl := round(mpg*0.425, 1)] # := is the assignment operator
dt

# Create multiple columns in one shot by using it as a function
dt[, `:=`(kpl = round(mpg*0.425, 1),
          mileage_bin = ifelse(mpg > 22, 'high', 'low'))]
dt

# Running multiple lines of code in the 'j' position
dt[, kpl := {temp_var <- mpg*0.425;
             round(temp_var, 1)}] # the last line is returned
dt


# Mini-challenge ---------------------------------------------------------------
# 1. Create a datatable base on the in-built airquality dataset.
# 2. Create a new column named 'MonthDay' in the resulting datatable so that
#    all values in 'MonthDay' has 3 characters.
# 3. What are the MonthDays on which Ozone is missing? How many of them in total?
# 4. What is the mean value of 'Solar.R', 'Wind', and 'Temp', when 'Ozone' is missing?


# Solution:
# 1.
dt <- data.table(airquality)

# 2.
dt[, MonthDay := ifelse(nchar(Day) > 1, paste0(Month, Day), paste0(Month, paste0(0, Day)))]

# 3.
dt[is.na(Ozone), .(MonthDay)]
NROW(dt[is.na(Ozone), .(MonthDay)])

# 4.
dt1 <- dt[is.na(Ozone), ]
dt1[is.na(Ozone), .(mean_solar = mean(Solar.R, na.rm = T),
                    mean_wind = mean(Wind, na.rm = T),
                    mean_temp = mean(Temp, na.rm = T))]


# 5. ------------ GROUPING THE DATA.TABLE WAY ----------------------------------
# Prepare data.table
mtcars$carname <- rownames(mtcars)
dt <- data.table(mtcars)
dt


# Grouping using 'by' argument
# Compute mean mileage for each cylinder type
dt[, .(mean_mileage = mean(mpg))]
dt[, .(mean_mileage = mean(mpg)), by = cyl]


# Grouping by multiple variables
dt[, .(mean_mileage = mean(mpg)), by =. (cyl, gear)] # mind the '.' after by =


# Select the first occuring value of mileage for each Cyl type
dt[, .(first_mileage = mpg[1]), by = cyl]


# Select the second occuring value of mileage
dt[, .(second_mileage = mpg[2]), by = cyl]


# Select the last occuring value of mileage
dt[, .(last_mileage = mpg[length(mpg)]), by = cyl]


# 6. ------------ THE .N VARIABLE ----------------------------------------------
# Select last occuring value of mileage (data.table way)
# The number of elements gets stored in a special object called 'dot N'
dt[, .(last_mileage = mpg[.N]), by = cyl]

# .N contains the number of items present in the dataset.
# Since we used the by argument, it will contain the number of items
# corresponding to each value of the by variable.

# When no group is specified:
dt[, .I]

# How to get the row numbers of items that satisfy a given condition:
dt[cyl == 6, .I] # gives wrong row indexes. I.e. returns indexes of filtered data

# Returns the wrong answer because datatable has already filtered the rows
# that contain cyl value of 6. So, what you want to do instead is to write that
# condition to subset dot I, instead of the whole datatable.
dt[, .I[cyl == 6]] # CORRECT!


# 7. ------------ THE .SD VARIABLE ---------------------------------------------
# The .SD contains the subset data itself for each group specified in the 'by' 
# argument

# .SD for subsetting/breaking the table to multiple tables based on argument
# Print 3 datasets, one for each value for cyl column
dt[,
   {cat("cyl: ", cyl, '\n');
     print(.SD)}, # run two lines in J position
   by = cyl]

# Number of rows for each 'cyl' type
dt[, nrow(.SD), by = cyl]
dt[, .N, by = cyl]

# Row numbers for each unique value of cyl
dt[, .I, by = cyl]


# 8. ------------ WRITING apply() FUNCTION WITHIN J AND .SDcols ----------------
# Let's compute the mean for every column for each 'cyl' type

# Apply function on SD
dt[, sapply(.SD, mean), by = cyl] # Mean. error - since on of the columns has characters

# The above code will try to compute the mean for all the columns and will 
# give an error because one of the columns contain rownames which is a character.

# If you want to select only particular column, you can provide it in the
# .SDcols argument
dt[, .(mean_val = lapply(.SD, mean)), by = cyl, .SDcols = c('mpg')]


# Mini-challenge ---------------------------------------------------------------
# 1. Compute the number of cars and the mean mileage for each gear type.
# 2. At what position does the minimum value of 'mpg' occur for each group of 'cyl' column.
# 3. At what row number does the minimum value of 'mpg' occur for each unique value 'cyl' column.
# 4. Compute the mean value of mpg, hp and wt columns grouped by cyl.
#    Use .SD symbol for this


# Solution:
dt <- data.table(mtcars)

# 1. 
# library(dplyr)
dt[, .(.N, mileage = mean(mpg) %>% round(2)), by = gear]

# 2. 
dt[, .(rownum = which.min(mpg)), by = cyl]

# 3.
dt[, .(index = .I[which.min(mpg)]), by = cyl]

# 4.
dt[, .(mean_val = lapply(.SD, mean), colname = colnames(.SD)), by = cyl, .SDcols = c('mpg', 'hp', 'wt')]



# 8. ------------ SETTING KEYS -------------------------------------------------
# Create the data
mtcars$carname <- rownames(mtcars)
View(mtcars)

# Let's first understand why keys can be useful and how to set it.

# Setting one or more columns as keys on a data.table enables it to perform
# binary search and do advanced sorting algorithms.
# Once the keys are set the operations are even faster after that.

# Setting a key sorts the data.table by that key. So you need to be careful
# in case you don't want to change the order of the rows.


# Setting key ------------------------------------------------------------------
# When creating a datatable
mtcars_dt <- data.table(mtcars, key = 'carname')

# Using setkey() for existing data-tables
mtcars_dt <- data.table(mtcars)
setkey(mtcars_dt, carname)

# It looks like nothing happened, but it invisibly sorted the data.table by 'carname'
View(mtcars_dt)


# Checking key of an existing data table ---------------------------------------
key(mtcars_dt)

# Once the key is set, you can filter the datatable directly by passing
# just the values from the key column

# Filter using the key
mtcars_dt["Merc 230"]
mtcars_dt["Valiant"]

# This was almost instantaneous. You might wonder if you even need the keys feature.
# When the dataset becomes really big, it significantly reduces the computation time.
# Consider the following example with 100 million rows and 2 columns.


# Use case where key is needed -------------------------------------------------
dt <- data.table(name = sample(c("krishna", "benjamin", "mustafa", "stella"),
                               100000000, replace = T),
                 vals = runif(100000000, 20, 200))

# Filter names of "Krishna", "Stella"
# conventional way
system.time(dt1 <- dt[name %in% c("krishna", "stella"), ])

# after setting keys
setkey(dt, name)
system.time(dt1 <- dt[c("krishna", "stella")]) # more than twice as fast


# Setting multiple keys --------------------------------------------------------
setkey(mtcars_dt, cyl, gear)
key(mtcars_dt)


# Filtering for numeric keys ---------------------------------------------------
# When key is numeric enclose the filters in a dot or J function
# Now mtcars datatable has 2 key columns, you can provide filtering conditions
# for both columns

# Use dot or J function
mtcars_dt[.(8, 3)]
mtcars_dt[J(8, 3)]

# Here 8 is applied to cyl column and 3 is applied to gear column

# Filter multiple values for same column
mtcars_dt[.(c(8, 6), c(3, 4))]


# Removing the keys ------------------------------------------------------------
# On removing keys, the datatable won't go back to original unsorted order
setkey(mtcars_dt, NULL)
View(mtcars_dt)


# Setting key using the 'by' function ------------------------------------------
# Another aspect of setting keys is using the 'keyby' argument that you can
# use when aggregating.
output <- mtcars_dt[, .(mean_mpg = mean(mpg),
                        mean_disp = mean(disp),
                        mean_wt = mean(wt),
                        mean_qsec = mean(qsec)), by = cyl][order(cyl), ]
View(output)
key(output)


output <- mtcars_dt[, .(mean_mpg = mean(mpg),
                        mean_disp = mean(disp),
                        mean_wt = mean(wt),
                        mean_qsec = mean(qsec)), keyby = cyl]

output
key(output)


# 9. ------------ FAST LOOPS WITH SET() ----------------------------------------
# Create datatable of hundred thousand rows and 2 columns
M <- matrix(round(runif(400000, 20, 100)), ncol = 4)
dt <- as.data.table(M)
dt


# For Loop vs Fast loop usting sets --------------------------------------------
# Usual Way
# Calculate the time taken for a for-loop to set each row of 1st column to zero
system.time({
  for(i in 1:nrow(dt)){
    dt[i, V1:=0]
  }
}) # 26.58 seconds
dt[1:5, ]

# Using set() function
# The set function takes the dataset as the first argument, followed by row number,
# then column number or column name and finally the value to be replaced.
dt <- as.data.table(M)
system.time({
  for(i in 1:nrow(dt)){
    set(dt, i, 1L, 0)
  }
}) # 0.15 seconds

# In real practice you don't have to iterate through each row if all the rows
# in the column has to be of same value. If all the rows in a column get the same value
# then set i equal to NULL in order to set the value for whole column. 
M <- matrix(1, nrow = 100000, ncol = 2)
dt <- as.data.table(M)
dt

set(dt, i=NULL, j=1L, 0) # Set all values in 1st column to 0

# The set function works equally well on dataframes as well.


# Mini-challenge
# Input data
source('https://bit.ly/2I56pq8')

# OR
set.seed(100)
M <- matrix(round(runif(100*100), 2), nrow = 100, ncol = 100)
df <- as.data.frame(M)
df[1:5, 1:5]

# Solution
# Replace the diagonal of dataframe 'df' to the respective row number
# using the set function.
# 1.
for(i in 1:nrow(df)){
  set(df, i, i, i)
}
df[1:5, 1:5]

# 2. 
set(df, i=which(df$v2 < 0.5), j="v1", df$V2[df$v2 < 0.5])
df

# OR
setDT(df)
df[V2 < 0.5, V1:=V2]
df
