# Set the working directory
dir_path <- 'C:/Users/igorr/Documents/LEARNING/R - Yelp JSON Data Challenge'
setwd(dir_path)
getwd()

# loading the necessary library
library(jsonlite)

# importing JSON data
yelp <- fromJSON("yelp_academic_dataset_business.json")

# the command above gives an error in parse_con(txt, bigint_as_char) (...)
# that is because this JSON file turned out to be something called Newline delimited JSON
# which means there are multiple JSON values inside this file and each of the JSON
# values is considered as an independent object.

# this can be fixed with a 'jsonlite' function 'stream_in()'
# I need to use 'file()' function to create a connection
# for accessing to the files on my disk when I use 'stream_in()' function
yelp <- stream_in(file('yelp_academic_dataset_business.json'))

# FLATTEN YELP DATAFRAME
# how the data has been imported?
str(yelp)

# 'hours' variable is actually a data frame that contains 7 data frames
# each of which is for a weekday like 'Tuesday'. 
# The weekday variables themselves are data frames and each contains two
# 'character' variables of 'open' and 'close'
# This is reflecting the original JSON data structure, but is confusing for analyzing in R.
# I'm going to use 'flatten()' function from 'jsonlite' to make the nested
# hiearchical data structure into a flatten manner by assigning each of the nested
# variable as its own column as much as possible
yelp_flat <- flatten(yelp)
str(yelp_flat)

# Now the data structure looks a lot easier to grasp and even easier to see.
# I'm going to use 'as_tibble()' function from the tibble package
# to make it easier to see the data frame in R console UI.
library(tibble)
yelp_tbl <- as_tibble(yelp_flat)
yelp_tbl

# 'categories' variable is a 'list' data type and it is 'nested'
# this is because 'categories' includes an array data in the original JSON data.
# Let's see the values inside of this type of variable using as.character() function
# I am also goin go need the select() function from 'dplyr' package
library(dplyr)
yelp_tbl %>% mutate(categories = as.character(categories)) %>%
  select(categories)

# REMOVE UNNECESSARY VARIABLES
# Now, looking at the data again, looks like there are bunch of variables whose name
# starts with either 'hours' or 'attributes'
# For now, I am not really interested in those data so I'm going to remove them
# with 'select()' command. 
yelp_tbl %>%
  select(-starts_with("hours"), -starts_with("attribute"))

# COUNT HOW MANY RESTAURANTS THERE ARE
# I can use 'str_deterct()' function from 'stringr' package to find the businesses
# whose 'categories' variable values contain 'Restaurant' text.
library(stringr)
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant"))

# The number of rows is 21,892 in contrast to the original 61,184 rows.
# This means there are 21,892 'Restaurant' businesses in this data out of 61,184.
# To confirm this let's look at the caregories column with 'as.character()' function again.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  mutate(categories = as.character(categories)) %>% select(categories)

# Looks like they all have 'Restaurant' as on of the categories.
# What type of the restaurants are more common in this data set?

# UNNEST A LIST VARIABLE
# To break out 'categories' variable and create on row for each value
# I'll use 'unnest()' function from tidyr package.
library(tidyr)
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories)

# Emil's Lounge is now repeated 5 times. This is because it has 5 different
# categories assigned to this business. This will allow me to do a quick
# summarization with 'count()' function.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories) %>%
  count(categories)

# Now, I want to see the top categories - I'll use "arrange()" function to sort.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories) %>%
  count(categories) %>%
  arrange(desc(n))

# I'll get rid of rows with 'Restaurant' because I know every single row in this
# dataset has something to do with 'Restaurant' now.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(categories) %>%
  arrange(desc(n))

# 'Fast Food' is no. 1 and 'Pizza' and 'Mexican' come after.

# WHERE ARE THE MOST COMMON RESTAURANT TYPES PER STATE / PROVINECE
# To learn this I can simply add 'state' variable into 'count()' function.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  arrange(desc(n))

# Now, what is the top restaurant category for each state?
# I'll use 'group_by()' function to group the data by state and use 
# 'top_n()' function to keep only the top category. Both functions are from 'dplyr' package.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  group_by(state) %>%
  top_n(1, n)

# Let's filter out the smaller numbers, like '1' for 'FIF' province
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  filter(n > 10) %>%
  group_by(state) %>%
  top_n(1, n)

# The result is a list of the top restaurant category for each of the 12 state or province.

# This is a very simple analysis on Yelp business data. Given where we started, with the raw JSON
# format it demonstrates how quickly, incrementally and interactively we can get some interesting
# information out of such un-traditional data format relatively easy. 