setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# ---------------------- INTRODUCTION TO GGPLOT --------------------------------
# Run the library and get data
#install.packages('ggplot2')
library(ggplot2)
data('midwest', package = 'ggplot2')

# Midwest dataset contains demographic information (such as population, area, etc)
dim(midwest)
head(midwest)


# 1. Initialize a basic ggplot based on the midwest dataset
g <- ggplot(midwest, aes(x = area, y = poptotal)) # main arg is the dataframe
plot(g)

# ggplot2 is designed to work with a dataframe instead of vectors.
# No lines or points because we haven't explicitly added a layer.


# 2. Plot data points using the geom_point()
# Method 1: Specify the geom layer
g <- ggplot(midwest) +
  geom_point(aes(x = area, y = poptotal))
plot(g)

# Method 2: Specify the m,ain ggplot call. The same X and Y axis applies 
g <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point()
plot(g)


# 3. Add line of best fit 
g <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point() +
  geom_smooth(method = 'lm')
plot(g)


# 4. Adjust X and Y limits for better visibility
# Method 1: By deleting points outside of the range
# Affects the slope of the line itself.
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000))

# Method 2: By zooming in
# Does not affect the line of fit since data is untouched
g + coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 1000000)) # zoom in


# 5. Remove the confidence interval (shaded region)
g <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 2500000))

plot(g)


# 6. Add axes titles
g + labs(title = "Area Vs Population",
         subtitle = "From midwest dataset",
         y = "Population",
         x = "Area")


# Full function call in single code:
g <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 2500000)) +
  labs(title = "Area Vs Population",
       subtitle = "From midwest dataset",
       y = "Population",
       x = "Area")


# -------------------- CUSTOMIZE GGPLOT COMPONENTS -----------------------------

# library(ggplot2)
# data('midwest', package = 'ggplot2)


# 7. Changing color
# Method 1: Color all points the same
ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point(col = "steelblue") + # add color
  geom_smooth(method = 'lm', col = 'firebrick', se = FALSE) + # add color
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 250000)) +
  labs(title = "Area vs Population",
       subtitle = "From midwest dataset",
       y = "Population",
       x = "Area")

# Method 2: Color the points based on given columns ('State)
gg <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point(aes(col = state)) + # add color
  geom_smooth(method = 'lm', col = 'firebrick', se = FALSE) + # add color
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 250000)) +
  labs(title = "Area vs Population",
       subtitle = "From midwest dataset",
       y = "Population",
       x = "Area")

plot(gg)


# 8. Changing Size 
gg <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point(aes(col = state, size = scale(popdensity))) + 
  geom_smooth(method = 'lm', col = 'firebrick', se = FALSE) +
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 250000)) +
  labs(title = "Area vs Population \nFrom midwest dataset",
       subtitle = "From midwest dataset",
       y = "Population",
       x = "Area")

plot(gg)


# 9. Remove the legend
gg + theme(legend.position = 'None')

# Alternate method of turning off legend in full function call
gg <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point(aes(col = state), show.legend = F) + # turn off legend
  geom_smooth(method = 'lm', col = 'firebrick', se = FALSE) +
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 250000)) +
  labs(title = "Area vs Population \nFrom midwest dataset",
       subtitle = "From midwest dataset",
       y = "Population",
       x = "Area")

plot(gg)


# 10. Change color palette 
gg + scale_color_brewer(palette = 'Set1')
gg + scale_color_brewer(palette = 'Dark2')
gg + scale_color_brewer(palette = 'PuBuGn')
gg + scale_color_brewer(palette = 'Spectral')

# See list of color palettes
library(RColorBrewer)
brewer.pal.info


# 11. Customize Axis Test (breaks and labels)
# X Axis breaks
brks <- seq(0, 0.1, 0.01) # min: 0, max: 0.1, interval: 0.01
gg + scale_x_continuous(breaks = brks)

# Change the text as well
lbls <- paste0((brks*1000000), "ha")
gg + scale_x_continuous(breaks = brks, labels = lbls)

# Y Axis breaks
gg + scale_x_continuous(breaks = brks, labels = lbls) +
  scale_y_continuous(breaks = seq(0, 25000, 50000))

# Get rid of any scientific notation
options(scipen = 999)
gg + scale_x_continuous(breaks = brks, labels = lbls) +
  scale_y_continuous(breaks = seq(0, 25000, 50000))


# 12. Adjust Theme Elements (Look and Feel)
# Adjust properties of components (plot, axis titles, axis text)

# theme() is the main function / layer to adjust the look and feel of the plot
# Accepts various plot components as arguments

gg1 <- gg + theme(plot.title = element_text(size = 12,
                                            face = "bold",
                                            color = "steelblue",
                                            lineheight = 1.2,
                                            hjust = 0.5),
                  axis.title.x = element_text(size = 10),
                  axis.title.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10, angle = 30),
                  axis.text.y = element_text(size = 10))

plot(gg1)


# Change the entire theme in one line
gg1 + theme_bw()
gg1 + theme_minimal()
gg1 + theme_classic()

# see ?theme_bw for more themes

# The extension gothemes provides awesome themes
install.packages('ggthemes')
library(ggthemes)

# Stata theme
gg1 + theme_stata() + scale_colour_stata()

# Economist theme
gg1 + theme_economist() + scale_colour_economist()

# FiveThirtyEight Theme
gg1 + geom_smooth(method = 'lm', se = FALSE) +
  scale_color_fivethirtyeight('cyl') +
  theme_fivethirtyeight()


# Mini-challenge

# Make a plot using ggplot's minimal theme to visualize the Asian population
# (`popasian`) against the American Indian population (`popameridian`) for a 
# population range from 0 to 1000.
# Draw a line of best fit to this population range


# Solution:
# library(ggplot2)
data('midwest', package = 'ggplot2')

gg <- ggplot(midwest, aes(x = popamerindian, y = popasian)) +
  geom_point(aes(col = state)) +
  geom_smooth(method = 'lm', col = 'firebrick') +
  xlim(c(0, 1000)) +
  ylim(c(0, 1000)) +
  labs(title = "Population of American Indians vs Asians",
       y = "Pop. Asian",
       x = "Pop. American Indian")

plot(gg)

gg + theme(plot.title = element_text(size = 12,
                                     face = 'bold',
                                     color = 'steelblue',
                                     lineheight = 1.2,
                                     hjust = 0.5),
           axis.title.x = element_text(size = 10),
           axis.title.y = element_text(size = 10),
           axis.text.x = element_text(size = 10),
           axis.text.y = element_text(size = 10))


# TEXT ANNOTATIONS, FLIP AXES, CUSTOM TEXT
# library(ggplot2)
# data('midwest', package = 'ggplot2)
# head(midwest)

# 1. Adding Labels

# Add labels to counties if their population is > 200,000
gg <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point(aes(col = state, size = popdensity)) +
  geom_smooth(method = 'lm', col = 'firebrick', se = F) +
  xlim(c(0, 0.1)) +
  ylim(c(0, 250000)) +
  labs(title = "Area vs Population", y = "Population", x = "Area")

plot(gg)


# Step 1: Make dataset with filtered rows.
x <- 150000
midwest_sub <- midwest[midwest$poptotal > x, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > x,
                                   midwest_sub$county,
                                   "")


# Step 2: Plot
gg + geom_text(aes(label = large_county),
               size = 2,
               data = midwest_sub)

# Step 2 Option 2: Use geom_label
gg_label <- gg + geom_label(aes(label = large_county),
                            size = 3,
                            data = midwest_sub,
                            alpha = 0.75)
plot(gg_label)


# Step 2 Option 3: Use ggrepel
install.packages('ggrepel')
library(ggrepel)
gg_textrepel <- gg + geom_text_repel(aes(label = large_county),
                                         size = 3, 
                                         data = midwest_sub,
                                         alpha = 0.9)

plot(gg_textrepel)


# Flip axes
gg + coord_flip()


# Reversing scales of axes
gg + scale_x_reverse() + scale_y_reverse()


# 2. Adding custom text --------------------------------------------------------

library(grid)
my_text <- "Graph is draft version and confidential. Do not share"

# Define a grob. Grob is a piece of annotation containing the text
my_grob = grid.text(my_text,
                   x = 0.7,
                   y = 0.9,
                   gp = gpar(col='firebrick', fontsize = 10, fontface = "bold"))

class(my_grob)
gg + annotation_custom(my_grob)


# Mini-challenge

# get data:
gg <- ggplot(data = cars, aes(x = speed, y = dist, size = dist)) +
  geom_point() +
  geom_smooth() +
  labs(title = "cars", x = "speed", y = "Dist")
print(gg)

my_text <- "The faster the car, the longer it takes to stop."
my_grob <- grid.text(my_text, x = 0.3, y = 0.95,
                     gp = gpar(col = 'springgreen4',
                               fontsize = 14,
                               fontface = 'bold'))

gg + annotation_custom(my_grob) + theme(legend.position = "None")


# MANIPULATING LEGEND

# Recreate the first graph
# library(ggplot)
data('midwest', package = 'ggplot2')
head(midwest)

gg <- ggplot(midwest, aes(x = area, y = poptotal)) + # define data
  geom_point(aes(col = state, size = popdensity)) + # add scatterplot
  geom_smooth(method = 'lm', col = 'firebrick', se = F) + # add best fit line
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 250000)) + # limit x and y
  labs(title = "Area vs Population", y = "Population", x = "Area") # labels

# hide legend
gg + theme(legend.position = "none")


# 1. Change Legend title -------------------------------------------------------
gg + labs(color = "State", size = "Density")


# 2. Control the legend position -----------------------------------------------

# outside the plot area
gg + theme(legend.position = 'left')
gg + theme(legend.position = 'bottom')

# inside the plot area
gg + theme(legend.justification = c(1, 0), # hinge point at bottom left
           legend.position = c(1, 0)) # bottom left

gg + theme(legend.justification = c(1,1), # hinge point at top right
           legend.position = c(1,1)) # top right


# 3. Change styling of texts in the legend -------------------------------------
gg <- gg + theme(legend.title = element_text(size = 12, 
                                             color = "firebrick"), # title
                 legend.text = element_text(size = 10),            # text
                 legend.key = element_rect(fill = 'gray')) +       # key
  guides(colour = guide_legend(override.aes = list(size = 2,       # size
                                                   stroke = 1.5))) # boundary
plot(gg)


# Mini Challenge ---------------------------------------------------------------

# Modify the following code to remove the legend title.
# Then, place the legend in top left position

gg <- ggplot(midwest, aes(x = area, y = poptotal)) +
  geom_point(aes(col = state, size = popdensity)) +
  geom_smooth(method = 'lm', col = 'firebrick', se = F) +
  coord_cartesian(xlims = c(0, 0.1), ylim = c(0, 250000)) +
  labs(title = "Area vs Population", y = "Population", x = "Area")

plot(gg)

gg + labs(color = "", size = "") + theme(legend.justification = c(0.01, .99),
                                         legend.position = c(0.01, .99))


# DEALING WITH OVERLAPPING DATAPOINTS WITH JUTTER AND BUBBLE PLOT

# get data
# library(ggplot2)
theme_set(theme_bw())
data(mpg, package = 'ggplot2')
mpg <- read.csv("https://bit.ly/2Ihh7dc")

dim(mpg)
head(mpg)

# Create Scatterpolot for mileage in hway vs mileage in city -------------------
theme_set(theme_bw())

g <- ggplot(mpg, aes(cty, hwy))

titles <- labs(title = "City vs Highway Mileage",
               y = "Highway Mileage",
               x = "City Mileage")

g + geom_point() + titles


# ways to deal with scatterplot with overlapping points ------------------------

# jitter plot
g + geom_jitter(width = .5, size = 1.5) + titles

# count plots
g + geom_count(col = 'steelblue') + titles


# FACETING - DRAW MULTIPLE SMALL PLOTS FOR EACH GROUP --------------------------
# load package
# library(ggplot2)

# get the data
dim(mpg)
head(mpg)
data(mpg, package = 'ggplot2')

# 1. Displacement vs Mileage
# Check how displacement is related to highway mileage

# Displacement measures the volume swept within engine.
# For the same engine - higher displacement means fuel being

g <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

plot(g)


# 2. How this relationship varies for different kinds of vehicles
g + facet_wrap(~ class, scales = 'free')

# Default setting (only 1 row)
g + facet_grid(~ class)



# 3. Two Way Facets
# How hway mileage varies with disp., by cyl. type & manufacturer

g2 <- g + facet_grid(cyl ~ class)
plot(g2)

g1 <- g + facet_grid(manufacturer ~ class)
plot(g1)


# 4. Arranging two plots adjecent to each other
library(gridExtra)
gridExtra::grid.arrange(g1, g2, ncol = 2)


# CREATING BAR CHARTS, BOXPLOTS, TIME SERIES, AND OTHER POPULAR GEOMS

# Get data
library(ggplot2)
theme_set(theme_bw())
data(mpg, package = 'ggplot2')

# Set theme
theme_set(theme_bw())


# 1. Histogram with geom_bar() on categorical variable -------------------------
# Default - Performs Count if only 1 variable is present
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar() + labs(title = "Car Models Counts")


# Change width and colors of bars
g + geom_bar(width = 0.5, fill = 'steelblue')

# Color the bars by class of cars
g + geom_bar(width = 0.5, aes(fill = class)) + labs(title = 'Car Models Count by Segment')

# Place side by side
g + geom_bar(width = 0.5, aes(fill = class), position = 'dodge') +
  labs(title = 'Car Models Count by Segment')

# Use predefined palettes
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info

# set 3 palette
g + geom_bar(width = 0.5, aes(fill = class)) + scale_fill_brewer(palette = 'Set3')

# Spectral palette
g + geom_bar(width = 0.5, aes(fill = class)) + scale_fill_brewer(palette = 'Spectral')


# For proportion (Bar graph to scale of 100%)
g + geom_bar(width = 0.5, aes(fill = class), position = 'fill')


# 2. Histogram with geom_bar() on Continuous variable --------------------------

# Using geom_bar(i.e. the bar chart function)
g <- ggplot(mpg, aes(cty))
g + geom_bar() + labs(title = 'City Mileage Histogram')


# 3. Modify bins using geom_histogram() ----------------------------------------
g + geom_histogram(binwidth = 1)
g + geom_histogram(bins = 100)


# 4. Bar charts with geom_bar(): Use stat = 'identity' -------------------------
# Aggregate data and then draw plots

# Case: I want the mean mileage data at manufacturer level

# Step 1: Aggregate data
cty_mpg <- aggregate(mpg$cty, by = list(mpg$manufacturer), FUN = mean)
colnames(cty_mpg) <- c("Manufacturer", "Mileage")
cty_mpg

# Step 2: Draw the plot
g <- ggplot(cty_mpg, aes(x = Manufacturer, y = Mileage, fill = Mileage))
g + geom_bar(stat = 'identity') + labs(title = 'Bar lot of mean mileage')



# BOXPLOTS

# 1. Visualize the distribution of city mileage using Boxplot ------------------
library(ggplot2)
g <- ggplot(mpg, aes(y = cty))
g + geom_boxplot() + labs(title = 'Box Plot of City Mileage')

# 2. Visualisze the distribution of city mileage by each brand -----------------
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot() + labs(title = 'Box Plot of City Mileage by Brand')

# 3. Color the Box plots by clas -----------------------------------------------
# Honda has highest city mileage. 
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot(aes(fill = class), width = 0.5) +
  labs(title = 'Box Plot of City Mileage by Brand and Segment')

# 4. Violin Plot (Alternate to box plots) --------------------------------------
g + geom_violin(width = 3, aes(fill = class)) +
  labs(title = "Violin Plot of City Mileage by Brand and Segment")

# 5. Boxplot of 'displ' for each manufacturer with each segment.
g <- ggplot(mpg, aes(manufacturer, displ))
g + geom_boxplot(aes(fill = class), width = 0.5)



# CREATING A TIME SERIES PLOTS
# Get 'Airpassenger' data, which is in time series format ----------------------
AirPassengers
class(AirPassengers)

# Classic time series data.
# Monthly totals of international airline passengers, 1949 to 1960.

# Load packages
install.packages('gcookbook')
install.packages('ggfortify')
install.packages('hrbrthemes')
install.packages('usethis')
install.packages('devtools', lib = "C:\\Program Files\\R\\R-4.1.0\\library")
install.packages('zoo')
install.packages('lubridate')

library(ggfortify)
library(ggplot2)
library(zoo)
library(devtools)

# Convert timeseries (ts) to data.frame
ts_data <- data.frame(date = zoo::as.Date(AirPassengers),
                      value = as.numeric(AirPassengers))

ts_data


# 1. geom_line() to draw a time series -----------------------------------------
ggplot(ts_data, aes(date, value)) + geom_line() + labs(title)

# Adjust color, thickness and title
ggplot(ts_data, aes(date, value)) +
  geom_line(color = 'firebrick', size = 1) +
  labs(title = 'AirPassengers') +
  theme(plot.title = element_text(hjust = 0.5))

# 2. Show all years in the axes ------------------------------------------------
brks <- seq.Date(as.Date('1949-01-01'), as.Date('1961-01-01'), by = 'year')
brks

library(lubridate)
lbls <- year(brks)
lbls

# 3. Adjust X axis
ggplot(ts_data, aes(date, value)) +
  geom_line() +
  scale_x_date(breaks = brks, labels = lbls) +
  geom_line(color = 'firebrick', size = 1) +
  labs(title = 'AirPassengers') +
  theme(plot.title = element_text(hjust = 0.5))


# 4. Plot multiple lines for three different data ------------------------------
# Create additional data columns (dummy)
set.seed(100)
ts_data$score1 <- ts_data$value + runif(nrow(ts_data), 50, 100)
ts_data$score2 <- ts_data$value + runif(nrow(ts_data), 50, 100)
head(ts_data)

# Plot all three time sereis (columns)
ggplot(ts_data, aes(x = date)) +
  geom_line(aes(y = value)) +
  geom_line(aes(y = score1)) +
  geom_line(aes(y = score2)) +
  scale_x_date(breaks = brks, labels = lbls)

# Add legends to the time series graph -----------------------------------------
# To add legend, set the col (color) arg in the respective graphs
ggplot(ts_data, aes(date)) +
  geom_line(aes(y = value, col = 'Y')) +
  geom_line(aes(y = score1, col = 'Economic Score')) +
  geom_line(aes(y = score2, col = 'Development Score')) +
  scale_x_date(breaks = brks, labels = lbls)
