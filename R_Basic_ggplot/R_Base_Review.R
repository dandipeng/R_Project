# R Project Review
# get directory
getwd()
# reset directory
setwd('Documents/Programming/R_Project/R_Basic_ggplot/')
# list files/directories in a directory
list.files()
setwd('..') # set directory to the parent one
list.dirs()
# reset to what we want
setwd('R_Basic_ggplot/')
# list all items in current global environment
ls()
# read data inside
dogs <- readRDS('data/dogs/dogs_full.rds')

# dataset structure
head(dogs,5)
nrow(dogs)
ncol(dogs)
dim(dogs)

# first scanning
str(dogs) # most useful one in my opinion
sapply(dogs, typeof)
summary(dogs)
?IQR # interquartile range
IQR(dogs$weight,na.rm = T) # = quantile(dogs$weight,3/4,na.rm=T)-quantile(dogs$weight,1/4,na.rm=T)
dogs[order(dogs$weight),]
sort(dogs$weight)
?order
?na.omit
na.omit(dogs[order(dogs$weight),]$weight)
sort(rivers)
IQR(rivers)
quantile(rivers)

# count numbers for categorical numbers
table(dogs$size)

# select out rows or columns
dogs[, "breed"]
dogs[c(1:3,5),]
dogs[1:3,-c(1:12)]

library(ggplot2)
ggplot(data=dogs, aes(x=datadog, y =popularity,color=group))+geom_point()+
  geom_text(aes(label=breed),hjust='left',vjust='top') + scale_y_reverse()+
  labs(title = "Best in Show", x = "Computed DataDog Score",
       y = "AKC Popularity Ranking")+
  guides(color = guide_legend(title='Dog Type'))

# The default stat for points is "identity", which doesn't transform the data
# at all.
#
# In this case we want to count up the groups, so instead of "identity", we use
# the default stat from geom_bar, which is "count".
ggplot(dogs, aes(x = group)) + geom_point(stat = "count")




# When you make a vector, R converts to the highest type (see below)
typeof(c(5, "hello", 4.1))

# Type Hierarchy
# --------------
# lists -- list()
# character -- "hello"
# complex -- 1+4i, ...
# double -- 3.1, 5.222, ...
# integers -- 1L, 2L, ...
# logical -- TRUE, FALSE
# ------------
# Outside the hierarchy:
# functions -- mean, median, typeof, ...

# all() : Given a set of logical vectors, are all of the values true?
all(dogs[[4]] == dogs$popularity_all)

dogs[[4]][[3]] # same as dogs[[3, 4]]

# You can see the class(es) of an object with class():
class(dogs)

# You can remove the class(es) of an object with unclass().
#
# unclass() lets you see how the object looks "under the hood".
#
# So if we unclass() a data frame, we can see that it looks like a named list.
unclass(dogs)

# Create A named list, for comparison:
list(x = 1, y = 2)


# rename levels
# Get category names with levels():
levels(dogs$size)
dogs$size[20:30]
# Rename categories:
levels(dogs$size) = c("HUGE", "Medium", "Small") # this will correspond to the output of levels(dogs$size)
levels(dogs$size)
dogs$size[20:30]
dogs <- readRDS('data/dogs/dogs_full.rds')

# Right way to reorder levels:
size_fix = factor(dogs$size, c("small", "medium", "large"))

# Make table() show NA as a category:
table(dogs$kids) # this will ignore NA
table(dogs$kids, useNA = "always")

?fivenum # Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) for the input data
fivenum(dogs$datadog)

# A boxplot shows Tukey's five number summary graphically:
ggplot(dogs, aes(y = datadog)) + geom_boxplot()

# A histogram cuts the data into bins, and shows the count for each bin.
ggplot(dogs, aes(x = datadog)) + geom_histogram()

# A density plot is a smoothed histogram.
ggplot(dogs, aes(x = datadog)) + geom_density()

ggplot(dogs, aes(x = datadog)) + geom_density(bw = 0.01)

# show skewed situation
mu = mean(dogs$datadog, na.rm = T)
m = median(dogs$datadog, na.rm = T)
ggplot(dogs, aes(x = datadog)) + geom_density() +
  geom_vline(aes(xintercept = m)) +
  geom_vline(aes(xintercept = mu), color = "red") +
  geom_text(aes(x = m, y = 0.2, label = 'median'), angle=90, vjust = 1, text=element_text(size=11))+
  geom_text(aes(x = mu, y = 0.2, label = 'mean'),color = 'red', angle=90, vjust = -0.4, text=element_text(size=11))

# pivot table
# (categorical, categorical) -> frequencies
#
# Similar to the univariate case!
#
tbl = table(size = dogs$size, group = dogs$group)
tbl
# add sum column and row
addmargins(tbl)

# give proportion
prop.table(tbl) # total proportions
prop.table(tbl, margin = 1) # proportions row-wise
prop.table(tbl, margin = 2) # proportions column-wise

# A 2d density plot shows where most of the points are at.
ggplot(dogs, aes(height, weight)) + geom_density2d() + geom_point()

# We can also check for linear relationship with correlation:
cor(dogs$height, dogs$weight, use = "complete.obs") # to avoid NA 

ggplot(dogs, aes(x = size, y = height)) + geom_boxplot()


# Aggregation -- computing statistics on groups

#         |-----------|-- relationship of interest
#         v           v     v---- data
aggregate(height ~ size, dogs, mean, na.rm = TRUE)
#                               ^---- statistic


# You can group by more than one categorical variable:
aggregate(height ~ size + grooming, dogs, mean, na.rm = TRUE)
#                  ^      ^
#                  |------|-------- compute statistic for all combinations


# Alternative syntax:
aggregate(dogs$height, list(dogs$size, dogs$grooming), mean)

aggregate(dogs$height, dogs[c("size", "grooming")], mean)

# Dog height distributions, faceted by size:
ggplot(dogs, aes(height)) + geom_histogram() + facet_wrap(~ size)

# Dog height distributions, faceted by grooming needs and size:
ggplot(dogs, aes(height)) + geom_histogram() + facet_grid(grooming ~ size)

# If we use a density plot, how can we display the groups?
ggplot(dogs, aes(color = group, height)) + geom_density()

# Too many lines! We can use a ridge plot instead to show many densities at
# once.
library(ggridges)

ggplot(dogs, aes(x = height, y = group)) + geom_density_ridges()

# Putting ggplots side-by-side:
g1 = ggplot(anscombe, aes(x1, y1)) + geom_point()
g2 = ggplot(anscombe, aes(x2, y2)) + geom_point()
g3 = ggplot(anscombe, aes(x3, y3)) + geom_point()
g4 = ggplot(anscombe, aes(x4, y4)) + geom_point()
library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

# If we want to make our own bins:
#   * cut() -- built-in
#   * cut_interval(), cut_number(), cut_width() -- in ggplot2
# cut continuous variables into several intervals



# match(M,N) method: give indexes of M in N
match(c("A","D"),c("A","B","C","D","E")) # 1 4
match(c("A","D"),c("A","B","C","D","E")) # 1 NA


#read csv
air = read.csv('data/airlines/2018.01_air_delays.csv', header = T)

head(air)
names(air)
dim(air)
summary(air)
str(air)

# quite messy for every variables
day_of_week = factor(air$DAY_OF_WEEK)
class(air$DAY_OF_WEEK)

days = read.csv("data/airlines/L_WEEKDAYS.csv_")
days
str(days)
levels(day_of_week)
m = match(day_of_week, days$Code)

day_of_week = days$Description[m]

air$DAY_OF_WEEK = day_of_week


air$ontime <- air$ARR_DELAY<=0
ggplot(air,aes(ontime, fill = OP_UNIQUE_CARRIER))+
  geom_bar(position = "dodge")
ggplot(air,aes(ontime, fill = OP_UNIQUE_CARRIER))+
  geom_bar(position = "stack")

air_complete = air[complete.cases(air), ]
str(air_complete) # no observations
str(air[rowSums(is.na(air['WEATHER_DELAY']))==0,]) # exclude the NA noise

# What kinds of questions can we ask (or answer) with the airlines data?
#
# * What airports are most likely to have delays? Or least likely?
aggregate(height ~ size + grooming, dogs, mean, na.rm = TRUE)
num_delay_air <- aggregate(DAY_OF_WEEK~OP_UNIQUE_CARRIER,air,FUN=length)
num_delay_air <- num_delay_air[order(num_delay_air$DAY_OF_WEEK),]
ggplot(num_delay_air,aes(x=OP_UNIQUE_CARRIER,y=DAY_OF_WEEK))+
  geom_bar(stat='identity')

ggplot(air,aes(x=ontime,fill=OP_UNIQUE_CARRIER))+geom_bar(position='dodge')
check_num_delay <- table(air$ontime,air$OP_UNIQUE_CARRIER)
colnames(check_num_delay)[which(order(check_num_delay[2,],decreasing = T)==1)]

# * Check for seasonal delays (but we would need data on more months)
# * What area or region is most likely to have delays?
# * What are the main causes of delay?
#     * How often does weather cause a delay?
# * Does a delay on one flight cause later delays (for the same plane)?
na.omit(air)