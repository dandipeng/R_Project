# R Project Review
# get directory
getwd()
# reset directory
setwd('Documents/Programming/R_Project/STA141A/')
# list files/directories in a directory
list.files()
setwd('..') # set directory to the parent one
list.dirs()
# reset to what we want
setwd('STA141A/')
# list all items in current global environment
ls()
# read data inside
dogs <- readRDS('dogs_full.rds')

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
dogs <- readRDS('dogs_full.rds')

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
