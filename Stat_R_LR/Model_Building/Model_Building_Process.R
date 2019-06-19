setwd('/Documents/Programming/R_Project/Stat_R_LR/Model_Building/')
# 1. Data collection and processing -----------

# 2. Exploratory data analysis ----------
# 2.1 Distribution of each variable: symmetric or skewed? outliers?
#     Quantitative: histogram, boxplot, summary statistics, etc.
#     Qualitative: pie chart, frequency table, etc.


# 2.2 Relationships among variables.
#     scatter plot matrix, correlation matrix,
pairs()
cor()
#     nonlinear pattern? clusters? outliers?

# 3. Preliminary model investigation ---------
# 3.1 Residual plots based on initial fits:
#     -> nonlinearity? departure from Normality? nonconstant error variance?
#     -> transformations needed?
#     -> omission of important predictors/interaction terms/high-order power terms?
fit1 <- lm(Y~., data = data)

#     -> transformations needed?
library(MASS)
boxcox(fit1)
# 3.2 The goal is to decide on:
#     -> Functional forms in which variables should enter the regression model.
#     -> Potential pool of predictors, interactions and higher-order powers to be considered in subsequent analysis.
# 3.3 This process should be aided by prior knowledge and domain expertise if possible.

# 4. Model selection ---------
#    Why is there a need for model selection?
#    -> Models with many X variables tend to have large sampling
#       variability. They are also hard to maintain and interpret.
#    -> On the other hand, omission of key X variables leads to biased 
#       fitted regression functions and predictions.
#    The goal of model selection is to choose a subset of X
#    variables which balances between model variance and bias,
#    i.e., achieves bias-variance trade-off.

# 5. Model diagnostic and validation --------






# Examples:
# 206-HW5-5
property <- read.table(file = 'hw5_property.txt')
colnames(property) <- c('Y','X1','X2','X3','X4')
# colnames(property) <- c('rental_rates', 'age', 'operation_expense', 'vacancy_rate', 'footage')
str(property)
dim(property)

# response variable
par(mfrow = c(2,2))
hist(property$rental_rates)
hist(log(property$rental_rates))
hist(sqrt(property$rental_rates))
hist(1/property$rental_rates)

# looks like the original one works best
# keep going, seems to be unnecessary to do transformation
# (1)
sapply(property[,2:ncol(property)],hist)
# age: bimodal; operantion_expense: left-skewed; vacancy_rate: right-skewed; footage: right-skewed
sapply(property[,2:ncol(property)],summary)
apply(property,MARGIN = 2,summary) # MARGIN chosen from c(1,2), which corresponds to rows and columns

# (2) scatter_plots: no clear non-linearity
pairs(property)
cor(property)
# X1 and X3, X2 and X3, X1 and Y are negatively correlated, X3 and X4, X3 and Y
# are not much correlated, other pairs are moderately positively correlated.

# (3) additive models
fit1 <- lm(rental_rates~., data = property)
summary(fit1)
# MSE = (1.137)^2 = 1.293
anova(fit1)

# with one interaction
fit2 <- lm(rental_rates~.+ age:operation_expense, data = property)
summary(fit2)

# cover all interactions
fit3 <- lm(rental_rates~(.)^2, data = property)
summary(fit3)

# (4)
plot(fit1)
boxplot(fit1$residuals,ylim = c(-2.5,2))
# Q-Q plot shows to be a little heavy tailed.

# (5) predictor variables vs. residuals
apply(property[,colnames(property)[2:5]],2, function(x) plot(x, fit1$residuals))
#   -> no obvious pattern
property[,2] * property[,3]
for (i in c(2,3,4)){
  for (j in c((i+1):5)){
    interactions <- property[,i]* property[,j]
    plot(interactions, fit1$residuals, xlab = paste(colnames(property)[i],'*',colnames(property)[j]))
    abline(h=0,lty = 3)
  }
}

# get predictions
newX = data.frame(age=10, operation_expense=8.8, vacancy_rate=0.18, footage=75080)
predict(fit1, newX, interval="confidence", level=0.99, se.fit=TRUE)
predict(fit1, newX, interval="prediction", level=0.99, se.fit=TRUE)

# Test for whether a regression relation exists here
# H_0: beta_1=beta_2=...=beta_4=0
# H_a: At least one of them is not zero.
n = nrow(property)
p = ncol(property)
anova(fit1)
SSR_fit = sum(anova(fit1)[,2][-5])
SSE_fit = anova(fit1)[,2][5]
MSR_fit = SSR_fit/p
MSE_fit = SSE_fit/(n-p-1)
F_test_val = MSR_fit/MSE_fit # ~ F(p, n-p-1)
pf(q=0.99,df1 = p,df2 = n-p-1)
F_test_val # larger than F(p, n-p-1), reject the null.

