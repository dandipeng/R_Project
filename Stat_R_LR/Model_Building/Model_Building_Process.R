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
plot(fit1, which=4) # Cook's distanct
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

# STA206_HW6
# (a)
n <- length(property[,1])
sample_mean <- c(0,0,0,0,0)
sample_sd <- c(0,0,0,0,0)
for(i in c(1,2,3,4,5)) {
  sample_mean[i] <- mean(property[,i])
  sample_sd[i] <- sd(property[,i])
  print(paste("Sample mean of", names(property)[i], "is" ,sample_mean[i]))
  print(paste("Sample sd of", names(property)[i], "is" ,sample_sd[i]))
}

# Standardized X
X_star <- as.matrix(cbind(rep(1,n), property[,2:5]))
names(X_star)[1] = "1"
for(i in c(2,3,4,5)) {
  X_star[,i] <- (1/sqrt(n-1))*((X_star[ ,i] - sample_mean[i])/sample_sd[i])
}

# multicollinearity : VIF = 1/(1-R_k^2)
X_star_sq <- t(X_star)%*%X_star
r_inverse <- solve(X_star_sq)[2:5,2:5]
r_sq_1 <- summary(lm(formula=age~operation_expense+vacancy_rate+footage, data=property))$r.squared
r_sq_2 <- summary(lm(formula=operation_expense~age+vacancy_rate+footage, data=property))$r.squared
r_sq_3 <- summary(lm(formula=vacancy_rate~age+operation_expense+footage, data=property))$r.squared
r_sq_4 <- summary(lm(formula=footage~age+operation_expense+vacancy_rate, data=property))$r.squared
c(r_inverse[1,1], r_inverse[2,2], r_inverse[3,3], r_inverse[4,4])

# VIF (>1 a little bit exploded but <10, then it is okay)
c(1/(1-r_sq_1),1/(1-r_sq_2),1/(1-r_sq_3),1/(1-r_sq_4))
# 1.240348 1.648225 1.323552 1.412722

# train vs. test split
# split data into two halves
set.seed(100)
n = nrow(property)/2 # equally split
ind = sample(1:(2*n), n, replace=FALSE)
train = property[ind, ]  #training set
valid = property[-ind, ]  #validation set

##### Stepwise Regression
none_mod = lm(y~1, data = train)
full_mod = lm(y~., data = train)
library(MASS)
stepAIC(none_mod, scope=list(upper = full_mod), direction="forward", k=log(n))
?stepAIC


none_mod = lm(y~1, data=train)
full_mod = lm(y~sqf+bedrm+bathrm+garage+factor(AC)+factor(pool)+factor(quality)+factor(style)+factor(hw), data=train)
#forward selection based on AIC
library(MASS)         
stepAIC(none_mod, scope=list(upper=full_mod), direction="forward", k=2,trace = F)
#backward elimination based on AIC
stepAIC(full_mod, direction="backward", k=2)
#forward stepwise selection based on AIC
stepAIC(none_mod, scope=list(upper=full_mod), direction="both", k=2)

#selection based on BIC
stepAIC(none_mod, scope=list(upper=full_mod), direction="forward", k=log(n))
stepAIC(full_mod, direction="backward", k=log(n))
stepAIC(full_mod, scope=list(upper=full_mod), direction="both", k=log(n))

stepAIC(none_mod, scope=list(lower=lm(y~sqf+bedrm,data=train),upper=full_mod), direction="both", k=log(n))

##### Model Validation
#BIC model
train1 = lm(y ~ sqf + garage + factor(quality), data = train)
valid1 = lm(y ~ sqf + garage + factor(quality), data = valid)

mod_sum = cbind(coef(summary(train1))[,1], coef(summary(valid1))[,1], coef(summary(train1))[,2], coef(summary(valid1))[,2])
colnames(mod_sum) = c("Train Est","Valid Est","Train s.e.","Valid s.e.")
mod_sum

sse_t = sum(train1$residuals^2)
sse_v = sum(valid1$residuals^2)
Radj_t = summary(train1)$adj.r.squared
Radj_v = summary(valid1)$adj.r.squared
train_sum = c(sse_t,Radj_t)
valid_sum = c(sse_v,Radj_v)
criteria = rbind(train_sum,valid_sum)
colnames(criteria) = c("SSE","R2_adj")
criteria

#Get MSPE from new data
newdata = valid[, -1]
y.hat = predict(train1, newdata)

MSPE = mean((valid$y - y.hat)^2)
MSPE
sse_t/n

##### Model Diagnostics
#check outliers in Y
#studentized deleted residuals
fit = lm(y ~ sqf + garage, data=train)
stu.res.del = studres(fit)
head(sort(abs(stu.res.del), decreasing=TRUE))

p = 3
qt(1-.1/(2*n), n-p-1) #Bonferroni's Threshold (alpha=0.1, n=sample size)

#check outliers in X
h = as.vector(influence(fit)$hat)
index.X = which(h>(2*p/n))
index.X #17 outliers

#Cooks distance
res = fit$residuals
mse = anova(fit)["Residuals", 3]
cook.d = res^2*h/(p*mse*(1-h)^2)

sort(cook.d[index.X], decreasing = TRUE)
4/(n-p)
sort(cook.d[index.X], decreasing = TRUE) > 4/(n-p)

par(mfrow = c(1,1))
plot(fit1, which=4)
plot(fit1, which=5)

