# logistic Regression
library(readxl)
sparrow = read_excel('survival_sparrow.xls')
# 1. A description of the data and the goal of the analysis.
sparrow <- as.data.frame(sparrow)
str(sparrow)
class(sparrow)
sparrow$STATUS = factor(sparrow$STATUS)
sparrow$AG = factor(sparrow$AG)
summary(sparrow)
# check the relationship among quantitative predictor variables
pairs(sparrow[,3:11])
cor(sparrow[,3:11])
# check the distribution of quantitative predictor variables to response variable
par(mfrow=c(2,2))
apply(sparrow[,3:11], 2, function(x) boxplot(x~sparrow[,1], xlab = 'STATUS'))
for (i in 3:11){
  boxplot(sparrow[,i]~sparrow[,1], xlab = 'STATUS', ylab = names(sparrow)[i],
          main=paste('boxplot of STATUS to ',names(sparrow)[i]))
}

# check the distribution of quantitative predictor variables to quantitative predictor variable
for (i in 3:11){
  boxplot(sparrow[,i]~sparrow[,2], xlab = 'AG', ylab = names(sparrow)[i],
          main=paste('boxplot of AG to ',names(sparrow)[i]))
}

# check the relationship between age qualitative variable and response variable
library(gmodels)
joint = CrossTable(sparrow$STATUS,sparrow$AG,prop.chisq = FALSE)
par(mfrow=c(1,1))
STATUS_given_AGE = joint$prop.col
barplot(STATUS_given_AGE, col = rainbow(2), ylab = 'Proportion of status given age', xlab = 'AGE')
legend('topright', inset = c(.05,.01),c('Perished','Survived'),pch = 15, col=rainbow(2),bty='o', bg='white')
# change the column name
names(sparrow) = c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10')
# change y to 0 - 1
sparrow$y = ifelse(sparrow$y == 'Survived', 1, 0)
sparrow$y = factor(sparrow$y)
sparrow$x1 = factor(sparrow$x1)
# check the type of variables
str(sparrow)
# check the initial data
summary(sparrow)
# logistic regression
model_full= glm(y~., data=sparrow, family=binomial)
model_initial = glm(y~1, data = sparrow, family = binomial)
summary(model_full)
# # check the VIF
library(car)
vif(model_full)
# forward stepwise
step(model_initial, direction = 'forward',
     scope = list(lower = model_initial, upper = model_full))
# backward stepwise
step(model_full,direction = 'backward',
     scope = list(lower = model_initial, upper = model_full))
# stepwise
step(model_full,direction = 'backward',
     scope = list(lower = model_initial, upper = model_full))
# create the table for selection
sparrow_2 = sparrow[,c('y','x1','x2','x4','x6','x10')]
# model full & model initial
model2_full = glm(y~.*.,data = sparrow_2, family = binomial)
model2_initial = glm(y~1,data = sparrow_2, family = binomial)
# forward stepwise with AIC
step(model2_initial, direction = 'forward',
     scope = list(lower = model2_initial, upper = model2_full))
# backward stepwise
step(model2_full,direction = 'backward',
     scope = list(lower = model2_initial, upper = model2_full))
# stepwise
final_model = step(model2_full,direction = 'both',
                   scope = list(lower = model2_initial, upper = model2_full), trace = 0)
fnl_model = glm(y~x2+x4+x6+x10, data = sparrow_2, family = binomial)
# check the VIF
vif(fnl_model)
# summary the final model
summary(fnl_model)
# get the basic diagnostic plots
par(mfrow=c(2,2))
plot(fnl_model)
# plot pearson and deviance residuals against fitted values respectively
par(mfrow=c(2,2))
residual_pearson = resid(fnl_model,type = 'pear')
residual_devience = resid(fnl_model)
plot(fitted(fnl_model),residual_pearson,main = 'Pearson Residual Plot',xlab = 'Fitted values',ylab = 'Pearson residuals')
plot(fitted(fnl_model),residual_devience,main = 'Deviance Residual Plot',xlab = 'Fitted values',ylab = 'Deviance residuals')
# obtain half normal plot
library(faraway)
halfnorm(residual_pearson,ylab = 'Pearson Residuals')
halfnorm(residual_devience,ylab = 'Deviance Residuals')
# get the fitted value and convert it to 1 when it's larger than 0.5,
# convert it to 0 when it's less than 0.5
sparrow_2['fitted'] = fitted(fnl_model)
sparrow_2['fitted'] = ifelse(sparrow_2['fitted'] >0.5,1,0)
sparrow_2$fitted = factor(sparrow_2$fitted)
# get the correct ratio
sum(sparrow_2$y == sparrow_2$fitted)/length(sparrow_2[,1])