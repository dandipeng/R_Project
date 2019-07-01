# GLM
library(pscl)
library(MASS)
# poisson
glm.D93 = glm(counts ~ outcome + treatment, family=poisson())
# ngeative binomial
gsa_neg = glm.nb(Pre_SGMA~.,data = GSA_complete)
# Gamma
gsa_gamma=glm(Pre_SGMA+1 ~., family = Gamma(link = log), data = GSA_complete)

# Logistic regression
sparrow = read_excel('survival_sparrow.xls')
# 1. A description of the data and the goal of the analysis.
sparrow <- as.data.frame(sparrow)
str(sparrow)
class(sparrow)
sparrow$STATUS = factor(sparrow$STATUS)
sparrow$AG = factor(sparrow$AG)
# rename the variables
names(sparrow) = c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10')
# change y to 0 - 1
sparrow$y = ifelse(sparrow$y == 'Survived', 1, 0)
sparrow$y = factor(sparrow$y)
sparrow$x1 = factor(sparrow$x1)

# # check the VIF
library(car)
vif(model_full)

# bulit the full logistic regression
model_full= glm(y~., data=sparrow, family=binomial)
model_initial = glm(y~1, data = sparrow, family = binomial)
stepAIC(model_initial, list(upper = model_full), trace = F, direction = 'both', k = 2)

# correlation analysis

# feature engineering

# clustering

# bootstrap

# time series forecast

# XGboost


# Gradient Descent
### 1-c
X = c(-13.87,-2.53,-2.44,-2.40,-1.75,-1.34,-1.05,
      -0.23,-0.07,0.27,1.77,2.76,3.29,3.47,3.71,
      3.8,4.24,4.53,43.21,56.75)
theta = seq(-50,50,by=.01)
loglkhd = c()
n=length(X)
i = 1
for (j in theta) {
  l_theta = -n*log(pi)
  for (xi in X){
    l_theta = l_theta -log(1+(j-xi)^2)
  }
  loglkhd[[i]] = l_theta
  i = i+1
}
log_frame = data.frame(x=theta, y = loglkhd)
plot(log_frame$x,log_frame$y,type = 'l',xlab = expression(theta),
     ylab = 'loglikelihood')

#### 1-d
# estimate theta from the question
theta = c(-11, -1, 0, 1.4, 4.1, 4.8, 7, 8, 38)

# fx1 to get the first derivative
fx1 = function(x0){
  return(-2*sum((x0-X)/(1+(x0-X)^2)))
}

# fx2 to get the second derivative
fx2 = function(x0){
  return(-2*sum((1-(x0-X)^2)/((1+(x0-X)^2)^2)))
}

#Newton's method
Newton = function(theta){
  n = 0
  x0 = theta
  print(paste('initial is', x0))
  abschange = 1
  fx_1 = fx1(x0)
  
  # given conditions that the first derivative is not equal to 0;
  # the h(t) (change) is larger than .00001;
  # the iteration times is less than 10000
  while (fx_1 !=0 & abschange>.00001 & n<10000){
    fx_1 = fx1(x0)
    fx_2 = fx2(x0)
    # as the second derivative may be 0 during the iterations,
    # when it happens, a simple way is adopted to make the numerator of 
    # the first derivative as the new function fx_1 = sum(X_i -X), 
    # and play the same method, the second derivative fx_2 becomes n.
    if (fx_2 != 0){
      change = fx_1/fx_2
      abschange = abs(change)
    } else {
      fx_1 = sum(x0-X)
      fx_2 = length(X)
      change = fx_1/fx_2
      abschange = abs(change)
    }
    x0 = x0 - change
    n = n + 1
  }
  print(paste('Newton Estimate:',x0,'; iterations:',n))
  return(c(x0, n))
}

for (i in 1:length(theta)){
  Newton(theta[i])
}

