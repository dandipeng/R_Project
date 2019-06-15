#read data and load library
setwd("D:/R/R-HW")
library("MASS")
library("dplyr")
library("ggplot2")
library("maps")

data = read.csv("calif_penn_2011.CSV", header=TRUE)

colnames(data) = tolower(colnames(data))

gre = function(X){
    unlist(strsplit(X,", +"))[2]
}


# data description
str(data)

dropcol = c("x","geo.id2","tractce")
data1 = data[,-which(names(data)%in%dropcol)]
data1[,c("statefp","countyfp")] <- lapply(data1[,c("statefp","countyfp")] , factor)
data1$geo.display.label = as.character(data1$geo.display.label )
data1$county = unlist(lapply(data1$geo.display.label,gre))

ca = subset(data1,statefp=="6")
length(unique(ca$countyfp))

Bay_Area = c("Alameda County", "Contra Costa County", "Marin County", "Monterey County", 
             "Napa County", "San Benito County","San Francisco County", "San Mateo County", "Santa Clara County", 
             "Santa Cruz County", "Solano County", "Sonoma County")
Southern = c("Imperial County", "Kern County", "Orange County", "Riverside County", "San Bernardino County", 
             "San Diego County", "San Luis Obispo County", "Santa Barbara County", "Ventura County") 
Los_Angeles = c("Los Angeles County")
Central = c("Alpine County","Amador County","Calaveras County","El Dorado County","Fresno County","Inyo County","Kings County",
            "Madera County","Mariposa County","Merced County","Mono County","Placer County","Sacramento County",
            "San Joaquin County","Stanislaus County","Tuolumne County","Tulare County","Yolo County","Yuba County","Sutter County")
Superior = c("Butte County","Colusa County","Del Norte County","Glenn County","Humboldt County","Lake County","Lassen County"
             ,"Mendocino County","Modoc County","Nevada County","Plumas County","Shasta County","Sierra County",
             "Siskiyou County","Tehama County","Trinity County")

data1$county[which(data1$county %in%Los_Angeles)] ="LA"
data1$county[which(data1$county %in%Bay_Area)] ="Bay_Area"
data1$county[which(data1$county %in%Southern)] ="Southern"
data1$county[which(data1$county %in%Central)] ="Central"
data1$county[which(data1$county %in%Superior)] ="Superior"
data1$county = factor(data1$county)

dropcol = c("countyfp","latitude","geo.display.label","longitude")
data1 = data1[,-which(names(data1)%in%dropcol)]
dim(data1)

data2=data1
data2$vacant_ratio = data2$vacant_units/data2$total_units
drop1 = c("vacant_units")
data2 = data2[,-which(names(data2)%in%drop1)]
drop2 = c("bedrooms_5_or_more")
data3 = data2[,-which(names(data2)%in%drop2)]

head(data3)
dim(data3)

drop3 = c("built_1939_or_earlier")
data4.1 = data3[,-which(names(data3)%in%drop3)]

a = c(grep("built",colnames(data3)))
cname = colnames(data3)
colyear = cname[a]
colyear

data4.2 = data3[,-which(names(data3)%in%drop3)]
data4.2$built_1950_or_earlier = data4.2$built_1940s+data4.2$built_1950s
data4.2$built_1960_to_1970 = data4.2$built_1970s+data4.2$built_1960
data4.2$built_1980s_to_1990s = data4.2$built_1990s+data4.2$built_1980s
data4.2$built_after_2000 = data4.2$built_2000_to_2004+data4.2$built_2005_or_later

data4.2 = data4.2[,-which(names(data4.2)%in%colyear)]

head(data4.2)
dim(data4.2)

ca4.1 = data4.1[data4.1$statefp == "6",]
#pe4.1 = data4.1[data4.1$statefp == "42",]
ca4.2 = data4.2[data4.1$statefp == "6",]
#pe4.2 = data4.2[data4.1$statefp == "42",]
ca4.1 = ca4.1[,-1]
#pe4.1 = pe4.1[,-1]
ca4.2 = ca4.2[,-1]
#pe4.2 = pe4.2[,-1]
ca4.1$county = factor(ca4.1$county)
ca4.2$county = factor(ca4.2$county)

head(ca4.1)
dim(ca4.1)
head(ca4.2)
dim(ca4.2)

colSums(sapply(ca4.1, is.na))
colSums(sapply(ca4.2, is.na))

ca5.1 = ca4.1[!is.na(ca4.1$median_house_value),]
ca5.2 = ca4.2[!is.na(ca4.2$median_house_value),]

ca5.1 = na.omit(ca5.1)
rownames(ca5.1) <- NULL
ca5.2 = na.omit(ca5.2)
rownames(ca5.2) <- NULL

colSums(sapply(ca5.1, is.na))

dim(ca5.1)
dim(ca5.2)

#transform responsible variable
par(mfrow = c(2,2))
hist(ca5.1$median_house_value)
hist(sqrt(ca5.1$median_house_value))
hist(1/(ca5.1$median_house_value))
hist(log(ca5.1$median_house_value))

ca6.1 = ca5.1
ca6.1$median_house_value = sqrt(ca6.1$median_house_value)
ca6.2 = ca5.2
ca6.2$median_house_value = sqrt(ca6.2$median_house_value)

#explore variable  for ca6.1
target = "median_house_value"
factors = "county"
cname = colnames(ca6.1)
quanti1 = cname[! (cname %in% factors)]
xquanti1 = cname[!((cname %in% target) | (cname %in% factors))]
cname = colnames(ca6.2)
quanti2 = cname[! (cname %in% factors)]
xquanti2 = cname[!((cname %in% target) | (cname %in% factors))]

corma1 = cor(ca6.1[quanti1])
corma1
corma2 = cor(ca6.2[quanti2])
corma2

dropcol5 = c("renters","median_household_income")
ca7.1 = ca6.1[,-which(names(ca6.1)%in%dropcol5)]
cname = colnames(ca7.1)
quanti = cname[! (cname %in% factors)]
xquanti = cname[!((cname %in% target) | (cname %in% factors))]

ca7.2 = ca6.2[,-which(names(ca6.2)%in%dropcol5)]
cname = colnames(ca7.2)
quanti = cname[! (cname %in% factors)]
xquanti = cname[!((cname %in% target) | (cname %in% factors))]

head(ca7.1)
colnames(ca7.1)
head(ca7.2)
colnames(ca7.2)

n = nrow(ca7.1)

ca8.1 = data.frame(scale(ca7.1[,!colnames(ca7.1) == "county"]))/sqrt(n-1)
ca8.1$county = ca7.1$county
ca8.1$median_house_value = ca7.1$median_house_value

ca8.2 = data.frame(scale(ca7.2[,!colnames(ca7.2) == "county"]))/sqrt(n-1)
ca8.2$county = ca7.2$county
ca8.2$median_house_value = ca7.2$median_house_value

head(ca8.1)
head(ca8.2)

#for quantitative variables
fac = sapply(ca8.1, class) == "factor"
n =dim(ca8.1)[1]
lbls=c("Bay_Area","Central" ,"LA","Southern","Superior")
pct=round(100*table(ca8.1$county)/n)
lab=paste(lbls,pct)
lab=paste(lab,"%",sep="")

pie(table(ca8.1$county),labels=lab,main="pie chart of county")

barplot(table(ca8.1$county),main="barplot of county")

boxplot(ca8.1$median_house_value~ca6.1$county)

#explore quanti
par(mfrow = c(3,3))
for(i in 1:ncol(ca8.1)){
  
  if(!(class(ca8.1[1,i]) == "factor")){
    main1 = paste("Histogram of", names(ca8.1)[i])
    hist(ca8.1[,i], xlab = names(ca8.1)[i], main = main1)
  }
}


par(mfrow = c(3,3))
for(i in 1:ncol(ca8.2)){
  
  if(!(class(ca8.2[1,i]) == "factor")){
    main1 = paste("Histogram of", names(ca8.2)[i])
    hist(ca8.2[,i], xlab = names(ca8.2)[i], main = main1)
  }
}

set.seed(666)
nobs = nrow(ca8.1)
train = sample(nobs,0.7*nobs)
test = setdiff(seq_len(nobs),train)
ca8.1train = ca8.1[train,]
ca8.1test = ca8.1[test,]

set.seed(666)
nobs = nrow(ca8.2)
train = sample(nobs,0.7*nobs)
test = setdiff(seq_len(nobs),train)
ca8.2train = ca8.2[train,]
ca8.2test = ca8.2[test,]

#Second Order model1 with outlier
fitnone = lm(median_house_value~1,data=ca8.1train)
fitfull = lm(median_house_value~.^2,data = ca8.1train)
fit1=stepAIC(fitnone,scope=list(upper=fitfull, lower=~1), direction="both", k=log(n))
fit1$anova

summary(fit1)

anova(fit1)

## find X outliers
res=residuals(fit1)# residuals of the final model
n = nrow(ca8.1train)
p1 = 49
h1 = influence(fit1)$hat
d.res.std=studres(fit1) #studentized deleted residuals
idx.X = as.vector(which(h1>(2*p1/n)))
length(idx.X)
#plot(h1,res,xlab="leverage",ylab="residuals")

ca8.1train_X=ca8.1train[-idx.X,]

#Second Order model1 without outlierX
fit1.2 = lm(formula = median_house_value ~ mean_household_income + median_rooms + 
    county + owners + built_1980s + built_2005_or_later + mean_household_size_renters + 
    built_1990s + bedrooms_0 + built_2000_to_2004 + built_1970s + 
    bedrooms_3 + bedrooms_2 + bedrooms_1 + bedrooms_4 + mean_household_size_owners + 
    mean_household_income:county + mean_household_income:mean_household_size_renters + 
    county:owners + median_rooms:bedrooms_2 + mean_household_income:bedrooms_3 + 
    median_rooms:owners + mean_household_income:built_1970s + 
    mean_household_income:bedrooms_2 + mean_household_income:owners + 
    median_rooms:bedrooms_0 + bedrooms_1:bedrooms_4 + owners:mean_household_size_owners + 
    mean_household_size_renters:bedrooms_3 + bedrooms_3:bedrooms_1 + 
    built_2005_or_later:bedrooms_1 + built_2005_or_later:bedrooms_0 + 
    county:built_1990s + built_1990s:built_2000_to_2004 + built_1970s:bedrooms_2 + 
    mean_household_size_renters:mean_household_size_owners, data = ca8.1train_X)

anova(fit1.2)

#Second Order model2 with outlier
fitnone = lm(median_house_value~1,data=ca8.2train)
fitfull = lm(median_house_value~.^2,data = ca8.2train)
fit3=stepAIC(fitnone,scope=list(upper=fitfull, lower=~1), direction="both", k=log(n))
fit3$anova

summary(fit3)

anova(fit3)

## find X outliers
res=residuals(fit3)# residuals of the final model
n = nrow(ca8.2train)
p2 = 50
h1 = influence(fit3)$hat
d.res.std=studres(fit3) #studentized deleted residuals
idx.X = as.vector(which(h1>(2*p2/n)))
length(idx.X)
#plot(h1,res,xlab="leverage",ylab="residuals")

ca8.2train_X=ca8.2train[-idx.X,]

#Second Order model2 without outlierX
fit3.2 = lm(formula = median_house_value ~ mean_household_income + median_rooms + 
    county + built_1980s_to_1990s + owners + built_after_2000 + 
    mean_household_size_renters + bedrooms_0 + bedrooms_3 + built_1960_to_1970 + 
    bedrooms_2 + bedrooms_1 + bedrooms_4 + mean_household_size_owners + 
    mean_household_income:county + mean_household_income:mean_household_size_renters + 
    county:built_1980s_to_1990s + county:built_after_2000 + county:owners + 
    mean_household_income:bedrooms_3 + built_1960_to_1970:bedrooms_2 + 
    mean_household_income:bedrooms_2 + median_rooms:owners + 
    mean_household_income:owners + owners:mean_household_size_owners + 
    mean_household_size_renters:bedrooms_3 + built_after_2000:bedrooms_3 + 
    built_after_2000:bedrooms_0 + mean_household_size_renters:mean_household_size_owners + 
    bedrooms_1:bedrooms_4 + median_rooms:bedrooms_0 + median_rooms:bedrooms_2 + 
    bedrooms_3:bedrooms_1 + median_rooms:built_after_2000, data = ca8.2train_X)


summary(fit3.2)

anova(fit1.2)

drop = "median_house_value"
newdata8.1 = ca8.1test[,!names(ca8.1test) %in% drop]
newdata8.2 = ca8.2test[,!names(ca8.2test) %in% drop]

pred.final1.2=predict.lm(fit1.2, newdata8.1)
mspe.final1.2=mean((pred.final1.2-ca8.1test$median_house_value)^2)
mspe.final1.2

pred.final3.2=predict.lm(fit3.2, newdata8.2)
mspe.final3.2=mean((pred.final3.2-ca8.2test$median_house_value)^2)
mspe.final3.2


pred.with1.2=predict.lm(fit1, newdata8.1)
mspe.with1.2=mean((pred.final1.2-ca8.1test$median_house_value)^2)
mspe.with1.2
pred.with3.2=predict.lm(fit3, newdata8.2)
mspe.with3.2=mean((pred.with3.2-ca8.2test$median_house_value)^2)
mspe.with3.2


AIC.final.8.1 =n*log(23286940/n)+2*p1
AIC.final.8.1
AIC.with.8.1 =n*log(27747937.18/n)+2*p1
AIC.with.8.1
BIC.with.8.1 =n*log(27747937.18/n)+log(n)*p1
BIC.with.8.1
BIC.final.8.1 =n*log(23286940/n)+2*p1
BIC.final.8.1


AIC.final.8.2 =n*log(23309740/n)+2*p2
AIC.final.8.2
AIC.with.8.2 =n*log(27633874.19/n)+2*p2
AIC.with.8.2
BIC.with.8.2 =n*log(27633874.19/n)+log(n)*p2
BIC.with.8.2
BIC.final.8.2 =n*log(23309740/n)+2*p2
BIC.final.8.2

#final model
summary(fit3.2)

fit_f=lm(formula = median_house_value ~ mean_household_income + median_rooms + 
    county + built_1980s_to_1990s + owners + built_after_2000 + 
    mean_household_size_renters + bedrooms_0 + bedrooms_3 + built_1960_to_1970 + 
    bedrooms_2 + bedrooms_1 + bedrooms_4 + mean_household_size_owners + 
    mean_household_income:county + mean_household_income:mean_household_size_renters + 
    county:built_1980s_to_1990s + county:built_after_2000 + county:owners + 
    mean_household_income:bedrooms_3 + built_1960_to_1970:bedrooms_2 + 
    mean_household_income:bedrooms_2 + median_rooms:owners + 
    mean_household_income:owners + owners:mean_household_size_owners + 
    mean_household_size_renters:bedrooms_3 + built_after_2000:bedrooms_3 + 
    built_after_2000:bedrooms_0 + mean_household_size_renters:mean_household_size_owners + 
    bedrooms_1:bedrooms_4 + median_rooms:bedrooms_0 + median_rooms:bedrooms_2 + 
    bedrooms_3:bedrooms_1 + median_rooms:built_after_2000, data = ca8.2)

plot(fit_f,which=4)

#remove outlier X and Y
res=residuals(fit_f)# residuals of the final model
n = nrow(ca8.2)
p = 50
h1 = influence(fit_f)$hat
d.res.std=studres(fit_f) #studentized deleted residuals

idx.X = as.vector(which(h1>(2*p/n)))
idx.X ## two outliers
plot(h1,res,xlab="leverage",ylab="residuals")

max(abs(d.res.std))
sort(abs(d.res.std),decreasing=T)
qt(1-0.1/(2*n),n-p-1) # bonferronis thresh hold
idx.Y = as.vector(which(abs(d.res.std)>=qt(1-0.1/(2*n),n-p-1)))
idx.Y ## outliers
cook.d = res^2*h1/(p*1.293*(1-h1)^2)
cook.max = cook.d[which(cook.d==max(cook.d))]
pf(cook.max,p,n-p)
idx = c(idx.X,idx.Y)
cook.d[idx]
pf(cook.d[idx],p,n-p)



length(idx)

ca9.2 = ca8.2[-idx,]

fit_final=lm(formula = median_house_value ~ mean_household_income + median_rooms + 
    county + built_1980s_to_1990s + owners + built_after_2000 + 
    mean_household_size_renters + bedrooms_0 + bedrooms_3 + built_1960_to_1970 + 
    bedrooms_2 + bedrooms_1 + bedrooms_4 + mean_household_size_owners + 
    mean_household_income:county + mean_household_income:mean_household_size_renters + 
    county:built_1980s_to_1990s + county:built_after_2000 + county:owners + 
    mean_household_income:bedrooms_3 + built_1960_to_1970:bedrooms_2 + 
    mean_household_income:bedrooms_2 + median_rooms:owners + 
    mean_household_income:owners + owners:mean_household_size_owners + 
    mean_household_size_renters:bedrooms_3 + built_after_2000:bedrooms_3 + 
    built_after_2000:bedrooms_0 + mean_household_size_renters:mean_household_size_owners + 
    bedrooms_1:bedrooms_4 + median_rooms:bedrooms_0 + median_rooms:bedrooms_2 + 
    bedrooms_3:bedrooms_1 + median_rooms:built_after_2000, data = ca9.2)

summary(fit_final)

anova(fit_final)

plot(fit_final)
