setwd("E:/UCDavis-Courses/2018Winter-UCDavis/STA 207 - Stat Method in Research/HW")
## this is the working directory in your computer
#19.5
mydata = read.csv('19-5.csv',header = F)
names(mydata) = c("ave","factorA","factorB")
mydata$factorA = factor(mydata$factorA)
mydata$factorB = factor(mydata$factorB)

ave_b <- with(mydata, tapply(ave, factorB, mean))
ave_a <- with(mydata, tapply(ave, factorA, mean))
View(mydata)

#ave_b = ave()
#19.5-(b)
#check whether interaction is important or not
with(mydata,interaction.plot(factorB, factorA, ave))
#a simple transformation
mydata$log_ave = log(mydata$ave)
mydata$log_A = log(mydata$factorA)
mydata$log_B = log(mydata$factorB)
View(mydata)
with(mydata,interaction.plot(factorB, factorA, log_ave))
mydata$square_root_ave = mydata$ave^(0.5)
mydata$square_root_A = mydata$factorA^(0.5)
mydata$square_root_B = mydata$factorB^(0.5)