getwd()
prices<-read.csv("boston_prices.csv",header=T,stringsAsFactors = F)
summary(bp)

#Missing Value Treatment
prices$MEDV[is.na(prices$MEDV)]<-mean(prices$MEDV,na.rm = T)
#.......OR.......
library(randomForest)
prices$MEDV<-na.roughfix(prices$MEDV)

#outlier plots
par(mfrow=c(2,7))
list<-names(prices)
list<-list[-4]
for(i in 1:length(list))
{
  boxplot(prices[,list[i]],main=list[i])
}

for(i in 1:length(list))
{
  plot(prices[,list[i]],main=list[i])
}

dev.off()

#Outlier Treatment
for(i in 1:length(list))
{
  x<-boxplot(prices[,list[i]],main=list[i])
  out<-x$out
  index<-which(prices[,list[i]]%in% x$out)
  prices[index,list[i]]<-mean(prices[,list[i]])
  rm(x)
  rm(out)
}
for(i in 1:length(list))
{
  y<-boxplot(prices[,list[i]],main=list[i])
}
y

for(i in 1:length(list))
{
  plot(prices[,list[i]],main=list[i])
}

#Exploratory Analysis
library(ggplot2)
hist(prices$MEDV,breaks=6,label=TRUE,xlim=c(10,35))

#Function to get the correlation between the IDV's and DV.
list
list1<-list[-13]
for(i in 1:length(list1))
{
  x<-cor(bp$MEDV,prices[list[i]])
  print(x)
}

#Log Transformation for all variables
prices$log_CRIM<-log(prices$CRIM)
prices$log_ZN<-log(prices$ZN)
prices$log_NOX<-log(prices$nitric.oxides.concentration)
prices$log_RM<-log(prices$X.rooms.dwelling)
prices$log_AGE<-log(prices$AGE)
prices$log_DIS<-log(prices$DIS)
prices$log_RAD<-log(prices$RAD)
prices$log_TAX<-log(prices$TAX)
prices$log_PTRATIO<-log(prices$PTRATIO)
prices$log_B<-log(prices$B)
prices$log_LSTAT<-log(prices$LSTAT)
prices$log_MEDV<-log(prices$MEDV) #DV
prices$log_INDUS<-log(prices$INDUS)

#Function to get the list of correlations between : log_DV and log of IDV's
list_log<-names(prices)[c(15:25,27)]
for(i in 1:length(list_log))
{
  xlog<-cor(prices$log_MEDV,prices[list_log[i]])
  print(xlog)
}

#Function to get the list of correlations between : log_DV and IDV's
list_log_DV<-names(prices)[1:13]
list_log_DV<-list_log_DV[-4]
for(i in 1:length(list_log_DV))
{
  xlogdv<-cor(prices$log_MEDV,prices[list_log_DV[i]])
  print(xlogdv)
}

#Sampling Data Set
sampling<-sort(sample(nrow(prices), nrow(prices)*.7))
length(sampling)
head(sampling)

#Selecting training and test samples
train<-prices[sampling,]
test<-prices[-sampling,]

##Building SimpLe Linear Regression Model
#Metrics :
#Rsquare
#Coefficients
#P values : Significance levels of the IDV's
#Residuals distribution

#Factor variables as IDV's
#All good modelssummm
Reg<-lm(log_MEDV~CRIM+INDUS+RAD+TAX+B+
          Charles.River.dummy.variable+
          DIS+ZN+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)

summary(Reg)

#Getting the formula
formula(Reg)

#Removing insignificant variables :

Reg1<-lm(log_MEDV~
           Charles.River.dummy.variable+
           DIS+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)
summary(Reg1)


#Removing insignificant variables :

Reg2 <- lm(log_MEDV ~CRIM+INDUS+RAD+TAX+B+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+X.rooms.dwelling+nitric.oxides.concentration, data=train)
summary(Reg2)

#Removing insignificant variables :
Reg3 <- lm(log_MEDV ~CRIM+RAD+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+nitric.oxides.concentration, data=train)
summary(Reg3)

#Trying some other combination :
Reg4<-lm(log_MEDV~INDUS  +ZN + X.rooms.dwelling + LSTAT+CRIM + Charles.River.dummy.variable,data=train)
summary(Reg4)

#The best model happens to be : Reg3

##Getting predicted values
predicted<-predict(Reg3)
plot(predicted)
length(predicted)


##Finding Residuals
residuals<-resid(Reg3)
plot(residuals)
length(residuals)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity
##There should be no trend between predicted values and residual values
plot(predicted,residuals,abline(0,0))

#There seems to be an inverse pattern for some points. So this model may not be the preferred model.

#Attaching predicted values to test data
predicted<-predict(Reg3,newdata=test)
length(predicted)
test$p<-predicted

#Calculating error in the test dataset - (Actual- predicted)/predicted values
test$error<-(test$log_MEDV-test$p)/test$log_MEDV

#Calculating average error in the dataset
mean(test$error)*100 


##Plotting actual vs predicted values
plot(test$p,col="blue",type="l")
lines(test$log_MEDV,col="red",type="l")

#checking for Correlation between variables
library(car)
vif(Reg3)

# Variables with a vif>10 should be dropped off as it means there is high correlation between variables
# Here all the variables are well below vif of 10. Thus the formula Reg3 can be used to predict the 
# Housing Prices in Suburban Boston.