##Xuechan Peng 
##DataMining HW01
HousingData=read.csv("file:///C:/Users/XUP4/Downloads/housing.csv")
summary(HousingData)##  give summary to data

CRIM=HousingData$CRIM;
ZN=HousingData$ZN;
INDUS=HousingData$INDUS;
CHAS=HousingData$CHAS;
NOX=HousingData$NOX;
RM=HousingData$RM;
AGE=HousingData$AGE;
DIS=HousingData$DIS;
RAD=HousingData$RAD;
TAX=HousingData$TAX;
PTRATIO=HousingData$PTRATIO;
B=HousingData$B;
LSTAT=HousingData$LSTAT;
MEDV=HousingData$MEDV;

## set every variable as object
sd(CRIM)
sd(ZN)
sd(INDUS)
sd(NOX)
sd(RM)
sd(AGE)
sd(DIS)
sd(RAD)
sd(TAX)
sd(PTRATIO)
sd(B)
sd(LSTAT)
sd(MEDV)

##GET THE SD for numerical variables
densityplot(CRIM,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(ZN,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(INDUS,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(NOX,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(RM,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(AGE,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(DIS,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(RAD,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(TAX,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(PTRATIO,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="black")
densityplot(B,data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="balck")
##GET THE density plot of numerical variables;
xyplot(MEDV~CRIM, data=HousingData, col="black")
xyplot(MEDV~ZN, data=HousingData, col="black")
xyplot(MEDV~INDUS, data=HousingData, col="black")
xyplot(MEDV~NOX, data=HousingData, col="black")
xyplot(MEDV~RM, data=HousingData, col="black")
xyplot(MEDV~AGE, data=HousingData, col="black")
xyplot(MEDV~DIS, data=HousingData, col="black")
xyplot(MEDV~RAD, data=HousingData, col="black")
xyplot(MEDV~TAX, data=HousingData, col="black")
xyplot(MEDV~PTRATIO, data=HousingData, col="black")
xyplot(MEDV~B, data=HousingData, col="black")
xyplot(MEDV~LSTAT, data=HousingData, col="black")
##scatter plot
cor(HousingData)
##get the correlation 
densityplot(MEDV|CHAS, data=HousingData,layout=c(1,5),
            plot.point=FALSE,col="balck")
##get conditional density plot


mean(HousingData[HousingData$CHAS>0,"MEDV"])
##mean for CHAS=1
mean(HousingData[HousingData$CHAS==0,"MEDV"])
##mean for CHAS=0

pairs(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX,
      data=HousingData)
##Check the statistic standanrds of Data

fit=lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+RAD+TAX+PTRATIO+B+LSTAT
       ,data=HousingData)
summary(fit)
##multiple linear regression model
mean.mse=mean((rep(mean(MEDV),length(MEDV))-MEDV)^2)
model.mse=mean(residuals(fit)^2)
rmse=sqrt(model.mse)
rmse
## obtain RMSE for the regression 

library(MASS)
fit1=lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+RAD+TAX, data=HousingData)
stepAIC(fit, direction="backward")
##Use stepwise model



n=length(MEDV)
error=dim(n)
for (k in 1:n){
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+RAD+TAX, data=HousingData[train,])
  pred=predict(m2,newdat=HousingData[-train,])
  obs=MEDV[-train]
  error[k]=obs-pred
  }
me=mean(error)
me
rmse=sqrt(mean(error^2))
rmse
##leave-one-out cross validation



X2=NOX^2
X3=NOX^3
fit2=lm(MEDV~NOX+X2+X3,data=df)
summary(fit2)
mean.mse=mean((rep(mean(MEDV),length(MEDV))-MEDV)^2)
model.mse=mean(residuals(fit2)^2)
rmse=sqrt(model.mse)
rmse
##polynomial regression


