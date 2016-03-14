##Data MiningHW2
##Xuechan Peng 
BankData=read.csv("file:///C:\Users\XUP4\Desktop\bank.csv");
AGE=BankData$age;
Balance=BankData$balance;

##Data Preparation;
Marital=BankData$marital;
Default=BankData$default;
Housing=BankData$housing;
Loan=BankData$loan;
Y=BankData$y;

## Give summary to numerical data
summary(BankData);

##generate the SD of numerical varaible;
sd(AGE);
sd(Balance);

##Density distribution for numerical variable;
library(lattice);
suppressWarnings(densityplot(AGE, data=BankData,xlab="Age", 
            main="The distribution of Age", plot.points=FALSE, 
            from=19, to=87, col="blue"));

suppressWarnings(densityplot(Balance, data=BankData,xlab="Balance", 
                             main="The distribution of Balance", 
                             plot.points=FALSE, from=-3311, to=71188, 
                             col="blue"));

##histogram in categorical variable
library(lattice);
histogram(~Y|Marital, data=BankData, 
          main="conditional histogram on Marital status");
histogram(~Y|Default, data=BankData, 
          main="conditional histogram on Default status");
histogram(~Y|Housing, data=BankData, 
          main="conditional histogram on Housing status");
histogram(~Y|Loan, data=BankData, 
          main="conditional histogram on Loan status");

## Transform data into desired form;
BankData=transform(BankData,DeFault=ifelse(Default=='yes',1,0));
BankData=transform(BankData,HouSing=ifelse(Housing=='yes',1,0));
BankData=transform(BankData, Subscribe=ifelse(Y=='yes',1,0));
##BankData=transform(BankData, Married=ifelse(Marital=='married',1,0));
##BankData=transform(BankData, Divorced=ifelse(Marital=='divorced',1,0));
##BankData=transform(BankData, Single=ifelse(Marital=='single',1,0));
BankData$balance=cut(BankData$balance,breaks=c(-Inf,0,1000,3000,5000,10000,Inf), 
                     labels=c("minus","OneThou","OnetwoThou","ThreeFiveThous","tenthous","highest"));
BankData$age=cut(BankData$age,breaks=c(0,36,59,90),
                 labels=c("young","middle","old"));
##Select the data for design matrix 
BankDataRevised=BankData[,c(1,2,4,12,13,14)];

XBankDataRevised=model.matrix(Subscribe~.,data=BankDataRevised);

n.test=floor(n*0.1);
test=sample(1:n,n.test);
xtrain=XBankDataRevised[-test,];
xtest=XBankDataRevised[test,];
ytrain=BankData$Subscribe[-test];
ytest=BankData$Subscribe[test];

m1=glm(Subsribe~.,family=binomial,data=data.frame(Subsribe=ytrain,xtrain));
summary(m1)
suppressWarnings(ptest=predict(m1,newdata=data.frame(xtest),type="response");)

##confusion matrix
btest=floor(ptest+0.7);
conf.matrix=table(ytest,btest);
conf.matrix

##lift chart
df=cbind(ptest,ytest);
rank.df=as.data.frame(df[order(ptest,decreasing=TRUE),]);
colnames(rank.df)=c('predicted','actual');

baserate=mean(ytest);
ax=dim(n.test);
ay.base=dim(n.test);
ay.pred=dim(n.test);
ax[1]=1;
ay.base[1]=baserate;
ay.pred[1]=rank.df$actual[1];

for(i in 2:n.test){
  ax[i]=i;
  ay.base[i]=baserate*i;
  ay.pred[i]=ay.pred[i-1]+rank.df$actual[i];
}

df=cbind(rank.df,ay.pred,ay.base);
plot(ax,ay.pred,xlab="number of cases",
     ylab="number of successes", main="Lift chart");
points(ax,ay.base,type="l");


##Roc curve
data=data.frame(predictions=ptest,labels=ytest);
pred=prediction(data$predictions,data$labels);
library(ROCR);
pred=prediction(data$predictions,data$labels);
perf=performance(pred,"sens","fpr");
plot(perf,main="ROC curve");


##If omit the housing variable 
BankDataRevised=BankData[,c(1,2,4,12,14)];
XBankDataRevised=model.matrix(Subscribe~.,data=BankDataRevised);

n.test=floor(n*0.1);
ptest=predict(m2,newdata=data.frame(xtest),type="response");
btest=floor(ptest+0.7);
conf.matrix=table(ytest,btest);
conf.matrix

df=cbind(ptest,ytest);
rank.df=as.data.frame(df[order(ptest,decreasing=TRUE),]);
colnames(rank.df)=c('predicted','actual');
baserate=mean(ytest);
ax=dim(n.test);
ay.base=dim(n.test);
ay.pred=dim(n.test);
ax[1]=1;
ay.base[1]=baserate;
for(i in 2:n.test){
  ax[i]=i;
  ay.base[i]=baserate*i;
  ay.pred[i]=ay.pred[i-1]+rank.df$actual[i];
}

df=cbind(rank.df,ay.pred,ay.base);
plot(ax,ay.pred,xlab="number of cases",
     ylab="number of successes", main="Lift chart");
points(ax,ay.base,type="l");


data=data.frame(predictions=ptest,labels=ytest);
pred=prediction(data$predictions,data$labels);
library(ROCR);
pred=prediction(data$predictions,data$labels);
perf=performance(pred,"sens","fpr");
plot(perf,main="ROC curve");

