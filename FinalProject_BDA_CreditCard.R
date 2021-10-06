
#Import data set
library("readxl")
rm(list = ls())
setwd("C:/Users/ameya/Documents/GSU/GSU Course/Big Data R/Project/R Project 9/Code")

#ignore first X1, X2... row while reading xls
credit_card <- read_excel("credit card defaulters.xls",skip=1)

#################### Data Pre-processing ################################

#remove index column
credit_card<-credit_card[,-1]

#rename columns
credit_card <- setNames(credit_card, c("limit_bal","sex","education","marriage","age" ,"pay.sep",    
                                       "pay.aug","pay.jul","pay.jun","pay.may","pay.arp","bill.amt.sep",
                                       "bill.amt.aug","bill.amt.jul","bill.amt.jun","bill.amt.may","bill.amt.arp","pay.amt.sep", 
                                       "pay.amt.aug","pay.amt.jul","pay.amt.jun","pay.amt.may","pay.amt.arp","def.pay"  ))

#remove rows with unknown value in education and marriage column
credit_card<-subset(credit_card, education!=0 & marriage!=0)

#remove rows with NaN values 
credit_card<-na.omit(credit_card)


######################## Data Exploration ##################################

#create sex table which has counts in ratio of male and female
sex.ratio<-table(credit_card$sex)
sex.ratio<-setNames(as.data.frame(table(credit_card$sex)), c("sex", "count"))
sex.ratio$sex<-c('male','female')
sex.ratio$ratio<- round(sex.ratio$count / sum(sex.ratio$count) *100,digits = 2)

#Pie chart in sex column
library(ggplot2)
sex.pie<-ggplot(sex.ratio, aes(x="", y=ratio, fill=sex)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(count,
                               " (",
                               ratio,"%",
                               ")")),
            position = position_stack(vjust = 0.5))+
  theme_void() 
sex.pie+ggtitle("sex Ratio")


#Create the default pay next month(defaulter ratio) and plot it
def.ratio<-table(credit_card$def.pay)
def.ratio<-setNames(as.data.frame(table(credit_card$def.pay)), c("defaulter", "count"))
def.ratio$def.pay<-c('0','1')
def.ratio$ratio<- round(def.ratio$count / sum(def.ratio$count) *100,digits = 2)

def.pie<-ggplot(def.ratio, aes(x="", y=ratio, fill=def.pay)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(count,
                               " (",
                               ratio,"%",
                               ")")),
            position = position_stack(vjust = 0.5))+
  theme_void() 
def.pie+ggtitle("Defaulter Ratio")


#Create marriage table
marriage.ratio<-setNames(as.data.frame(table(credit_card$marriage)),c("status","count"))
marriage.ratio$status<-c('married','single') #,'others')
marriage.ratio$status
marriage.ratio$ratio<- round(marriage.ratio$count / sum(marriage.ratio$count) *100,digits = 2)


#Pie chart in marriage column 
marriage.pie<-ggplot(marriage.ratio, aes(x="", y=ratio, fill=status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(count,
                               " (",
                               ratio,"%",
                               ")")),
            position = position_stack(vjust = 0.5))+
  theme_void() 
marriage.pie + ggtitle("Marriage Status Ratio")


#Create age table and group them in some range
table(credit_card$age)
credit_card$age.groups<- cut(credit_card$age, breaks=c(0,20,30,40,50,60,70,Inf))
table(credit_card$age.groups)


#Plot credit limit by age group
bal.age<-ggplot(aes(x=age.groups,y=limit_bal,fill=age.groups),data=credit_card)
bal.age+geom_boxplot()+labs(title="Credit Limits in Different Age Groups",x ="Age Group", y = "Credit Limit")+scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels=c("(20,30]" = "21~30", "(30,40]" = "31~40","(40,50]" = "41~50","(50,60]" = "51~60","(60,70]" = "61~70","(70,Inf]" = "Above 70"))+
  theme(legend.position = "none")


#Create total payment and total bill column
cols.bill <- c("bill.amt.sep",
               "bill.amt.aug","bill.amt.jul","bill.amt.jun","bill.amt.may","bill.amt.arp")
credit_card$total.bill <- apply(credit_card[,cols.bill],1,sum)
cols.pay<-c("pay.amt.sep", 
            "pay.amt.aug","pay.amt.jul","pay.amt.jun","pay.amt.may","pay.amt.arp")
credit_card$total.pay <- apply(credit_card[,cols.pay],1,sum)


bill.by.groups<-aggregate(credit_card$total.bill, list(credit_card$age.groups), sum)
pay.by.groups<-aggregate(credit_card$total.pay, list(credit_card$age.groups), sum)
bill.pay.df <- merge(bill.by.groups,pay.by.groups,by="Group.1")
bill.pay.df<-setNames(bill.pay.df,c("group","total.bill","total.pay"))

bill.pay.df

#Plot total bill and total payment amount by age group
library(reshape2)
mdf <- melt(bill.pay.df,id.vars="group")
mdf
options(scipen=999)
bill.to.pay <-ggplot(mdf, aes( x=group, y=value, colour=variable, group=variable )) + 
  geom_line()+
  labs(title="Bill Compare to Payment by Age Group",x ="Age Group", y = "New Taiwan Dollar(NTD)")+
  scale_x_discrete(labels=c("(20,30]" = "21~30", "(30,40]" = "31~40","(40,50]" = "41~50","(50,60]" = "51~60","(60,70]" = "61~70","(70,Inf]" = "Above 70"))
bill.to.pay


######################################### Models ###############################################

######################### Logistic regression ##################################

# Partition data
set.seed(2)
credit_card<-subset(credit_card, select=-c(age.groups,25,26))
train.index <- sample(c(1:dim(credit_card)[1]), dim(credit_card)[1]*0.6)
train.df <- credit_card[train.index, ]
valid.df <- credit_card[-train.index, ]

#Logistic Regression
logit.reg <- glm(def.pay ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)
# install.packages("jtools")    # jtools helps present regression results
library(jtools)
summ(logit.reg, digits=5)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# Use Confusion Matrix to evaluate performance
# install.packages("caret") # Package installation is required for the first time, it takes some time!
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), 
                as.factor(valid.df$def.pay))

# Map into TP, FP, TN, FN
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, "Positive", "Negative")), 
                as.factor(ifelse(valid.df$def.pay==1,"Positive", "Negative")))

########################## Forward Variable Selection ##################################

null<-glm(def.pay ~ 1, data = train.df, family = "binomial") 
stats::step(null, scope=list(lower=null, upper=logit.reg), direction="forward")

########################## Classification using KNN ##################################

## A 3-nearest neighbours model with no normalization
# install.packages("DMwR")
# install.packages("kknn")
# install.packages("class")
library(class)
library(DMwR)
library(kknn)
library(class)
credit_card$def.pay <- factor(credit_card$def.pay)

# Create training and validation sets.
credit.var <- c("def.pay","limit_bal","age" ,"pay.sep",    
                "pay.aug","pay.jul","pay.jun","pay.may","pay.arp","bill.amt.sep",
                "bill.amt.aug","bill.amt.jul","bill.amt.jun","bill.amt.may","bill.amt.arp","pay.amt.sep", 
                "pay.amt.aug","pay.amt.jul","pay.amt.jun","pay.amt.may","pay.amt.arp")
train.index <- sample(c(1:dim(credit_card)[1]), dim(credit_card)[1]*0.6)  
train.knn <- credit_card[train.index, credit.var]
valid.knn <- credit_card[-train.index, credit.var]
train.knn
library(class)
nn3 <- knn(train.knn,valid.knn,train.knn$def.pay,k=3)

# Compute knn for different k on validation.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
for(i in 1:14) {
  kNN.pred <- knn(train.knn,valid.knn,train.knn$def.pay,k=i)
  accuracy.df[i, 2] <- confusionMatrix(kNN.pred, valid.knn$def.pay)$overall[1] 
}
accuracy.df
attach(accuracy.df)
accuracy.df <- accuracy.df[order(accuracy),] 
detach(accuracy.df)

# Find optimal K
set.seed(502)
grid1 <- expand.grid(.k = seq(2, 20, by = 1))
control <- trainControl(method = "cv")
knn.train <- train(def.pay ~ ., data = train.knn,
                   method = "knn",
                   trControl = control,
                   tuneGrid = grid1)
knn.train

knn.pred <- predict(knn.train, newdata = valid.knn)
confusionMatrix(as.factor(knn.pred), as.factor(valid.knn$def.pay))

# Different distance weighting
# install.packages("kknn")
library(kknn)
set.seed(123)
kknn.train <- train.kknn( def.pay~ ., data = train.knn, kmax = 25, 
                          distance = 2, 
                          kernel = c("rectangular", "triangular", "epanechnikov"))
plot(kknn.train)

kknn.train

kknn.pred <- predict(kknn.train, newdata = valid.knn)
confusionMatrix(kknn.pred, valid.knn$def.pay)

########################## Neural Network ####################################

#Normalize data
library(neuralnet)
library(caret)
str(credit_card)
procValues <- preProcess(credit_card[,-c(2,3,4,24)], method = c("center", "scale"))
procValues 
scaledTraindata <-  predict(procValues, train.df)
scaledTraindata 
scaledTestdata <-  predict(procValues, valid.df)
neural<-neuralnet(def.pay ~ ., data = scaledTraindata[,-c(2,3,4)], hidden = c(2,1),linear.output = FALSE)
neural
neural.pred<-compute(neural, scaledTestdata[,-c(24)])
neural.pred$net.result
preds.class<-ifelse(neural.pred$net.result>0.5,1,0)
preds.class
confusionMatrix(as.factor(preds.class),as.factor(valid.df$def.pay))


########################## Naive bayes ####################################

library(e1071)  # For NB
pcs.df.nb <- naiveBayes(as.factor(def.pay) ~ ., data = train.df)

## Predict probabilities: Training
pred.prob <- predict(pcs.df.nb, newdata = train.df, type = "raw")
pred.prob 

## Predict class membership
pred.class <- predict(pcs.df.nb, newdata = train.df)
pred.class
confusionMatrix(pred.class, as.factor(train.df$def.pay))

## Predict class membership
pred.class <- predict(pcs.df.nb, newdata = valid.df)
confusionMatrix(pred.class, as.factor(valid.df$def.pay))


#################################### Ensemble Methods ####################################

## Majority voting
#install.packages("gains")
library(gains)
valid.df$lr.n = ifelse(logit.reg.pred > 0.5, 1, 0) # Logistic Regression
valid.df$class.n = ifelse(preds.class == 1, 1, 0)  #Neural
valid.df$svm.n = ifelse(kknn.pred == 1, 1, 0) # KNN

valid.df$pred_majority<-as.factor(ifelse(valid.df$lr.n ==1 & valid.df$class.n==1,1,
                                         ifelse(valid.df$class.n==1 & valid.df$svm.n==1,1,
                                                ifelse(valid.df$svm.n ==1 & valid.df$lr.n==1,1,0))))
confusionMatrix(valid.df$pred_majority, as.factor(valid.df$def.pay))

