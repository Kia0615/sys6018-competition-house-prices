library(readr)
library(dplyr)

#Read in the training data
train=read.csv("train.csv")
test=read.csv("test.csv")
train


sub <- sample(1:891,size=446)
#Check which variables have missing values
apply(train,2,function(col)sum(is.na(col)))

str(train)

#Replace missing values

train$LotFrontage[is.na(train$LotFrontage)]=mean(train$LotFrontage,na.rm=TRUE)


train$Alley <-factor(as.character(train$Alley),exclude=NULL)
train$BsmtQual <-factor(as.character(train$BsmtQual),exclude=NULL)
train$BsmtCond <-factor(as.character(train$BsmtCond),exclude=NULL)
train$BsmtExposure <-factor(as.character(train$BsmtExposure),exclude=NULL)
train$BsmtFinType1 <-factor(as.character(train$BsmtFinType1),exclude=NULL)
train$BsmtFinType2 <-factor(as.character(train$BsmtFinType2),exclude=NULL)

nrow(train[train$MasVnrType=='BrkCmn',])
nrow(train[train$MasVnrType=='BrkFace',])
nrow(train[train$MasVnrType=='None',])
nrow(train[train$MasVnrType=='Stone',])
train$MasVnrType[is.na(train$MasVnrType)]='None'
train$MasVnrArea[is.na(train$MasVnrArea)]=0

train$Electrical[is.na(train$Electrical)]='SBrkr'

train$FireplaceQu <-factor(as.character(train$FireplaceQu),exclude=NULL)

train$GarageType <-factor(as.character(train$GarageType),exclude=NULL)
train$GarageYrBlt <-factor(as.character(train$GarageYrBlt),exclude=NULL)
train$GarageFinish <-factor(as.character(train$GarageFinish),exclude=NULL)
train$GarageQual <-factor(as.character(train$GarageQual),exclude=NULL)
train$GarageCond <-factor(as.character(train$GarageCond),exclude=NULL)

train$PoolQC <-factor(as.character(train$PoolQC),exclude=NULL)
train$Fence <-factor(as.character(train$Fence),exclude=NULL)
train$MiscFeature <-factor(as.character(train$MiscFeature),exclude=NULL)

lm <- lm(SalePrice~.-GarageYrBlt-YrSold,data=train)
lm$xlevels[["GarageYrBlt"]] <- union(lm$xlevels[["GarageYrBlt"]], levels(test$GarageYrBlt))
lm$xlevels[["YrSold"]] <- union(lm$xlevels[["YrSold"]], levels(test$GarageYrSold))
summary(lm)
anova(lm)

test$LotFrontage[is.na(test$LotFrontage)]=mean(train$LotFrontage,na.rm=TRUE)

test$Alley <-factor(as.character(test$Alley),exclude=NULL)
test$BsmtQual <-factor(as.character(test$BsmtQual),exclude=NULL)
test$BsmtCond <-factor(as.character(test$BsmtCond),exclude=NULL)
test$BsmtExposure <-factor(as.character(test$BsmtExposure),exclude=NULL)
test$BsmtFinType1 <-factor(as.character(test$BsmtFinType1),exclude=NULL)
test$BsmtFinType2 <-factor(as.character(test$BsmtFinType2),exclude=NULL)


test$MasVnrType[is.na(test$MasVnrType)]='None'
test$MasVnrArea[is.na(test$MasVnrArea)]=0

test$Electrical[is.na(test$Electrical)]='SBrkr'

test$FireplaceQu <-factor(as.character(test$FireplaceQu),exclude=NULL)

test$GarageType <-factor(as.character(test$GarageType),exclude=NULL)
test$GarageYrBlt <-factor(as.character(test$GarageYrBlt),exclude=NULL)
test$GarageFinish <-factor(as.character(test$GarageFinish),exclude=NULL)
test$GarageQual <-factor(as.character(test$GarageQual),exclude=NULL)
test$GarageCond <-factor(as.character(test$GarageCond),exclude=NULL)

test$PoolQC <-factor(as.character(test$PoolQC),exclude=NULL)
test$Fence <-factor(as.character(test$Fence),exclude=NULL)
test$MiscFeature <-factor(as.character(test$MiscFeature),exclude=NULL)

apply(test,2,function(col)sum(is.na(col)))
test$MSZoning[is.na(test$MSZoning)]='RL'
test$Utilities[is.na(test$Utilities)]='AllPub'

test$Exterior1st[is.na(test$Exterior1st)]='VinylSd'
test$Exterior2nd[is.na(test$Exterior2nd)]='VinylSd'
test$BsmtFinSF1[is.na(test$BsmtFinSF1)]=0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)]=0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)]=0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)]=0
test$BsmtFullBath[is.na(test$BsmtFullBath)]=0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)]=0

nrow(test[test$KitchenQual=='Ex',])
nrow(test[test$KitchenQual=='Fa',])
nrow(test[test$KitchenQual=='Gd',])
nrow(test[test$KitchenQual=='TA',])
test$KitchenQual[is.na(test$KitchenQual)]='TA'

test$Functional[is.na(test$Functional)]='Typ'
test$GarageCars[is.na(test$GarageCars)]=0

test$GarageArea[is.na(test$GarageArea)]=0
test$SaleType[is.na(test$SaleType)]='WD'



preds<-predict(lm,newdata=test,type="response")
sum(is.na(preds))

mypreds <-cbind(test$Id,preds)
mypreds


write.table(mypreds, file = "Competition1-5_submissions.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")









predictions <- predict(lm,test)








apply(train,2,is.factor)

train$MSSubClass <- factor(train$MSSubClass)
train$MSZoning <- factor(train$MSZoning)
train$Street <- factor(train$Street)

