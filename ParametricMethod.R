library(readr)
library(ggplot2)

#Read in the data
train=read.csv("train.csv")
test=read.csv("test.csv")

#Plot the data
ggplot(train,aes(y=SalePrice,x=GrLivArea))+geom_point()

#Outlier removal
train <- train[train$GrLivArea<=4000,]
#New plot with new training data
ggplot(train,aes(y=SalePrice,x=GrLivArea))+geom_point()

#Check which variables have missing values
apply(train,2,function(col)sum(is.na(col)))
apply(test,2,function(col)sum(is.na(col)))
str(train)

#Replace missing values

#Replace with mean
train$LotFrontage[is.na(train$LotFrontage)]=mean(train$LotFrontage,na.rm=TRUE)
test$LotFrontage[is.na(test$LotFrontage)]=mean(train$LotFrontage,na.rm=TRUE)

#Those "NA"s are not missing values, therefore treat NA as a factor level
train$Alley <-factor(as.character(train$Alley),exclude=NULL)
train$BsmtQual <-factor(as.character(train$BsmtQual),exclude=NULL)
train$BsmtCond <-factor(as.character(train$BsmtCond),exclude=NULL)
train$BsmtExposure <-factor(as.character(train$BsmtExposure),exclude=NULL)
train$BsmtFinType1 <-factor(as.character(train$BsmtFinType1),exclude=NULL)
train$BsmtFinType2 <-factor(as.character(train$BsmtFinType2),exclude=NULL)
train$FireplaceQu <-factor(as.character(train$FireplaceQu),exclude=NULL)
train$GarageType <-factor(as.character(train$GarageType),exclude=NULL)
train$GarageYrBlt <-factor(as.character(train$GarageYrBlt),exclude=NULL)
train$GarageFinish <-factor(as.character(train$GarageFinish),exclude=NULL)
train$GarageQual <-factor(as.character(train$GarageQual),exclude=NULL)
train$GarageCond <-factor(as.character(train$GarageCond),exclude=NULL)
train$PoolQC <-factor(as.character(train$PoolQC),exclude=NULL)
train$Fence <-factor(as.character(train$Fence),exclude=NULL)
train$MiscFeature <-factor(as.character(train$MiscFeature),exclude=NULL)
test$Alley <-factor(as.character(test$Alley),exclude=NULL)
test$BsmtQual <-factor(as.character(test$BsmtQual),exclude=NULL)
test$BsmtCond <-factor(as.character(test$BsmtCond),exclude=NULL)
test$BsmtExposure <-factor(as.character(test$BsmtExposure),exclude=NULL)
test$BsmtFinType1 <-factor(as.character(test$BsmtFinType1),exclude=NULL)
test$BsmtFinType2 <-factor(as.character(test$BsmtFinType2),exclude=NULL)
test$FireplaceQu <-factor(as.character(test$FireplaceQu),exclude=NULL)
test$GarageType <-factor(as.character(test$GarageType),exclude=NULL)
test$GarageYrBlt <-factor(as.character(test$GarageYrBlt),exclude=NULL)
test$GarageFinish <-factor(as.character(test$GarageFinish),exclude=NULL)
test$GarageQual <-factor(as.character(test$GarageQual),exclude=NULL)
test$GarageCond <-factor(as.character(test$GarageCond),exclude=NULL)
test$PoolQC <-factor(as.character(test$PoolQC),exclude=NULL)
test$Fence <-factor(as.character(test$Fence),exclude=NULL)
test$MiscFeature <-factor(as.character(test$MiscFeature),exclude=NULL)

#Replace missing values with the "mode", aka the most common factor level
nrow(train[train$MasVnrType=='BrkCmn',])
nrow(train[train$MasVnrType=='BrkFace',])
nrow(train[train$MasVnrType=='None',])
nrow(train[train$MasVnrType=='Stone',])
train$MasVnrType[is.na(train$MasVnrType)]='None'
train$Electrical[is.na(train$Electrical)]='SBrkr'
test$MasVnrType[is.na(test$MasVnrType)]='None'
test$Electrical[is.na(test$Electrical)]='SBrkr'
test$KitchenQual[is.na(test$KitchenQual)]='TA'
test$Functional[is.na(test$Functional)]='Typ'
test$MSZoning[is.na(test$MSZoning)]='RL'
test$Utilities[is.na(test$Utilities)]='AllPub'
test$SaleType[is.na(test$SaleType)]='WD'
test$Exterior1st[is.na(test$Exterior1st)]='VinylSd'
test$Exterior2nd[is.na(test$Exterior2nd)]='VinylSd'

#Replace missing values with 0 as it is supposed to be 0
train$MasVnrArea[is.na(train$MasVnrArea)]=0
test$MasVnrArea[is.na(test$MasVnrArea)]=0
test$BsmtFinSF1[is.na(test$BsmtFinSF1)]=0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)]=0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)]=0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)]=0
test$BsmtFullBath[is.na(test$BsmtFullBath)]=0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)]=0
test$GarageCars[is.na(test$GarageCars)]=0
test$GarageArea[is.na(test$GarageArea)]=0

#Subset to cross validate
set.seed(123)
nrow(train)
sub <- sample(1:1460,size=1022)
subtrain <- train[sub,]
valid <- train[-sub,]

#The full model generated with sub-training data
lm1 <- lm(SalePrice~.-GarageYrBlt-YrSold-Id,data=subtrain)
lm1$xlevels[["Neighborhood"]] <- union(lm1$xlevels[["Neighborhood"]], levels(valid$Neighborhood))
lm1$xlevels[["Condition2"]] <- union(lm1$xlevels[["Condition2"]], levels(valid$Condition2))
lm1$xlevels[["RoofMatl"]] <- union(lm1$xlevels[["RoofMatl"]], levels(valid$RoofMatl))
lm1$xlevels[["Exterior1st"]] <- union(lm1$xlevels[["Exterior1st"]], levels(valid$Exterior1st))
lm1$xlevels[["Exterior2nd"]] <- union(lm1$xlevels[["Exterior2nd"]], levels(valid$Exterior2nd))
lm1$xlevels[["ExterCond"]] <- union(lm1$xlevels[["ExterCond"]], levels(valid$ExterCond))
lm1$xlevels[["Heating"]] <- union(lm1$xlevels[["Heating"]], levels(valid$Heating))
lm1$xlevels[["HeatingQC"]] <- union(lm1$xlevels[["HeatingQC"]], levels(valid$HeatingQC))
lm1$xlevels[["Electrical"]] <- union(lm1$xlevels[["Electrical"]], levels(valid$Electrical))
lm1$xlevels[["GarageYrBlt"]] <- union(lm1$xlevels[["GarageYrBlt"]], levels(valid$GarageYrBlt))
lm1$xlevels[["GarageCond"]] <- union(lm1$xlevels[["GarageCond"]], levels(valid$GarageCond))
lm1$xlevels[["MiscFeature"]] <- union(lm1$xlevels[["MiscFeature"]], levels(valid$MiscFeature))
lm1$xlevels[["SaleType"]] <- union(lm1$xlevels[["SaleType"]], levels(valid$SaleType))
lm1$xlevels[["RoofStyle"]] <- union(lm1$xlevels[["RoofStyle"]], levels(valid$RoofStyle))
lm1$xlevels[["Functional"]] <- union(lm1$xlevels[["Functional"]], levels(valid$Functional))
lm1$xlevels[["PoolQC"]] <- union(lm1$xlevels[["PoolQC"]], levels(valid$PoolQC))
summary(lm1)

#Check the anova table
anova(lm1)

#Test the full model on the validation set
predicts1<-as.vector(predict(lm1,newdata=valid))
predicts1

observed<-as.vector(valid$SalePrice)
SSE <- sum((observed-predicts1)^2)
MSE <- SSE/(nrow(valid)-2)
MSE #6.929823e+15 Huge!

#Try different combinations of explanatory variables:

#Model 2: All variables that are significant in anova testing
selected_var <- c('MSSubClass','MSZoning','LotFrontage','LotArea','Street','Alley','LotShape','LandContour',
                   'Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType',
                   'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodAdd','RoofStyle','RoofMatl',
                   'Exterior1st','Exterior2nd','MasVnrType','MasVnrArea','ExterQual','Foundation','BsmtQual',
                   'BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','HeatingQC',
                   'X1stFlrSF', 'X2ndFlrSF','LowQualFinSF','BedroomAbvGr','KitchenAbvGr','KitchenQual','Functional',
                   'Fireplaces','GarageCars','GarageArea','GarageCond','ScreenPorch','PoolArea','PoolQC','SaleType',
                  'SalePrice')
selected1 <- subtrain[,selected_var]
lm2 <- lm(SalePrice~.-SalePrice,data=selected1)
lm2$xlevels[["Neighborhood"]] <- union(lm2$xlevels[["Neighborhood"]], levels(valid$Neighborhood))
lm2$xlevels[["Condition2"]] <- union(lm2$xlevels[["Condition2"]], levels(valid$Condition2))
lm2$xlevels[["RoofMatl"]] <- union(lm2$xlevels[["RoofMatl"]], levels(valid$RoofMatl))
lm2$xlevels[["Exterior1st"]] <- union(lm2$xlevels[["Exterior1st"]], levels(valid$Exterior1st))
lm2$xlevels[["Exterior2nd"]] <- union(lm2$xlevels[["Exterior2nd"]], levels(valid$Exterior2nd))
lm2$xlevels[["HeatingQC"]] <- union(lm2$xlevels[["HeatingQC"]], levels(valid$HeatingQC))
lm2$xlevels[["GarageCond"]] <- union(lm2$xlevels[["GarageCond"]], levels(valid$GarageCond))
lm2$xlevels[["SaleType"]] <- union(lm2$xlevels[["SaleType"]], levels(valid$SaleType))
lm2$xlevels[["RoofStyle"]] <- union(lm2$xlevels[["RoofStyle"]], levels(valid$RoofStyle))
lm2$xlevels[["Functional"]] <- union(lm2$xlevels[["Functional"]], levels(valid$Functional))
lm2$xlevels[["PoolQC"]] <- union(lm2$xlevels[["PoolQC"]], levels(valid$PoolQC))
summary(lm2)
anova(lm2)

#Test the second model on the validation set
predicts2<-as.vector(predict(lm2,newdata=valid))
predicts2

SSE2 <- sum((observed-predicts2)^2)
MSE2 <- SSE2/(nrow(valid)-2)
MSE2 #7.924021e+14 smaller than the full model

#Model 3: fewer explanatory variables
lm3 <- lm(SalePrice~.-SalePrice-BsmtFinSF2-HeatingQC-LowQualFinSF-KitchenAbvGr-GarageCond-GarageArea
          -PoolArea,data=selected1)
summary(lm3)
lm3$xlevels[["Neighborhood"]] <- union(lm3$xlevels[["Neighborhood"]], levels(valid$Neighborhood))
lm3$xlevels[["Condition2"]] <- union(lm3$xlevels[["Condition2"]], levels(valid$Condition2))
lm3$xlevels[["RoofMatl"]] <- union(lm3$xlevels[["RoofMatl"]], levels(valid$RoofMatl))
lm3$xlevels[["Exterior1st"]] <- union(lm3$xlevels[["Exterior1st"]], levels(valid$Exterior1st))
lm3$xlevels[["Exterior2nd"]] <- union(lm3$xlevels[["Exterior2nd"]], levels(valid$Exterior2nd))
lm3$xlevels[["HeatingQC"]] <- union(lm3$xlevels[["HeatingQC"]], levels(valid$HeatingQC))
lm3$xlevels[["SaleType"]] <- union(lm3$xlevels[["SaleType"]], levels(valid$SaleType))
lm3$xlevels[["RoofStyle"]] <- union(lm3$xlevels[["RoofStyle"]], levels(valid$RoofStyle))
lm3$xlevels[["Functional"]] <- union(lm3$xlevels[["Functional"]], levels(valid$Functional))
lm3$xlevels[["PoolQC"]] <- union(lm3$xlevels[["PoolQC"]], levels(valid$PoolQC))

#Test the third model on the validation set
predicts3<-as.vector(predict(lm3,newdata=valid))
predicts3

SSE3 <- sum((observed-predicts3)^2)
MSE3 <- SSE3/(nrow(valid)-2)
MSE3 #3.596267e+15 larger than the previous model


#Model 4: explanatory variables selected based on the team's choices
selected_var2 <- c('MSZoning','GrLivArea','Neighborhood','Condition1','Condition2','X1stFlrSF',
                  'LotArea','KitchenQual','Functional','SaleCondition','HouseStyle','Heating','Exterior1st',
                  'KitchenAbvGr','BedroomAbvGr','OverallQual','OverallCond','YearBuilt','GarageArea','RoofMatl',
                  'SaleType','PoolArea','SalePrice')

selected2 <- subtrain[,selected_var2]
lm4 <- lm(SalePrice~.-SalePrice,data=selected2)
summary(lm4)

lm4$xlevels[["Neighborhood"]] <- union(lm4$xlevels[["Neighborhood"]], levels(valid$Neighborhood))
lm4$xlevels[["RoofMatl"]] <- union(lm4$xlevels[["RoofMatl"]], levels(valid$RoofMatl))
lm4$xlevels[["Condition2"]] <- union(lm4$xlevels[["Condition2"]], levels(valid$Condition2))
lm4$xlevels[["Exterior1st"]] <- union(lm4$xlevels[["Exterior1st"]], levels(valid$Exterior1st))
lm4$xlevels[["SaleType"]] <- union(lm4$xlevels[["SaleType"]], levels(valid$SaleType))
lm4$xlevels[["Functional"]] <- union(lm4$xlevels[["Functional"]], levels(valid$Functional))
lm4$xlevels[["Heating"]] <- union(lm4$xlevels[["Heating"]], levels(valid$Heating))

#Test the fourth model on the validation set
predicts4<-as.vector(predict(lm4,newdata=valid))
predicts4

SSE4 <- sum((observed-predicts4)^2)
MSE4 <- SSE4/(nrow(valid)-2)
MSE4 #5.278794e+15 large mse

#Model 5: explanatory variables selected based on the team's choices

select_var3 <- c('OverallQual','OverallCond','YearBuilt','ExterQual','ExterCond','TotalBsmtSF',
                'GrLivArea','BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd','Fireplaces',
                 'GarageArea','OpenPorchSF','PoolArea','Neighborhood','SalePrice')
selected3 <- subtrain[,select_var3]

lm5 <- lm(SalePrice~.-SalePrice,data=selected3)
summary(lm5)

lm5$xlevels[["ExterCond"]] <- union(lm5$xlevels[["ExterCond"]], levels(valid$ExterCond))

#Test the fifth model on the validation set
predicts5<-as.vector(predict(lm5,newdata=valid))
predicts5

SSE5 <- sum((observed-predicts5)^2)
MSE5 <- SSE5/(nrow(valid)-2)
MSE5 #1.963677e+14 smallest of all models!


#Model 2 and Model 5 have the best performances
#Develop the final model with the whole training set

#==============================Final Model 1=====================================
selectedtrain1 <- train[,selected_var]
lmf1 <- lm(SalePrice~.-SalePrice,data=selectedtrain1)
summary(lmf1)

#==============================Final Model 2=====================================
selectedtrain2 <- train[,select_var3]
lmf2 <- lm(SalePrice~.-SalePrice,data=selectedtrain2)
summary(lmf2)

#Predictions from the final model 1
preds1<-predict(lmf1,newdata=test,type="response")
preds1
#Check no missing values
sum(is.na(preds1))

preds2<-predict(lmf2,newdata=test,type="response")
preds2
#Check no missing values
sum(is.na(preds2))

#Output two set of predictions
mypreds1 <-cbind(test$Id,preds1)
mypreds1

mypreds2 <-cbind(test$Id,preds2)
mypreds2

write.table(mypreds1, file = "Competition1-5_submissions1.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")
write.table(mypreds2, file = "Competition1-5_submissions2.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")

