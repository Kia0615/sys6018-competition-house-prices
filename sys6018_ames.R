# competition1.5
library(mice)
library(dplyr)

set.seed(20)
prices_train <- read.csv("train.csv")
prices_test <- read.csv("test.csv")

ames_clean <- function(ames_train) {
  imputes <- c("MSZoning","LotFrontage","Utilities","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea",
               "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Electrical","KitchenQual","Functional"
               ,"GarageCars","GarageArea","SaleType","GarageYrBlt")
  factors <- c("MSSubClass")
  natonone <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu",
                "GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")
  ames_train_imp=complete(mice(ames_train[imputes],m=2))
  ames_train[imputes] <- ames_train_imp[imputes]
  # Treating MSSubClass as factor
  ames_train[,factors]<-as.factor(ames_train[,factors])
  # Converting NA to None
  for (f in natonone){
    levels(ames_train[,f])
    if(class(ames_train[,f])=="factor"){
      levels(ames_train[,f]) <- c(levels(ames_train[,f]),"None")
    }
  }
  ames_train[is.na(ames_train)]<-"None"
  return(ames_train)
}
prices_train_clean <- ames_clean(prices_train)
prices_test_clean <- ames_clean(prices_test)
summary(prices_test_clean)
# Training Model
ames_model <- lm(SalePrice~MSZoning+LotArea+Street+
              LotConfig+LandSlope+Neighborhood+Condition1+OverallQual+OverallCond+
              YearBuilt+RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
              BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+
              ScreenPorch+PoolArea+PoolQC,prices_train_clean)
summary(ames_model)



# Calculating prediction values
prices_test_predict <- predict(ames_model, newdata = data.frame(prices_test_clean)) 


# Writing to file
write.csv(test.imp$ximp[,c("PassengerId","Survived")], file = "np2hf_submissions.csv", row.names=FALSE)