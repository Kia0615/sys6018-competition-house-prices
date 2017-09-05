library(mice) 
library(ggplot2) 
library(corrplot) 

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
summary(prices_train_clean)
summary(prices_test_clean)
str(prices_train_clean)

## Histogram of SalePrice
options(scipen=100000)
ggplot(prices_train_clean, aes(x=SalePrice)) + 
  geom_histogram(color="black",fill="blue",bins=100) + 
  scale_x_continuous(name="SalePrice", limits=c(0,800000)) + 
  scale_y_continuous(name="") +
  theme(axis.text.x = element_text(angle=90,size=7)) +
  labs(x="",y="",title="Histogram of SalePrice")

## Correlation matrix
fact_train <- prices_train_clean
nums <- sapply(fact_train, is.numeric)
fact_train[,nums]   # drop non-numeric variables
str(fact_train[,nums])
corr_df <- fact_train[,nums]
corrplot(cor(corr_df),method="square",  tl.col = "black")

