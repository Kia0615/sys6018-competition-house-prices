# competition1.5
# install.packages("class")
install.packages("dummies")
install.packages("xgboost")
install.packages("FNN")
library(class)
library(mice)
library(dplyr)
library(dummies)
library(Matrix)
library(xgboost)
library(FNN)


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
  ames_train_imp=complete(mice(ames_train[imputes],m=3))
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
anova(ames_model)
# Calculating prediction values
prices_test_clean$SalePrice <- predict(ames_model, newdata = data.frame(prices_test_clean)) 

# # Writing to file
write.csv(prices_test_clean[,c("Id","SalePrice")], file = "ames_submissions.csv", row.names=FALSE)
# scored 0.19211

# # Prepping data for non parametric model
prices_train_nonp_clean <- ames_clean(prices_train)
prices_test_nonp_clean <- ames_clean(prices_test)
prices_test_nonp_clean["Utilities"] <- NULL
prices_train_nonp_clean["Utilities"] <- NULL
prices_train_label <- prices_train_nonp_clean$SalePrice
prices_train_nonp_clean["SalePrice"] <- NULL
str(prices_test_nonp_clean)
# # apply(prices_train_nonp_clean,2,function(col)sum(is.na(col)))

# create a dummy data frame
ames_int <- function(ames_int){
  f <- sapply(ames_int, is.factor)
  factor_lab <- colnames(ames_int[,f])
  new_ames_int <- dummy.data.frame(ames_int, names = factor_lab, all = FALSE)
  new_ames_int <- cbind(ames_int[,!colnames(ames_int) %in% factor_lab],new_ames_int)
}
# prices_nonp_clean <- rbind(prices_train_nonp_clean,prices_test_nonp_clean)
# new_prices_nonp_clean <- ames_int(prices_nonp_clean)
prices_train_nonp_clean["BsmtFullBath"] <- NULL
prices_train_nonp_clean["BsmtHalfBath"] <- NULL
prices_test_nonp_clean["BsmtFullBath"] <- NULL
prices_test_nonp_clean["BsmtHalfBath"] <- NULL
new_prices_train_nonp_clean <- ames_int(prices_train_nonp_clean)
new_prices_test_nonp_clean <- ames_int(prices_test_nonp_clean)
# new_prices_train_nonp_clean<-new_prices_nonp_clean[1:nrow(prices_train_nonp_clean),]
# new_prices_test_nonp_clean<-new_prices_nonp_clean[1:nrow(prices_test_nonp_clean),]
new_prices_train_nonp_clean[] <- lapply(new_prices_train_nonp_clean, as.numeric)
new_prices_test_nonp_clean[] <- lapply(new_prices_test_nonp_clean, as.numeric)

str(new_prices_train_nonp_clean)
# XGBoost

#convert into numeric for XGBoost implementation
str(new_prices_train_nonp_clean)
prices_train_matrix=xgb.DMatrix(as.matrix(new_prices_train_nonp_clean),label= prices_train_label)
prices_test_matrix=xgb.DMatrix(as.matrix(new_prices_test_nonp_clean))

#xgboost parameters
xgb_params = list(
  booster = 'gblinear',
  objective = 'reg:linear'
)

# train data
prices_dt=xgboost(prices_train_matrix,params = xgb_params, nrounds = 150)
new_prices_test_nonp_clean_submit <- new_prices_test_nonp_clean
new_prices_test_nonp_clean_submit$SalePrice=predict(prices_dt,prices_test_matrix)
write.csv(new_prices_test_nonp_clean_submit[,c("Id","SalePrice")], file = "ames_submissionsxgb.csv", row.names=FALSE)
# scored 0.18799

# PCA Analysis
prices_nonp_clean <- rbind(prices_train_nonp_clean,prices_test_nonp_clean)
new_prices_nonp_clean <- ames_int(prices_nonp_clean)
new_prices_nonp_clean["BsmtFullBath"] <- NULL
new_prices_nonp_clean["BsmtHalfBath"] <- NULL
new_prices_nonp_clean["Id"] <- NULL
new_prices_nonp_clean[] <- lapply(new_prices_nonp_clean, as.numeric)
prices_prin_comp <- prcomp(new_prices_nonp_clean, scale. = T)
prices_prin_comp$rotation
biplot(prices_prin_comp, scale = 0)
pr_var<-(prices_prin_comp$sdev^2)
prop_varex <- pr_var/sum(pr_var)
#scree plot
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")

# knn model
new_prices_train_nonp_clean<-new_prices_nonp_clean[1:nrow(prices_train_nonp_clean),]
new_prices_test_nonp_clean<-new_prices_nonp_clean[1:nrow(prices_test_nonp_clean),]
prices_nonp <- knn.reg(train = new_prices_train_nonp_clean, test = new_prices_test_nonp_clean,prices_train_label, k=15)
prices_test_knn <- prices_test_clean
prices_test_knn$SalePrice <- prices_nonp$pred
write.csv(prices_test_knn[,c("Id","SalePrice")], file = "ames_submissionsknn.csv", row.names=FALSE)
# scored 0.22040
# summary(prices_train_nonp_clean)

#Try random forest
RF<-ranger(SalePrice~.,data=train,write.forest=T)
predsRF<-predict(RF,test)
mypreds3<-data.frame(Id=test$Id,SalePrice=prediction$predictions)
write.csv(result,"Competition1-5_submissions3.csv",row.names=F)
