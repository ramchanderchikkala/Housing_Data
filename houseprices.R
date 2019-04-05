workingdir <- 'D:/INSOFE/WS'
setwd(workingdir)
getwd()
rm(list=ls())
houseprices <-read.csv("D:/Datasets/Kaggle/Housing/train.csv",header=T,sep=",",na.strings=c(" ","?",NA))
str(houseprices)
sum(is.na(houseprices))

#Removing with columns with maximum null values
colSums(is.na(houseprices))

houseprices["MiscFeature"] <- NULL
houseprices["Fence"] <- NULL
houseprices["PoolQC"] <- NULL
houseprices["Alley"] <- NULL
houseprices["RoofMatl"] <- NULL
houseprices["Exterior1st"]<-NULL
houseprices["Exterior2nd"]<-NULL
houseprices["Heating"]<-NULL
houseprices["Electrical"]<-NULL
houseprices["GarageQual"]<-NULL

yvar <-houseprices["P"]
colSums(is.na(houseprices))
str(houseprices)
summary(houseprices)

##Handling of Missing values

#Since price has to be evaluated for each ID, cannot remove any rows
#MSSubclass - Q3-70 and Max is 190
#LotFrontage - imputing by Median
#Street only 6 rows are grvl rest are all pave; hence column can be dropped
#Utilities only 1 row is different from others hence column can be dropped
#MasVnrType only 8 NA's and majority are None - hence replacing values with None
#MasVnrArea impute by mean
#BsmtQual, NA means no basement
#BsmtCond, NA means no basement
#BsmtExposure, NA means no basement
#BsmtFinType1, NA means no basement
#BsmtFinType2, NA means no basement
#Electrical, most houses have SBrkr impute 1 value with same
#FireplaceQu, NA means no Fireplace
#GarageType, NA means no Garage
#GarageYrBlt, NA means no Garage
#GarageQual, NA means no Garage
#GarageCond, NA means no Garage


library(imputeR)

#LotFrontage replacing my mode
max(table(houseprices$LotFrontage))
colSums(is.na(houseprices))
##LF_med <- median(houseprices$LotFrontage)
plot(houseprices$LotFrontage)
houseprices$LotFrontage[is.na(houseprices$LotFrontage)]<-60
table(houseprices$LotFrontage)

#Street
houseprices$Street<- NULL

#Utilities
houseprices$Utilities <- NULL

#MASVnrType
table(houseprices$MasVnrType)
colSums(is.na(houseprices))
houseprices$MasVnrType[is.na(houseprices$MasVnrType)] <- "None"
table(houseprices$MasVnrType)

#MasVnrArea assume as 0
table(houseprices$MasVnrArea)
houseprices$MasVnrArea[is.na(houseprices$MasVnrArea)] <- 0


#BsmtQual,BsmtExposure,BsmtFinType1,BsmtFinType2 NA means No Basement

#BsmtQual
houseprices$BsmtQual <- as.character(houseprices$BsmtQual)
houseprices$BsmtQual<-ifelse(is.na(houseprices$BsmtQual),"No Basement",houseprices$BsmtQual)
table(houseprices$BsmtQual)

#BsmtCond
houseprices$BsmtCond <- as.character(houseprices$BsmtCond)
houseprices$BsmtCond <-ifelse(is.na(houseprices$BsmtCond),"No Basement",houseprices$BsmtCond)
table(houseprices$BsmtCond)

#BsmtExposure
houseprices$BsmtExposure <- as.character(houseprices$BsmtExposure)
houseprices$BsmtExposure <-ifelse(is.na(houseprices$BsmtExposure),"No Basement",houseprices$BsmtExposure)
table(houseprices$BsmtExposure)

#BsmtFinType1
houseprices$BsmtFinType1 <- as.character(houseprices$BsmtFinType1)
houseprices$BsmtFinType1 <-ifelse(is.na(houseprices$BsmtFinType1),"No Basement",houseprices$BsmtFinType1)
table(houseprices$BsmtFinType1)

#BsmtFinType2
houseprices$BsmtFinType2 <- as.character(houseprices$BsmtFinType2)
houseprices$BsmtFinType2 <-ifelse(is.na(houseprices$BsmtFinType2),"No Basement",houseprices$BsmtFinType2)
table(houseprices$BsmtFinType2)

#Electrical
colSums(is.na(houseprices))
houseprices$Electrical <- as.character(houseprices$Electrical)
houseprices$Electrical <-ifelse(is.na(houseprices$Electrical),"SBrkr",houseprices$Electrical)
table(houseprices$Electrical)

str(houseprices)

#FireplaceQu
table(houseprices$FireplaceQu)
houseprices$FireplaceQu <- as.character(houseprices$FireplaceQu)

houseprices$FireplaceQu <-ifelse(is.na(houseprices$FireplaceQu),"No Fireplace",houseprices$FireplaceQu)
table(houseprices$FireplaceQu)



#GarageType
table(houseprices$GarageType)
houseprices$GarageType <- as.character(houseprices$GarageType)

houseprices$GarageType <-ifelse(is.na(houseprices$GarageType),"No Garage",houseprices$GarageType)
table(houseprices$GarageType)


#GarageYrBlt is not real a significant factor type, size are more relevant hence droping

houseprices$GarageYrBlt <-NULL


#GarageQual
table(houseprices$GarageQual)
houseprices$GarageQual <- as.character(houseprices$GarageQual)

houseprices$GarageQual <-ifelse(is.na(houseprices$GarageQual),"No Garage",houseprices$GarageQual)

table(houseprices$GarageQual)


#GarageCond
table(houseprices$GarageCond)
houseprices$GarageCond <- as.character(houseprices$GarageCond)
houseprices$GarageCond <-ifelse(is.na(houseprices$GarageCond),"No Garage",houseprices$GarageCond)
table(houseprices$GarageCond)

#GarageFinish
table(houseprices$GarageFinish)
houseprices$GarageFinish <- as.character(houseprices$GarageFinish)
houseprices$GarageFinish <-ifelse(is.na(houseprices$GarageFinish),"No Garage",houseprices$GarageFinish)
table(houseprices$GarageFinish)


sum(is.na(houseprices))
colSums(is.na(houseprices))


## Imputation complete left with 74 variables


## PLotting to check distribution

library(ggplot2)
pl1 <- ggplot(houseprices,aes(SalePrice))
pl1 + geom_density(fill = "red", alpha = "0.7")
summary(houseprices$SalePrice)



## Checking for outliers
library(dplyr)
boxplot(houseprices$SalePrice)

Q1 <- houseprices %>% filter(SalePrice<quantile(SalePrice))
nrow(Q1)
Q2 <- houseprices %>% filter(SalePrice>quantile(SalePrice,0.25))
nrow(Q2)
Q3 <-houseprices %>% filter(SalePrice>quantile(SalePrice,0.5))
nrow(Q3)
Q4 <- houseprices %>% filter(SalePrice > quantile(SalePrice,0.75))
nrow(Q4)

##Subseting Factors and Numeric data
str(houseprices)
houseprice_cat <- subset(houseprices$LotFrontage,is.factor())

##install.packages("caret")
library(caret)
##install.packages("dummies")
library(dummies)
houseprices1 <-dummy.data.frame(houseprices,sep="_")
str(houseprices1)


##building linear model

##install.packages("ROCR")
library(ROCR)
##install.packages("MASS")
library(MASS)



model1 <- lm(SalePrice~.,houseprices1)

par(mfrow=c(2,2))
plot(model1)

summary(model1)
SP_Predict <- predict(model1,newdata=houseprices1)
write.csv(SP_Predict,file="PredictvaluesTrainMod1")

##Model 2 without column Condition1 and Condition2
houseprices["Condition1"] <- NULL
houseprices["Condition2"] <- NULL

houseprices2 <-dummy.data.frame(houseprices,sep="_")
houseprices2["HouseStyle_1.5Fin"]<-NULL
houseprices2["HouseStyle_2.5Fin"]<-NULL
houseprices2["RoofMatl_ClyTile"]<-NULL
houseprices2["RoofMatl_Membran"]<-NULL
houseprices2["RoofMatl_Metal"]<-NULL
model2 <- lm(SalePrice~.,houseprices2)

par(mfrow=c(2,2))
plot(model2)

summary(model2)



###model1perf <-modelPerformance(model1)

SP_Predict2 <- predict(model2,newdata=houseprices2)
write.csv(SP_Predict,file="PredictvaluesTrainMod2")


### Using Test data now ---------------------------------
houseprices_test <- read.csv("test - test.csv",header=T,sep=",",na.strings=c(" ","?",NA))

colnames(houseprices_test)

houseprices_test["MiscFeature"] <- NULL
houseprices_test["Fence"] <- NULL
houseprices_test["PoolQC"] <- NULL
houseprices_test["Alley"] <- NULL
houseprices_test["RoofMatl"] <- NULL
houseprices_test["Exterior1st"]<-NULL
houseprices_test["Exterior2nd"]<-NULL
houseprices_test["Heating"]<-NULL
houseprices_test["Electrical"]<-NULL
houseprices_test["GarageQual"]<-NULL
#LotFrontage replacing my mode
max(table(houseprices_test$LotFrontage))
colSums(is.na(houseprices_test))
##LF_med <- median(houseprices$LotFrontage)
plot(houseprices_test$LotFrontage)
houseprices_test$LotFrontage[is.na(houseprices_test$LotFrontage)]<-60
table(houseprices_test$LotFrontage)

#Street
houseprices_test$Street<- NULL

#Utilities
houseprices_test$Utilities <- NULL

#MASVnrType
table(houseprices_test$MasVnrType)
colSums(is.na(houseprices_test))
houseprices_test$MasVnrType[is.na(houseprices_test$MasVnrType)] <- "None"
table(houseprices_test$MasVnrType)

#MasVnrArea assume as 0
table(houseprices_test$MasVnrArea)
houseprices_test$MasVnrArea[is.na(houseprices_test$MasVnrArea)] <- 0


#BsmtQual,BsmtExposure,BsmtFinType1,BsmtFinType2 NA means No Basement

#BsmtQual
houseprices_test$BsmtQual <- as.character(houseprices_test$BsmtQual)
houseprices_test$BsmtQual<-ifelse(is.na(houseprices_test$BsmtQual),"No Basement",houseprices_test$BsmtQual)
table(houseprices_test$BsmtQual)

#BsmtCond
houseprices_test$BsmtCond <- as.character(houseprices_test$BsmtCond)
houseprices_test$BsmtCond <-ifelse(is.na(houseprices_test$BsmtCond),"No Basement",houseprices_test$BsmtCond)
table(houseprices_test$BsmtCond)

#BsmtExposure
houseprices_test$BsmtExposure <- as.character(houseprices_test$BsmtExposure)
houseprices_test$BsmtExposure <-ifelse(is.na(houseprices_test$BsmtExposure),"No Basement",houseprices_test$BsmtExposure)
table(houseprices_test$BsmtExposure)

#BsmtFinType1
houseprices_test$BsmtFinType1 <- as.character(houseprices_test$BsmtFinType1)
houseprices_test$BsmtFinType1 <-ifelse(is.na(houseprices_test$BsmtFinType1),"No Basement",houseprices_test$BsmtFinType1)
table(houseprices_test$BsmtFinType1)

#BsmtFinType2
houseprices_test$BsmtFinType2 <- as.character(houseprices_test$BsmtFinType2)
houseprices_test$BsmtFinType2 <-ifelse(is.na(houseprices_test$BsmtFinType2),"No Basement",houseprices_test$BsmtFinType2)
table(houseprices_test$BsmtFinType2)

#Electrical
colSums(is.na(houseprices_test))
houseprices_test$Electrical <- as.character(houseprices_test$Electrical)
houseprices_test$Electrical <-ifelse(is.na(houseprices_test$Electrical),"SBrkr",houseprices_test$Electrical)
table(houseprices_test$Electrical)

str(houseprices_test)

#FireplaceQu
table(houseprices_test$FireplaceQu)
houseprices_test$FireplaceQu <- as.character(houseprices_test$FireplaceQu)

houseprices_test$FireplaceQu <-ifelse(is.na(houseprices_test$FireplaceQu),"No Fireplace",houseprices_test$FireplaceQu)
table(houseprices_test$FireplaceQu)



#GarageType
table(houseprices_test$GarageType)
houseprices_test$GarageType <- as.character(houseprices_test$GarageType)

houseprices_test$GarageType <-ifelse(is.na(houseprices_test$GarageType),"No Garage",houseprices_test$GarageType)
table(houseprices_test$GarageType)


#GarageYrBlt is not real a significant factor type, size are more relevant hence droping

houseprices_test$GarageYrBlt <-NULL


#GarageQual
table(houseprices_test$GarageQual)
houseprices_test$GarageQual <- as.character(houseprices_test$GarageQual)

houseprices_test$GarageQual <-ifelse(is.na(houseprices_test$GarageQual),"No Garage",houseprices_test$GarageQual)

table(houseprices_test$GarageQual)


#GarageCond
table(houseprices_test$GarageCond)
houseprices_test$GarageCond <- as.character(houseprices_test$GarageCond)
houseprices_test$GarageCond <-ifelse(is.na(houseprices_test$GarageCond),"No Garage",houseprices_test$GarageCond)
table(houseprices_test$GarageCond)

#GarageFinish
table(houseprices_test$GarageFinish)
houseprices_test$GarageFinish <- as.character(houseprices_test$GarageFinish)
houseprices_test$GarageFinish <-ifelse(is.na(houseprices_test$GarageFinish),"No Garage",houseprices_test$GarageFinish)
table(houseprices_test$GarageFinish)


sum(is.na(houseprices_test))

colSums(is.na(houseprices_test))


##MSZONING

table(houseprices_test$MSZoning)
max(table(houseprices_test$MSZoning))
houseprices_test$MSZoning <- as.character(houseprices_test$MSZoning)
houseprices_test$MSZoning <-ifelse(is.na(houseprices_test$MSZoning),"RL",houseprices_test$MSZoning)

str(houseprices_test)
##Exterior1st
table(houseprices_test$Exterior1st)
max(table(houseprices_test$Exterior1st))
##houseprices_test$Exterior1st <- as.character(houseprices_test$Exterior1st)
houseprices_test$Exterior1st[is.na(houseprices_test$Exterior1st)] <- "VinylSd"

##Exterior2nd
table(houseprices_test$Exterior2nd)
max(table(houseprices_test$Exterior2nd))
##houseprices_test$Exterior1st <- as.character(houseprices_test$Exterior1st)
houseprices_test$Exterior2nd[is.na(houseprices_test$Exterior2nd)] <- "VinylSd"

table(houseprices_test$BsmtFinSF1)
max(table(houseprices_test$BsmtFinSF1))
houseprices_test$BsmtFinSF1[is.na(houseprices_test$BsmtFinSF1)] <- 0

table(houseprices_test$BsmtFinSF2)
max(table(houseprices_test$BsmtFinSF2))
houseprices_test$BsmtFinSF2[is.na(houseprices_test$BsmtFinSF2)] <- 0

table(houseprices_test$BsmtUnfSF)
max(table(houseprices_test$BsmtUnfSF))
houseprices_test$BsmtUnfSF[is.na(houseprices_test$BsmtUnfSF)] <- 0


table(houseprices_test$TotalBsmtSF)
max(table(houseprices_test$TotalBsmtSF))
houseprices_test$TotalBsmtSF[is.na(houseprices_test$TotalBsmtSF)] <- 0



table(houseprices_test$BsmtFullBath)
max(table(houseprices_test$BsmtFullBath))
houseprices_test$BsmtFullBath[is.na(houseprices_test$BsmtFullBath)] <- 0



table(houseprices_test$BsmtHalfBath)
max(table(houseprices_test$BsmtHalfBath))
houseprices_test$BsmtHalfBath[is.na(houseprices_test$BsmtHalfBath)] <- 0


##KitchenQual
table(houseprices_test$KitchenQual)
max(table(houseprices_test$KitchenQual))
houseprices_test$KitchenQual <- as.character(houseprices_test$KitchenQual)
houseprices_test$KitchenQual<-ifelse(is.na(houseprices_test$KitchenQual),"TA",houseprices_test$KitchenQual)

##GarageCars 
table(houseprices_test$GarageCars)
max(table(houseprices_test$GarageCars))
houseprices_test$GarageCars[is.na(houseprices_test$GarageCars)] <- 2

###GarageArea 
table(houseprices_test$GarageArea)
max(table(houseprices_test$GarageArea))
houseprices_test$GarageArea[is.na(houseprices_test$GarageArea)] <- 0


##Functional *------
table(houseprices_test$Functional)
max(table(houseprices_test$Functional))
houseprices_test$Functional <- as.character(houseprices_test$Functional)
houseprices_test$Functional<-ifelse(is.na(houseprices_test$Functional),"Typ",houseprices_test$Functional)

##SaleType
table(houseprices_test$SaleType)
max(table(houseprices_test$SaleType))
houseprices_test$SaleType <- as.character(houseprices_test$SaleType)
houseprices_test$SaleType<-ifelse(is.na(houseprices_test$SaleType),"WD",houseprices_test$SaleType)

houseprices_test$Condition1 <-NULL
houseprices_test$Condition2 <-NULL


sum(is.na(houseprices_test))

colSums(is.na(houseprices_test))
table(houseprices_test$MSZoning)

### Missing values treatment done
### Dummification & prediction
library(dummies)
houseprices_test1 <-dummy.data.frame(houseprices_test,sep="_")


colnames(houseprices_test1)
SP_Predict_test <- predict(model2,newdata=houseprices_test1,se.fit=TRUE)
write.csv(SP_Predict_test,file="PredictvaluesTest")

##table(houseprices$GarageQual)
summary(model2)


## Model Performance evaluation

modelSummary <- summary(model2)
modelCoeffs <- modelSummary$coefficients 
beta.estimate <- modelCoeffs["SalePrice","Estimate"]

std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)