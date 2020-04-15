getwd()
setwd("D://House_Price")
Train_HP <- read.csv("D://House_Price/train.csv")
Test_Hp <- read.csv("D://House_Price/test.csv")
table(dim(Train_HP))
table(dim(Test_Hp))
summary(Train_HP)
str(Train_HP)
#------Check skewness of Target variable--------
plot(density(Train_HP$SalePrice),col='Red')

apply(Train_HP,2,function(x) length(unique(x))) #Check unique lenth of each column

Train_HP_Sale <- Train_HP$SalePrice #Save target variable to Train_HP_Sale as new variable
Train_HP$SalePrice <- NULL  # Drop Target column from Train
table(dim(Train_HP))

Combinedf <- merge(Train_HP,Test_Hp,all = TRUE) #Merge Train & Test
table(dim(Combinedf))
str(Combinedf)
table(is.na(Combinedf))
#View(Combinedf)
#---------Below variables deleted becuase they contains more than 70%  NA Values------
table(is.na(Combinedf$Alley))
table(is.na(Combinedf$PoolQC))
table(is.na(Combinedf$Fence))
table(is.na(Combinedf$MiscFeature))
table(is.na(Combinedf$MiscVal))

Combinedf$Alley <-  NULL
Combinedf$PoolQC <- NULL
Combinedf$Fence <- NULL
Combinedf$MiscFeature <- NULL
Combinedf$MiscVal <- NULL

colSums(is.na(Combinedf))
Combinedf$FireplaceQu <- NULL

# --------------Saperate numeric and categorical columns--------------

cat_vars <- Combinedf[,sapply(Combinedf,is.factor)]
str(cat_vars)

num_vars <- Combinedf[,!sapply(Combinedf,is.factor)]
str(num_vars)

length(num_vars)
length(cat_vars)

#------------Find missing values for numeric variables-----------

apply(num_vars,2,function(x)table(is.na(x))) # Check missing values columnwise


plot(density(Combinedf$LotFrontage,na.rm = T),col="Red") #Skewed
plot(density(Combinedf$MasVnrArea,na.rm = T),col="Red")  #Skewed
plot(density(Combinedf$BsmtFinSF1,na.rm = T),col="Red")  #Skewed
plot(density(Combinedf$BsmtFinSF2,na.rm = T),col="Red")  #Skewed
plot(density(Combinedf$BsmtUnfSF,na.rm = T),col="Red")  #Skewed
plot(density(Combinedf$TotalBsmtSF,na.rm = T),col="Red") #Skewed       #--Check skewness of each numeric variable---
plot(density(Combinedf$BsmtFullBath,na.rm = T),col="Red")  #Normal
plot(density(Combinedf$BsmtHalfBath,na.rm = T),col="Red")  #Normal
plot(density(Combinedf$GarageYrBlt,na.rm = T),col="Red")  #Skewed
plot(density(Combinedf$GarageCars,na.rm = T),col="Red")   #Normal
plot(density(Combinedf$GarageArea,na.rm = T),col="Red")   #Skewed
plot(density(Combinedf$BsmtFullBath,na.rm = T),col="Red") #Normal


#-------Filling Skewed variable missing values with median-----------

Combinedf$LotFrontage[is.na(Combinedf$LotFrontage)] <- median(Combinedf$LotFrontage,na.rm = T)
Combinedf$MasVnrArea[is.na(Combinedf$MasVnrArea)] <- median(Combinedf$MasVnrArea,na.rm = T)
Combinedf$BsmtFinSF1[is.na(Combinedf$BsmtFinSF1)] <- median(Combinedf$BsmtFinSF1,na.rm = T)
Combinedf$BsmtFinSF2[is.na(Combinedf$BsmtFinSF2)] <- median(Combinedf$BsmtFinSF2,na.rm = T)
Combinedf$BsmtUnfSF[is.na(Combinedf$BsmtUnfSF)] <- median(Combinedf$BsmtUnfSF,na.rm = T)
Combinedf$TotalBsmtSF[is.na(Combinedf$TotalBsmtSF)] <- median(Combinedf$TotalBsmtSF,na.rm = T)
Combinedf$GarageYrBlt[is.na(Combinedf$GarageYrBlt)] <- median(Combinedf$GarageYrBlt,na.rm = T)
Combinedf$GarageArea[is.na(Combinedf$GarageArea)] <- median(Combinedf$GarageArea,na.rm = T)
Combinedf$BsmtFullBath[is.na(Combinedf$BsmtFullBath)] <- median(Combinedf$BsmtFullBath,na.rm = T)

#-------Filling Normal variable missing values with mean-------

Combinedf$BsmtHalfBath[is.na(Combinedf$BsmtHalfBath)] <- mean(Combinedf$BsmtHalfBath,na.rm = T)
Combinedf$GarageCars[is.na(Combinedf$GarageCars)] <- mean(Combinedf$GarageCars,na.rm = T)


table(is.na(Combinedf)) #Check a values after missing value imputation for numeric variables

#----- Missing value imputation for categoriacl data-------
#----Here Combinedf(i)means categorical variable in Combinedf---

for(i in colnames(cat_vars))
  {
  
    for(j in colnames(Combinedf))
      {
        if(i==j)
          {
            val <- unique(Combinedf[i][!is.na(Combinedf[i])]);val
            mode <- val[which.max(tabulate(match(Combinedf[i], val)))];mode  #Mode of Combinedf(i)
            Combinedf[i][is.na(Combinedf[i])] <- mode
      
          }
    
      }
  
  }

table(is.na(Combinedf)) # Final check for missing values


#----Create dummy variables for categorical variables in Combinedf--------
#install.packages("caret",repos = "http://cran.r-project.org")
library(caret)
dmy <- dummyVars(" ~ .", data = Combinedf)
trsf <- data.frame(predict(dmy, newdata = Combinedf))
print(trsf)
str(trsf)

Combinedf <- data.frame(Combinedf,trsf) #Adding dummy variables in exising Combinedf data
str(Combinedf)

apply(trsf,2,function(x)length(unique(x))) # Check unique length after adding dummy variables

str(Combinedf)


#---------------Working for outliers-----------------
par(mfrow=c(1,4))
boxplot(Combinedf$LotArea.1);boxplot(Combinedf$LotFrontage.1);boxplot(Combinedf$MSSubClass.1);boxplot(Combinedf$OpenPorchSF)

par(mfrow=c(1,4))
boxplot(Combinedf$GarageArea);boxplot(Combinedf$GrLivArea);boxplot(Combinedf$X2ndFlrSF);boxplot(Combinedf$X1stFlrSF)

par(mfrow=c(1,4))
boxplot(Combinedf$TotalBsmtSF);boxplot(Combinedf$BsmtUnfSF);boxplot(Combinedf$BsmtFinSF1);boxplot(Combinedf$MasVnrArea)

par(mfrow=c(1,2))
boxplot(Combinedf$LotArea);boxplot(Combinedf$MSSubClass)



#----------Create function to capping the outliers with respective quantiles--------------

capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .98), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

#------Imputing outliers with capOutlier() function-----
Combinedf$LotArea.1 <- capOutlier(Combinedf$LotArea.1)
Combinedf$LotFrontage.1 <- capOutlier(Combinedf$LotFrontage.1)
Combinedf$MSSubClass.1 <- capOutlier(Combinedf$MSSubClass.1)
Combinedf$OpenPorchSF <- capOutlier(Combinedf$OpenPorchSF)
Combinedf$GarageArea <- capOutlier(Combinedf$GarageArea)
Combinedf$GrLivArea <- capOutlier(Combinedf$GrLivArea)
Combinedf$X2ndFlrSF <- capOutlier(Combinedf$X2ndFlrSF)
Combinedf$X1stFlrSF <- capOutlier(Combinedf$X1stFlrSF)
Combinedf$TotalBsmtSF <- capOutlier(Combinedf$TotalBsmtSF)
Combinedf$BsmtUnfSF <- capOutlier(Combinedf$BsmtUnfSF)
Combinedf$BsmtFinSF1 <- capOutlier(Combinedf$BsmtFinSF1)
Combinedf$MasVnrArea <- capOutlier(Combinedf$MasVnrArea)
Combinedf$LotArea <- capOutlier(Combinedf$LotArea)
Combinedf$MSSubClass<- capOutlier(Combinedf$MSSubClass)

#-----------------------------

#----------Split Dataset in to Train and Test--------

table(dim(Combinedf))
Train_House <- Combinedf[1:1460,]
table(dim(Train_House))
Test_House <- Combinedf[1461:2919,]
table(dim(Test_House))
Train_House$SalePrice


#--------------  #Add original Target variable as Target variable in Train_House.------
Train_House$SalePrice <- Train_HP_Sale
table(dim(Train_House))
#------------Remove outliers from Target vaiable-----
boxplot(Train_House$SalePrice)
Train_House$SalePrice <- capOutlier(Train_House$SalePrice)
boxplot(Train_House$SalePrice)
#------------------
#rm(list=ls()) #Clean Global Environment

#----Linear model-----
train_linear1 <- lm(SalePrice~.,data = Train_House)
summary(train_linear1)

#library(car)
#vif(train_linear1)

train_linear_new <- lm(Train_House$SalePrice~Train_House$MSZoning.RH + Train_House$LotFrontage + Train_House$LotArea + Train_House$Street.Pave + Train_House$LandSlope.Mod + 
                         Train_House$Neighborhood.Crawfor + Train_House$Neighborhood.Edwards + Train_House$Neighborhood.MeadowV + Train_House$Neighborhood.StoneBr + Train_House$Utilities.NoSeWa + 
                         Train_House$Neighborhood.Mitchel + Train_House$Neighborhood.StoneBr + Train_House$Condition1.Feedr + Train_House$Condition1.Norm + Train_House$Condition1.PosN + 
                         Train_House$Condition1.RRAn + Train_House$Condition1.RRNn + Train_House$Condition2.PosA + Train_House$Condition2.PosN + Train_House$OverallQual + Train_House$OverallCond +
                         Train_House$YearBuilt + Train_House$YearRemodAdd + Train_House$Exterior1st.HdBoard + Train_House$MasVnrType.Stone + Train_House$Foundation.PConc + Train_House$Foundation.Wood + Train_House$BsmtCond.Po + 
                         Train_House$BsmtExposure.Gd + Train_House$BsmtFinSF1 + Train_House$TotalBsmtSF + Train_House$Heating.GasA + Train_House$Heating.OthW + Train_House$Heating.Wall + Train_House$HeatingQC.Gd + Train_House$HeatingQC.TA + 
                         Train_House$CentralAir.Y + Train_House$Electrical.Mix + Train_House$LowQualFinSF + Train_House$GrLivArea + Train_House$BsmtFullBath + Train_House$FullBath + Train_House$KitchenAbvGr + Train_House$KitchenQual.Gd + 
                         Train_House$KitchenQual.TA + Train_House$Functional.Typ + Train_House$Fireplaces + 
                         Train_House$GarageType.Attchd+Train_House$GarageType.Basment, data = Train_House)


summary(train_linear_new)
vif(train_linear_new)

attach(Train_House)
train_linear_Final <- lm(SalePrice~LotArea+LandSlope.Mod+Neighborhood.Crawfor+Neighborhood.Edwards+Neighborhood.MeadowV+
                           Neighborhood.StoneBr+Neighborhood.Mitchel+Condition1.Norm+
                           OverallQual+OverallCond+YearBuilt+YearRemodAdd+
                           Exterior1st.HdBoard+Foundation.PConc+BsmtCond.Po+BsmtFinSF1+TotalBsmtSF+ 
                           HeatingQC.TA+Electrical.Mix+GrLivArea+BsmtFullBath+KitchenAbvGr+
                           KitchenQual.Gd+KitchenQual.TA+Functional.Typ+Fireplaces, data = Train_House)

  new_test <- data.frame(LotArea,LandSlope.Mod,Neighborhood.Crawfor,Neighborhood.Edwards,Neighborhood.MeadowV,
                         Neighborhood.StoneBr,Neighborhood.Mitchel,Condition1.Norm,
                         OverallQual,OverallCond,YearBuilt,YearRemodAdd,
                         Exterior1st.HdBoard,Foundation.PConc,BsmtCond.Po,BsmtFinSF1,TotalBsmtSF, 
                         HeatingQC.TA,Electrical.Mix,GrLivArea,BsmtFullBath,KitchenAbvGr,
                         KitchenQual.Gd,KitchenQual.TA,Functional.Typ,Fireplaces)
  
  summary(train_linear_Final)
#plot(train_linear_Final)
fittd_val <- fitted(train_linear_Final)
#residuals_val <- residuals(train_linear_Final)
#plot(fittd_val,residuals_val,type = 'o')


library(MASS)

stepAIC(train_linear_Final,direction = "both")


Final_fit <- lm(SalePrice ~ LotArea + LandSlope.Mod + Neighborhood.Crawfor + 
                Neighborhood.Edwards + Neighborhood.StoneBr + Neighborhood.Mitchel + 
                Condition1.Norm + OverallQual + OverallCond + YearBuilt + 
                YearRemodAdd + Exterior1st.HdBoard + Foundation.PConc + BsmtFinSF1 + 
                TotalBsmtSF + HeatingQC.TA + GrLivArea + KitchenAbvGr + KitchenQual.Gd + 
                KitchenQual.TA + Functional.Typ + Fireplaces,data=Train_House)


test_linear <- predict(Final_fit,newdata = new_test)

test_linear

#plot(test_linear,type = "l",lty=1.8,col="blue")


#---Model validation part-------------------------------------------------------------------------

#----1st method---RMSE()
rmse <- sqrt(mean(test_linear-Train_House$SalePrice)^2)
rmse

#----2nd method---Min_Max Accuray()
Min_Max_Accuracy <-  mean(min(Train_House$SalePrice, test_linear)/max(Train_House$SalePrice, test_linear))
#---------------------------------------------------------------------------------------------------

#Submit <- data.frame("Id"=Test_House$Id,"SalePrice"=test_linear)
New <- write.csv(test_linear,"D://House_Price/New_pred.csv")
head(test_linear)
head(Train_House$SalePrice)


#-------Decision Tree--------

library(rpart)
library(rpart.plot)
fit_dt <- rpart(Train_House$SalePrice~.,data = Train_House,method = "anova")
prp(fit_dt)


#----Plot the model---
plot(fit_dt, uniform=TRUE,main="Regression Tree")
text(fit_dt, use.n=TRUE, cex = .6)

printcp(fit_dt)
plotcp(fit_dt)

pred_dt <- predict(fit_dt,newdata = Test_House,method="anova")
pred_dt

#-------Check model accuracy-----

#a <- table(actual=Train_House$SalePrice,prediction=pred_dt);a
#sum(diag(a))/sum(a)#Check model accuracy

#library(caret)
#p1 <- predict(fit_dt,Train_House)
#confusionMatrix(p1,Train_House$SalePrice)
#length(p1)
#length(Train_House$SalePrice)

write.csv(pred_dt,"D://House_Price/Decision_Tree.csv")


#--------Random forest model--------
#attach(Train_House)
library(randomForest)
set.seed(123)
#rand_model <- randomForest(SalePrice~.,data = Train_House)
rand_model <- randomForest(SalePrice~.,data = Train_House,ntree=500,mtry=20,importance=T,proximity=T)

rand_model
print(rand_model)
importance(rand_model)
varImpPlot(rand_model)
varImp(rand_model)
getTree(rand_model,1)
pred_rf <- predict(rand_model,newdata = Test_House)


head(pred_rf)
attributes(rand_model)

RMSE(pred_rf,SalePrice)
write.csv(pred_rf,"D://House_Price/Random_Forest_New_test2.csv")

#-------Check randomForest model accuracy using lm()----------
#totaltest <- cbind(Test_House,pred_rf)
#reg <- lm(SalePrice~pred_rf,data = totaltest)
#summary(reg)
#------------------






