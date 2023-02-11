library("tidyverse")
library("sqldf")
library("readxl")
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("corrplot")
library("car")
library("estimatr")
library("caret")
library("writexl")
library(mice)
library (caTools)
library(glmnet)
library(ggplot2)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(mice)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(psych)
library(knitr)


options(max.print=1000000)
train <- read.csv("D:/MMA 867 Predictive Modelling/Assignments/Team Assignment/train.csv",
                  header=TRUE,
                  sep = ",")
test <- read.csv("D:/MMA 867 Predictive Modelling/Assignments/Team Assignment/test.csv",
                 header=TRUE,
                 sep = ",")
#Getting rid of the IDs from train and test sets but storing them in a vector. 

test$SalePrice <- NA
combined <- rbind(train, test)
dim(combined)
ncol(train)
ncol(test)

#Finding the variables which have NAs
names(which(colSums(is.na(combined))>0))

#Replacing the NAs of variables
#MSZoning
barchart(combined$MSZoning)
combined$MSZoning[is.na(combined$MSZoning)] <- "RL" #Replacing with most frequent value
combined$MSZoning <- as.factor(combined$MSZoning) #Converting it into a factor as there is no ranking

#Alley
combined$Alley[is.na(combined$Alley)] <- "No alley access" #NA for Alley means No alley access
combined$Alley <- as.factor(combined$Alley) #Converting it into a factor as there is no ranking

#Utilities
table(combined$Utilities)
barchart(combined$Utilities)
kable(combined[is.na(combined$Utilities) | combined$Utilities=='NoSeWa', 1:9])
#There are 2916 houses which have AllPub, 1 house with NoSewa and 2 houses with NA
#for utilities in the test dataset. So if we replace the NA in the utilities 
#for two houses in the test dataset, almost all houses will be having AllPub in utilities.
#This will make it  useless indicator. Hence dropping it.
combined$Utilities <- NULL

#Exterior1st
barchart(combined$Exterior1st)
#VinylSd is the most occuring value.
kable(combined[is.na(combined$Exterior1st) , 1:3])
#There is only one house with an NA for Exterior1st, in the testdataset.
#We will replace it with the most occuring value VinylSd
combined$Exterior1st[is.na(combined$Exterior1st)] <- "VinylSd"
#Converting it into a factor as there is no ranking
combined$Exterior1st <- as.factor(combined$Exterior1st)

#Exterior2nd
barchart(combined$Exterior2nd)
#VinylSd is the most occuring value.
kable(combined[is.na(combined$Exterior2nd) , 1:3])
#There is only one house with an NA for Exterior2nd, in the test dataset.
#We will replace it with the most occuring value VinylSd
combined$Exterior2nd[is.na(combined$Exterior2nd)] <- "VinylSd"
#Converting it into a factor as there is no ranking
combined$Exterior2nd <- as.factor(combined$Exterior2nd)

#ExterQual
kable(combined[is.na(combined$ExterQual) , 1:3])
#There are no NAs.
#Converting into a ranking
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$ExterQual<-as.integer(revalue(combined$ExterQual, Qualities))

#ExterCond
kable(combined[is.na(combined$ExterCond) , 1:3])
#There are no NAs.
#Converting into a ranking
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$ExterCond<-as.integer(revalue(combined$ExterCond, Qualities))

#MasVnrType (Checking MasVnrArea simultaneously)
kable(combined[is.na(combined$MasVnrType) , 1:3])
#MasVnrType has 24 NAs in both train and test datasets
kable(combined[is.na(combined$MasVnrArea) , 1:3])
#MasVnrArea has 23 NAs in both train and test datasets
#If a house has a MasVnrType, it should also have a MasVnrArea.
#Checking if the 23 houses which have MasVnrArea also have a NA for MasVnrType
kable(combined[is.na(combined$MasVnrType)& is.na(combined$MasVnrArea), 1:3])
#They do. But there is one house which has NA for MasVnrType but a numerical value 
#for MasVnrArea. This cannot happen and should be fixed.
#Finding that row. It happens to be row 2611
kable(combined[is.na(combined$MasVnrType)& !is.na(combined$MasVnrArea), 1:3])
barchart(combined$MasVnrType)
#None is the most common MasVnrType followed by BrkFace
#Replacing the MasVnrType for the row 2611 with BrkFace
combined$MasVnrType[2611] <- "BrkFace"
#Replacing the NAs of the remaianing 23 MasVnrType with None 
#(as it means the house has no Masonry)
combined$MasVnrType[is.na(combined$MasVnrType)] <- 'None'
#Checking whether MasVnrType is an ordinal variable
combined[!is.na(combined$SalePrice),] %>% group_by(MasVnrType) %>% dplyr::summarize(median = median(SalePrice), counts=n()) %>% arrange(median)
#Brick Common and None seem to be the least priced followed by Brick Face and Stone
#Hence introducing ranking
MasonryRanking <- c( 'BrkCmn'=0,'None'=0, 'BrkFace'=1, 'Stone'=2)
combined$MasVnrType<-as.integer(revalue(combined$MasVnrType, MasonryRanking))
#Converting single remaining MasvnrArea which is NA to 0
combined$MasVnrArea[is.na(combined$MasVnrArea)] <-0

#Basement related Categorical variables which have NAs
length(which(is.na(combined$BsmtQual))>0) #BsmtQual has 81 NAs
length(which(is.na(combined$BsmtCond))>0) #BsmtCond has 82 NAs
length(which(is.na(combined$BsmtExposure))>0) #BsmtExposure has 82 NAs
length(which(is.na(combined$BsmtFinType1))>0) #BsmtFinType1 has 79 NAs
length(which(is.na(combined$BsmtFinType2))>0) #BsmtFinType2 has 80 NAs
length(which(is.na(combined$BsmtQual) & is.na(combined$BsmtCond) & is.na(combined$BsmtExposure) & is.na(combined$BsmtFinType1) & is.na(combined$BsmtFinType2)))
#So above 5 variables have 79 common NAs. This implies there are 79 homes 
#without basement.
#Finding the rows where BsmtFinType1 does not have an NA and the other 
#4 basement variables have NA
combined[!is.na(combined$BsmtFinType1) & (is.na(combined$BsmtCond)|is.na(combined$BsmtQual)|is.na(combined$BsmtExposure)|is.na(combined$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
#These 9 houses have a basement. The values of one of the basement related variables
#are just missing (have NAs). We can fill those NAs with the most common category value
#Replacing NA of BsmtFinType2 IN row 333 with 
barchart(combined$BsmtFinType2) #"Unf" as it is the most common value
combined$BsmtFinType2[333] <- "Unf"
#Replacing NA of BsmtFinType2 IN row 949,1488,2349 with 
barchart(combined$BsmtExposure)
combined$BsmtExposure[c(949,1488,2349)] <- "No"#"No" as it is the most common value
#Replacing NA of BsmtFinType2 IN row 2041,2186,2525 with 
barchart(combined$BsmtCond)
combined$BsmtCond[c(2041,2186,2525)] <- "TA"#"TA" as it is the most common value
#Replacing NA of BsmtQual IN row 2218,2219 with 
barchart(combined$BsmtQual)
combined$BsmtQual[c(2218,2219 )] <- "TA"#"TA" as it is the most common value
#Replacing remaining 79 NAs of BsmtFinType1 with "No_Basement"
combined$BsmtFinType1[is.na(combined$BsmtFinType1)] <- 'No_Basement'
# Since BsmtFinType1 has ranking, converting it into an integer
finishtype <- c('No_Basement'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
combined$BsmtFinType1 <- as.integer(revalue(combined$BsmtFinType1, finishtype))
#Replacing remaining 79 NAs of BsmtFinType2 with "No_Basement"
combined$BsmtFinType2[is.na(combined$BsmtFinType2)] <- 'No_Basement'
# Since BsmtFinType1 has ranking, converting it into an integer
finishtype <- c('No_Basement'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
combined$BsmtFinType2 <- as.integer(revalue(combined$BsmtFinType2, finishtype))
#Replacing remaining 79 NAs of BsmtExposure with "No_Basement"
combined$BsmtExposure[is.na(combined$BsmtExposure)] <- 'No_Basement'
# Since BsmtExposure has ranking, converting it into an integer
Exposure <- c('No_Basement'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
combined$BsmtExposure<-as.integer(revalue(combined$BsmtExposure, Exposure))
#Replacing remaining 79 NAs of BsmtCond with "No_Basement"
combined$BsmtCond[is.na(combined$BsmtCond)] <- 'No_Basement'
# Since BsmtCond has ranking, converting it into an integer
Qualities <- c('No_Basement' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$BsmtCond<-as.integer(revalue(combined$BsmtCond, Qualities))
#Replacing remaining 79 NAs of BsmtQualwith "No_Basement"
combined$BsmtQual[is.na(combined$BsmtQual)] <- 'No_Basement'
# Since BsmtQual has ranking, converting it into an integer
Qualities <- c('No_Basement' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$BsmtQual<-as.integer(revalue(combined$BsmtQual, Qualities))

#Basement related Numerical variables which have NAs
length(which(is.na(combined$BsmtFinSF1))>0) #BsmtFinSF1 has 1 NA
length(which(is.na(combined$BsmtFinSF2))>0) #BsmtFinSF2 has 1 NA
length(which(is.na(combined$BsmtUnfSF))>0) #BsmtUnfSF has 1 NA
length(which(is.na(combined$TotalBsmtSF))>0) #TotalBsmtSF has 1 NA
length(which(is.na(combined$BsmtFullBath))>0) #BsmtFullBath has 2 NAs
length(which(is.na(combined$BsmtHalfBath))>0) #BsmtHalfBath has 2 NAs
#All Basement related Numerical variables which have NAs 
#should have value of zero when any of the categorical variables have value 0
#Checking which rows have NAs for numerical Basement variables
combined[(is.na(combined$BsmtFinSF1)|is.na(combined$BsmtFinSF2)|is.na(combined$BsmtUnfSF)|is.na(combined$TotalBsmtSF)|is.na(combined$BsmtFullBath)|is.na(combined$BsmtHalfBath)), c('BsmtFinType1','BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
#It is clear that these NA numerical Basement variables should be made 0
combined$BsmtFinSF1[is.na(combined$BsmtFinSF1)] <-0
combined$BsmtFinSF2[is.na(combined$BsmtFinSF2)] <-0
combined$BsmtUnfSF[is.na(combined$BsmtUnfSF)] <-0
combined$TotalBsmtSF[is.na(combined$TotalBsmtSF)] <-0
combined$BsmtFullBath[is.na(combined$BsmtFullBath)] <-0
combined$BsmtHalfBath[is.na(combined$BsmtHalfBath)] <-0

#Garage related variables which have NAs
length(which(is.na(combined$GarageType))>0) #GarageType has 157 NAs
length(which(is.na(combined$GarageYrBlt))>0) #GarageYrBlt has 159 NAs
length(which(is.na(combined$GarageFinish))>0) #GarageFinish has 159 NAs
length(which(is.na(combined$GarageCars))>0) #GarageCars has 1 NA
length(which(is.na(combined$GarageArea))>0) #GarageArea has 1 NA
length(which(is.na(combined$GarageQual))>0) #GarageQual has 159 NAs
length(which(is.na(combined$GarageCond))>0) #GarageCond has 159 NAs    
#Assuming GarageYrBlt is YearBuilt for GarageYrBlt NAs
combined$GarageYrBlt[is.na(combined$GarageYrBlt)] <- combined$YearBuilt[is.na(combined$GarageYrBlt)]
#Checking if the same rows have NAs in GarageType,GarageFinish,GarageQual,GarageCond
length(which(is.na(combined$GarageType) & is.na(combined$GarageFinish) & is.na(combined$GarageCond) & is.na(combined$GarageQual)))
#They do. These 157 houses have no garages.
#There are two other houses which have NAs for GarageFinish,GarageQual,GarageCond 
#Finding those two houses
kable(all[!is.na(all$GarageType) & is.na(all$GarageQual), c( 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish','GarageCars')])
#They are houses 2127, 2577. House 2127 looks like a house that has a garage.
#House 2577 looks like a house that does not have a garage.
#We will impute values for house 2127 and make NAs of Garage numerical variables
#and GarageType of house 2577 as "No Garage"
barchart(combined$GarageFinish)
combined$GarageFinish[c(2127)] <- "Unf"#"Unf" as it is the most common value
barchart(combined$GarageQual)
combined$GarageQual[c(2127)] <- "TA"#"TA" as it is the most common value
barchart(combined$GarageCond)
combined$GarageCond[c(2127)] <- "TA"#"TA" as it is the most common value
combined$GarageCars[2577] <- 0
combined$GarageArea[2577] <- 0
combined$GarageType[2577] <- NA
#Converting the rest of the NA Garage variables 
combined$GarageType[is.na(combined$GarageType)] <- 'No Garage'
combined$GarageType <- as.factor(combined$GarageType) #Not converting to a rank as there
#seems to be no ranking
combined$GarageFinish[is.na(combined$GarageFinish)] <- 'No Garage'
Finish <- c('No Garage'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
combined$GarageFinish<-as.integer(revalue(combined$GarageFinish, Finish))#Converting to a 
#rank as there seems to be ranking
combined$GarageQual[is.na(combined$GarageQual)] <- 'No Garage'
Qualities <- c('No Garage' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$GarageQual<-as.integer(revalue(combined$GarageQual, Qualities))#Converting to a 
#rank as there seems to be ranking
combined$GarageCond[is.na(combined$GarageCond)] <- 'No Garage'
Qualities <- c('No Garage' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$GarageCond<-as.integer(revalue(combined$GarageCond, Qualities))#Converting to a 
#rank as there seems to be  ranking

#486 (of 2920 i.e. 17%) LotFrontage values have NA
length(which(is.na(combined$LotFrontage))>0)
#Replacing NAs with multiple imputation values
df <- combined[,c("LotFrontage","Neighborhood")]
imputed_data <- mice(df, m=17, maxit=30, meth='pmm', seed=1)
completed_data <- complete(imputed_data, 1)
combined$LotFrontage <- completed_data$LotFrontage

#Electrical
barchart(combined$Electrical)
combined$Electrical[is.na(combined$Electrical)] <- "SBrkr" #Replacing with most frequent value
combined$Electrical <- as.factor(combined$Electrical)# Not converting to a 
#rank as there seems to be no ranking

#KitchenQual
barchart(combined$KitchenQual)
combined$KitchenQual[is.na(combined$KitchenQual)] <- 'TA' #replace with most common value
Qualities <- c( 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$KitchenQual<-as.integer(revalue(combined$KitchenQual, Qualities))#Converting to a 
#rank as there seems to be  ranking

#FireplaceQu
barchart(combined$KitchenQual)
combined$FireplaceQu[is.na(combined$FireplaceQu)] <- 'No Fireplace' #replace with most common value
Qualities <- c( 'No Fireplace'=0,'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined$FireplaceQu<-as.integer(revalue(combined$FireplaceQu, Qualities))#Converting to a 
#rank as there seems to be  ranking
table(combined$FireplaceQu)

#PoolQC
combined$PoolQC[is.na(combined$PoolQC)] <- 'No Pool'
Qualities <- c('No Pool' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)
combined$PoolQC<-as.integer(revalue(combined$PoolQC, Qualities)) #Converting to a rank 
#as there seems to be ranking
# A house that has PoolArea should also have PoolQC and vice versa.If  not,
#it is an error we should rectify
combined[combined$PoolArea>0 & combined$PoolQC==0, c('PoolArea', 'PoolQC')]
combined[combined$PoolArea==0 & combined$PoolQC!=0, c('PoolArea', 'PoolQC')]
#There are no houses without PoolArea and PoolQC but there are three houses
#with PoolArea but PoolQC.- House 2421, 2504, 2600. We will assume that the pool quality of these 
#houses is the same as the overall quality of the houses.
combined[combined$PoolArea>0 & combined$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]
#Hence, the Below Average overall quality of House 2421 translates to Fair Pool
#Quality(1). Above Average overall quality of House 2504 translates to Average/Typical 
#Pool Quality(2). Fair overall quality of House 2600 translates to Fair Pool Quality(1)
combined$PoolQC[2421] <- 1
combined$PoolQC[2504] <- 2
combined$PoolQC[2600] <- 1

#Functional
barchart(combined$Functional)
combined$Functional[is.na(combined$Functional)] <- 'Typ' #replace with most common value
Functionality <- c( 'Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4,'Min2' = 5,'Min1' = 6,'Typ' = 7)
combined$Functional<-as.integer(revalue(combined$Functional, Functionality))#Converting to a 
#rank as there seems to be  ranking

#Fence
combined$Fence[is.na(combined$Fence)] <- 'No Fence'
#Checking if there is ranking in Fence
combined[!is.na(combined$SalePrice),] %>% group_by(Fence) %>% dplyr::summarize(median = median(SalePrice), counts=n())
#Counterintuitive that No Fence house command a higher price in general. Hence not ranking
#it and instead converting it into a factor.
combined$Fence<-as.factor(combined$Fence)

#MiscFeature
combined$MiscFeature[is.na(combined$MiscFeature)] <- 'None'
#Checking if there is ranking in Fence
combined[!is.na(combined$SalePrice),] %>% group_by(MiscFeature) %>% dplyr::summarize(median = median(SalePrice), counts=n())
#Houses with no garage command a higher price in houses with shed and other miscfeatures.
#This does not make sense intuitively. Though considered the option of ranking them
#because houses with tennis court command much higher price, there is only one house
#with tennis court in the entire dataset. Hence not introducing a ranking and converting it
#into a factor.
combined$MiscFeature <- as.factor(combined$MiscFeature)

#SaleType
barchart(combined$SaleType)
combined$SaleType[is.na(combined$SaleType)] <- "WD" #replace with most common value
combined$SaleType <- as.factor(combined$SaleType)
table(combined$SaleType)

#Checking if remaining character variables need to be changed to Factors/Need Ranking 
Charcolumns <- names(combined[,sapply(combined, is.character)])
Charcolumns
#There are 18 character variables now. Lets check them one by one.

#Street. Common Knowledge tells us that houses which are on a paved street command
#more price than houses on gravel streets.So will introduce ranking.
StreetType <- c( 'Grvl'=0, 'Pave'=1)
combined$Street <-as.integer(revalue(combined$Street, StreetType))

#LotShape. Common Knowledge tells us that regular shape properties command
#more price than houses with irregular shape.So will introduce ranking.
LotShapeType <- c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)
combined$LotShape<-as.integer(revalue(combined$LotShape,LotShapeType))

#LandContour. Intuitively, we feel near flat/level properties should command more price
#than those which are on a depression. Lets check if such a relation exists.
combined[!is.na(combined$SalePrice),] %>% group_by(LandContour) %>% dplyr::summarize(median = median(SalePrice), counts=n())
#The results indicate our intuition was wrong. Hence, not introducing a ranking and 
#converting it into as a factor
combined$LandContour <- as.factor(combined$LandContour)

#LotConfig. Intuition does not indicate properties with different lot configuration
#command different prices. Confirming the same below
combined[!is.na(combined$SalePrice),] %>% group_by(LotConfig) %>% dplyr::summarize(median = median(SalePrice), counts=n())
#It is confirmed. Hence, not introducing a ranking and converting it into as a factor
combined$LotConfig <- as.factor(combined$LotConfig)

#LandSlope. Intuitively, we feel properties with general slope should command more price
#than those which are on a severe slope. 
Ranking <- c('Sev'=0, 'Mod'=1, 'Gtl'=2)
combined$LandSlope<-as.integer(revalue(combined$LandSlope, Ranking))
table(combined$LandSlope)

#Neighborhood. Does not seem to have any ranking. So converting to a factor
combined$Neighborhood <- as.factor(combined$Neighborhood)

#Condition1. Does not seem to have any ranking. So converting to a factor
combined$Condition1 <- as.factor(combined$Condition1)

#Condition2. Does not seem to have any ranking. So converting to a factor
combined$Condition2 <- as.factor(combined$Condition2)

#BldgType. Does not seem to have any ranking. So converting to a factor
combined$BldgType <- as.factor(combined$BldgType)

#HouseStyle. Does not seem to have any ranking. So converting to a factor
combined$HouseStyle <- as.factor(combined$HouseStyle)

#RoofStyle. Does not seem to have any ranking. So converting to a factor
combined$RoofStyle <- as.factor(combined$RoofStyle)

#RoofMatl. Does not seem to have any ranking. So converting to a factor
combined$RoofMatl <- as.factor(combined$RoofMatl)

#Foundation. Not sure if there would be ranking here. Checking the same.
combined[!is.na(combined$SalePrice),] %>% group_by(Foundation) %>% dplyr::summarize(median = median(SalePrice), counts=n())
#Does not seem to have any ranking. So converting to a factor
combined$Foundation <- as.factor(combined$Foundation)

#Heating. Does not seem to have any ranking. So converting to a factor
combined$Heating <- as.factor(combined$Heating)

#HeatingQC. Obviously has a ranking.
Ranking <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)
combined$HeatingQC<-as.integer(revalue(combined$HeatingQC, Ranking))

#CentralAir. Obviously, houses with Central air conditioning command higher prices
Ranking <- c('N' = 0, 'Y' = 1)
combined$CentralAir <-as.integer(revalue(combined$CentralAir, Ranking))

#PavedDrive. Similar to Street, has a ranking
combined$PavedDrive<-as.integer(revalue(combined$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))

#SaleCondition. Does not seem to have any ranking. So converting to a factor
combined$SaleCondition <- as.factor(combined$SaleCondition)
table(combined$SaleCondition)

#Converting numerical variables which should actually be categorical variables
combined$MoSold <- as.factor(combined$MoSold)
combined$MSSubClass <- as.factor(combined$MSSubClass)

#Adding a variable which captures whether a house is new or not
#as new houses command more price
combined$new <- ifelse(combined$YrSold==combined$YearBuilt, 1, 0)
table(combined$new)

#Adding a variable which captures whether a house has been remodeled or not
#as remodelled houses command more price
combined$Remod <- ifelse(combined$YearBuilt==combined$YearRemodAdd, 0, 1) 

#Dropping YrSold and YearRemodAdd and instead adding age
#as older houses command less price and new houser command more prices
combined$Age <- as.numeric(combined$YrSold)-combined$YearRemodAdd
combined$'YrSold' <- NULL
combined$'YearRemodAdd'  <- NULL
combined$'YearBuilt'  <- NULL

#Correcting the fat finger errors
combined$GarageYrBlt[combined$GarageYrBlt=="2207"] <- "2007"
#Correcting the fat finger errors

#Analyzing the relationship between SalePrice and Numerical Variables
numericalVars <- which(sapply(combined , is.numeric)) #filtering numeric variables
numericalVarNames <- names(numericalVars) 

all_numericalVar <- combined[, numericalVars]
#Checking correlation of all numerical variables
cor_numericalVar <- cor(all_numericalVar, use="pairwise.complete.obs") 
cor_numericalVar

#FEATURE ENGINEERING
#Adding Bathrooms
combined$TotBathrooms <- combined$FullBath + (combined$HalfBath*0.5) + combined$BsmtFullBath + (combined$BsmtHalfBath*0.5)

#Adding Square Feet
combined$TotalSqFeet <- combined$GrLivArea + combined$TotalBsmtSF

#Adding Porch Square Feet
combined$TotalPorchSF <- combined$OpenPorchSF + combined$EnclosedPorch + combined$X3SsnPorch + combined$ScreenPorch

#Classifying the Neighborhood into 3 categories-poor, medium, rich
df <- combined[!is.na(combined$SalePrice),] %>% group_by(Neighborhood) %>% dplyr::summarize(median = median(SalePrice), counts=n())
View(df)
combined$NeighborhoodRich[combined$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
combined$NeighborhoodRich[!combined$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
combined$NeighborhoodRich[combined$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(combined$NeighborhoodRich)

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numericalVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
HighCorrelation <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numericalVar <- cor_numericalVar[HighCorrelation, HighCorrelation]

corrplot.mixed(cor_numericalVar, tl.col="black", tl.pos = "lt")
#The numerical variables that have high correlation (>0.6) with SalePrice are OverallQual, GrLivArea,
#ExterQual, KitchenQual, GarageCars, GarageArea, TotalBsmtSF,X1stFlrSF. 
#These play an important role as predictor variables
#It is evident that there is multicollinearity between related variables 
#eg.X1stFlrSF and TotalBsmtSF. This multicollinearity will affect our predictions
#We will choose a cutoff of 0.8 and removed variables which have a correlation of more 
#than 0.8 with each other. Of the two variables we will drop the variable which has a
#lower correlation with SalePrice. Hence we will drop 
#GarageArea, TotRmsAbvGrd, 'TotalBsmtSF', 'GarageYrBlt'
drop1 <- c( 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
combined <- combined[,!(names(combined) %in% drop1)]

#Dropping the numerical variables which were added
drop2 <- c( 'BsmtHalfBath', 'ScreenPorch')
combined <- combined[,!(names(combined) %in% drop2)]

#Transforming and Normalizing the numerical variables
numericalVars <- which(sapply(combined , is.numeric)) #filtering numeric variables
numericalVarNames <- names(numericalVars) 
numericalVarNames
numericalVarNames <- numericalVarNames[(numericalVarNames %in% c("LotFrontage","LotArea","MasVnrArea","BsmtFinSF2","BsmtUnfSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr", "KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageCars","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal","TotBathrooms","TotalSqFeet","TotalPorchSF"))]
combinednumeric <- combined[, numericalVarNames]
combinedcategoric <- combined[, !(names(combined) %in% numericalVarNames)]


#If the predictor variables are skewed to right/left, we will log transform them
for(i in 1:ncol(combinednumeric)){
  if (abs(skew(combinednumeric[,i]))>0.8){
    combinednumeric[,i] <- log(combinednumeric[,i] +1)
  }
}
View(combinednumeric)

Preprocesscombined <- preProcess(combinednumeric, method=c("center", "scale"))
print(Preprocesscombined)

combinednumeric <- predict(Preprocesscombined, combinednumeric)
dim(combinednumeric)

combined <- cbind(combinednumeric, combinedcategoric)

#Looking at the distribution of SalePrice
hist(combined$SalePrice[combined$SalePrice!="NA"])
#SalePrice is skewed to the right.The distribution is also not normal
#Verifying if the log transformation of SalePrice is normal.
hist(log(combined$SalePrice[combined$SalePrice!="NA"]))
#It is. Hence SalePrice has a lognormal distribution. We will transform the
#response variable to log
combined$SalePrice <- log(combined$SalePrice) 
write.csv(combined, file = "combined.csv")
colnames(combined)
View(combined)

#Feature Selection
remove_cols <- nearZeroVar(combined[,-c(68)], names = TRUE, 
                           freqCut = 2, uniqueCut = 20)
remove_cols

all_cols <- names(combined)

combined_small <- combined[ , setdiff(all_cols, remove_cols)]

dim(combined)
dim(combined_small)
names(combined_small)

#Correlation matrix to further decrease number of variables
numericalVars <- which(sapply(combined_small , is.numeric)) #filtering numeric variables
numericalVarNames <- names(numericalVars) 

all_numericalVar <- combined_small[, numericalVars]
#Checking correlation of all numerical variables
cor_numericalVar <- cor(all_numericalVar, use="pairwise.complete.obs") 
cor_numericalVar

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numericalVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
HighCorrelation <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numericalVar <- cor_numericalVar[HighCorrelation, HighCorrelation]

corrplot.mixed(cor_numericalVar, tl.col="black", tl.pos = "lt")
#We will take a cutoff of 0.7 and drop FullBath and TotRmsAbvGrd
drop3 <- c( 'FullBath', 'TotRmsAbvGrd')
combined_small <- combined_small[,!(names(combined_small) %in% drop3)]

#MODELLING
write.csv(combined_small, file = "combined_small.csv", row.names=FALSE)
train <- read.csv("combined_small.csv", header=TRUE, sep = ",")

colnames(train)

# Split the train data into Training/Testing sets
train.given <- train[c(1:1460),]
test <- train[c(1461:2919),-which(names(train) %in% c("SalePrice"))]
dim(train)

#Carrying out linear regression to decide which variables go to the matrix
housing.fit=lm(SalePrice~.,data=train.given) 
summary(housing.fit)

new_data <- dummyVars("~ .", train.given)
train.given_dummyd <- data.frame(predict(new_data, train.given))

housing.fit1=lm(SalePrice~LotArea+BsmtUnfSF+GrLivArea+HalfBath+TotBathrooms+TotalSqFeet+MSSubClass
                +NeighborhoodCrawfor+NeighborhoodEdwards+NeighborhoodIDOTRR+NeighborhoodMeadowV
                +NeighborhoodNridgHt+NeighborhoodOldTown+NeighborhoodStoneBr+HouseStyleSLvl
                +OverallQual+FoundationCBlock+FoundationPConc+FoundationSlab+FoundationStone+
                  BsmtQual+BsmtFinType1+HeatingQC+KitchenQual+GarageFinish+Age,data=train.given_dummyd) 
summary(housing.fit1)

housing.fit2=lm(SalePrice~LotArea+BsmtUnfSF+GrLivArea+HalfBath+TotBathrooms+TotalSqFeet+MSSubClass
                +NeighborhoodCrawfor+NeighborhoodEdwards+NeighborhoodIDOTRR
                +NeighborhoodNridgHt+NeighborhoodOldTown+NeighborhoodStoneBr+HouseStyleSLvl
                +OverallQual+FoundationCBlock+FoundationPConc+FoundationSlab+
                  BsmtQual+BsmtFinType1+HeatingQC+KitchenQual+GarageFinish+Age,data=train.given_dummyd) 
summary(housing.fit2)

#Matching the number of variables in X and X_test
test$HouseStyle2.5Fin <-0
test$MiscFeatureTenC <- 0
#Matching the number of variables in X and X_test

set.seed(6)
inx <- sample.split(seq_len(nrow(train.given)), 0.75)
train.training <- train.given[inx, ]
train.validation <-  train.given[!inx, ]

#create the y variable and matrix (capital X) of x variables 
#(will make the code below easier to read + will ensure that all interactions exist)
y.training<-train.training$SalePrice

X<-model.matrix(~LotArea+BsmtUnfSF+GrLivArea+TotBathrooms+TotalSqFeet+MSSubClass
                +Neighborhood
                +OverallQual+Foundation+
                  BsmtQual+BsmtFinType1+HeatingQC+KitchenQual+GarageFinish+Age, train.given)[,-1]
ncol(X)
nrow(X)
names(X)


# split X into trainig and validation as before
X.training<-X[inx,]
X.validation<-X[!inx,]
View(X.validation)
#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y.training, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#Selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 1) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the validation set
lasso.validation <- predict(lasso.opt.fit, s = penalty.lasso, newx =X.validation)
lasso.validation.RMSE <- mean((lasso.validation- train.validation$SalePrice )^2) #calculate and display MSE in the testing set


#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval.ridge <-  cv.glmnet(x = X.training, y = y.training, alpha = 0)
plot(crossval.ridge)
penalty.ridge <- crossval.ridge$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.validation <- predict(ridge.opt.fit, s = penalty.ridge, newx =X.validation)
ridge.validation.RMSE <- mean((ridge.validation- train.validation$SalePrice )^2)


# MODEL SELECTION: comparing the prediction error in the testing set
lasso.validation.RMSE # LASSO RMSE is 0.01706529
ridge.validation.RMSE # Ridge RMSE is 0.01718574
# Lasso is better, so use it for prediction

# Using the Lasso regression to predict Sales Price in test file
X_test<-model.matrix(~LotArea+BsmtUnfSF+GrLivArea+TotBathrooms+TotalSqFeet+MSSubClass
                     +Neighborhood
                     +OverallQual+Foundation+
                       BsmtQual+BsmtFinType1+HeatingQC+KitchenQual+GarageFinish+Age, test)[,-1]
ncol(X_test)
ncol(X)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X_test))
View(lasso.testing)

# Export the predictions to a csv file
write.csv(lasso.testing, file = "Predicted Boston Housing Prices.csv")


