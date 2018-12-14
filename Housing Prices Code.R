#loading libraries

install.packages("gridExtra")
install.packages("randomForest")
install.packages("dummies")
install.packages("e1071")
install.packages("ranger")
install.packages("DiagrammeR")

library(dummies)
library(randomForest)
library(data.table)
library(plyr)
library(dplyr)
library(foreach)
library(e1071)
library(ranger)
library(caret)
library(xgboost)
library(DiagrammeR)
library(Matrix)
library(ggplot2)
library(corrplot)
library(corrgram)
library(GGally)
library(reshape)
library(progress)
library(prettyunits)
library(tidyr)
library(RColorBrewer)
library(ggfortify)

setwd("C:/Users/Admin/Desktop/MSBAIM/Fall Mod 1/DM/Assignments/HW2")

#loading the test and training data
train <- read.csv("train.csv",header = T)
test <- read.csv("test.csv",header = T)
head(train)

#**********************************************************************************
#Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#**************************************************************************************

#Cleaning Data

p1 <- subset(train, !is.na(GrLivArea))
p1 <- ggplot(p1, aes(GrLivArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p2 <- subset(train, !is.na(LotArea))
p2 <- ggplot(p2, aes(LotArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p3 <- subset(train, !is.na(LotFrontage))
p3 <- ggplot(p3, aes(LotFrontage, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p4 <- subset(train, !is.na(GarageArea))
p4 <- ggplot(p4, aes(GarageArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
multiplot(p1, p2, p3, p4, cols=2)

#Removing the outliers 
  
train <- subset(train, GrLivArea < 4000 | is.na(GrLivArea))
train <- subset(train, LotArea < 100000 | is.na(LotArea))
train <- subset(train, LotFrontage < 200 | is.na(LotFrontage))
train <- subset(train, GarageArea < 1500 | is.na(GarageArea))

#***************************************************************************************
  
#scatter plots for gr living area by neighborhood

ggplot(train, aes(GrLivArea, SalePrice)) + geom_point(aes(color = Neighborhood)) + 
  scale_x_continuous("GrLivArea") +
  scale_y_continuous("SalePrice") +
  theme_bw() + facet_wrap( ~ Neighborhood) + theme(legend.position="none")

#***************************************************************************************

#Combing test and train for fixing NAs and further feature engineering
combi <- rbind(train[,-c(ncol(train))], test)    #understand this code

### Assume 0 linear feet of street connected to property if NA
combi$LotFrontage[is.na(combi$LotFrontage)] <- 0

#
combi$Alley <- NULL
combi$PoolQC <- NULL
combi$Fence <- NULL
combi$MiscFeature <- NULL

#fixing categorical values with modes
out <- table(combi$Utilities)
mode <- names(out)[out==max(out)]
combi$Utilities[is.na(combi$Utilities)] <- mode

out <- table(combi$Electrical)
mode <- names(out)[out==max(out)]
combi$Electrical[is.na(combi$Electrical)] <- mode

out <- table(combi$KitchenQual)
mode <- names(out)[out==max(out)]
combi$KitchenQual[is.na(combi$KitchenQual)] <- mode

#special handling of consistency, if area = 0, type is None for MasVnrArea an MasVnr Type
combi[is.na(combi$MasVnrType) & is.na(combi$MasVnrArea),]$MasVnrArea <- 0
combi[is.na(combi$MasVnrType) & combi$MasVnrArea >= 0,]$MasVnrType <- 'None'
combi[combi$MasVnrType %in% 'None' & combi$MasVnrArea >= 0,]$MasVnrArea <- 0
combi[!combi$MasVnrType %in% 'None' & combi$MasVnrArea == 0,]$MasVnrType <- 'None'

#### Special handling for consistency - Garage (if Area is 0, the rest of the Garage feature 
##should be None)

combi$GarageType <- sapply(combi$GarageType, as.character)
combi$GarageFinish <- sapply(combi$GarageFinish, as.character)
combi$GarageQual <- sapply(combi$GarageQual, as.character)
combi$GarageCond <- sapply(combi$GarageCond, as.character)

combi$GarageType[is.na(combi$GarageType)] <- ' '
combi$GarageFinish[is.na(combi$GarageFinish)] <- ' '
combi$GarageQual[is.na(combi$GarageQual)] <- ' '
combi$GarageCond[is.na(combi$GarageCond)] <- ' '
combi$GarageYrBlt[is.na(combi$GarageYrBlt)] <- 0
combi$GarageCars[is.na(combi$GarageCars)] <- 0
combi$GarageArea[is.na(combi$GarageArea)] <- 0

combi[combi$GarageArea == 0,]$GarageType <- 'None'

#calculating mode of garage finish
out <- table(combi$GarageFinish)
mode <- names(out)[out==max(out)]


combi[combi$GarageYrBlt %in% '0' & 
        combi$GarageType %in% 'Detchd' & 
        combi$GarageCars > 0 & 
        combi$GarageArea > 0,]$GarageFinish <- mode

out <- table(combi$GarageQual)
mode <- names(out)[out==max(out)]

combi[combi$GarageYrBlt %in% '0' & 
        combi$GarageType %in% 'Detchd' & 
        combi$GarageCars > 0 & 
        combi$GarageArea > 0,]$GarageQual <- mode

out <- table(combi$GarageCond)
mode <- names(out)[out==max(out)]

combi[combi$GarageYrBlt %in% '0' & 
        combi$GarageType %in% 'Detchd' & 
        combi$GarageCars > 0 & 
        combi$GarageArea > 0,]$GarageCond <- mode

#### Special handling for consistency - Basement (if Area is 0, the rest of 
##the Basement feature should be None)

combi$BsmtQual <- sapply(combi$BsmtQual, as.character)
combi$BsmtCond <- sapply(combi$BsmtCond, as.character)
combi$BsmtExposure <- sapply(combi$BsmtExposure, as.character)
combi$BsmtFinType1 <- sapply(combi$BsmtFinSF1, as.character)
combi$BsmtFinType2 <- sapply(combi$BsmtFinType2, as.character)

combi$BsmtQual[is.na(combi$BsmtQual)] <- ' '
combi$BsmtCond[is.na(combi$BsmtCond)] <- ' '
combi$BsmtExposure[is.na(combi$BsmtExposure)] <- ' '
combi$BsmtFinType1[is.na(combi$BsmtFinType1)] <- ' '
combi$BsmtFinType2[is.na(combi$BsmtFinType2)] <- ' '

combi[is.na(combi$TotalBsmtSF),]$BsmtFinSF1 <- 0
combi[is.na(combi$TotalBsmtSF),]$BsmtFinSF2 <- 0
combi[is.na(combi$TotalBsmtSF),]$BsmtUnfSF <- 0
combi[is.na(combi$TotalBsmtSF),]$BsmtFullBath <- 0
combi[is.na(combi$TotalBsmtSF),]$BsmtHalfBath <- 0
combi[is.na(combi$TotalBsmtSF),]$TotalBsmtSF <- 0

combi[combi$TotalBsmtSF == 0,]$BsmtFullBath <- 0
combi[combi$TotalBsmtSF == 0,]$BsmtHalfBath <- 0

#**********************************************************************
#**********************************************************************
#Exploratory Data Analysis
#**********************************************************************
### Identify the numerical features
#**********************************************************************

#*************************************************************************
#Correlations


combi.num <- sapply(combi,is.numeric)
combinum <- combi[,combi.num]   #dataframe of numerical data 
cor.data <- cor(combi[,combi.num])
corrplot<- corrplot(cor.data,method='color')  
#**************************************************************************
#Adding total areas and square feet area

combinum$TotalArea <- combinum$LotFrontage + combinum$LotArea + combinum$MasVnrArea + combinum$BsmtFinSF1 + 
  combinum$BsmtFinSF2 + combinum$BsmtUnfSF + combinum$TotalBsmtSF + combinum$X1stFlrSF + 
  combinum$X2ndFlrSF + combinum$GrLivArea + combinum$GarageArea + combinum$WoodDeckSF +
  combinum$OpenPorchSF + combinum$EnclosedPorch + combinum$X3SsnPorch + 
  combinum$ScreenPorch + combinum$LowQualFinSF + combinum$PoolArea

combinum$TotalArea1st2nd <- combinum$X1stFlrSF + combinum$X2ndFlrSF

#***************************************************************************

#Removing unrelated variables
### Remove from Correlation Analysis
combinum <- combinum[,!colnames(combinum) %in% 'EnclosedPorch']
combinum <- combinum[,!colnames(combinum) %in% 'LowQualFinSF']
combinum <- combinum[,!colnames(combinum) %in% 'MiscVal']
combinum <- combinum[,!colnames(combinum) %in% 'OpenPorchSF']
combinum <- combinum[,!colnames(combinum) %in% 'PoolArea']
combinum <- combinum[,!colnames(combinum) %in% 'ScreenPorch']
combinum <- combinum[,!colnames(combinum) %in% 'X3SsnPorch']

### Normalise the numeric features
combinum <- data.frame(lapply(combinum, function(x) {log1p(x)}))

#*****************************************************************************
#Identifying Categorical Features
#*****************************************************************************
#identified factors
combi.fac <- sapply(combi,is.factor)
combifac <- combi[,combi.fac]


#Create new categorial feature by grouping months with high sales transactions. 
#Months with high transactions may have an impact to Sale Price.
analysedata <- combi %>% group_by(MoSold) %>% summarise(Count = n())
analysedata$MoSold <- as.factor(analysedata$MoSold)
ggplot(data=analysedata, aes(x=MoSold, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
#it is observed that 4,5,6,7 are more popular

combifac$PopularMonth <- ifelse(combi$MoSold==4 | combi$MoSold==5 |
                                combi$MoSold==6 | combi$MoSold==7,1,0)

#Grouping Dwellings with higher transcations
analysedata <- combi %>% group_by(MSSubClass) %>% summarise(Count = n())
analysedata$MSSubClass <- as.factor(analysedata$MSSubClass)

ggplot(data=analysedata, aes(x=MSSubClass, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
#Certain types of dwellings may be popular and hence have an impact to Sale Price.
#20,50,60,120 have higher demands
combifac$PopularDwelling <- ifelse(combi$MSSubClass==20 | combi$MSSubClass==50 |
                                  combi$MSSubClass==60 | combi$MSSubClass==120,1,0)

# These categorial features deserve to have a numeric scoring features
qual2Val <- c('None:0','Po:1','Fa:2','TA:3','Gd:4','Ex:5')

combinum$ExterCond <- ifelse(combifac$ExterQual=='None',0,ifelse(combifac$ExterQual=='Po',
                    1,ifelse(combifac$ExterQual=='Fa',2,ifelse(combifac$ExterQual=='TA',3,
          ifelse(combifac$ExterQual=='Gd',4,ifelse(combifac$ExterQual=='Ex',5,NA))))))

combinum$ExterCond <- ifelse(combifac$ExterCond=='None',0,ifelse(combifac$ExterCond=='Po',
                                                                 1,ifelse(combifac$ExterCond=='Fa',2,ifelse(combifac$ExterCond=='TA',3,
                                                                                                            ifelse(combifac$ExterCond=='Gd',4,ifelse(combifac$ExterCond=='Ex',5,NA))))))
combinum$HeatingQC <- ifelse(combifac$HeatingQC=='None',0,ifelse(combifac$HeatingQC=='Po',
                                                                 1,ifelse(combifac$HeatingQC=='Fa',2,ifelse(combifac$HeatingQC=='TA',3,
                                                                                                            ifelse(combifac$HeatingQC=='Gd',4,ifelse(combifac$HeatingQC=='Ex',5,NA))))))
combinum$KitchenQual <- ifelse(combifac$KitchenQual=='None',0,ifelse(combifac$KitchenQual=='Po',
                                                                     1,ifelse(combifac$KitchenQual=='Fa',2,ifelse(combifac$KitchenQual=='TA',3,
                                                                                                                  ifelse(combifac$KitchenQual=='Gd',4,ifelse(combifac$KitchenQual=='Ex',5,NA))))))
combinum$FireplaceQu <- ifelse(combifac$FireplaceQu=='None',0,ifelse(combifac$FireplaceQu=='Po',
                                                                     1,ifelse(combifac$FireplaceQu=='Fa',2,ifelse(combifac$FireplaceQu=='TA',3,
                                                                                                                  ifelse(combifac$FireplaceQu=='Gd',4,ifelse(combifac$FireplaceQu=='Ex',5,NA))))))
combinum$GarageQual <- ifelse(combi$GarageQual=='None',0,ifelse(combi$GarageQual=='Po',
                                                                   1,ifelse(combi$GarageQual=='Fa',2,ifelse(combi$GarageQual=='TA',3,
                                                                                                               ifelse(combi$GarageQual=='Gd',4,ifelse(combi$GarageQual=='Ex',5,NA))))))
combinum$GarageCond <- ifelse(combi$GarageCond=='None',0,ifelse(combi$GarageCond=='Po',
                                                                1,ifelse(combi$GarageCond=='Fa',2,ifelse(combi$GarageCond=='TA',3,
                                                                                                         ifelse(combi$GarageCond=='Gd',4,ifelse(combi$GarageCond=='Ex',5,NA))))))
combinum$BsmtQual <- ifelse(combi$BsmtQual=='None',0,ifelse(combi$BsmtQual=='Po',
                                                            1,ifelse(combi$BsmtQual=='Fa',2,ifelse(combi$BsmtQual=='TA',3,
                                                                                                   ifelse(combi$BsmtQual=='Gd',4,ifelse(combi$BsmtQual=='Ex',5,NA))))))
combinum$BsmtCond <- ifelse(combi$BsmtCond=='None',0,ifelse(combi$BsmtCond=='Po',
                                                            1,ifelse(combi$BsmtCond=='Fa',2,ifelse(combi$BsmtCond=='TA',3,
                                                                                                   ifelse(combi$BsmtCond=='Gd',4,ifelse(combi$BsmtCond=='Ex',5,NA))))))

combinum$BmtExposure <- ifelse(combi$BsmtExposure=='None',0,ifelse(combi$BsmtExposure=='No',
                    1,ifelse(combi$BsmtExposure=='Mn',2,ifelse(combi$BsmtExposure=='Av',3,
                    ifelse(combi$BsmtExposure=='Gd',4,NA)))))

combinum$BsmtFinType2 <- ifelse(combi$BsmtFinType2=='None',0,ifelse(combi$BsmtFinType2=='Unf',1,
      ifelse(combi$BsmtFinType2=='LwQ',2,ifelse(combi$BsmtFinType2=='Rec',3,
      ifelse(combi$BsmtFinType2=='BLQ',4,ifelse(combi$BsmtFinType2=='ALQ',5,
                      ifelse(combi$BsmtFinType2=='GLQ',6,NA)))))))


combinum$Functional <- ifelse(combifac$Functional=='Oth',0,ifelse(combi$Functional=='Sal',1,
                  ifelse(combi$Functional=='Sev2',2,ifelse(combi$Functional=='Maj2',3,
                  ifelse(combi$Functional=='Maj1',4,ifelse(combi$Functional=='Mod',5,
                 ifelse(combi$Functional=='Min2',6,ifelse(combi$Functional=='Min1',7,
                   ifelse(combi$Functional=='Typ',8,NA)))))))))

combinum$GarageFinish <- ifelse(combi$GarageFinish=='None',0,ifelse(combi$GarageFinish=='Unf',1,
            ifelse(combi$GarageFinish=='RFn',2,ifelse(combi$GarageFinish=='Fin',3,NA))))
              
# Tweaking scoring system of the overall condition and quality of the property

combinum$OverallQual <- as.numeric(as.character(combi$OverallQual))
combinum$OverallCond <- as.numeric(as.character(combi$OverallCond))

combinum$OverallQual2<- ifelse(combi$OverallQual==1,1,ifelse(combi$OverallQual==2,1,
                    ifelse(combi$OverallQual==3,1,ifelse(combi$OverallQual==4,2,
                   ifelse(combi$OverallQual==5,2,ifelse(combi$OverallQual==6,2,
                   ifelse(combi$OverallQual==7,3,ifelse(combi$OverallQual==8,3,
                   ifelse(combi$OverallQual==9,3,ifelse(combi$OverallQual==10,3,NA))))))))))
       
combinum$OverallCond2<- ifelse(combi$OverallCond==1,1,ifelse(combi$OverallCond==2,1,
                   ifelse(combi$OverallCond==3,1,ifelse(combi$OverallCond==4,2,
                 ifelse(combi$OverallCond==5,2,ifelse(combi$OverallCond==6,2,
                  ifelse(combi$OverallCond==7,3,ifelse(combi$OverallCond==8,3,
                  ifelse(combi$OverallCond==9,3,ifelse(combi$OverallCond==10,3,NA))))))))))


# Is Prime Zone?

combifac$IsPrimeZone <- ifelse(combifac$MSZoning=='FV' | combifac$MSZoning=='RL'|
                                 combifac$MSZoning=='RP',1,0)

# Is Normal Zone?
combifac$IsPrimeZone <- ifelse(combifac$MSZoning=='RH' | combifac$MSZoning=='RM',1,0)

# Is there a reason the price is a discounted price?
combifac$SaleNormal <- ifelse(combifac$SaleCondition=='Normal'|
                                combifac$SaleCondition=='Partial',1,0)

# Is it an uncompleted sale?
combifac$SaleComplete <- ifelse(combifac$SaleCondition=='Partial',1,0)

# Is the shape regular?
combifac$IsRegularShape <- ifelse(combifac$LotShape=='Reg',1,0)

#Neighborhood bin

combinum$NeighborhoodBin <- ifelse(combifac$Neighborhood=='MeadowV',0,ifelse(combifac$Neighborhood=='IDOTRR',1,
              ifelse(combifac$Neighborhood=='BrDale',1,ifelse(combifac$Neighborhood=='OldTown',1,
             ifelse(combifac$Neighborhood=='Edwards',1,ifelse(combifac$Neighborhood=='BrkSide',1,
            ifelse(combifac$Neighborhood=='Sawyer',1,ifelse(combifac$Neighborhood=='Blueste',1,
          ifelse(combifac$Neighborhood=='SWISU',2,ifelse(combifac$Neighborhood=='NAmes',2,
           ifelse(combifac$Neighborhood=='NPkVill',2,ifelse(combifac$Neighborhood=='Mitchel',2,
            ifelse(combifac$Neighborhood=='SawyerW',2,ifelse(combifac$Neighborhood=='Gilbert',2,
            ifelse(combifac$Neighborhood=='NWAmes',2,ifelse(combifac$Neighborhood=='Blmngtn',2,
             ifelse(combifac$Neighborhood=='CollgCr',2,ifelse(combifac$Neighborhood=='ClearCr',3,
            ifelse(combifac$Neighborhood=='Crawfor',3,ifelse(combifac$Neighborhood=='Veenker',3,
             ifelse(combifac$Neighborhood=='Somerst',3,ifelse(combifac$Neighborhood=='Timber',3,
           ifelse(combifac$Neighborhood=='StoneBr',4,ifelse(combifac$Neighborhood=='NoRidge',4,
          ifelse(combifac$Neighborhood=='NridgHt',4,NA)))))))))))))))))))))))))

## Convert these categorial feature to numeric because there is an expectation that higher the count,
#higher the Sale Price 
combinum$BsmtBath <- as.numeric(as.character(combi$BsmtFullBath)) + as.numeric(as.character(combi$BsmtHalfBath)) * 0.5
combinum$Bath <- as.numeric(as.character(combi$FullBath)) + as.numeric(as.character(combi$HalfBath)) * 0.5
combinum$TotalBath <- combinum$BsmtBath + combinum$Bath
combinum$BedroomAbvGr <- as.numeric(as.character(combi$BedroomAbvGr))
combinum$KitchenAbvGr <- as.numeric(as.character(combi$KitchenAbvGr))
combinum$TotRmsAbvGrd <- as.numeric(as.character(combi$TotRmsAbvGrd))
combinum$Fireplaces <- as.numeric(as.character(combi$Fireplaces))
combinum$GarageCars <- as.numeric(as.character(combi$GarageCars))

# Combining the Month and Year.  Creating a timestamp feature for each transaction.
combinum$DtSold <- as.numeric(as.character(combi$YrSold)) + 
  as.numeric(as.character(combi$MoSold))/12
combinum$DtSold <- normalized(combinum$DtSold)

##*****************************************************
##Normalized function
normalized<-function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}
#********************************************************
##Removing Irrelevant Columns
#**************************************************************
combifac <- combifac[,!colnames(combifac) %in% 'LotShape']
combifac <- combifac[,!colnames(combifac) %in% 'LandContour']
combifac <- combifac[,!colnames(combifac) %in% 'LandSlope']
combifac <- combifac[,!colnames(combifac) %in% 'Electrical']
combifac <- combifac[,!colnames(combifac) %in% 'GarageType']
combifac <- combifac[,!colnames(combifac) %in% 'PavedDrive']
combifac <- combifac[,!colnames(combifac) %in% 'MiscFeature']
combifac <- combifac[,!colnames(combifac) %in% 'Neighborhood']
combifac <- combifac[,!colnames(combifac) %in% 'MSSubClass']
combifac <- combifac[,!colnames(combifac) %in% 'YrSold']
combifac <- combifac[,!colnames(combifac) %in% 'MoSold']
combifac <- combifac[,!colnames(combifac) %in% 'YearRemodAdd']
combifac <- combifac[,!colnames(combifac) %in% 'YearBuilt']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtFullBath']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtHalfBath']
combifac <- combifac[,!colnames(combifac) %in% 'FullBath']
combifac <- combifac[,!colnames(combifac) %in% 'HalfBath']
combifac <- combifac[,!colnames(combifac) %in% 'BedroomAbvGr']
combifac <- combifac[,!colnames(combifac) %in% 'KitchenAbvGr']
combifac <- combifac[,!colnames(combifac) %in% 'TotRmsAbvGrd']
combifac <- combifac[,!colnames(combifac) %in% 'Fireplaces']
combifac <- combifac[,!colnames(combifac) %in% 'OverallQual']
combifac <- combifac[,!colnames(combifac) %in% 'OverallCond']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtQual']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtCond']
combifac <- combifac[,!colnames(combifac) %in% 'HeatingQC']
combifac <- combifac[,!colnames(combifac) %in% 'KitchenQual']
combifac <- combifac[,!colnames(combifac) %in% 'FireplaceQu']
combifac <- combifac[,!colnames(combifac) %in% 'GarageQual']
combifac <- combifac[,!colnames(combifac) %in% 'GarageCond']
combifac <- combifac[,!colnames(combifac) %in% 'PoolQC']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtExposure']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtFinType1']
combifac <- combifac[,!colnames(combifac) %in% 'BsmtFinType2']
combifac <- combifac[,!colnames(combifac) %in% 'Functional']
combifac <- combifac[,!colnames(combifac) %in% 'GarageFinish']
combifac <- combifac[,!colnames(combifac) %in% 'Fence']
combifac <- combifac[,!colnames(combifac) %in% 'GarageYrBlt']
combifac <- combifac[,!colnames(combifac) %in% 'GarageCars']

combinum <- combinum[,!colnames(combinum) %in% 'X2ndFlrSF']
combinum <- combinum[,!colnames(combinum) %in% 'MasVnrArea']
combinum <- combinum[,!colnames(combinum) %in% 'WoodDeckSF']

#***************************************************************
##Final Data Preparation
#****************************************************************
##Recombine the numeric and categorial features and splitting the dataset back to the 
##training and test dataset. Normalised Sale Price is added back to the training set.

combi2 <- cbind(combinum,combifac)

train <- cbind(combi2[1:nrow(train),],SalePrice = train[,c(ncol(train))])
test <- combi2[(nrow(train)+1):nrow(combi2),]
train$SalePrice <- log1p(train$SalePrice)

# Data splitting the training set into 2 subset.  
# One for training the model.  One for out of sample validation.

inTrain <- createDataPartition(y=train$SalePrice, p=0.80, list=FALSE)
validation.data <- train[-inTrain,]
train.data <- train[inTrain,]

traindata <- data.table(train.data)
validationdata <- data.table(validation.data)

#************************************************************************
#Training the Model
#************************************************************************

install.packages('xgboost')
library(xgboost)
set.seed(3567)
numCol <- ncol(traindata)-1

trainx <- Matrix(data.matrix(traindata[,c(1:numCol),with=FALSE]), sparse=TRUE)
trainy <- as.numeric(traindata$SalePrice)
inputValid <- Matrix(data.matrix(validationdata[,c(1:numCol),with=FALSE]), sparse=TRUE)

xgbGrid <- expand.grid(
  nrounds = c(10000),
  max_depth = seq(3,6,by=1),
  eta = seq(0.03,0.05,by=0.01),
  gamma = seq(0,1,by=1),
  colsample_bytree = seq(0.4,0.6,by = 0.1),
  min_child_weight = seq(1,1,by = 0.5),
  subsample = seq(0.4,0.6,by = 0.1)
)

rmseErrorsHyperparameters <- apply(xgbGrid, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentMin_Child_Weight <- parameterList[["min_child_weight"]]
  currentGamma <- parameterList[["gamma"]]
  currentEta <- parameterList[["eta"]]
  currentMax_Depth <- parameterList[["max_depth"]]
  currentNrounds <- parameterList[["nrounds"]]
  
  params <- list(objective = "reg:linear", 
                 #booster = "gbtree", 
                 #eta = 2/currentNrounds,
                 eta = currentEta, 
                 gamma = currentGamma, 
                 max_depth = currentMax_Depth, 
                 min_child_weight = currentMin_Child_Weight, 
                 subsample = currentSubsampleRate, 
                 colsample_bytree = currentColsampleRate)
  
  xgbcv <- xgb.cv(params = params, 
                  data = trainx, label = trainy,
                  nrounds = currentNrounds, nfold = 5, 
                  showsd = T, stratified = T, early_stopping_rounds = 20, maximize = F)
  
  testrmse <- xgbcv$evaluation_log$test_rmse_mean[xgbcv$best_iteration]
  trainrmse <- xgbcv$evaluation_log$train_rmse_mean[xgbcv$best_iteration]
  
  return(c(testrmse, trainrmse, currentSubsampleRate, currentColsampleRate,
           currentMin_Child_Weight,currentGamma,currentEta,
           currentMax_Depth,currentNrounds,xgbcv$best_iteration))
  
})










#corecing the columns to correct datatype

combi$MSSubClass <- as.factor(combi$MSSubClass)   
combi$MSZoning <- as.factor(combi$MSZoning)    
combi$LotFrontage <- as.numeric(combi$LotFrontage)   
combi$Street <- as.factor(combi$Street)       
combi$Alley <- as.factor(combi$Alley)        
combi$LotShape <- as.factor(combi$LotShape)
combi$LandContour <- as.factor(combi$LandContour)
combi$Utilities <- as.factor(combi$Utilities)
combi$LotConfig <- as.factor(combi$LotConfig)
combi$LandSlope <- as.factor(combi$LandSlope)
combi$Neighborhood <- as.factor(combi$Neighborhood)
combi$Condition1 <- as.factor(combi$Condition1)
combi$Condition2 <- as.factor(combi$Condition2)
combi$BldgType <- as.factor(combi$BldgType)
combi$HouseStyle <- as.factor(combi$HouseStyle) 
combi$RoofStyle <- as.factor(combi$RoofStyle)  
combi$RoofMatl <- as.factor(combi$RoofMatl)
combi$Exterior1st <- as.factor(combi$Exterior1st)
combi$Exterior2nd <- as.factor(combi$Exterior2nd)
combi$MasVnrType <- as.factor(combi$MasVnrType)
combi$MasVnrArea <- as.numeric(combi$MasVnrArea)
combi$ExterQual <- as.factor(combi$ExterQual)   
combi$ExterCond <- as.factor(combi$ExterCond)
combi$Foundation <- as.factor(combi$Foundation)
combi$BsmtQual <- as.factor(combi$BsmtQual)      
combi$BsmtCond <- as.factor(combi$BsmtCond)
combi$BsmtExposure <- as.factor(combi$BsmtExposure)
combi$BsmtFinType1 <- as.factor(combi$BsmtFinType1)
combi$BsmtFinType2 <- as.factor(combi$BsmtFinType2)  
combi$Heating <- as.factor(combi$Heating)
combi$HeatingQC <- as.factor(combi$HeatingQC)
combi$CentralAir <- as.factor(combi$CentralAir)
combi$Electrical <- as.factor(combi$Electrical)
combi$KitchenQual <- as.factor(combi$KitchenQual)
combi$Functional <- as.factor(combi$Functional) 
combi$FireplaceQu <- as.factor(combi$FireplaceQu)   
combi$GarageType <- as.factor(combi$GarageType) 
combi$GarageYrBlt <- as.numeric(combi$GarageYrBlt)
combi$GarageFinish <- as.factor(combi$GarageFinish)  
combi$GarageQual <- as.factor(combi$GarageQual)  
combi$GarageCond <- as.factor(combi$GarageCond)  
combi$PavedDrive <- as.factor(combi$PavedDrive)    
combi$PoolQC <- as.factor(combi$PoolQC)       
combi$Fence <- as.factor(combi$Fence)        
combi$MiscFeature <- as.factor(combi$MiscFeature) 
combi$SaleType <- as.factor(combi$SaleType)
combi$SaleCondition <- as.factor(combi$SaleCondition)


