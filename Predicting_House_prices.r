> library(data.table)
> library(testthat)
> library(gridExtra)
> library(corrplot)
> library(GGally)
> library(ggplot2)
> library(e1071)
> library(dplyr)
> library(Metrics)


> # Loading the data Train and Test both
> train_data <- fread('D:\\Project\\Datasets\\train.csv', colClasses = c('MiscFeature' = "character", 'Alley' = "character", 'PoolQC' = "character"))
> test_data <- fread('D:\\Project\\Datasets\\test.csv', colClasses = c('MiscFeature' = "character", 'Alley' = "character", 'PoolQC' = "character"))


> #Dividing the training data into categorical and continuous variables
> categorical_var <- names(train_data)[which(sapply(train_data,is.character))]
> continuous_var <- names(train_data)[which(sapply(train_data,is.numeric))]


> #Defining the Structure of the data set
> dim(train_data)
[1] 1460   81
> # The above results show that there are 1460 rows and 81 features including the target feature which the SalePrice in this data set
> str(train_data)  #A more detailed view of the features
Classes ‘data.table’ and 'data.frame':	1460 obs. of  81 variables:
 $ Id           : int  1 2 3 4 5 6 7 8 9 10 ...
 $ MSSubClass   : int  60 20 60 70 60 50 20 60 50 190 ...
 $ MSZoning     : chr  "RL" "RL" "RL" "RL" ...
 $ LotFrontage  : int  65 80 68 60 84 85 75 NA 51 50 ...
 $ LotArea      : int  8450 9600 11250 9550 14260 14115 10084 10382 6120 7420 ...
 $ Street       : chr  "Pave" "Pave" "Pave" "Pave" ...
 $ Alley        : chr  NA NA NA NA ...
 $ LotShape     : chr  "Reg" "Reg" "IR1" "IR1" ...
 $ LandContour  : chr  "Lvl" "Lvl" "Lvl" "Lvl" ...
 $ Utilities    : chr  "AllPub" "AllPub" "AllPub" "AllPub" ...
 $ LotConfig    : chr  "Inside" "FR2" "Inside" "Corner" ...
 $ LandSlope    : chr  "Gtl" "Gtl" "Gtl" "Gtl" ...
 $ Neighborhood : chr  "CollgCr" "Veenker" "CollgCr" "Crawfor" ...
 $ Condition1   : chr  "Norm" "Feedr" "Norm" "Norm" ...
 $ Condition2   : chr  "Norm" "Norm" "Norm" "Norm" ...
 $ BldgType     : chr  "1Fam" "1Fam" "1Fam" "1Fam" ...
 $ HouseStyle   : chr  "2Story" "1Story" "2Story" "2Story" ...
 $ OverallQual  : int  7 6 7 7 8 5 8 7 7 5 ...
 $ OverallCond  : int  5 8 5 5 5 5 5 6 5 6 ...
 $ YearBuilt    : int  2003 1976 2001 1915 2000 1993 2004 1973 1931 1939 ...
 $ YearRemodAdd : int  2003 1976 2002 1970 2000 1995 2005 1973 1950 1950 ...
 $ RoofStyle    : chr  "Gable" "Gable" "Gable" "Gable" ...
 $ RoofMatl     : chr  "CompShg" "CompShg" "CompShg" "CompShg" ...
 $ Exterior1st  : chr  "VinylSd" "MetalSd" "VinylSd" "Wd Sdng" ...
 $ Exterior2nd  : chr  "VinylSd" "MetalSd" "VinylSd" "Wd Shng" ...
 $ MasVnrType   : chr  "BrkFace" "None" "BrkFace" "None" ...
 $ MasVnrArea   : int  196 0 162 0 350 0 186 240 0 0 ...
 $ ExterQual    : chr  "Gd" "TA" "Gd" "TA" ...
 $ ExterCond    : chr  "TA" "TA" "TA" "TA" ...
 $ Foundation   : chr  "PConc" "CBlock" "PConc" "BrkTil" ...
 $ BsmtQual     : chr  "Gd" "Gd" "Gd" "TA" ...
 $ BsmtCond     : chr  "TA" "TA" "TA" "Gd" ...
 $ BsmtExposure : chr  "No" "Gd" "Mn" "No" ...
 $ BsmtFinType1 : chr  "GLQ" "ALQ" "GLQ" "ALQ" ...
 $ BsmtFinSF1   : int  706 978 486 216 655 732 1369 859 0 851 ...
 $ BsmtFinType2 : chr  "Unf" "Unf" "Unf" "Unf" ...
 $ BsmtFinSF2   : int  0 0 0 0 0 0 0 32 0 0 ...
 $ BsmtUnfSF    : int  150 284 434 540 490 64 317 216 952 140 ...
 $ TotalBsmtSF  : int  856 1262 920 756 1145 796 1686 1107 952 991 ...
 $ Heating      : chr  "GasA" "GasA" "GasA" "GasA" ...
 $ HeatingQC    : chr  "Ex" "Ex" "Ex" "Gd" ...
 $ CentralAir   : chr  "Y" "Y" "Y" "Y" ...
 $ Electrical   : chr  "SBrkr" "SBrkr" "SBrkr" "SBrkr" ...
 $ 1stFlrSF     : int  856 1262 920 961 1145 796 1694 1107 1022 1077 ...
 $ 2ndFlrSF     : int  854 0 866 756 1053 566 0 983 752 0 ...
 $ LowQualFinSF : int  0 0 0 0 0 0 0 0 0 0 ...
 $ GrLivArea    : int  1710 1262 1786 1717 2198 1362 1694 2090 1774 1077 ...
 $ BsmtFullBath : int  1 0 1 1 1 1 1 1 0 1 ...
 $ BsmtHalfBath : int  0 1 0 0 0 0 0 0 0 0 ...
 $ FullBath     : int  2 2 2 1 2 1 2 2 2 1 ...
 $ HalfBath     : int  1 0 1 0 1 1 0 1 0 0 ...
 $ BedroomAbvGr : int  3 3 3 3 4 1 3 3 2 2 ...
 $ KitchenAbvGr : int  1 1 1 1 1 1 1 1 2 2 ...
 $ KitchenQual  : chr  "Gd" "TA" "Gd" "Gd" ...
 $ TotRmsAbvGrd : int  8 6 6 7 9 5 7 7 8 5 ...
 $ Functional   : chr  "Typ" "Typ" "Typ" "Typ" ...
 $ Fireplaces   : int  0 1 1 1 1 0 1 2 2 2 ...
 $ FireplaceQu  : chr  NA "TA" "TA" "Gd" ...
 $ GarageType   : chr  "Attchd" "Attchd" "Attchd" "Detchd" ...
 $ GarageYrBlt  : int  2003 1976 2001 1998 2000 1993 2004 1973 1931 1939 ...
 $ GarageFinish : chr  "RFn" "RFn" "RFn" "Unf" ...
 $ GarageCars   : int  2 2 2 3 3 2 2 2 2 1 ...
 $ GarageArea   : int  548 460 608 642 836 480 636 484 468 205 ...
 $ GarageQual   : chr  "TA" "TA" "TA" "TA" ...
 $ GarageCond   : chr  "TA" "TA" "TA" "TA" ...
 $ PavedDrive   : chr  "Y" "Y" "Y" "Y" ...
 $ WoodDeckSF   : int  0 298 0 0 192 40 255 235 90 0 ...
 $ OpenPorchSF  : int  61 0 42 35 84 30 57 204 0 4 ...
 $ EnclosedPorch: int  0 0 0 272 0 0 0 228 205 0 ...
 $ 3SsnPorch    : int  0 0 0 0 0 320 0 0 0 0 ...
 $ ScreenPorch  : int  0 0 0 0 0 0 0 0 0 0 ...
 $ PoolArea     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ PoolQC       : chr  NA NA NA NA ...
 $ Fence        : chr  NA NA NA NA ...
 $ MiscFeature  : chr  NA NA NA NA ...
 $ MiscVal      : int  0 0 0 0 0 700 0 350 0 0 ...
 $ MoSold       : int  2 5 9 2 12 10 8 11 4 1 ...
 $ YrSold       : int  2008 2007 2008 2006 2008 2009 2007 2009 2008 2008 ...
 $ SaleType     : chr  "WD" "WD" "WD" "WD" ...
 $ SaleCondition: chr  "Normal" "Normal" "Normal" "Abnorml" ...
 $ SalePrice    : int  208500 181500 223500 140000 250000 143000 307000 200000 129900 118000 ...
 - attr(*, ".internal.selfref")=<externalptr> 

> #Identifying the missing data in categorical features as well as the continuous features
> colSums(sapply(categorical_var_train, is.na))
     MSZoning        Street         Alley      LotShape   LandContour 
            0             0          1369             0             0 
    Utilities     LotConfig     LandSlope  Neighborhood    Condition1 
            0             0             0             0             0 
   Condition2      BldgType    HouseStyle     RoofStyle      RoofMatl 
            0             0             0             0             0 
  Exterior1st   Exterior2nd    MasVnrType     ExterQual     ExterCond 
            0             0             8             0             0 
   Foundation      BsmtQual      BsmtCond  BsmtExposure  BsmtFinType1 
            0            37            37            38            37 
 BsmtFinType2       Heating     HeatingQC    CentralAir    Electrical 
           38             0             0             0             1 
  KitchenQual    Functional   FireplaceQu    GarageType  GarageFinish 
            0             0           690            81            81 
   GarageQual    GarageCond    PavedDrive        PoolQC         Fence 
           81            81             0          1453          1179 
  MiscFeature      SaleType SaleCondition 
         1406             0             0
> #From above we can notice that the following categorical features have missing values(NA)
> # Alley - 1369
> # MasVnrType - 8
> # BsmtQual - 37
> # BsmtCond - 37
> # BsmtExposure - 38
> # BsmtFinType1 - 37
> # BsmtFinType2 - 38
> # FireplaceQu - 690
> # GarageType - 81
> # GarageFinish - 81
> # GarageQual - 81
> # GarageCond - 81	
> # PoolQC - 1453
> # Fence - 1179
> # MiscFeature - 1406
	 
> colSums(sapply(continuous_var_train, is.na))
           Id    MSSubClass   LotFrontage       LotArea   OverallQual 
            0             0           259             0             0 
  OverallCond     YearBuilt  YearRemodAdd    MasVnrArea    BsmtFinSF1 
            0             0             0             8             0 
   BsmtFinSF2     BsmtUnfSF   TotalBsmtSF      1stFlrSF      2ndFlrSF 
            0             0             0             0             0 
 LowQualFinSF     GrLivArea  BsmtFullBath  BsmtHalfBath      FullBath 
            0             0             0             0             0 
     HalfBath  BedroomAbvGr  KitchenAbvGr  TotRmsAbvGrd    Fireplaces 
            0             0             0             0             0 
  GarageYrBlt    GarageCars    GarageArea    WoodDeckSF   OpenPorchSF 
           81             0             0             0             0 
EnclosedPorch     3SsnPorch   ScreenPorch      PoolArea       MiscVal 
            0             0             0             0             0 
       MoSold        YrSold     SalePrice 
            0             0             0 

> #From above we can notice that the following continuous features have missing values(NA)
> # LotFrontage - 259
> # MasVnrArea - 8
> # GarageYrBlt - 81
		
> #Plotting different graphs to gain more details on the variables. We will be using bar plots, histograms and frequency plots.
> histogram_plot <- function(train_data_in, i) {
+ new_data <- data.frame(x=train_data_in[[i]])
+ plot1 <- ggplot(data = new_data, aes(x = factor(x))) + stat_count() + xlab(colnames(train_data_in)[i]) + theme_gray() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
+ return(plot1)
+ }
 

> #Arranging in grids for a better view
> grid_plot <- function(train_data_in, fun, j, ncol=3) {
+ plot2 <- list()
+ for(i in j) {
+ plot1 <- fun(train_data_in = train_data_in, i=i)
+ plot2 <- c(plot2, list(plot1))
+ }
+ do.call("grid.arrange", c(plot2, ncol=ncol))
+ }


> #Frequency Plots
> frequency_plots <- function(train_data_in, i) {
+ new_data <- data.frame(x=train_data_in[[i]], SalePrice = train_data_in$SalePrice)
+ plot1 <- ggplot(data = new_data) + geom_line(aes(x=x), stat = 'density', size = 1, alpha = 1.0) + xlab(paste0((colnames(train_data_in)[i]), '\n', 'Skewness: ', round(skewness(train_data_in[[i]], na.rm = TRUE), 2))) + theme_gray()
+ return(plot1)
+ }


> #Converting Characters into factors
> train_data[, (categorical_var) := lapply(.SD, as.factor), .SDcols = categorical_var]
> categorical_var_train <- train_data[,.SD,.SDcols = categorical_var]
> continuous_var_train <- train_data[,.SD,.SDcols = continuous_var]


> #Plotting Bar Graphs for categorical variables 
> grid_plot(categorical_var_train, fun = histogram_plot, j = 1:6, ncol = 2)
> grid_plot(categorical_var_train, fun = histogram_plot, j = 7:12, ncol = 2)
> grid_plot(categorical_var_train, fun = histogram_plot, j = 13:18, ncol = 2)
> grid_plot(categorical_var_train, fun = histogram_plot, j = 19:24, ncol = 2)
> grid_plot(categorical_var_train, fun = histogram_plot, j = 25:30, ncol = 2)
> grid_plot(categorical_var_train, fun = histogram_plot, j = 31:36, ncol = 2)
> grid_plot(categorical_var_train, fun = histogram_plot, j = 37:43, ncol = 2)


> #Frequency Plots for continuous variables
> grid_plot(continuous_var_train, fun = frequency_plots, j = 2:7, ncol = 2)
> grid_plot(continuous_var_train, fun = frequency_plots, j = 8:13, ncol = 2) 
> grid_plot(continuous_var_train, fun = frequency_plots, j = 14:19, ncol = 2)
> grid_plot(continuous_var_train, fun = frequency_plots, j = 20:25, ncol = 2)
> grid_plot(continuous_var_train, fun = frequency_plots, j = 26:29, ncol = 2) 
> grid_plot(continuous_var_train, fun = frequency_plots, j = 30:38, ncol = 2)


> #Plotting the target variable :- Histogram
> hist(SalePrice, main = "Histogram depicting the Target Feature SalePrice", xlab = "SalePrice", border = "blue", col = "grey")
> # Since the target feature is skewed and does not follow normal distribution, therefore we will go for logarithmic transformation
> hist(log(SalePrice), main = "Histogram depicting the Target Feature SalePrice", xlab = "SalePrice", border = "blue", col = "grey")


> #Cleaning the data
> names(train_data)
 [1] "Id"            "MSSubClass"    "MSZoning"      "LotFrontage"  
 [5] "LotArea"       "Street"        "Alley"         "LotShape"     
 [9] "LandContour"   "Utilities"     "LotConfig"     "LandSlope"    
[13] "Neighborhood"  "Condition1"    "Condition2"    "BldgType"     
[17] "HouseStyle"    "OverallQual"   "OverallCond"   "YearBuilt"    
[21] "YearRemodAdd"  "RoofStyle"     "RoofMatl"      "Exterior1st"  
[25] "Exterior2nd"   "MasVnrType"    "MasVnrArea"    "ExterQual"    
[29] "ExterCond"     "Foundation"    "BsmtQual"      "BsmtCond"     
[33] "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"    "BsmtFinType2" 
[37] "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "Heating"      
[41] "HeatingQC"     "CentralAir"    "Electrical"    "1stFlrSF"     
[45] "2ndFlrSF"      "LowQualFinSF"  "GrLivArea"     "BsmtFullBath" 
[49] "BsmtHalfBath"  "FullBath"      "HalfBath"      "BedroomAbvGr" 
[53] "KitchenAbvGr"  "KitchenQual"   "TotRmsAbvGrd"  "Functional"   
[57] "Fireplaces"    "FireplaceQu"   "GarageType"    "GarageYrBlt"  
[61] "GarageFinish"  "GarageCars"    "GarageArea"    "GarageQual"   
[65] "GarageCond"    "PavedDrive"    "WoodDeckSF"    "OpenPorchSF"  
[69] "EnclosedPorch" "3SsnPorch"     "ScreenPorch"   "PoolArea"     
[73] "PoolQC"        "Fence"         "MiscFeature"   "MiscVal"      
[77] "MoSold"        "YrSold"        "SaleType"      "SaleCondition"
[81] "SalePrice"   
 
> #I have decided to use Dummy variables and convert them to numeric values for some categorical variables
> names(categorical_var_train)
 [1] "MSZoning"      "Street"        "Alley"         "LotShape"     
 [5] "LandContour"   "Utilities"     "LotConfig"     "LandSlope"    
 [9] "Neighborhood"  "Condition1"    "Condition2"    "BldgType"     
[13] "HouseStyle"    "RoofStyle"     "RoofMatl"      "Exterior1st"  
[17] "Exterior2nd"   "MasVnrType"    "ExterQual"     "ExterCond"    
[21] "Foundation"    "BsmtQual"      "BsmtCond"      "BsmtExposure" 
[25] "BsmtFinType1"  "BsmtFinType2"  "Heating"       "HeatingQC"    
[29] "CentralAir"    "Electrical"    "KitchenQual"   "Functional"   
[33] "FireplaceQu"   "GarageType"    "GarageFinish"  "GarageQual"   
[37] "GarageCond"    "PavedDrive"    "PoolQC"        "Fence"        
[41] "MiscFeature"   "SaleType"      "SaleCondition"

> table(train_data$MSZoning)

C (all)      FV      RH      RL      RM 
     10      65      16    1151     218 
> price <- summarize(group_by(train_data, MSZoning), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  MSZoning `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1  C (all)                      74528.0
2       FV                     214014.1
3       RH                     131558.4
4       RL                     191005.0
5       RM                     126316.8
> train_data$MSZoning1[train_data$MSZoning %in% c("C (all)")] <- 1
> train_data$MSZoning1[train_data$MSZoning %in% c("FV")] <- 2
> train_data$MSZoning1[train_data$MSZoning %in% c("RH")] <- 3
> train_data$MSZoning1[train_data$MSZoning %in% c("RL")] <- 4
> train_data$MSZoning1[train_data$MSZoning %in% c("RM")] <- 5

> table(train_data$Street)

Grvl Pave 
  6   1454 
 
> price <- summarise(group_by(train_data, Street),mean(SalePrice, na.rm = T))
> price
# A tibble: 2 × 2
  Street `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1     Grvl                     130190.5
2     Pave                     181130.5
> train_data$Street1[train_data$Street %in% c('Pave')] <- 1
> train_data$Street1[!train_data$Street %in% c('Pave')] <- 0

> table(train_data$Alley)

Grvl Pave 
  50   41 
 
> price <- summarize(group_by(train_data, Alley), mean(SalePrice, na.rm = T))
> price
# A tibble: 3 × 2
   Alley `mean(SalePrice, na.rm = T)`
  <fctr>                        <dbl>
1   Grvl                     122219.1
2   Pave                     168000.6
3     NA                     183452.1
> train_data$Alley1[train_data$Alley %in% c('Pave')] <- 1
> train_data$Alley1[!train_data$Alley %in% c('Pave')] <- 0

> table(train_data$PoolQC)

Ex Fa Gd 
 2  2  3 

      
> table(train_data$LotShape)

IR1 IR2 IR3 Reg 
484  41  10 925 
> price <- summarise(group_by(train_data, LotShape), mean(SalePrice, na.rm = T))
> price
# A tibble: 4 × 2
  LotShape `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1      IR1                     206101.7
2      IR2                     239833.4
3      IR3                     216036.5
4      Reg                     164754.8
> train_data$LotShape1[train_data$LotShape == "Reg"] <- 1
> train_data$LotShape1[train_data$LotShape != "Reg"] <- 0

> table(train_data$LandContour)

 Bnk  HLS  Low  Lvl 
  63   50   36 1311 
> train_data$LandContour1[train_data$LandContour == "Lvl"] <- 1
> train_data$LandContour1[train_data$LandContour != "Lvl"] <- 0

> table(train_data$Utilities)

AllPub NoSeWa 
  1459      1 
> train_data$Utilities1[train_data$Utilities == "AllPub"] <- 1
> train_data$Utilities1[train_data$Utilities != "AllPub"] <- 0

> table(train_data$LotConfig)

 Corner CulDSac     FR2     FR3  Inside 
    263      94      47       4    1052 
> price <- summarise(group_by(train_data, LotConfig), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  LotConfig `mean(SalePrice, na.rm = T)`
     <fctr>                        <dbl>
1    Corner                     181623.4
2   CulDSac                     223854.6
3       FR2                     177934.6
4       FR3                     208475.0
5    Inside                     176938.0
> train_data$LotConfig1[train_data$LotConfig %in% c("CulDSac", "FR3")] <- 1
> train_data$LotConfig1[!train_data$LotConfig %in% c("CulDSac", "FR3")] <- 0
 
> table(train_data$LandSlope)

 Gtl  Mod  Sev 
1382   65   13 
> train_data$LandSlope1[train_data$LandSlope == "Gtl"] <- 1
> train_data$LandSlope1[train_data$LandSlope == "Mod"] <- 2
> train_data$LandSlope1[train_data$LandSlope == "Sev"] <- 3

> table(train_data$Neighborhood)

Blmngtn Blueste  BrDale BrkSide ClearCr CollgCr Crawfor Edwards Gilbert  IDOTRR 
     17       2      16      58      28     150      51     100      79      37 
MeadowV Mitchel   NAmes NoRidge NPkVill NridgHt  NWAmes OldTown  Sawyer SawyerW 
     17      49     225      41       9      77      73     113      74      59 
Somerst StoneBr   SWISU  Timber Veenker 
     86      25      25      38      11 
	 
> neighbor_based_price <- summarise(group_by(train_data, Neighborhood), mean(SalePrice, na.rm = T))
> neighbor_based_price
# A tibble: 25 × 2
   Neighborhood `mean(SalePrice, na.rm = T)`
         <fctr>                        <dbl>
1       Blmngtn                     194870.9
2       Blueste                     137500.0
3        BrDale                     104493.8
4       BrkSide                     124834.1
5       ClearCr                     212565.4
6       CollgCr                     197965.8
7       Crawfor                     210624.7
8       Edwards                     128219.7
9       Gilbert                     192854.5
10       IDOTRR                     100123.8
# ... with 15 more rows

> neighbor_low_price <- filter(neighbor_based_price, neighbor_based_price$`mean(SalePrice, na.rm = T)` < 150000)
> neighbor_low_price <- filter(neighbor_based_price, neighbor_based_price$`mean(SalePrice, na.rm = T)` >= 150000 & neighbor_based_price$`mean(SalePrice, na.rm = T)` < 200000)
> neighbor_med_price <- filter(neighbor_based_price, neighbor_based_price$`mean(SalePrice, na.rm = T)` >= 150000 & neighbor_based_price$`mean(SalePrice, na.rm = T)` < 200000)
> neighbor_low_price <- filter(neighbor_based_price, neighbor_based_price$`mean(SalePrice, na.rm = T)` < 150000)
> neighbor_high_price <- filter(neighbor_based_price, neighbor_based_price$`mean(SalePrice, na.rm = T)` >= 200000)

> train_data$Neighborhood1[train_data$Neighborhood %in% neighbor_low_price$Neighborhood] <- 1
> train_data$Neighborhood1[train_data$Neighborhood %in% neighbor_med_price$Neighborhood] <- 2
> train_data$Neighborhood1[train_data$Neighborhood %in% neighbor_high_price$Neighborhood] <- 3

> table(train_data$Condition1)

Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNe   RRNn 
    48     81   1260      8     19     11     26      2      5 
> train_data$Condition1_1[train_data$Condition1 %in% c("PosA", "PosN")] <- 1
> train_data$Condition1_1[!train_data$Condition1 %in% c("PosA", "PosN")] <- 0

> table(train_data$Condition2)

Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNn 
     2      6   1445      1      2      1      1      2 
> train_data$Condition2_1[!train_data$Condition2 %in% c("PosA", "PosN")] <- 0
> train_data$Condition2_1[train_data$Condition2 %in% c("PosA", "PosN")] <- 1

> table(train_data$BldgType)

  1Fam 2fmCon Duplex  Twnhs TwnhsE 
  1220     31     52     43    114 
> price <- summarise(group_by(train_data, BldgType), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  BldgType `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1     1Fam                     185763.8
2   2fmCon                     128432.3
3   Duplex                     133541.1
4    Twnhs                     135911.6
5   TwnhsE                     181959.3
> train_data$BldgType1[train_data$BldgType %in% c("1Fam", "TwnhsE")] <- 1
> train_data$BldgType1[!train_data$BldgType %in% c("1Fam", "TwnhsE")] <- 0

> table(train_data$HouseStyle)

1.5Fin 1.5Unf 1Story 2.5Fin 2.5Unf 2Story SFoyer   SLvl 
   154     14    726      8     11    445     37     65 
> housestyle_price <- summarise(group_by(train_data,HouseStyle), mean(SalePrice, na.rm = T))
> housestyle_price
# A tibble: 8 × 2
  HouseStyle `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1     1.5Fin                     143116.7
2     1.5Unf                     110150.0
3     1Story                     175985.5
4     2.5Fin                     220000.0
5     2.5Unf                     157354.5
6     2Story                     210051.8
7     SFoyer                     135074.5
8       SLvl                     166703.4

> housestyle_low_price <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 150000)
> housestyle_med_price <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 150000 & housestyle_price$`mean(SalePrice, na.rm = T)` < 200000)
> housestyle_high_price <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 200000)
 
> train_data$HouseStyle1[train_data$HouseStyle %in% housestyle_low_price$HouseStyle] <- 1
> train_data$HouseStyle1[train_data$HouseStyle %in% housestyle_med_price$HouseStyle] <- 2
> train_data$HouseStyle1[train_data$HouseStyle %in% housestyle_high_price$HouseStyle] <- 3

> table(train_data$RoofStyle)

   Flat   Gable Gambrel     Hip Mansard    Shed 
     13    1141      11     286       7       2 
> price <- summarise(group_by(train_data,RoofStyle), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  RoofStyle `mean(SalePrice, na.rm = T)`
     <fctr>                        <dbl>
1      Flat                     194690.0
2     Gable                     171484.0
3   Gambrel                     148909.1
4       Hip                     218876.9
5   Mansard                     180568.4
6      Shed                     225000.0
 
> train_data$RoofStyle1[train_data$RoofStyle %in% c("Hip", "Shed")] <- 1
> train_data$RoofStyle1[!train_data$RoofStyle %in% c("Hip", "Shed")] <- 0

> table(train_data$RoofMatl)

ClyTile CompShg Membran   Metal    Roll Tar&Grv WdShake WdShngl 
      1    1434       1       1       1      11       5       6 
> price <- summarise(group_by(train_data,RoofMatl), mean(SalePrice, na.rm = T))
> price
# A tibble: 8 × 2
  RoofMatl `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1  ClyTile                     160000.0
2  CompShg                     179803.7
3  Membran                     241500.0
4    Metal                     180000.0
5     Roll                     137000.0
6  Tar&Grv                     185406.4
7  WdShake                     241400.0
8  WdShngl                     390250.0

> train_data$RoofMatl1[train_data$RoofMatl %in% c("WdShngl", "Membran", "WdShake")] <- 1
> train_data$RoofMatl1[!train_data$RoofMatl %in% c("WdShngl", "Membran", "WdShake")] <- 0

> table(train_data$Exterior1st)

AsbShng AsphShn BrkComm BrkFace  CBlock CemntBd HdBoard ImStucc MetalSd Plywood 
     20       1       2      50       1      61     222       1     220     108 
  Stone  Stucco VinylSd Wd Sdng WdShing 
      2      25     515     206      26 
> exterior1_price <- summarise(group_by(train_data,Exterior1st), mean(SalePrice, na.rm = T))
> exterior1_price
# A tibble: 15 × 2
   Exterior1st `mean(SalePrice, na.rm = T)`
        <fctr>                        <dbl>
1      AsbShng                     107385.6
2      AsphShn                     100000.0
3      BrkComm                      71000.0
4      BrkFace                     194573.0
5       CBlock                     105000.0
6      CemntBd                     231690.7
7      HdBoard                     163077.5
8      ImStucc                     262000.0
9      MetalSd                     149422.2
10     Plywood                     175942.4
11       Stone                     258500.0
12      Stucco                     162990.0
13     VinylSd                     213732.9
14     Wd Sdng                     149841.6
15     WdShing                     150655.1

> exterior1_low_price <- filter(exterior1_price, exterior1_price$`mean(SalePrice, na.rm = T)` < 150000)
> exterior1_med_price <- filter(exterior1_price, exterior1_price$`mean(SalePrice, na.rm = T)` >= 150000 & exterior1_price$`mean(SalePrice, na.rm = T)` < 200000)
> exterior1_high_price <- filter(exterior1_price, exterior1_price$`mean(SalePrice, na.rm = T)` >= 200000)
 
> train_data$Exterior1st_1[train_data$Exterior1st %in% exterior1_low_price$Exterior1st] <- 1
> train_data$Exterior1st_1[train_data$Exterior1st %in% exterior1_med_price$Exterior1st] <- 2
> train_data$Exterior1st_1[train_data$Exterior1st %in% exterior1_high_price$Exterior1st] <- 3

> table(train_data$Exterior1st)

AsbShng AsphShn BrkComm BrkFace  CBlock CemntBd HdBoard ImStucc MetalSd Plywood 
     20       1       2      50       1      61     222       1     220     108 
  Stone  Stucco VinylSd Wd Sdng WdShing 
      2      25     515     206      26 
> exterior2_price <- summarise(group_by(train_data,Exterior2nd), mean(SalePrice, na.rm = T))
> exterior2_price
# A tibble: 16 × 2
   Exterior2nd `mean(SalePrice, na.rm = T)`
        <fctr>                        <dbl>
1      AsbShng                     114060.6
2      AsphShn                     138000.0
3      Brk Cmn                     126714.3
4      BrkFace                     195818.0
5       CBlock                     105000.0
6      CmentBd                     230093.8
7      HdBoard                     167661.6
8      ImStucc                     252070.0
9      MetalSd                     149803.2
10       Other                     319000.0
11     Plywood                     168112.4
12       Stone                     158224.8
13      Stucco                     155905.2
14     VinylSd                     214432.5
15     Wd Sdng                     148386.1
16     Wd Shng                     161328.9

> exterior2_low_price <- filter(exterior2_price, exterior2_price$`mean(SalePrice, na.rm = T)` < 150000)
> exterior2_med_price <- filter(exterior2_price, exterior2_price$`mean(SalePrice, na.rm = T)` >= 150000 & exterior2_price$`mean(SalePrice, na.rm = T)` < 200000)
> exterior2_high_price <- filter(exterior2_price, exterior2_price$`mean(SalePrice, na.rm = T)` >= 200000)
 
> table(train_data$Exterior2nd)

AsbShng AsphShn Brk Cmn BrkFace  CBlock CmentBd HdBoard ImStucc MetalSd   Other 
     20       3       7      25       1      60     207      10     214       1 
Plywood   Stone  Stucco VinylSd Wd Sdng Wd Shng 
    142       5      26     504     197      38 
	
> train_data$Exterior2nd_1[train_data$Exterior2nd %in% exterior2_low_price$Exterior2nd] <- 1
> train_data$Exterior2nd_1[train_data$Exterior2nd %in% exterior2_med_price$Exterior2nd] <- 2
> train_data$Exterior2nd_1[train_data$Exterior2nd %in% exterior2_high_price$Exterior2nd] <- 3

> table(train_data$MasVnrType)

 BrkCmn BrkFace    None   Stone 
     15     445     864     128 
> price <- summarise(group_by(train_data, MasVnrType), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  MasVnrType `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1     BrkCmn                     146318.1
2    BrkFace                     204691.9
3       None                     156221.9
4      Stone                     265583.6
5         NA                     236484.2

> train_data$MasVnrType1[train_data$MasVnrType %in% c("Stone", "BrkFace") | is.na(train_data$MasVnrType)] <- 1
> train_data$MasVnrType1[!train_data$MasVnrType %in% c("Stone", "BrkFace") | !is.na(train_data$MasVnrType)] <- 0

> table(train_data$ExterQual)

 Ex  Fa  Gd  TA 
 52  14 488 906 
> price<-summarise(group_by(train_data,ExterQual), mean(SalePrice, na.rm = T))
> price
# A tibble: 4 × 2
  ExterQual `mean(SalePrice, na.rm = T)`
     <fctr>                        <dbl>
1        Ex                    367360.96
2        Fa                     87985.21
3        Gd                    231633.51
4        TA                    144341.31

> train_data$ExterQual1[train_data$ExterQual == "TA"] <- 1
> train_data$ExterQual1[train_data$ExterQual == "Gd"] <- 2
> train_data$ExterQual1[train_data$ExterQual == "Fa"] <- 3
> train_data$ExterQual1[train_data$ExterQual == "Ex"] <- 4

> table(train_data$ExterCond)

  Ex   Fa   Gd   Po   TA 
   3   28  146    1 1282 
> price<-summarise(group_by(train_data,ExterCond), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  ExterCond `mean(SalePrice, na.rm = T)`
     <fctr>                        <dbl>
1        Ex                     201333.3
2        Fa                     102595.1
3        Gd                     168897.6
4        Po                      76500.0
5        TA                     184034.9

> train_data$ExterCond1[train_data$ExterCond == "TA"] <- 1
> train_data$ExterCond1[train_data$ExterCond == "Po"] <- 2
> train_data$ExterCond1[train_data$ExterCond == "Gd"] <- 3
> train_data$ExterCond1[train_data$ExterCond == "Fa"] <- 4
> train_data$ExterCond1[train_data$ExterCond == "Ex"] <- 5
 
> table(train_data$Foundation)

BrkTil CBlock  PConc   Slab  Stone   Wood 
   146    634    647     24      6      3 
> price<-summarise(group_by(train_data,Foundation), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  Foundation `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1     BrkTil                     132291.1
2     CBlock                     149805.7
3      PConc                     225230.4
4       Slab                     107365.6
5      Stone                     165959.2
6       Wood                     185666.7
> train_data$Foundation1[train_data$Foundation == "PConc"] <- 1
> train_data$Foundation1[train_data$Foundation != "PConc"] <- 0
 
> table(train_data$BsmtQual)

 Ex  Fa  Gd  TA 
121  35 618 649 
> price<-summarise(group_by(train_data,BsmtQual), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  BsmtQual `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1       Ex                     327041.0
2       Fa                     115692.0
3       Gd                     202688.5
4       TA                     140759.8
5       NA                     105652.9
> train_data$BsmtQual1[is.na(train_data$BsmtQual)] <- 1
> train_data$BsmtQual1[train_data$BsmtQual == "TA"] <- 2
> train_data$BsmtQual1[train_data$BsmtQual == "Gd"] <- 3
> train_data$BsmtQual1[train_data$BsmtQual == "Fa"] <- 4
> train_data$BsmtQual1[train_data$BsmtQual == "Ex"] <- 5

> table(train_data$BsmtCond)

  Fa   Gd   Po   TA 
  45   65    2 1311 
> price<-summarise(group_by(train_data,BsmtCond), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  BsmtCond `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1       Fa                     121809.5
2       Gd                     213599.9
3       Po                      64000.0
4       TA                     183632.6
5       NA                     105652.9
> train_data$BsmtCond1[is.na(train_data$BsmtCond)] <- 1
> train_data$BsmtCond1[train_data$BsmtCond == "TA"] <- 2
> train_data$BsmtCond1[train_data$BsmtCond == "Po"] <- 3
> train_data$BsmtCond1[train_data$BsmtCond == "Gd"] <- 4
> train_data$BsmtCond1[train_data$BsmtCond == "Fa"] <- 5

> table(train_data$BsmtExposure)

 Av  Gd  Mn  No 
221 134 114 953 
> price<-summarise(group_by(train_data,BsmtExposure), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  BsmtExposure `mean(SalePrice, na.rm = T)`
        <fctr>                        <dbl>
1           Av                     206643.4
2           Gd                     257689.8
3           Mn                     192789.7
4           No                     165652.3
5           NA                     107938.3
> train_data$BsmtExposure1[is.na(train_data$BsmtExposure)] <- 1
> train_data$BsmtExposure1[train_data$BsmtExposure == "No"] <- 2
> train_data$BsmtExposure1[train_data$BsmtExposure == "Mn"] <- 3
> train_data$BsmtExposure1[train_data$BsmtExposure == "Gd"] <- 4
> train_data$BsmtExposure1[train_data$BsmtExposure == "Av"] <- 5

> table(train_data$BsmtFinType1)

ALQ BLQ GLQ LwQ Rec Unf 
220 148 418  74 133 430 
> price<-summarise(group_by(train_data,BsmtFinType1), mean(SalePrice, na.rm = T))
> price
# A tibble: 7 × 2
  BsmtFinType1 `mean(SalePrice, na.rm = T)`
        <fctr>                        <dbl>
1          ALQ                     161573.1
2          BLQ                     149493.7
3          GLQ                     235413.7
4          LwQ                     151852.7
5          Rec                     146889.2
6          Unf                     170670.6
7           NA                     105652.9
 
> train_data$BsmtFinType1_1[is.na(train_data$BsmtFinType1)] <- 1
> train_data$BsmtFinType1_1[train_data$BsmtFinType1 == "Unf"] <- 2
> train_data$BsmtFinType1_1[train_data$BsmtFinType1 == "Rec"] <- 3
> train_data$BsmtFinType1_1[train_data$BsmtFinType1 == "LwQ"] <- 4
> train_data$BsmtFinType1_1[train_data$BsmtFinType1 == "GLQ"] <- 5
> train_data$BsmtFinType1_1[train_data$BsmtFinType1 == "BLQ"] <- 6
> train_data$BsmtFinType1_1[train_data$BsmtFinType1 == "ALQ"] <- 7

> table(train_data$BsmtFinType2)

 ALQ  BLQ  GLQ  LwQ  Rec  Unf 
  19   33   14   46   54 1256 
> price<-summarise(group_by(train_data,BsmtFinType2), mean(SalePrice, na.rm = T))
> price
# A tibble: 7 × 2
  BsmtFinType2 `mean(SalePrice, na.rm = T)`
        <fctr>                        <dbl>
1          ALQ                     209942.1
2          BLQ                     151101.0
3          GLQ                     180982.1
4          LwQ                     164364.1
5          Rec                     164917.1
6          Unf                     184694.7
7           NA                     110346.2
> train_data$BsmtFinType2_1[is.na(train_data$BsmtFinType2)] <- 1
> train_data$BsmtFinType2_1[train_data$BsmtFinType2 == "Unf"] <- 2
> train_data$BsmtFinType2_1[train_data$BsmtFinType2 == "Rec"] <- 3
> train_data$BsmtFinType2_1[train_data$BsmtFinType2 == "LwQ"] <- 4
> train_data$BsmtFinType2_1[train_data$BsmtFinType2 == "GLQ"] <- 5
> train_data$BsmtFinType2_1[train_data$BsmtFinType2 == "BLQ"] <- 6
> train_data$BsmtFinType2_1[train_data$BsmtFinType2 == "ALQ"] <- 7

> table(train_data$Heating)

Floor  GasA  GasW  Grav  OthW  Wall 
    1  1428    18     7     2     4 
> price<-summarise(group_by(train_data,Heating), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  Heating `mean(SalePrice, na.rm = T)`
   <fctr>                        <dbl>
1   Floor                     72500.00
2    GasA                    182021.20
3    GasW                    166632.17
4    Grav                     75271.43
5    OthW                    125750.00
6    Wall                     92100.00
> train_data$Heating1[train_data$Heating %in% c("GasA", "GasW")] <- 1
> train_data$Heating1[!train_data$Heating %in% c("GasA", "GasW")] <- 0

> table(train_data$HeatingQC)

 Ex  Fa  Gd  Po  TA 
741  49 241   1 428 
> price<-summarise(group_by(train_data,HeatingQC), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
  HeatingQC `mean(SalePrice, na.rm = T)`
     <fctr>                        <dbl>
1        Ex                     214914.4
2        Fa                     123919.5
3        Gd                     156858.9
4        Po                      87000.0
5        TA                     142362.9
> train_data$HeatingQC1[train_data$HeatingQC == "TA"] <- 1
> train_data$HeatingQC1[train_data$HeatingQC == "Po"] <- 2
> train_data$HeatingQC1[train_data$HeatingQC == "Gd"] <- 3
> train_data$HeatingQC1[train_data$HeatingQC == "Fa"] <- 4
> train_data$HeatingQC1[train_data$HeatingQC == "Ex"] <- 5

> table(train_data$CentralAir)

   N    Y 
  95 1365 
> price<-summarise(group_by(train_data,CentralAir), mean(SalePrice, na.rm = T))
> price
# A tibble: 2 × 2
  CentralAir `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1          N                     105264.1
2          Y                     186186.7
> train_data$CentralAir1[train_data$CentralAir == "Y"] <- 1
> train_data$CentralAir1[train_data$CentralAir == "N"] <- 0

> table(train_data$Electrical)

FuseA FuseF FuseP   Mix SBrkr 
   94    27     3     1  1334 
> price<-summarise(group_by(train_data,Electrical), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  Electrical `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1      FuseA                    122196.89
2      FuseF                    107675.44
3      FuseP                     97333.33
4        Mix                     67000.00
5      SBrkr                    186825.11
6         NA                    167500.00
> train_data$Electrical1[train_data$Electrical == "SBrkr" | is.na(train_data$Electrical)] <- 1
> train_data$Electrical1[!train_data$Electrical == "SBrkr" | !is.na(train_data$Electrical)] <- 0

> table(train_data$KitchenQual)

 Ex  Fa  Gd  TA 
100  39 586 735 
> price<-summarise(group_by(train_data,KitchenQual), mean(SalePrice, na.rm = T))
> price
# A tibble: 4 × 2
  KitchenQual `mean(SalePrice, na.rm = T)`
       <fctr>                        <dbl>
1          Ex                     328554.7
2          Fa                     105565.2
3          Gd                     212116.0
4          TA                     139962.5
> train_data$KitchenQual1[train_data$KitchenQual == "TA"] <- 1
> train_data$KitchenQual1[train_data$KitchenQual == "Gd"] <- 2
> train_data$KitchenQual1[train_data$KitchenQual == "Fa"] <- 3
> train_data$KitchenQual1[train_data$KitchenQual == "Ex"] <- 4

> table(train_data$Functional)

Maj1 Maj2 Min1 Min2  Mod  Sev  Typ 
  14    5   31   34   15    1 1360 
> price<-summarise(group_by(train_data,Functional), mean(SalePrice, na.rm = T))
> price
# A tibble: 7 × 2
  Functional `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1       Maj1                     153948.1
2       Maj2                      85800.0
3       Min1                     146385.5
4       Min2                     144240.6
5        Mod                     168393.3
6        Sev                     129000.0
7        Typ                     183429.1
> 
> train_data$Functional1[train_data$Functional %in% c("Typ", "Mod")] <- 1
> train_data$Functional1[!train_data$Functional %in% c("Typ", "Mod")] <- 0

> table(train_data$FireplaceQu)

 Ex  Fa  Gd  Po  TA 
 24  33 380  20 313 
> price<-summarise(group_by(train_data,FireplaceQu), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  FireplaceQu `mean(SalePrice, na.rm = T)`
       <fctr>                        <dbl>
1          Ex                     337712.5
2          Fa                     167298.5
3          Gd                     226351.4
4          Po                     129764.1
5          TA                     205723.5
6          NA                     141331.5
 
> train_data$FireplaceQu1[train_data$FireplaceQu == "Po" | is.na(train_data$FireplaceQu)] <- 1
> train_data$FireplaceQu1[train_data$FireplaceQu == "TA"] <- 2
> train_data$FireplaceQu1[train_data$FireplaceQu == "Gd"] <- 3
> train_data$FireplaceQu1[train_data$FireplaceQu == "Fa"] <- 4
> train_data$FireplaceQu1[train_data$FireplaceQu == "Ex"] <- 5

> table(train_data$GarageType)

 2Types  Attchd Basment BuiltIn CarPort  Detchd 
      6     870      19      88       9     387 
> price<-summarise(group_by(train_data,GarageType), mean(SalePrice, na.rm = T))
> price
# A tibble: 7 × 2
  GarageType `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1     2Types                     151283.3
2     Attchd                     202892.7
3    Basment                     160570.7
4    BuiltIn                     254751.7
5    CarPort                     109962.1
6     Detchd                     134091.2
7         NA                     103317.3
> train_data$GarageType1[train_data$GarageType %in% c("Attchd", "BuiltIn")] <- 1
> train_data$GarageType1[!train_data$GarageType %in% c("Attchd", "BuiltIn")] <- 0

> table(train_data$GarageFinish)

Fin RFn Unf 
352 422 605 
> price<-summarise(group_by(train_data,GarageFinish), mean(SalePrice, na.rm = T))
> price
# A tibble: 4 × 2
  GarageFinish `mean(SalePrice, na.rm = T)`
        <fctr>                        <dbl>
1          Fin                     240052.7
2          RFn                     202068.9
3          Unf                     142156.4
4           NA                     103317.3

> train_data$GarageFinish1[train_data$GarageFinish %in% c("RFn", "Fin")] <- 1
> train_data$GarageFinish1[!train_data$GarageFinish %in% c("RFn", "Fin")] <- 0

> table(train_data$GarageQual)

  Ex   Fa   Gd   Po   TA 
   3   48   14    3 1311 
> price<-summarise(group_by(train_data,GarageQual), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  GarageQual `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1         Ex                     241000.0
2         Fa                     123573.4
3         Gd                     215860.7
4         Po                     100166.7
5         TA                     187489.8
6         NA                     103317.3
> train_data$GarageQual1[train_data$GarageQual == "Po" | is.na(train_data$GarageQual)] <- 1
> train_data$GarageQual1[train_data$GarageQual == "TA"] <- 2
> train_data$GarageQual1[train_data$GarageQual == "Gd"] <- 3
> train_data$GarageQual1[train_data$GarageQual == "Fa"] <- 4
> train_data$GarageQual1[train_data$GarageQual == "Ex"] <- 5

> table(train_data$GarageCond)

  Ex   Fa   Gd   Po   TA 
   2   35    9    7 1326 
> price<-summarise(group_by(train_data,GarageCond), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  GarageCond `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1         Ex                     124000.0
2         Fa                     114654.0
3         Gd                     179930.0
4         Po                     108500.0
5         TA                     187885.7
6         NA                     103317.3
> train_data$GarageCond1[train_data$GarageCond == "Po" | is.na(train_data$GarageCond)] <- 1
> train_data$GarageCond1[train_data$GarageCond == "TA"] <- 2
> train_data$GarageCond1[train_data$GarageCond == "Gd"] <- 3
> train_data$GarageCond1[train_data$GarageCond == "Fa"] <- 4
> train_data$GarageCond1[train_data$GarageCond == "Ex"] <- 5

> table(train_data$PavedDrive)

   N    P    Y 
  90   30 1340 
> price<-summarise(group_by(train_data,PavedDrive), mean(SalePrice, na.rm = T))
> price
# A tibble: 3 × 2
  PavedDrive `mean(SalePrice, na.rm = T)`
      <fctr>                        <dbl>
1          N                     115039.1
2          P                     132330.0
3          Y                     186434.0
> train_data$PavedDrive1[train_data$PavedDrive == "Y"] <- 1
> train_data$PavedDrive1[train_data$PavedDrive == "P"] <- 2
> train_data$PavedDrive1[train_data$PavedDrive == "N"] <- 3

> table(train_data$PoolQC)

Ex Fa Gd 
 2  2  3 
> price<-summarise(group_by(train_data,PoolQC), mean(SalePrice, na.rm = T))
> price
# A tibble: 4 × 2
  PoolQC `mean(SalePrice, na.rm = T)`
  <fctr>                        <dbl>
1     Ex                     490000.0
2     Fa                     215500.0
3     Gd                     201990.0
4     NA                     180404.7
> train_data$PoolQC1[train_data$PoolQC %in% c("Ex")] <- 1
> train_data$PoolQC1[!train_data$PoolQC %in% c("Ex")] <- 0

> table(train_data$Fence)

GdPrv  GdWo MnPrv  MnWw 
   59    54   157    11 
> price<-summarise(group_by(train_data,Fence), mean(SalePrice, na.rm = T))
> price
# A tibble: 5 × 2
   Fence `mean(SalePrice, na.rm = T)`
  <fctr>                        <dbl>
1  GdPrv                     178927.5
2   GdWo                     140379.3
3  MnPrv                     148751.1
4   MnWw                     134286.4
5     NA                     187596.8
> train_data$Fence1[!train_data$Fence %in% c("GdPrv")] <- 0
> train_data$Fence1[train_data$Fence %in% c("GdPrv")] <- 1

> table(train_data$SaleType)

  COD   Con ConLD ConLI ConLw   CWD   New   Oth    WD 
   43     2     9     5     5     4   122     3  1267 
> price<-summarise(group_by(train_data,SaleType), mean(SalePrice, na.rm = T))
> price
# A tibble: 9 × 2
  SaleType `mean(SalePrice, na.rm = T)`
    <fctr>                        <dbl>
1      COD                     143973.3
2      Con                     269600.0
3    ConLD                     138780.9
4    ConLI                     200390.0
5    ConLw                     143700.0
6      CWD                     210600.0
7      New                     274945.4
8      Oth                     119850.0
9       WD                     173401.8
> train_data$SaleType1[train_data$SaleType %in% c("New", "Con")] <- 1
> train_data$SaleType1[train_data$SaleType %in% c("ConLI", "CWD")] <- 2
> train_data$SaleType1[train_data$SaleType %in% c("WD")] <- 3
> train_data$SaleType1[train_data$SaleType %in% c("COD", "ConLw", "ConLD")] <- 4
> train_data$SaleType1[train_data$SaleType %in% c("Oth")] <- 5

> table(train_data$SaleCondition)

Abnorml AdjLand  Alloca  Family  Normal Partial 
    101       4      12      20    1198     125 
> price<-summarise(group_by(train_data,SaleCondition), mean(SalePrice, na.rm = T))
> price
# A tibble: 6 × 2
  SaleCondition `mean(SalePrice, na.rm = T)`
         <fctr>                        <dbl>
1       Abnorml                     146526.6
2       AdjLand                     104125.0
3        Alloca                     167377.4
4        Family                     149600.0
5        Normal                     175202.2
6       Partial                     272291.8
> train_data$SaleCondition1[train_data$SaleCondition %in% c("Partial")] <- 1
> train_data$SaleCondition1[train_data$SaleCondition %in% c("Alloca","Normal")] <- 2
> train_data$SaleCondition1[train_data$SaleCondition %in% c("Abnorml","Family")] <- 3
> train_data$SaleCondition1[train_data$SaleCondition %in% c("AdjLand")] <- 4
> 
> #All the CATEGORICAL Features have been changed. Now, we will drop the older ones.
> names(train_data)
  [1] "Id"             "MSSubClass"     "MSZoning"       "LotFrontage"   
  [5] "LotArea"        "Street"         "Alley"          "LotShape"      
  [9] "LandContour"    "Utilities"      "LotConfig"      "LandSlope"     
 [13] "Neighborhood"   "Condition1"     "Condition2"     "BldgType"      
 [17] "HouseStyle"     "OverallQual"    "OverallCond"    "YearBuilt"     
 [21] "YearRemodAdd"   "RoofStyle"      "RoofMatl"       "Exterior1st"   
 [25] "Exterior2nd"    "MasVnrType"     "MasVnrArea"     "ExterQual"     
 [29] "ExterCond"      "Foundation"     "BsmtQual"       "BsmtCond"      
 [33] "BsmtExposure"   "BsmtFinType1"   "BsmtFinSF1"     "BsmtFinType2"  
 [37] "BsmtFinSF2"     "BsmtUnfSF"      "TotalBsmtSF"    "Heating"       
 [41] "HeatingQC"      "CentralAir"     "Electrical"     "1stFlrSF"      
 [45] "2ndFlrSF"       "LowQualFinSF"   "GrLivArea"      "BsmtFullBath"  
 [49] "BsmtHalfBath"   "FullBath"       "HalfBath"       "BedroomAbvGr"  
 [53] "KitchenAbvGr"   "KitchenQual"    "TotRmsAbvGrd"   "Functional"    
 [57] "Fireplaces"     "FireplaceQu"    "GarageType"     "GarageYrBlt"   
 [61] "GarageFinish"   "GarageCars"     "GarageArea"     "GarageQual"    
 [65] "GarageCond"     "PavedDrive"     "WoodDeckSF"     "OpenPorchSF"   
 [69] "EnclosedPorch"  "3SsnPorch"      "ScreenPorch"    "PoolArea"      
 [73] "PoolQC"         "Fence"          "MiscFeature"    "MiscVal"       
 [77] "MoSold"         "YrSold"         "SaleType"       "SaleCondition" 
 [81] "SalePrice"      "MSZoning1"      "Street1"        "Alley1"        
 [85] "LotShape1"      "LandContour1"   "Utilities1"     "LotConfig1"    
 [89] "LandSlope1"     "Neighborhood1"  "Condition1_1"   "Condition2_1"  
 [93] "BldgType1"      "HouseStyle1"    "RoofStyle1"     "RoofMatl1"     
 [97] "Exterior1st_1"  "Exterior2nd_1"  "MasVnrType1"    "ExterQual1"    
[101] "ExterCond1"     "Foundation1"    "BsmtQual1"      "BsmtCond1"     
[105] "BsmtExposure1"  "BsmtFinType1_1" "BsmtFinType2_1" "Heating1"      
[109] "HeatingQC1"     "CentralAir1"    "Electrical1"    "KitchenQual1"  
[113] "Functional1"    "FireplaceQu1"   "GarageType1"    "GarageFinish1" 
[117] "GarageQual1"    "GarageCond1"    "PavedDrive1"    "PoolQC1"       
[121] "Fence1"         "SaleType1"      "SaleCondition1"
 
> train_data$MSZoning <- NULL
> train_data$Street <- NULL
> train_data$Alley <- NULL
> train_data$LotShape <- NULL
> train_data$LandContour <- NULL
> train_data$Utilities <- NULL
> train_data$LotConfg <- NULL
> train_data$LotConfig <- NULL
> train_data$LandSlope <- NULL
> train_data$Neighborhood <- NULL
> train_data$Condition1 <- NULL
> train_data$Condition2 <- NULL
> train_data$BldgType <- NULL
> train_data$HouseStyle <- NULL
> train_data$RoofStyle <- NULL
> train_data$RoofMatl <- NULL
> train_data$Exterior1st <- NULL
> train_data$Exterior2nd <- NULL
> train_data$MasVnrType <- NULL
> train_data$ExterQual <- NULL
> train_data$ExterCond <- NULL
> train_data$Foundation <- NULL
> train_data$BsmtQual <- NULL
> train_data$BsmtCond <- NULL
> train_data$BsmtExposure <- NULL
> train_data$BsmtFinType1 <- NULL
> train_data$BsmtFinType2 <- NULL
> train_data$Heating <- NULL
> train_data$HeatingQC <- NULL
> train_data$CentralAir <- NULL
> train_data$Electrical <- NULL
> train_data$KitchenQual <- NULL
> train_data$Functional <- NULL
> train_data$FireplaceQu <- NULL
> train_data$GarageType <- NULL
> train_data$GarageFinish <- NULL
> train_data$GarageQual <- NULL
> train_data$GarageCond <- NULL
> train_data$PavedDrive <- NULL
> train_data$PoolQC <- NULL
> train_data$Fence <- NULL
> train_data$MiscFeature <- NULL
> train_data$SaleType <- NULL
> train_data$SaleCondition <- NULL

#Replacing the NA values in the continuous features with 0
> train_data$GarageYrBlt[is.na(train_data$GarageYrBlt)] <- 0
> train_data$MasVnrArea[is.na(train_data$MasVnrArea)] <- 0
> train_data$LotFrontage[is.na(train_data$LotFrontage)] <- 0


# Structure of the data set after the transformation of categorical features
> str(train_data)

Classes ‘data.table’ and 'data.frame':	1460 obs. of  80 variables:
 $ Id            : int  1 2 3 4 5 6 7 8 9 10 ...
 $ MSSubClass    : int  60 20 60 70 60 50 20 60 50 190 ...
 $ LotFrontage   : int  65 80 68 60 84 85 75 0 51 50 ...
 $ LotArea       : int  8450 9600 11250 9550 14260 14115 10084 10382 6120 7420 ...
 $ OverallQual   : int  7 6 7 7 8 5 8 7 7 5 ...
 $ OverallCond   : int  5 8 5 5 5 5 5 6 5 6 ...
 $ YearBuilt     : int  2003 1976 2001 1915 2000 1993 2004 1973 1931 1939 ...
 $ YearRemodAdd  : int  2003 1976 2002 1970 2000 1995 2005 1973 1950 1950 ...
 $ MasVnrArea    : int  196 0 162 0 350 0 186 240 0 0 ...
 $ BsmtFinSF1    : int  706 978 486 216 655 732 1369 859 0 851 ...
 $ BsmtFinSF2    : int  0 0 0 0 0 0 0 32 0 0 ...
 $ BsmtUnfSF     : int  150 284 434 540 490 64 317 216 952 140 ...
 $ TotalBsmtSF   : int  856 1262 920 756 1145 796 1686 1107 952 991 ...
 $ 1stFlrSF      : int  856 1262 920 961 1145 796 1694 1107 1022 1077 ...
 $ 2ndFlrSF      : int  854 0 866 756 1053 566 0 983 752 0 ...
 $ LowQualFinSF  : int  0 0 0 0 0 0 0 0 0 0 ...
 $ GrLivArea     : int  1710 1262 1786 1717 2198 1362 1694 2090 1774 1077 ...
 $ BsmtFullBath  : int  1 0 1 1 1 1 1 1 0 1 ...
 $ BsmtHalfBath  : int  0 1 0 0 0 0 0 0 0 0 ...
 $ FullBath      : int  2 2 2 1 2 1 2 2 2 1 ...
 $ HalfBath      : int  1 0 1 0 1 1 0 1 0 0 ...
 $ BedroomAbvGr  : int  3 3 3 3 4 1 3 3 2 2 ...
 $ KitchenAbvGr  : int  1 1 1 1 1 1 1 1 2 2 ...
 $ TotRmsAbvGrd  : int  8 6 6 7 9 5 7 7 8 5 ...
 $ Fireplaces    : int  0 1 1 1 1 0 1 2 2 2 ...
 $ GarageYrBlt   : int  2003 1976 2001 1998 2000 1993 2004 1973 1931 1939 ...
 $ GarageCars    : int  2 2 2 3 3 2 2 2 2 1 ...
 $ GarageArea    : int  548 460 608 642 836 480 636 484 468 205 ...
 $ WoodDeckSF    : int  0 298 0 0 192 40 255 235 90 0 ...
 $ OpenPorchSF   : int  61 0 42 35 84 30 57 204 0 4 ...
 $ EnclosedPorch : int  0 0 0 272 0 0 0 228 205 0 ...
 $ 3SsnPorch     : int  0 0 0 0 0 320 0 0 0 0 ...
 $ ScreenPorch   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ PoolArea      : int  0 0 0 0 0 0 0 0 0 0 ...
 $ MiscVal       : int  0 0 0 0 0 700 0 350 0 0 ...
 $ MoSold        : int  2 5 9 2 12 10 8 11 4 1 ...
 $ YrSold        : int  2008 2007 2008 2006 2008 2009 2007 2009 2008 2008 ...
 $ SalePrice     : int  208500 181500 223500 140000 250000 143000 307000 200000 129900 118000 ...
 $ MSZoning1     : num  4 4 4 4 4 4 4 4 5 4 ...
 $ Street1       : num  1 1 1 1 1 1 1 1 1 1 ...
 $ Alley1        : num  0 0 0 0 0 0 0 0 0 0 ...
 $ LotShape1     : num  1 1 0 0 0 0 1 0 1 1 ...
 $ LandContour1  : num  1 1 1 1 1 1 1 1 1 1 ...
 $ Utilities1    : num  1 1 1 1 1 1 1 1 1 1 ...
 $ LotConfig1    : num  0 0 0 0 0 0 0 0 0 0 ...
 $ LandSlope1    : num  1 1 1 1 1 1 1 1 1 1 ...
 $ Neighborhood1 : num  2 3 2 3 3 2 3 2 1 1 ...
 $ Condition1_1  : num  0 0 0 0 0 0 0 1 0 0 ...
 $ Condition2_1  : num  0 0 0 0 0 0 0 0 0 0 ...
 $ BldgType1     : num  1 1 1 1 1 1 1 1 1 0 ...
 $ HouseStyle1   : num  3 2 3 3 3 1 2 3 1 1 ...
 $ RoofStyle1    : num  0 0 0 0 0 0 0 0 0 0 ...
 $ RoofMatl1     : num  0 0 0 0 0 0 0 0 0 0 ...
 $ Exterior1st_1 : num  3 1 3 1 3 3 3 2 2 1 ...
 $ Exterior2nd_1 : num  3 1 3 2 3 3 3 2 2 1 ...
 $ MasVnrType1   : num  0 0 0 0 0 0 0 0 0 0 ...
 $ ExterQual1    : num  2 1 2 1 2 1 2 1 1 1 ...
 $ ExterCond1    : num  1 1 1 1 1 1 1 1 1 1 ...
 $ Foundation1   : num  1 0 1 0 1 0 1 0 0 0 ...
 $ BsmtQual1     : num  3 3 3 2 3 3 5 3 2 2 ...
 $ BsmtCond1     : num  2 2 2 4 2 2 2 2 2 2 ...
 $ BsmtExposure1 : num  2 4 3 2 5 2 5 3 2 2 ...
 $ BsmtFinType1_1: num  5 7 5 7 5 5 5 7 2 5 ...
 $ BsmtFinType2_1: num  2 2 2 2 2 2 2 6 2 2 ...
 $ Heating1      : num  1 1 1 1 1 1 1 1 1 1 ...
 $ HeatingQC1    : num  5 5 5 3 5 5 5 5 3 5 ...
 $ CentralAir1   : num  1 1 1 1 1 1 1 1 1 1 ...
 $ Electrical1   : num  0 0 0 0 0 0 0 0 0 0 ...
 $ KitchenQual1  : num  2 1 2 2 2 1 2 1 1 1 ...
 $ Functional1   : num  1 1 1 1 1 1 1 1 0 1 ...
 $ FireplaceQu1  : num  1 2 2 3 2 1 3 2 2 2 ...
 $ GarageType1   : num  1 1 1 0 1 1 1 1 0 1 ...
 $ GarageFinish1 : num  1 1 1 0 1 0 1 1 0 1 ...
 $ GarageQual1   : num  2 2 2 2 2 2 2 2 4 3 ...
 $ GarageCond1   : num  2 2 2 2 2 2 2 2 2 2 ...
 $ PavedDrive1   : num  1 1 1 1 1 1 1 1 1 1 ...
 $ PoolQC1       : num  0 0 0 0 0 0 0 0 0 0 ...
 $ Fence1        : num  0 0 0 0 0 0 0 0 0 0 ...
 $ SaleType1     : num  3 3 3 3 3 3 3 3 3 3 ...
 $ SaleCondition1: num  2 2 2 3 2 2 2 2 3 2 ...
 - attr(*, ".internal.selfref")=<externalptr> 

 
> #Finding if there exists any correlation between the features
> correlations <- cor(train_data[c(5,6,7,8,16:25)], use = "everything")
> corrplot(correlations, method = "number", type = "lower", sig.level = 0.01, insig = "blank")
> correlations <- cor(train_data[c(5,6,7,8,26:35)], use = "everything")
> corrplot(correlations, method = "number", type = "lower", sig.level = 0.01, insig = "blank")
> correlations <- cor(train_data[c(5,6,7,8,66:75)], use = "everything")
> corrplot(correlations, method = "number", type = "lower", sig.level = 0.01, insig = "blank")

> # Scatter Plot Matrix
> attach(train_data)
> pairs(SalePrice~YearBuilt + OverallQual + TotalBsmtSF + GrLivArea, data = train_data, main = "Scatter Plot Matrix with high Correlation")
> pairs(SalePrice~YearBuilt + OverallQual + TotalBsmtSF + GrLivArea, data = train_data, main = "Scatter Plot Matrix with high Correlation", pch = 16, col = "brown")
> pairs(~YearBuilt + OverallQual + TotalBsmtSF + GrLivArea, data = train_data, main = "Scatter Plot Matrix with high Correlation")
> pairs(SalePrice~YearBuilt + OverallQual + TotalBsmtSF + GrLivArea, data = train_data, main = "Scatter Plot Matrix with high Correlation", pch = 16, col = "brown")

> #Interaction variables chosen on the basis of correlation
> train_data$Year_quality <- train_data$YearBuilt*train_data$OverallQual
> train_data$Year_remodeled_quality <- train_data$YearRemodAdd*train_data$OverallQual
> train_data$bsmt_quality <- train_data$OverallQual*train_data$TotalBsmtSF
> train_data$livingarea_quality <- train_data$OverallQual*train_data$GrLivArea
> train_data$bathroom_quality <- train_data$OverallQual*train_data$FullBath
> train_data$exterior_quality <- train_data$OverallQual*train_data$ExterCond1

# Structure of the data after adding the Interaction Features
> names(train_data)
 [1] "Id"                     "MSSubClass"             "LotFrontage"           
 [4] "LotArea"                "OverallQual"            "OverallCond"           
 [7] "YearBuilt"              "YearRemodAdd"           "MasVnrArea"            
[10] "BsmtFinSF1"             "BsmtFinSF2"             "BsmtUnfSF"             
[13] "TotalBsmtSF"            "1stFlrSF"               "2ndFlrSF"              
[16] "LowQualFinSF"           "GrLivArea"              "BsmtFullBath"          
[19] "BsmtHalfBath"           "FullBath"               "HalfBath"              
[22] "BedroomAbvGr"           "KitchenAbvGr"           "TotRmsAbvGrd"          
[25] "Fireplaces"             "GarageYrBlt"            "GarageCars"            
[28] "GarageArea"             "WoodDeckSF"             "OpenPorchSF"           
[31] "EnclosedPorch"          "3SsnPorch"              "ScreenPorch"           
[34] "PoolArea"               "MiscVal"                "MoSold"                
[37] "YrSold"                 "SalePrice"              "MSZoning1"             
[40] "Street1"                "Alley1"                 "LotShape1"             
[43] "LandContour1"           "Utilities1"             "LotConfig1"            
[46] "LandSlope1"             "Neighborhood1"          "Condition1_1"          
[49] "Condition2_1"           "BldgType1"              "HouseStyle1"           
[52] "RoofStyle1"             "RoofMatl1"              "Exterior1st_1"         
[55] "Exterior2nd_1"          "MasVnrType1"            "ExterQual1"            
[58] "ExterCond1"             "Foundation1"            "BsmtQual1"             
[61] "BsmtCond1"              "BsmtExposure1"          "BsmtFinType1_1"        
[64] "BsmtFinType2_1"         "Heating1"               "HeatingQC1"            
[67] "CentralAir1"            "Electrical1"            "KitchenQual1"          
[70] "Functional1"            "FireplaceQu1"           "GarageType1"           
[73] "GarageFinish1"          "GarageQual1"            "GarageCond1"           
[76] "PavedDrive1"            "PoolQC1"                "Fence1"                
[79] "SaleType1"              "SaleCondition1"         "Year_quality"          
[82] "Year_remodeled_quality" "bsmt_quality"           "livingarea_quality"    
[85] "bathroom_quality"       "exterior_quality"      

# Fitting a Linear Model
> linear_model1 <- lm(SalePrice ~ ., data = train_data)
> summary(linear_model1)

Call:
lm(formula = SalePrice ~ ., data = train_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-375690  -13268    -279   11905  188325 

Coefficients: (4 not defined because of singularities)
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)             1.728e+06  1.321e+06   1.308 0.190970    
Id                     -1.737e+00  1.831e+00  -0.949 0.342896    
MSSubClass             -1.318e+02  2.829e+01  -4.657 3.53e-06 ***
LotFrontage            -2.133e+01  2.581e+01  -0.826 0.408697    
LotArea                 3.287e-01  9.667e-02   3.401 0.000692 ***
OverallQual            -3.578e+05  9.335e+04  -3.833 0.000132 ***
OverallCond             5.669e+03  9.668e+02   5.864 5.64e-09 ***
YearBuilt              -1.627e+02  1.975e+02  -0.824 0.410177    
YearRemodAdd           -8.091e+02  2.690e+02  -3.007 0.002684 ** 
MasVnrArea              2.000e+01  5.238e+00   3.817 0.000141 ***
BsmtFinSF1              3.566e+01  1.065e+01   3.349 0.000832 ***
BsmtFinSF2              3.907e+01  1.271e+01   3.074 0.002152 ** 
BsmtUnfSF               2.308e+01  9.891e+00   2.333 0.019788 *  
TotalBsmtSF                    NA         NA      NA       NA    
`1stFlrSF`              1.535e+01  1.145e+01   1.342 0.179970    
`2ndFlrSF`              3.318e-01  1.198e+01   0.028 0.977902    
LowQualFinSF           -3.444e+01  1.950e+01  -1.766 0.077557 .  
GrLivArea                      NA         NA      NA       NA    
BsmtFullBath            6.089e+03  2.261e+03   2.693 0.007176 ** 
BsmtHalfBath            1.016e+03  3.484e+03   0.292 0.770624    
FullBath               -3.907e+04  9.748e+03  -4.008 6.46e-05 ***
HalfBath                2.867e+03  2.304e+03   1.244 0.213543    
BedroomAbvGr           -1.241e+03  1.531e+03  -0.810 0.417852    
KitchenAbvGr           -7.106e+03  5.177e+03  -1.372 0.170146    
TotRmsAbvGrd            3.401e+03  1.074e+03   3.166 0.001578 ** 
Fireplaces              8.722e+02  2.064e+03   0.423 0.672684    
GarageYrBlt            -5.301e+00  2.993e+00  -1.771 0.076754 .  
GarageCars              1.090e+04  2.549e+03   4.278 2.02e-05 ***
GarageArea             -1.928e+00  8.428e+00  -0.229 0.819084    
WoodDeckSF              2.370e+01  6.837e+00   3.467 0.000543 ***
OpenPorchSF             1.255e+01  1.312e+01   0.956 0.339258    
EnclosedPorch           2.814e+00  1.431e+01   0.197 0.844121    
`3SsnPorch`             3.615e+01  2.626e+01   1.376 0.168899    
ScreenPorch             6.581e+01  1.451e+01   4.537 6.20e-06 ***
PoolArea               -1.054e+02  2.307e+01  -4.568 5.37e-06 ***
MiscVal                 2.556e-01  1.562e+00   0.164 0.870038    
MoSold                 -1.385e+02  2.882e+02  -0.481 0.630826    
YrSold                  4.946e+01  5.985e+02   0.083 0.934143    
MSZoning1               2.670e+03  1.520e+03   1.757 0.079191 .  
Street1                 3.149e+04  1.275e+04   2.469 0.013681 *  
Alley1                  1.071e+03  5.327e+03   0.201 0.840679    
LotShape1               7.307e+02  1.831e+03   0.399 0.689891    
LandContour1            6.637e+03  3.093e+03   2.146 0.032064 *  
Utilities1              7.725e+04  2.981e+04   2.592 0.009652 ** 
LotConfig1              1.072e+04  3.341e+03   3.209 0.001364 ** 
LandSlope1              2.424e+03  3.689e+03   0.657 0.511169    
Neighborhood1           1.354e+04  1.533e+03   8.827  < 2e-16 ***
Condition1_1           -1.083e+04  6.097e+03  -1.777 0.075777 .  
Condition2_1           -1.491e+05  1.922e+04  -7.757 1.68e-14 ***
BldgType1              -3.159e+03  4.496e+03  -0.703 0.482383    
HouseStyle1            -2.409e+03  1.606e+03  -1.500 0.133955    
RoofStyle1                     NA         NA      NA       NA    
RoofMatl1               2.860e+04  9.226e+03   3.100 0.001972 ** 
Exterior1st_1           2.144e+03  3.009e+03   0.713 0.476175    
Exterior2nd_1          -2.794e+03  3.057e+03  -0.914 0.360798    
MasVnrType1                    NA         NA      NA       NA    
ExterQual1              6.132e+03  1.887e+03   3.249 0.001185 ** 
ExterCond1             -1.132e+04  4.715e+03  -2.402 0.016456 *  
Foundation1             5.355e+01  2.555e+03   0.021 0.983279    
BsmtQual1               4.854e+03  1.323e+03   3.670 0.000252 ***
BsmtCond1              -2.528e+03  1.222e+03  -2.069 0.038696 *  
BsmtExposure1           2.027e+03  7.846e+02   2.583 0.009901 ** 
BsmtFinType1_1          6.060e+02  5.754e+02   1.053 0.292365    
BsmtFinType2_1         -1.244e+03  1.237e+03  -1.005 0.314890    
Heating1                7.233e+03  8.726e+03   0.829 0.407275    
HeatingQC1              1.836e+02  5.609e+02   0.327 0.743507    
CentralAir1             4.258e+03  4.013e+03   1.061 0.288816    
Electrical1             1.867e+04  2.914e+04   0.641 0.521837    
KitchenQual1            7.037e+03  1.339e+03   5.256 1.70e-07 ***
Functional1             1.214e+04  3.583e+03   3.388 0.000723 ***
FireplaceQu1            3.036e+03  1.293e+03   2.348 0.019021 *  
GarageType1             1.032e+03  2.447e+03   0.422 0.673108    
GarageFinish1           8.519e+02  2.372e+03   0.359 0.719527    
GarageQual1            -1.184e+03  2.340e+03  -0.506 0.612940    
GarageCond1             4.367e+02  2.609e+03   0.167 0.867102    
PavedDrive1            -1.212e+03  1.881e+03  -0.644 0.519529    
PoolQC1                 1.658e+05  2.536e+04   6.539 8.70e-11 ***
Fence1                 -1.711e+03  4.053e+03  -0.422 0.673019    
SaleType1              -3.403e+03  1.940e+03  -1.755 0.079541 .  
SaleCondition1         -8.758e+03  2.683e+03  -3.265 0.001123 ** 
Year_quality            3.473e+01  3.354e+01   1.036 0.300592    
Year_remodeled_quality  1.430e+02  4.916e+01   2.908 0.003691 ** 
bsmt_quality           -4.561e+00  1.545e+00  -2.951 0.003218 ** 
livingarea_quality      5.357e+00  1.600e+00   3.347 0.000838 ***
bathroom_quality        7.224e+03  1.571e+03   4.598 4.66e-06 ***
exterior_quality        1.880e+03  8.425e+02   2.232 0.025783 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 28680 on 1378 degrees of freedom
Multiple R-squared:  0.8769,	Adjusted R-squared:  0.8697 
F-statistic: 121.2 on 81 and 1378 DF,  p-value: < 2.2e-16

> anova(linear_model1)

Analysis of Variance Table

Response: SalePrice
                         Df     Sum Sq    Mean Sq   F value    Pr(>F)    
Id                        1 4.4230e+09 4.4230e+09    5.3766 0.0205538 *  
MSSubClass                1 6.5040e+10 6.5040e+10   79.0647 < 2.2e-16 ***
LotFrontage               1 3.5281e+11 3.5281e+11  428.8882 < 2.2e-16 ***
LotArea                   1 5.3236e+11 5.3236e+11  647.1485 < 2.2e-16 ***
OverallQual               1 5.1892e+12 5.1892e+12 6308.1353 < 2.2e-16 ***
OverallCond               1 7.1387e+08 7.1387e+08    0.8678 0.3517282    
YearBuilt                 1 1.0128e+11 1.0128e+11  123.1168 < 2.2e-16 ***
YearRemodAdd              1 2.6990e+10 2.6990e+10   32.8101 1.246e-08 ***
MasVnrArea                1 2.1327e+11 2.1327e+11  259.2559 < 2.2e-16 ***
BsmtFinSF1                1 1.6448e+11 1.6448e+11  199.9447 < 2.2e-16 ***
BsmtFinSF2                1 8.9774e+09 8.9774e+09   10.9131 0.0009794 ***
BsmtUnfSF                 1 5.6198e+10 5.6198e+10   68.3151 3.240e-16 ***
`1stFlrSF`                1 1.1306e+11 1.1306e+11  137.4346 < 2.2e-16 ***
`2ndFlrSF`                1 4.7439e+11 4.7439e+11  576.6790 < 2.2e-16 ***
LowQualFinSF              1 2.9027e+09 2.9027e+09    3.5285 0.0605320 .  
BsmtFullBath              1 1.9110e+10 1.9110e+10   23.2306 1.596e-06 ***
BsmtHalfBath              1 1.3669e+08 1.3669e+08    0.1662 0.6836107    
FullBath                  1 7.1042e+08 7.1042e+08    0.8636 0.3528948    
HalfBath                  1 3.8355e+07 3.8355e+07    0.0466 0.8290755    
BedroomAbvGr              1 4.1181e+10 4.1181e+10   50.0608 2.365e-12 ***
KitchenAbvGr              1 4.0728e+09 4.0728e+09    4.9510 0.0262362 *  
TotRmsAbvGrd              1 2.6987e+10 2.6987e+10   32.8056 1.249e-08 ***
Fireplaces                1 8.8885e+09 8.8885e+09   10.8050 0.0010377 ** 
GarageYrBlt               1 5.0909e+08 5.0909e+08    0.6189 0.4316076    
GarageCars                1 8.3687e+10 8.3687e+10  101.7321 < 2.2e-16 ***
GarageArea                1 2.2521e+08 2.2521e+08    0.2738 0.6008977    
WoodDeckSF                1 9.0882e+09 9.0882e+09   11.0478 0.0009112 ***
OpenPorchSF               1 1.1807e+08 1.1807e+08    0.1435 0.7048513    
EnclosedPorch             1 3.6230e+07 3.6230e+07    0.0440 0.8338059    
`3SsnPorch`               1 2.8194e+08 2.8194e+08    0.3427 0.5583501    
ScreenPorch               1 1.3030e+10 1.3030e+10   15.8401 7.252e-05 ***
PoolArea                  1 2.0569e+09 2.0569e+09    2.5004 0.1140466    
MiscVal                   1 7.9881e+07 7.9881e+07    0.0971 0.7553784    
MoSold                    1 9.1871e+05 9.1871e+05    0.0011 0.9733455    
YrSold                    1 1.2437e+09 1.2437e+09    1.5119 0.2190664    
MSZoning1                 1 3.7343e+09 3.7343e+09    4.5395 0.0332977 *  
Street1                   1 3.8104e+09 3.8104e+09    4.6320 0.0315547 *  
Alley1                    1 8.4414e+08 8.4414e+08    1.0262 0.3112405    
LotShape1                 1 1.4086e+09 1.4086e+09    1.7123 0.1909034    
LandContour1              1 1.3397e+07 1.3397e+07    0.0163 0.8984713    
Utilities1                1 3.2139e+09 3.2139e+09    3.9068 0.0482890 *  
LotConfig1                1 6.1200e+09 6.1200e+09    7.4396 0.0064614 ** 
LandSlope1                1 4.7732e+09 4.7732e+09    5.8024 0.0161345 *  
Neighborhood1             1 1.0034e+11 1.0034e+11  121.9798 < 2.2e-16 ***
Condition1_1              1 7.8361e+09 7.8361e+09    9.5258 0.0020665 ** 
Condition2_1              1 1.9871e+10 1.9871e+10   24.1555 9.951e-07 ***
BldgType1                 1 1.6122e+08 1.6122e+08    0.1960 0.6580490    
HouseStyle1               1 2.8023e+09 2.8023e+09    3.4065 0.0651527 .  
RoofMatl1                 1 7.4678e+09 7.4678e+09    9.0780 0.0026342 ** 
Exterior1st_1             1 6.8411e+08 6.8411e+08    0.8316 0.3619645    
Exterior2nd_1             1 3.4921e+06 3.4921e+06    0.0042 0.9480605    
ExterQual1                1 8.9869e+10 8.9869e+10  109.2468 < 2.2e-16 ***
ExterCond1                1 8.1262e+08 8.1262e+08    0.9878 0.3204442    
Foundation1               1 2.7729e+09 2.7729e+09    3.3708 0.0665752 .  
BsmtQual1                 1 2.8045e+10 2.8045e+10   34.0916 6.548e-09 ***
BsmtCond1                 1 6.2496e+09 6.2496e+09    7.5971 0.0059232 ** 
BsmtExposure1             1 1.3497e+10 1.3497e+10   16.4076 5.393e-05 ***
BsmtFinType1_1            1 1.6122e+09 1.6122e+09    1.9599 0.1617528    
BsmtFinType2_1            1 8.2442e+08 8.2442e+08    1.0022 0.3169596    
Heating1                  1 1.3176e+05 1.3176e+05    0.0002 0.9899041    
HeatingQC1                1 7.6379e+08 7.6379e+08    0.9285 0.3354256    
CentralAir1               1 1.6894e+06 1.6894e+06    0.0021 0.9638612    
Electrical1               1 1.6512e+08 1.6512e+08    0.2007 0.6542061    
KitchenQual1              1 3.8780e+10 3.8780e+10   47.1420 9.958e-12 ***
Functional1               1 1.5610e+10 1.5610e+10   18.9757 1.422e-05 ***
FireplaceQu1              1 5.0019e+09 5.0019e+09    6.0804 0.0137900 *  
GarageType1               1 2.7776e+08 2.7776e+08    0.3376 0.5612854    
GarageFinish1             1 3.6357e+07 3.6357e+07    0.0442 0.8335199    
GarageQual1               1 2.7322e+08 2.7322e+08    0.3321 0.5645027    
GarageCond1               1 2.9220e+06 2.9220e+06    0.0036 0.9524838    
PavedDrive1               1 1.8020e+09 1.8020e+09    2.1906 0.1390852    
PoolQC1                   1 5.5117e+10 5.5117e+10   67.0012 6.115e-16 ***
Fence1                    1 7.9522e+08 7.9522e+08    0.9667 0.3256809    
SaleType1                 1 2.3167e+10 2.3167e+10   28.1627 1.298e-07 ***
SaleCondition1            1 7.6622e+09 7.6622e+09    9.3144 0.0023171 ** 
Year_quality              1 9.3888e+09 9.3888e+09   11.4132 0.0007496 ***
Year_remodeled_quality    1 2.4610e+10 2.4610e+10   29.9163 5.350e-08 ***
bsmt_quality              1 2.3767e+08 2.3767e+08    0.2889 0.5910047    
livingarea_quality        1 4.4627e+10 4.4627e+10   54.2495 3.029e-13 ***
bathroom_quality          1 1.7557e+10 1.7557e+10   21.3427 4.200e-06 ***
exterior_quality          1 4.0977e+09 4.0977e+09    4.9813 0.0257833 *  
Residuals              1378 1.1336e+12 8.2262e+08                        
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# In order to cross validate the model, we have divided the data into training set and testing set.
> sample_size <- floor(0.75 * nrow(train_data))
> set.seed(123)
> train_indices <- sample(seq_len(nrow(train_data)), size = sample_size)
> training_data <- train_data[train_indices,]
> testing_data <- train_data[-train_indices,]

#Dimension of the training and testing data
> dim(training_data)
[1] 1095   86
> dim(testing_data)
[1] 365  86

#Predicting the values used from the testing data set, building a predictive model and then calculating the root mean squared error
> prediction <- predict(linear_model1, testing_data, type="response")
> prediction_result <- cbind(testing_data,prediction)
> prediction_result$log_prediction <- log(prediction_result$prediction)
> prediction_result$log_SalePrice <- log(prediction_result$SalePrice)
> rmse(prediction_result$log_SalePrice,prediction_result$log_prediction)

#Performance Metrics
> summary(linear_model1)$r.squared
[1] 0.8768912
> summary(linear_model1)$adj.r.squared
[1] 0.8696548
> rmse(prediction_result$log_SalePrice,prediction_result$log_prediction)
[1] 0.1541019