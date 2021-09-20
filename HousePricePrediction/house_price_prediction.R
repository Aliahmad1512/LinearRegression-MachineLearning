########################################################################################
##################### CASE STUDY-House Price Prediction ################################
########################################################################################

##################################################################################################
#Business Objective: Perform a predictive regression analysis on the provided House_Price_Data 
#in R-studio to arrive at the final model from where significant variables can be summarized which 
#play a vital role in determining the purchasing price of the house.
#The objective here is to help customers optimize their purchase of houses
##################################################################################################

#1) Read the data-set in R studio (setting working directory)

house_data <- read.csv(choose.files(), stringsAsFactors = TRUE)

house_data

head(house_data)
tail(house_data)

View(house_data)

#2) Dimension and summary checking of the data for the understanding of data-set.
# Conclude your observations in #Comments below. 

dim(house_data)
# dim() function returns both the number of rows and column in a data frame.

nrow(house_data)
ncol(house_data)
#We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separetely.

summary(house_data)
# summary function returns some basic calculations for each column.

library(psych)
describe(house_data)
# This function provides more detailed statistics including standard deviation,
# mean absolute deviation, skew, etc

#3) Structure check and conversion of variables (if required). 

str(house_data)
#Parking_type is converted to factor
#City_type is converted to factor 


#4) Data pre-processing:
#check missing values
#i) Checking the presence of missing values and
#imputation of missing values with appropriate measures of CT.

colSums(is.na(house_data))
colSums(is.na(house_data))/nrow(house_data)

# so there are missing values found in "Taxi_dist", "Market_dist", "Hospital_dist",
# "Carpet_area" and "Builtup_area"

boxplot(house_data$Taxi_dist) 
mean(house_data$Taxi_dist, na.rm = T)
house_data$Taxi_dist[which(is.na(house_data$Taxi_dist))] <- 8229.728


boxplot(house_data$Market_dist) 
mean(house_data$Market_dist, na.rm = T)
house_data$Market_dist[which(is.na(house_data$Market_dist ))] <- 11018.75


boxplot(house_data$Hospital_dist)
mean(house_data$Hospital_dist, na.rm = T)
house_data$Hospital_dist[which(is.na(house_data$Hospital_dist))] <- 13072.09


boxplot(house_data$Carpet_area) 
mean(house_data$Carpet_area, na.rm = T)
house_data$Carpet_area[which(is.na(house_data$Carpet_area))] <- 1511.863


boxplot(house_data$Builtup_area)
mean(house_data$Builtup_area, na.rm = T)
house_data$Builtup_area[which(is.na(house_data$Builtup_area))] <- 1794.925

colSums(is.na(house_data)) 
# Now there is no missing values

#ii)Checking for the presence of outliers in variables using boxplot.

boxplot(house_data$Taxi_dist)
quantile(house_data$Taxi_dist, seq(0,1,0.02))
house_data$Taxi_dist <- ifelse(house_data$Taxi_dist>13000,13000, house_data$Taxi_dist)
house_data$Taxi_dist <- ifelse(house_data$Taxi_dist<3000,3000, house_data$Taxi_dist)
boxplot(house_data$Taxi_dist)


boxplot(house_data$Market_dist)
quantile(house_data$Market_dist, seq(0,1,0.02))
house_data$Market_dist <- ifelse(house_data$Market_dist>17000,17000, house_data$Market_dist)
house_data$Market_dist <- ifelse(house_data$Market_dist<5000,5000, house_data$Market_dist)
boxplot(house_data$Market_dist)


boxplot(house_data$Hospital_dist)
quantile(house_data$Hospital_dist, seq(0,1,0.02))
house_data$Hospital_dist <- ifelse(house_data$Hospital_dist>18000,18000, house_data$Hospital_dist)
house_data$Hospital_dist <- ifelse(house_data$Hospital_dist<7000,7000, house_data$Hospital_dist)
boxplot(house_data$Hospital_dist)


boxplot(house_data$Builtup_area)
quantile(house_data$Builtup_area, seq(0,1,0.02))
house_data$Builtup_area <- ifelse(house_data$Builtup_area>2300,2300, house_data$Builtup_area)
house_data$Builtup_area <- ifelse(house_data$Builtup_area<1000,1000, house_data$Builtup_area)
boxplot(house_data$Builtup_area)


boxplot(house_data$Carpet_area)
quantile(house_data$Carpet_area, seq(0,1,0.02))
house_data$Carpet_area <- ifelse(house_data$Carpet_area>2000,2000, house_data$Carpet_area)
house_data$Carpet_area <- ifelse(house_data$Carpet_area<1000,1000, house_data$Carpet_area)
boxplot(house_data$Carpet_area)

#Outlier Treatment Done

######################################################################
#5) Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis #####################

# Multiple Continuous Variables

ColsForHist <- c("Taxi_dist","Market_dist","Hospital_dist", "Carpet_area",
                 "Builtup_area", "Rainfall","Price_house" )

par(mfrow=c(2,2)) 

library(RColorBrewer)
# library for colors

# Using loops to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(house_data[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))}


# Multiple Categorical Variables

ColsForBar <- c("Parking_type","City_type")

par(mfrow=c(2,2)) 

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(house_data[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Spectral"))}

#################### Bivariate Analysis ########################

# Relationship between target variable and predictors variables
# Categorical vs Continuous --- Box Plot
# Continuous Vs Continuous --- Scatter plot

# Categorical vs Continuous analysis-- Boxplot

par(mfrow=c(2,2))

ColsForBar =c("Parking_type","City_type")

for(box_cols in ColsForBar){
  boxplot(Price_house~house_data[  ,c(box_cols)], data=house_data  , main=paste('Boxplot of :',box_cols),col=brewer.pal(8,"Accent"))
}

# Continuous Vs Continuous --- Scatter plot

ContinuousCols <- c("Taxi_dist","Market_dist","Hospital_dist", "Carpet_area","Builtup_area", "Rainfall","Price_house" )

par(mfrow=c(1,1))

plot(house_data[,ContinuousCols],col='blue')

############### Statistical Tests ##################
# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

############### Correlation test ################

# Continuous Vs Continuous ---- Correlation test
# correlation for checking continuous variable 
# whether it is a good or bad predictor

ContinuousCols <- c("Taxi_dist","Market_dist","Hospital_dist", "Carpet_area",
                    "Builtup_area", "Rainfall","Price_house" )

library(corrplot)
corrplot(cor(house_data[,ContinuousCols]))

CorrData<-cor(house_data[,ContinuousCols ], use = "complete.obs")
CorrData


abs(CorrData['Price_house',])>0.5

names(CorrData['Price_house',])
names(CorrData['Price_house',][abs(CorrData['Price_house',])>0.5])

#here we have taken the thresold value >0.3
#these continuous variables are not good predictors for the model
#hence, we will remove these columns.

###################### ANOVA TEST ############################

# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

colsforanova <- c("Parking_type","City_type")

for (i in colsforanova){
  test_summary=summary(aov(Price_house ~ house_data[,i], data = house_data))
  print(paste("The Anova test with",i))
  print(test_summary)
}

# "Parking_type" and "City_type" are significant for the model 
# Here the p-value is less, so we reject the null hypothesis. 


#7) Cross validation (splitting of data)

# Randomly splitting the dataset into training and test set
# please split data between 70% and 80% for training dataset
# and 20% to 30% for test dataset

library(caTools)

set.seed(1000)
split <- sample.split(house_data$Price_house, SplitRatio = 0.80)
split #This gives boolean value i.e 75% True - training data and 25% False - Test

table(split)
# table function will show how many are true and false value

training <- subset(house_data, split==TRUE)
nrow(training)
testing <- subset(house_data, split==FALSE)
nrow(testing)

#8)Model building and execution. 

# linear regression model function : lm(dv~., data=training)

lin_reg <- lm(Price_house~., data=training)
lin_reg
summary(lin_reg)

###########################
#Multiple R-squared: 0.4693 
#Adjusted R-squared: 0.4614 
###########################

# there are some variable which is not statically significant, 
# hence, we have to remove this.

lin_reg1 <- lm(Price_house~.-Taxi_dist-Market_dist-Hospital_dist-Rainfall-Builtup_area, data=training)
lin_reg1
summary(lin_reg1)

###########################
#Multiple R-squared: 0.4512 
#Adjusted R-squared: 0.4467 
###########################

#9)Predict and validate the final model in the test sample of your data 

# We can predict model by using test dataset

lin_pred <- predict(lin_reg1, newdata = testing)
lin_pred 

lin_pred_cbind <- cbind(testing$Price_house, lin_pred )
lin_pred_cbind

#10) Assumption Tests of Linear Regression model --

#We have to perform the Assumption Test

#Autocorrelation
library(lmtest)

library(faraway)

library(car)

#durwin watson test
dwtest(lin_reg1)

#DW = 1.8201, hence there is no autocorrelation found

#multicollinearity

vif(lin_reg1)

#no multicollinearity found

# 11) MAPE and MDAPE:

# Mean absolute percentage error (MAPE)

library(MLmetrics)

MeanAPE <- mean(abs((testing$Price_house-lin_pred)/testing$Price_house)) * 100
MeanAPE

# Median absolute percentage error (MDAPE)

MedianAPE <- median(abs((testing$Price_house-lin_pred)/testing$Price_house)) * 100
MedianAPE

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))
 
#######################################################
# Multiple R-squared:  0.4512	
# Adjusted R-squared:  0.4467 
# Mean Absolute Percentage Error (MAPE) : 22.98951 
# Median Absolute Percentage Error (MDAPE) : 17.05081
# Mean Accuracy of Linear Regression Model :  77.01
# Median Accuracy of Linear Regression Model :  82.94
#######################################################

#############################################################################################
################################## LINEAR REGRESSION ########################################
#############################################################################################

