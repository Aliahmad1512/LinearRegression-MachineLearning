########################## CASE STUDY - LINEAR REGRESSION #####################################
############################## ANIME RATING PREDICTION ########################################

# THE OBJECTIVE HERE IS TO TO PREDICT THE RATINGS SO THAT THE
# STUDIOS CAN DEVELOP THEIR STRATEGIES WHICH CAN IMPROVE THE RATINGS.


# 1) Identify the Problem Statement - what are you trying to solve?

## Predicting the Ratings of the Anime releases.

# 2) Import the dataset and identify the Target variable in the data.

## Here the target variable is "Rating" which is here Continuous in nature. 

# Importing the data
anime <- read.csv(choose.files())

anime

head(anime)

View(anime) ###### To see excel sheet in my rstudio #####
class(anime)
names(anime)

# Dimension and summary checking of the data-set. 

dim(anime)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.
# no of rows-7029
# no of columns-16

nrow(anime)
ncol(anime)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

summary(anime)
# summary function returns some basic calculations like
# mean,median,1st,2nd,3rd quartilefor each column.

library(psych)
### stats package - "psych" ###
describe(anime)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc


# 3) Identifying the type of variables:-

# "mediaType", "ongoing", "szOfRelease"-- are Categorical variables

# "eps", "duration", "contentWarn", "watched", "watching",	
# "wantWatch", "dropped",	"rating",	"votes"-- are Continuous variables


### Structure check and conversion of variables if required ###

length(unique(anime$mediaType))
# 9 unique values and also it can help to predict.

length(unique(anime$title))
# 7029 too many unique values and can't help in prediction

length(unique(anime$description))
# 3923 too many unique values and this also can't help in prediction

length(unique(anime$ongoing))
#consists of yes/no values
table(anime$ongoing)
#no-6942,yes-87
#it has very less value of yes compare to no
#so it doesn't help in the prediction of rating, reject the column
names(anime)
# now checking the structure of dataset
str(anime)

# Removing useless columns as they consists of too many unique values.
anime <- anime[,-c(1,5,7,8,9,10)]
# Now checking again for the sturcture of dataset
str(anime)

#Converting categorical variables to factors

anime$mediaType <- as.factor(anime$mediaType)

#now check again the structure
str(anime)

# 4) Data pre-processing:

# find the missing value by using visualization
install.packages("Amelia")
library(Amelia)

missmap(anime, main="ANIME - Finding Missing Data",
        col=c("red", "black"), legend = F)

## to check missing data in character or factor variable.
colSums(anime=="") 
colSums(anime=="")/nrow(anime) 
# 0.42% missing data in "mediatType" - impute the value
# 69% missing data found in "sznOfRelease"
# reject the column "sznOfRelease", higher than threshhold (0.25%)
anime$sznOfRelease <- NULL
anime$duration <- NULL

colSums(is.na(anime))
colSums(is.na(anime))/nrow(anime) 
# 87(1.23%) missing data found in "watched"
# 1696(24.1) missing data found in "duration"
#Reject the column duration

boxplot(anime$watched) 
mean(anime$watched, na.rm = T)
anime$watched[which(is.na(anime$watched))] <- 3364.31

# Now again checking for missing data in "Age" by visualisation
missmap(anime, main="ANIME - Finding Missing Data",
        col=c("red", "black"), legend = F)

colSums(is.na(anime))
## Now no missing values in the dataset ##

# Mode function to handle missing value in "mediaType"  
getmode <- function(anime) {
  uniqv <- unique(anime)
  uniqv[which.max(tabulate(match(anime, uniqv)))]
}
getmode(anime$mediaType)

#replace with mode 
anime$mediaType[anime$mediaType==""]<- getmode(anime$mediaType)
colSums(anime=="")
### Now there are no missing values ###

# Checking for the presence of outliers in variables using boxplot

boxplot(anime$watched) 
quantile(anime$watched, seq(0,1,0.02))
anime$watched <- ifelse(anime$watched>7000,7000, anime$watched)
boxplot(anime$watched)     # Outlier handled

boxplot(anime$votes) 
quantile(anime$votes, seq(0,1,0.01))
anime$votes <- ifelse(anime$votes>4500,4500, anime$votes)
boxplot(anime$votes)      # Outlier handled

boxplot(anime$mediaType) # No outlier found

boxplot(anime$eps)
quantile(anime$eps, seq(0,1,0.02))
anime$eps <- ifelse(anime$eps>25,25, anime$eps)
boxplot(anime$eps)       # Outlier handled

boxplot(anime$wantWatch) 
quantile(anime$wantWatch, seq(0,1,0.02))
anime$wantWatch <- ifelse(anime$wantWatch>3000,3000, anime$wantWatch)
boxplot(anime$wantWatch)  # Outlier handled

boxplot(anime$watching)
quantile(anime$watching, seq(0,1,0.01))
anime$watching <- ifelse(anime$watching>300,300, anime$watching)
boxplot(anime$watching)     # Outlier handled

boxplot(anime$dropped) 
quantile(anime$dropped, seq(0,1,0.02))
anime$dropped <- ifelse(anime$dropped>130,130, anime$dropped)
boxplot(anime$dropped)        # Outlier handled

boxplot(anime$rating) # no outlier found

# Outlier Treatment done 

#######################################################################
#5) Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis #####################

# Multiple Continuous Variables
ColsForHist <- c("eps", "watched", "watching","wantWatch", "dropped",	"rating",	"votes")
par(mfrow=c(2,2)) 

library(RColorBrewer)
# library for colors

# Using loops to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(anime[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))}

# Multiple Categorical Variables
ColsForBar <- c("mediaType")

par(mfrow=c(2,2)) 

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(anime[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Spectral"))}

#################### Bivariate Analysis ########################

# Relationship between target variable and predictors variables
# Categorical vs Continuous --- Box Plot
# Continuous Vs Continuous --- Scatter plot

# Categorical vs Continuous analysis-- Boxplot

par(mfrow=c(2,2))

ColsForBar =c("mediaType")

for(box_cols in ColsForBar){
  boxplot(rating~anime[  ,c(box_cols)], data=anime  , main=paste('Boxplot of :',box_cols),col=brewer.pal(8,"Accent"))
}

# Continuous Vs Continuous --- Scatter plot
ContinuousCols=c("rating","eps","watched","watching","wantWatch","dropped","rating",	"votes")

par(mfrow=c(1,1))

plot(anime[,ContinuousCols],col='blue')

### Statistical Tests ###
# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

############### Correlation test ################

# Continuous Vs Continuous ---- Correlation test
# correlation for checking continuous variable 
# whether it is a good or bad predictor

ContinuousCols=c("rating","eps","watched","watching","wantWatch","dropped","votes")

library(corrplot)
corrplot(cor(anime[,ContinuousCols]))

CorrData=cor(anime[,ContinuousCols ], use = "complete.obs")
CorrData

abs(CorrData['rating',])>0.3

names(CorrData['rating',])
names(CorrData['rating',][abs(CorrData['rating',])>0.3])

#here we have taken the thresold value >0.3
# So "eps" is a bad predictor for model.

###################### ANOVA TEST ############################

# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

summary(aov(rating ~ anime$mediaType, data = anime))

# mediaType is significant for the model 
# Here the p-value is less, so we reject the null hypothesis.

# 7) Splitting the data into train & test 

install.packages("caTools") 
library(caTools)

set.seed(150)
split <- sample.split(anime$rating, SplitRatio = 0.70)
split

table(split)

training <- subset(anime, split==TRUE)
nrow(training)
testing <- subset(anime, split==FALSE)
nrow(testing)

# 8) Model Building: 
# Building the Linear Regression model with training dataset
# linear regression model function : lm(dv~., data=training)

lin_regession <- lm(rating~., data=training)
lin_regession
summary(lin_regession)

###############################
# Multiple R-squared:  0.5642	
# Adjusted R-squared:  0.563  
###############################

lin_regession1 <- lm(rating~votes+dropped+wantWatch+watching+watched
                     +I(mediaType=="Web")+I(mediaType=="OVA")+I(mediaType=="Other")+I(mediaType=="Music Video")+I(mediaType=="Movie"),
                     data = training)
lin_regession1
summary(lin_regession1)

###############################
# Multiple R-squared:  0.5536	
# Adjusted R-squared:  0.5527  
###############################

#9) Predictions : predict model by using test dataset.

linear_pred <- predict(lin_regession1, newdata = testing)
linear_pred

# Combined actual Life Expectancy and the predicted Life Expectancy

linear_pred_cbind <- cbind(testing$rating, linear_pred)
linear_pred_cbind

#10) Assumption Tests of Linear Regression model --

install.packages("lmtest")
library(lmtest)

install.packages("faraway")
library(faraway)

install.packages("car")
library(car)

#### Autocorrelation ####
#we have to do durbin Watson test, if value falls between 0 and 4,
# there is no autocorrelation, any value which is less then 0 or more than 4,
# will consider autocorrelation

dwtest(lin_regession1)
#DW = 0.89151
#p-value < 2.2e-16
#hence, no Autocorrelation found

#### Multicollinearity check: means idv does influence another idv variable ####
# approach - Variance Inflation Factor (vif) = 1/(1-R^2)
# If vif value is equal to or less then 5, then we can consider there is no multicollinearity, 
# if vif value is more than 5, will consider multicollinearity.

vif(lin_regession1)

lin_regession2 <- lm(rating~+votes+dropped+wantWatch+watching
                     +I(mediaType=="Web")+I(mediaType=="OVA")+I(mediaType=="Other")+I(mediaType=="Music Video")+I(mediaType=="Movie"),
                     data = training)
summary(lin_regession2)

vif(lin_regession2)

lin_regession3 <- lm(rating~+votes+dropped+watching
                     +I(mediaType=="Web")+I(mediaType=="OVA")+I(mediaType=="Other")+I(mediaType=="Music Video")+I(mediaType=="Movie"),
                     data = training)
summary(lin_regession3)

vif(lin_regession3)

###############################
# Multiple R-squared:  0.5533	
# Adjusted R-squared:  0.5524   
###############################

# 11) MAPE and MDAPE:

# Mean absolute percentage error (MAPE)

library(MLmetrics)

MeanAPE <- mean(abs((testing$rating-linear_pred)/testing$rating)) * 100
MeanAPE

# Median absolute percentage error (MDAPE)

MedianAPE <- median(abs((testing$rating-linear_pred)/testing$rating)) * 100
MedianAPE

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

#######################################################
# Multiple R-squared:  0.5533	
# Adjusted R-squared:  0.5524 
# Mean Absolute Percentage Error (MAPE) : 18.97585 
# Median Absolute Percentage Error (MDAPE) : 13.58057
# Mean Accuracy of Linear Regression Model :  81.02
# Median Accuracy of Linear Regression Model :  86.41
#######################################################

###################### Business Recommendation #########################
#Production studios needs to promote their shows in TV Special format because 
#this format can provide higher rating of viewers.
#The studios should broadcast through OVA, as the mediaType can increase the 
#viewer's rating as compared to DVD Special. 
#The studios should release more on Web platform as nowadays present generation 
#like to watch on internet. This will elevate the rating of the anime.
##################################################################################

#######################################################################################
############################## LINEAR REGRESSION MODEL ################################
#######################################################################################


