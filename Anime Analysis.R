#-------------------Anime Rating Prediction---------------

# STEP-1
#Identifying the Problem Statement
#Problem Statement: To predict the Ratings received by the anime releases

#STEP-2
#Identifying the Target Variable
#Target Variable: rating

#STEP-3
#Importing Raw Data
Anime_Data=read.csv("C:/Users/SHRISTI SAHOO/Desktop/analytics/R Prog/Internship/Anime_Final.csv",
                            na.strings = c(""," ","NA","NULL","[]"),stringsAsFactors = T)

View(Anime_Data)
#Target Variable is 'rating' can be viewed
#rating is a continuous column

#STEP-4
#Exploring the Dataset

dim(Anime_Data)
#In this dataset there are all total
#16-Columns and 7029-Rows

##title: 
#Qualitative Column
#Name of the anime releases.
#Garbage Column 
#This column can be rejected

##mediaType: 
#Categorical Column
#Format of publication of the anime releases 
#Web,DVD special,Movie,TV special,TV,Music Video,Other,OVA

#Automating to Check for the Unique values in the columns
FunctionUniqueData=function(col_input){
  return(length(unique(col_input)))
}

FunctionUniqueData(Anime_Data$mediaType)

##eps: 
#Continuous Column
#Number of episodes (movies are considered 1 episode).

##duration: 
#Continuous Column
#Duration of each episode (in minutes).

##Ongoing: 
#Categorical Column
#Whether the anime is ongoing or not (Yes/No).

FunctionUniqueData(Anime_Data$ongoing)

##sznOfRelease: 
#Categorical Column
#The season of release of the anime (Winter/Spring/Fall/Summer).

FunctionUniqueData(Anime_Data$sznOfRelease)

##description: 
#Qualitative Column
#Synopsis of plot of the anime.
#Garbage Column 
#This column can be rejected

##studios: 
#Qualitative Column
#Studios responsible for the creation of different anime.

#helps in finding the total unique values present in 'studios' column

length(unique(Anime_Data$studios))

#532 unique values
#Number of unique values is large, this will inflate the dataset abruptly
#when so many dummies are created and won't help in predicting the rating
#Garbage Column 
#This column can be rejected

##tags: 
#Qualitative Column
#Tags, genres, etc. of different anime.

#helps in finding the total unique values present in 'tags' column

length(unique(Anime_Data$tags))
#4869 unique values which is very large will cause inflation in dataset
#Garbage Column 
#This column can be rejected

##contentWarn: 
#Qualitative Column
#Content warning provided for the different anime.
#helps in finding the total unique values present in 'contentWarn' column

length(unique(Anime_Data$contentWarn))
#131 unique values which is very large will cause inflation in dataset
#Garbage Column 
#This column can be rejected

##watched: 
#Continuous Column
#The number of users who completed watching it.

#watching: 
#Continuous Column
#The number of users who are watching it.

##wantWatch: 
#Continuous Column
#The number of users who want to watch it.

##dropped: 
#Continuous Column
#The number of users who dropped it before completion.

##rating: 
#Target Variable - Continuous Column
#Type of Problem : Linear Regression
#Provides Average user rating given by the viewers for the anime releases.

##votes: 
#Continuous Column
#The number of votes that contribute to the ratings received by different anime.

#Removing the Garbage Columns
garbage_cols=c("title","description","studios","tags","contentWarn")

for(gar_cols in garbage_cols){
  Anime_Data[,gar_cols]=NULL
}

#STEP-5
#Checking if all the categorical column are factor or not if not it needs to be converted 
#into factor

str(Anime_Data)

#Categorical Columns: mediaType, ongoing, sznOfRelease
#All categorical columns are in factor format

#STEP-6
#Identifying the Problem Type

#Target Variable: rating
#Type of Problem : Linear Regression

#STEP-7
#Checking and Treating Missing Values

colSums(is.na(Anime_Data))

#Number of missing values in the column 'sznOfRelease' is 4916 out 
#of a total of 7029 rows so this column can be rejected

Anime_Data$sznOfRelease=NULL

#For Categorical column mode is to be used to treat missing values

FunctionMode=function(InpData){
  ModeValue=names(table(InpData)[table(InpData)==max(table(InpData))])
  return(ModeValue)
}

Anime_Data$mediaType[is.na(Anime_Data$mediaType)]=FunctionMode(Anime_Data$mediaType)

#For continuous column median is to be used to treat missing values

Anime_Data$duration[is.na(Anime_Data$duration)]=median(Anime_Data$duration,na.rm=T)
Anime_Data$watched[is.na(Anime_Data$watched)]=median(Anime_Data$watched,na.rm=T)

colSums(is.na(Anime_Data))

#STEP-8
#Winsorization
#Checking the presence of outliers by creating boxplots and treating the outliers

#Stating all the continuous columns in a vector
cont_cols=c("eps","duration","watched","watching","wantWatch","dropped","rating","votes")

library(RColorBrewer)

#Splitting the plot window into 8 parts
par(mfrow=c(2,4))

for(outlier_cols in cont_cols){
  boxplot(Anime_Data[,c(outlier_cols)],data = Anime_Data,main=paste("BoxPlot of :", outlier_cols),
          col=brewer.pal(8,"Paired"),horizontal = T)
}

#eps,duration,watched,watching,wantWatch,dropped,votes has the presence of outliers
#rating doesn't have any outliers

#Column eps
quant_eps=quantile(Anime_Data$eps,c(0.998,0.9985,0.9987,0.9988,0.999))
quant_eps

#For eps column considering 251.496 approx as the last value in data
outlier_eps=round(quantile(Anime_Data$eps,0.9988))
outlier_eps

Anime_Data$eps=ifelse(Anime_Data$eps>outlier_eps,outlier_eps,Anime_Data$eps)

#Column duration
quant_duration=quantile(Anime_Data$duration,c(0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999))
quant_duration

#For duration column considering 120 approx as the last value in data
outlier_duration=round(quantile(Anime_Data$duration,0.995))
outlier_duration

Anime_Data$duration=ifelse(Anime_Data$duration>outlier_duration,outlier_duration,Anime_Data$duration)

#Column watched
quant_watched=quantile(Anime_Data$watched,c(0.99,0.995,0.996,0.997,0.998,0.999))
quant_watched

#For watched column considering 54346.4 approx as the last value in data
outlier_watched=round(quantile(Anime_Data$watched,0.995))
outlier_watched

Anime_Data$watched=ifelse(Anime_Data$watched>outlier_watched,outlier_watched,Anime_Data$watched)

#Column watching
quant_watching=quantile(Anime_Data$watching,c(0.99,0.995,0.996,0.997,0.998,0.999))
quant_watching

#For watching column considering 14341.79 approx as the last value in data
outlier_watching=round(quantile(Anime_Data$watching,0.999))
outlier_watching

Anime_Data$watching=ifelse(Anime_Data$watching>outlier_watching,outlier_watching,Anime_Data$watching)

#Column wantWatch
quant_wantWatch=quantile(Anime_Data$wantWatch,c(0.99,0.992,0.993,0.994,0.995,0.996,0.997,0.998,0.999))
quant_wantWatch

#For watching column considering 16676.58 approx as the last value in data
outlier_wantWatch=round(quantile(Anime_Data$wantWatch,0.995))
outlier_wantWatch

Anime_Data$wantWatch=ifelse(Anime_Data$wantWatch>outlier_wantWatch,outlier_wantWatch,Anime_Data$wantWatch)

#Column dropped
quant_dropped=quantile(Anime_Data$dropped,c(0.99,0.992,0.993,0.994,0.995,0.996,0.997,0.998,0.999))
quant_dropped

#For dropped column considering 3049.096 approx as the last value in data
outlier_dropped=round(quantile(Anime_Data$dropped,0.996))
outlier_dropped

Anime_Data$dropped=ifelse(Anime_Data$dropped>outlier_dropped,outlier_dropped,Anime_Data$dropped)

#Column votes
quant_votes=quantile(Anime_Data$votes,c(0.994,0.995,0.996,0.997,0.998,0.999))
quant_votes

#For votes column considering 48484.18 approx as the last value in data
outlier_votes=round(quantile(Anime_Data$votes,0.997))
outlier_votes

Anime_Data$votes=ifelse(Anime_Data$votes>outlier_votes,outlier_votes,Anime_Data$votes)

#Checking If the outliers are treated or not

#Splitting the plot window into 8 parts
par(mfrow=c(2,4))

for(outlier_cols in c("eps","duration","watched","watching","wantWatch","dropped","votes")){
  boxplot(Anime_Data[,c(outlier_cols)],data = Anime_Data,main=paste("BoxPlot of :", outlier_cols),
          col=brewer.pal(8,"Paired"),horizontal = T)
}

#STEP-9
# Explore each "Potential" predictor for distribution and Quality

colnames(Anime_Data)

##For Continuous Column

#Central Tendencies
sapply(Anime_Data[,cont_cols],mean)
sapply(Anime_Data[,cont_cols],median)

#Measures of location
sapply(Anime_Data[,cont_cols],quantile)

#Measure of Dispersion
sapply(Anime_Data[,cont_cols],sd)
sapply(Anime_Data[,cont_cols],var)
sapply(Anime_Data[,cont_cols],range)

#Plotting
#For Continuous Columns: Histogram 

#Splitting the plot window into 8 parts
par(mfrow=c(2,4))

for(hist_cols in cont_cols){
  hist(Anime_Data[,c(hist_cols)],main = paste("Histogram of :", hist_cols),
       col=brewer.pal(8,"Paired"))
}

##For Categorical Columns

#Stating all the categorical columns in a vector
cate_cols=c("mediaType","ongoing")

#Central Tendencies

#Using the FunctionMode, function which was created to find the mode of a variable

sapply(Anime_Data[,cate_cols],FunctionMode)

#Plotting
#For Categorical Columns: Barplot

#Splitting the plot window into 2 parts
par(mfrow=c(1,2))

for(bar_cols in cate_cols){
  barplot(table(Anime_Data[,c(bar_cols)]),main = paste("BarPlot of :", bar_cols),
          col=brewer.pal(8,"Paired"))
}

#STEP-10
#Bivariate Analysis
#Visual Relationship between Predictors and Target Variable

#Continuous(Target Variable) vs Continuous(Predictors) -> Scatter Plot

par(mfrow=c(1,1))

plot(Anime_Data[,cont_cols],col='blue')

#Continuous(Target variable) vs Categorical(Predictors) -> Box Plot

par(mfrow=c(1,2))

for(box_cols in cate_cols){
  boxplot(rating ~ Anime_Data[,c(box_cols)],data = Anime_Data,main=paste("BoxPlot of :", box_cols),
          col=brewer.pal(8,"Paired"))
}

# STEP-11
# Strength of Relationship between predictor and target variable

# Continuous Vs Continuous -> Correlation test

CorrCheck=cor(Anime_Data[,cont_cols],use = "complete.obs")
CorrCheck

CorrCheck[,"rating"]

#Correlation Coefficient of columns with respect to 'rating'
#eps=0.1324552 ; duration=0.2838795 ; watched=0.4515253 ; watching=0.3469004
#wantWatch=0.5545666; dropped=0.3417926 ; votes=0.4369128

#Considering the threshold value of 0.5 yields only 'wantWatch' column having correation with
#rating as the target variable 
#Taking the threshold value as 0.4 yields columns "watched","wantWatch","votes"
#having correlation with the target variable
#Columns "watched", "votes" have values in between 0.4 - 0.5 and can be given a chance 
#for the analysis in model

names(CorrCheck[,'rating'][abs(CorrCheck[,'rating'])>0.4])

#wantWatch, watched,votes are correlated with the target variable - 'rating'
#and are considered good for the model

# Continuous Vs Categorical -> ANOVA test

# H0(Null Hypothesis): Variables are NOT correlated
# Small P-Value(p-value<5%)-> Variables are correlated(H0 is rejected)
# Large P-Value(p-value>5%)-> Variables are NOT correlated (H0 is accepted)

for (aov_cols in cate_cols){
  aov_summary=summary(aov(rating ~ Anime_Data[,c(aov_cols)], data = Anime_Data))
  print(paste("The Anova test with",aov_cols))
  print(aov_summary)
}

#If Probability values > 5% or 0.05 then H0 Accepted thus variable not correlated with target variable

#If Probability value < 5% or 0.05 then H0 Rejected, variable are correlated with Target Variable

#Both mediaType and ongoing are rejected by Null Hypothesis
#Hence are correlated with the target variable  and considered for the model.

#STEP-12
#Generating Data for ML
#Getting Data into Standardization Form

InpData=Anime_Data
TargetVariableName="rating"

#These were the best predictor variable before performing the data transformation
#BestPredictorName=c("mediaType","ongoing","watched", "wantWatch", "votes")

#Varibale considered for the model after performing the data transformation are: 
#Variable watched_new, wantWatch_new were being transformed from the 
#variable watched and wantWatch respectively
BestPredictorName=c("mediaType","ongoing","watched_new", "wantWatch_new", "votes")


#Extracting target variable & Predictor variable from data respectively to create a generic DataSet

TargetVariable=InpData[,c(TargetVariableName)]
str(TargetVariable)

PredictorVariable=InpData[,BestPredictorName]
str(PredictorVariable)

#Creating Data for Machine Learning
Data_ML=data.frame(TargetVariable,PredictorVariable)
head(Data_ML)

#STEP-13
#Performing Sampling

# Sampling | Splitting data into 70% for training 30% for testing

set.seed(123)
TrainingSample=sample(1:nrow(Data_ML), size=0.7 * nrow(Data_ML))
length(TrainSample)

Data_MLTrain=Data_ML[TrainingSample, ]
Data_MLTest=Data_ML[-TrainingSample, ]

dim(Data_MLTrain)
dim(Data_MLTest)

head(Data_MLTrain)
head(Data_MLTest)

#Creating Predictive models on training data to check the accuracy on test data

# Linear Regression

#After performing Data Transformations reconsidering the model with tranformed columns
Model_Reg=lm(TargetVariable~.,data=Data_MLTrain)
summary(Model_Reg)

#Identifying and Remove the insignificant predictors one by one to arrive at the final model
#Variable ares eliminated in such a way, where the probability value is highest and less than 5%
#this process of elimination is done on variable one by one

#Column votes has the highest p-value so eliminating this variable from the model
Model_Reg_2=lm(TargetVariable~ongoing+watched_new+wantWatch_new+mediaType,data=Data_MLTrain)
summary(Model_Reg_2)

#Corresponding to the column mediaType of Category Other has highest p-value
#so eliminating this variable
Model_Reg_3=lm(TargetVariable~ongoing+watched_new+wantWatch_new+
                 I(mediaType=="Movie")+I(mediaType=="Music Video")+
                 I(mediaType=="TV")+I(mediaType=="OVA")+I(mediaType=="TV Special")+
                 I(mediaType=="Web"),data=Data_MLTrain)
summary(Model_Reg_3)

#Corresponding to the column mediaType of Category Web has highest p-value
#so eliminating this variable
Model_Reg_4=lm(TargetVariable~ongoing+watched_new+wantWatch_new+
                 I(mediaType=="Movie")+I(mediaType=="Music Video")+
                 I(mediaType=="TV")+I(mediaType=="OVA")+I(mediaType=="TV Special")
                 ,data=Data_MLTrain)
summary(Model_Reg_4)

#Corresponding to the column mediaType of Category TV has highest p-value
#so eliminating this variable
Model_Reg_5=lm(TargetVariable~ongoing+watched_new+wantWatch_new+
                 I(mediaType=="Movie")+I(mediaType=="Music Video")+
                 I(mediaType=="OVA")+I(mediaType=="TV Special"),data=Data_MLTrain)
summary(Model_Reg_5)

#Checking the presence of Multicollinearity
#And if exists corresponding removal of variables with high multicollinearity one by one

#Breaking down categorical variable to checking Multicollinearity
Model_Reg_5=lm(TargetVariable~watched_new+wantWatch_new+I(ongoing=="Yes")+
                 I(mediaType=="Movie")+I(mediaType=="Music Video")+
                 I(mediaType=="OVA")+I(mediaType=="TV Special"),data=Data_MLTrain)
summary(Model_Reg_5)


#library used for vif-variation inflation factor
library("car")

#If VIF = 1, then variable are not associated
# 1 < VIF < 5, then variables are moderately associated
#   VIF > 5, then highly associated
#So variable having VIF value greater than 5 are to be eliminated from the model

VIF=vif(Model_Reg_5)
data.frame(VIF)

#Variables votes has highest multicollinearity with the 
#target variable 'rating' hence eliminating form the model

Model_Reg_3=lm(TargetVariable~watched+wantWatch+
                 I(ongoing=="Yes")+I(mediaType=="Movie")+I(mediaType=="Music Video")+
                 I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV Special")+
                 I(mediaType=="Web"),data=Data_MLTrain)
summary(Model_Reg_3)

VIF=vif(Model_Reg_3)
data.frame(VIF)

###Considering Model_Reg_3 
#R-squared: 0.4251 ; Adjusted R-squared: 0.424 which is not a good value
#in terms of goodness of fit of the model
#As the model to be considered as good the range is [0.5, 0.95] and 1 as the ideal value
#in-order to improve the fit of the model various data transformations can be done 
#on the predictor variables

#In this data logarithmic transformation is done by reducing the skewness and 
#increasing the correaltion of the predictor variable with the target variable

#Considering the predictor variable - wantWatch

#A histogram plot is created to check the skewness of wantWatch column
hist(InpData$wantWatch)

#From the plot it can be seen that it is negatively skewed 

cor(x=InpData$wantWatch , y=InpData$rating)
#The correlation between wantWatch and rating is 0.5545651

min(InpData$wantWatch)
#It can be observed from the plot that the minimum value being 0 from the variable is huge
#as compared to other values

#Treating the Zeros in the Columns
#wantWatch: The number of users who want to watch it
#Considering users who want to watch as 0, as  minimum value to want to watch as 1 

InpData$wantWatch[InpData$wantWatch==0]=1

# Log Transformation
hist(log(InpData$wantWatch))
#From this plot it can be observed that the distribution is Symmetric in nature

cor(x=log(InpData$wantWatch) , y=InpData$rating)
#Now it can be observed that the correlation has 
#increased from 0.5545651 to 0.7643465

#so create a new column wantWatch_new inside InpData and use it for the model
#instead of wantWatch

Anime_Data$wantWatch_new=log(InpData$wantWatch)

#Considering the predictor variable - watched

#A histogram plot is created to check the skewness of watched column
hist(InpData$watched)

#From the plot it can be seen that it is negatively skewed 

cor(x=InpData$watched , y=InpData$rating)
#The correlation between watched and rating is 0.4515258

min(InpData$watched)
#It can be observed from the plot that the minimum value being 0 from the variable is huge
#as compared to other values

#Treating the Zeros in the Columns
#watched: The number of users who completed watching it
#Considering users watched as 0, as  minimum value to watched as 1 

InpData$watched[InpData$watched==0]=1

# Log Transformation
hist(log(InpData$watched))
#From this plot it can be observed that the distribution is Symmetric in nature

cor(x=log(InpData$watched) , y=InpData$rating)
#Now it can be observed that the correlation has 
#increased from 0.4515258 to 0.7319279

#so create a new column watched_new inside InpData and use it for the model
#instead of watched

Anime_Data$watched_new=log(InpData$watched)

### Various Tests on the model for validation

##Homoskedasticity or Heteroskedasticity

#H0: There exists Homoskedasticity, error in variances are equal
#p-value<5% -> Reject H0
#p-value>5% -> Accept H0

library("lmtest")

bptest(Model_Reg_2)

#As p-value < 2.2e-16 which is very low so H0 is rejected and 
#There exists Heteroskedasticity

##Serial Correlation Test

#H0: There exists no Auto Correlation
#p-value<5% -> Reject H0
#p-value>5% -> Accept H0

#library("lmtest") is used 

dwtest(Model_Reg_2)

#As p-value = 0.9858, which is greater than 5% so H0 is accepted 
#And concluded the absence of auto correlation

##Normality Test

#H0: Error terms are normally distributed
#p-value<5% -> Reject H0
#p-value>5% -> Accept H0

library("nortest")

#Extracting Residual part of the model for testing
resid_Model_Reg_2=Model_Reg_2$residuals

ad.test(resid_Model_Reg_2)

#As p-value < 2.2e-16, which is very low so H0 is rejected 
#And concluded that errors are not normally distributed

### Checking Accuracy of model on Testing data
Data_MLTest$Pred_LM=predict(Model_Reg_2,Data_MLTest)
head(Data_MLTest)

## Calculating the Absolute Percentage Error for each prediction
Data_MLTest$LM_APE=100*abs((Data_MLTest$TargetVariable-Data_MLTest$Pred_LM)/
                             Data_MLTest$TargetVariable)
head(Data_MLTest)

MeanAPE=mean(Data_MLTest$LM_APE)
MedianAPE=median(Data_MLTest$LM_APE)

print(paste("Mean Error Percentage is: ",MeanAPE))
print(paste("Median Error Percentage is: ",MedianAPE))

#Mean Error Percentage is:  23.81%
#Median Error Percentage is:  15.52%

print(paste('Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

#Mean Accuracy of Linear Regression Model is:  76.18%
#Median Accuracy of Linear Regression Model is:  84.47%




