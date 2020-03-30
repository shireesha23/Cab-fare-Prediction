rm(list = ls())

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(data.tree)
library(caTools)
library(ElemStatLearn)


#Loading the train and test data

Orginal_train = read.csv("E:/Sproject/train_cab.csv", header = T)

h = Orginal_train

Orginal_test = read.csv("E:/Sproject/test.csv",header = T)

k = Orginal_test

test_pickup_datetime = k["pickup_datetime"]

str(h)

#fare_amount is factor datatype value
#pickup_datetime is factor datatype value
#dropoff_longitude is numeric datatype
#dropoff_lattitude is numeric datatype
#passenger_count is numeric datatype

str(k)

summary(h)

#From summary it is clear that there are 55 NA's in passenger_count

summary(k)

#converting fare_amount of train_data from factor_datatype to numeric

h$fare_amount = as.numeric(as.character(h$fare_amount))

h$passenger_count=round(h$passenger_count)



####################################### Exploratory Data-Analysis ###########################################
# 1.Fare amount has a some negative values. it is also having 0 values . So we need to remove these fields.

h[which(h$fare_amount < 1 ),]
nrow(h[which(h$fare_amount < 1 ),])
h = h[-which(h$fare_amount < 1 ),]

#as we observed in the data , the passenger count in a cab should be below 6 , but there are values more than 6

#in the passenger_count attribute

#2.Passenger_count variable

for (i in seq(4,11,by=1)){
  print(paste('passenger_count above ' ,i,nrow(train[which(h$passenger_count > i ),])))
}

#the above for loop helps to identify the number of observations containing the values more than 6 , which are nothing
#but outliners

#passenger_count above  4 1367
#passenger_count above  5 322
#passenger_count above  6 20
#passenger_count above  7 20
#passenger_count above  8 20
#passenger_count above  9 20
#passenger_count above  10 20
#passenger_count above  11 20

# Checking if there are any passenger_count==0

nrow(h[which(h$passenger_count <1 ),])

#there are 58 observations in the passenger_count attribute whose value is 0
#Now We are removing the observations   which are above 6 value .

h = h[-which(h$passenger_count < 1 ),]
h = h[-which(h$passenger_count > 6),]

# 3.Latitudes range from -90 to 90.Longitudes range from -180 to 180.Removing which does not satisfy these ranges

print(paste('pickup_longitude above 180=',nrow(h[which(h$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(h[which(h$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(h[which(h$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(h[which(h$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(h[which(h$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(h[which(h$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(h[which(h$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(h[which(h$dropoff_latitude > 90 ),])))

# There's only one outlier which is in variable pickup_latitude.So we will remove it with nan.
# Also we will see if there are any values equal to 0.

nrow(h[which(h$pickup_longitude == 0 ),])
nrow(h[which(h$pickup_latitude == 0 ),])
nrow(h[which(h$dropoff_longitude == 0 ),])
nrow(h[which(h$dropoff_latitude == 0 ),])

#there are 311 observations in pickup_longitude,pickup_latitude,dropoff_latitude
#there are 312 observation in dropoff_longitude
# there are values which are equal to 0. we will remove them.

h = h[-which(h$pickup_latitude > 90),]
h = h[-which(h$pickup_longitude == 0),]
h = h[-which(h$dropoff_longitude == 0),]
 
str(h)

p = h


####################### Missing Value Analysis #########################################################
missing_val = data.frame(apply(p,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(p)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

# as passenger_count has missing values greater than 30%,then we need to fillup the missing values 
#using mean or median or knn imputation techniques

unique(p$passenger_count)
#there are NA's in passenger_count 

unique(k$passenger_count)

p[,'passenger_count'] = factor(p[,'passenger_count'], labels=(1:6))
k[,'passenger_count'] = factor(k[,'passenger_count'], labels=(1:6))

#here we are deciding which technique should be used for missing values , for that we are 
#comparing actual value with predicted value

# 1.For Passenger_count:
p$passenger_count[112]
#the actual value of 112th observation in passenger_count is 1
# Actual value = 1

p$passenger_count[112] = NA
#knn value is 1
# 2.For fare_amount:

p$fare_amount[112]
#the actual value of 112th observation in fare_amount is 17
# Actual value = 17

p$fare_amount[112] = NA 
  # Mean Method

mean(p$fare_amount, na.rm = T)
# mean value is 15.11
#Median Method

median(p$fare_amount, na.rm = T)
#median value is 8.5

# kNN Imputation
p = knnImputation(p, k = 181)
p$fare_amount[112] #knn value is 9.33
p$passenger_count[112]


sum(is.na(p))

str(p)
summary(p)

df = p


############################# Outlier Analysis ##################

# We Will do Outlier Analysis only on Fare_amount just for now and we will do outlier analysis after feature engineering laitudes and longitudes.
# Boxplot for fare_amount
pl1 = ggplot(p,aes(x = factor(passenger_count),y = fare_amount))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA and impute
vals = p[,"fare_amount"] %in% boxplot.stats(p[,"fare_amount"])$out
p[which(vals),"fare_amount"] = NA

#lets check the NA's
sum(is.na(p$fare_amount))
#there are 1358 NA's in fare_amount variable

#Imputing with KNN
p = knnImputation(p,k=3)

# lets check the missing values
sum(is.na(p$fare_amount))

str(p)

df2 = p
################### Extracting pickup_datetime variable ##########################
#here we are stripping pickup_datetime variable
# we will derive new features from pickup_datetime variable
# new features will be year,month,day_of_week,hour

#Convert pickup_datetime from factor to date_time format

p$pickup_date = as.Date(as.character(p$pickup_datetime))
p$pickup_weekday = as.factor(format(p$pickup_date,"%u")) 
p$pickup_mnth = as.factor(format(p$pickup_date,"%m"))
p$pickup_yr = as.factor(format(p$pickup_date,"%Y"))
pickup_time = strptime(p$pickup_datetime,"%Y-%m-%d %H:%M:%S")
p$pickup_hour = as.factor(format(pickup_time,"%H"))

#As we created year,month,day_of_week,hour in train data_set , we also need to create same features in test_data

k$pickup_date = as.Date(as.character(k$pickup_datetime))
k$pickup_weekday = as.factor(format(k$pickup_date,"%u"))# Monday = 1
k$pickup_mnth = as.factor(format(k$pickup_date,"%m"))
k$pickup_yr = as.factor(format(k$pickup_date,"%Y"))
pickup_time = strptime(k$pickup_datetime,"%Y-%m-%d %H:%M:%S")
k$pickup_hour = as.factor(format(pickup_time,"%H"))

sum(is.na(p)) 
# there was 1 'na' in pickup_datetime

# we will remove that 1 row of na's
p = na.omit(p) 

p = subset(p,select = -c(pickup_datetime,pickup_date))
k = subset(k,select = -c(pickup_datetime,pickup_date))

# 2.Calculate the distance travelled using longitude and latitude
deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 #1000 is used to convert to meters
}

# Using haversine formula to calculate distance fr both train and test
p$dist = haversine(p$pickup_longitude,p$pickup_latitude,p$dropoff_longitude,p$dropoff_latitude)
k$dist = haversine(k$pickup_longitude,k$pickup_latitude,k$dropoff_longitude,k$dropoff_latitude)

# We will remove the variables which were used to extract new variables
p = subset(p,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
k = subset(k,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

str(p)

summary(p)
################## Feature selection ###################

numeric_index = sapply(p,is.numeric)  

numeric_data = p[,numeric_index]

cnames = colnames(numeric_data)

#Correlation analysis for numeric variables

corrgram(p[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

#ANOVA for categorical variables with target numeric variable

#passenger_count,pickup_hour,pickup_weekday,pickup_yr,pickup_mnth,passenger_count are categorical ariables
# so we can perform  anova / chisquare test on them

anova_results = aov(fare_amount ~ passenger_count + pickup_hour + pickup_weekday + pickup_mnth + pickup_yr,data = p)

summary(anova_results)
# pickup_weekday has p value greater than 0.05  ie 0.790
 # we are selecting pickup_weekday variable
p = subset(p,select=-pickup_weekday)
#remove from test set
k = subset(k,select=-pickup_weekday)

################################## Feature Scaling ################################################
#Normality check
qqnorm(p$fare_amount)
histogram(p$fare_amount)

library(car)
# dev.off()

par(mfrow=c(1,2))
qqPlot(p$fare_amount)                              
truehist(p$fare_amount)                            
lines(density(p$fare_amount))  # Right skewed      

#Normalisation

print('dist')
p[,'dist'] = (p[,'dist'] - min(p[,'dist']))/
  (max(p[,'dist'] - min(p[,'dist'])))

# #check multicollearity

library(usdm)
vif(p[,-1])
 
#################### Splitting train into train and validation subsets ###################
set.seed(1000)
train_index = createDataPartition(p$fare_amount,p=0.75,list = FALSE) # 75% in trainin and 25% in Validation Datasets
train_data = p[train_index,]
test_data = p[-train_index,]



############# Linear regression  #################
lm_model = lm(fare_amount ~.,data=train_data)

summary(lm_model) 

str(train_data)

plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")


lm_predictions = predict(lm_model,test_data[,2:6])

qplot(x = test_data[,1], y = lm_predictions, data = test_data, color = I("red"), geom = "point")

#Error metrice , we are using RMSe in the place of MAPE , as we are dealing with timeseries

regr.eval(test_data[,1],lm_predictions)

# mae        mse       rmse       mape 
# 3.4996927 19.0887210 4.3690641  0.4502295 
 
#Using orginal test_data for prediction

lm_prdictions1 = predict(lm_model,k)

Lm_result = data.frame( lm_prdictions1)

write.csv( Lm_result, "E:/Multiple_Linear_Regression_fare_amount_prediction.csv", row.names = F)

#################  Decision Tree ##########################

Dt_model = rpart(fare_amount ~ ., data = train_data, method = "anova")

summary(Dt_model)

#Predict for new test cases
predictions_DT = predict(Dt_model, test_data[,2:6])

qplot(x = test_data[,1], y = predictions_DT, data = test_data, color = I("green"), geom = "point")

regr.eval(test_data[,1],predictions_DT)
#   mae         mse         rmse     mape 
# 1.9192543  6.6565701  2.5800330 0.2275129  

#Using orginal test_data for prediction

Dt_prdictions1 = predict(Dt_model, k)

DT_result = data.frame( Dt_prdictions1)

write.csv( DT_result, "E:/Decision_Tree_fare_amount_prediction.csv", row.names = F)


#############Random forest#####################

rf_model = randomForest(fare_amount ~.,data=train_data)

summary(rf_model)

rf_predictions = predict(rf_model,test_data[,2:6])

qplot(x = test_data[,1], y = rf_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],rf_predictions)
# mae         mse      rmse      mape 
#1.8898471  6.3913687 2.5281156  0.2326638 

#Using orginal test_data for prediction

Rf_prdictions1 = predict(rf_model, k)

Rf_result = data.frame( Rf_prdictions1)

write.csv( Rf_result, "E:/Random_forest_fare_amount_prediction.csv", row.names = F)


















