#Code for Melbourne House Price prediction

#clear environment
rm(list = ls())

#Load library and install package if needed
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("caret")) install.packages("caret")
library(caret)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)
if (!require("kknn")) install.packages("kknn")
library(kknn)
if (!require("knitr")) install.packages("knitr")
library(knitr)
if (!require("kableExtra")) install.packages("kableExtra")
library(kableExtra)
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
if (!require("DAAG")) install.packages("DAAG")
library(DAAG)
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

# Import data from file on computer
url <- "https://raw.githubusercontent.com/camklost/Melbourne-House-Price-Prediction/main/house_price_data.csv"
dat <- read.csv(url)
# Set seed
set.seed(1,sample.kind="Rounding")

# --------------------Data Preparation and Manipulation--------------------------

#2. Data Preparation
#2.1. Data Description
# Explore data
# Check size of data set
dim(dat)
# Data description table
Variables <- c("Suburb","Address","Rooms","Type","Price","Method","SellerG",
               "Date","Distance","Postcode","Bedroom2","Bathroom","Car",
               "Landsize","BuildingArea","YearBuilt","CouncilArea",
               "Lattitude", "Longtitude","Regionname","Propertycount")
Description <- c("Name of houses' suburb",
                 "Address of houses",
                 "Number of rooms",
                 "Houses' types: h - house, cottage, villa, semi, terrace;
                 u - unit, duplex; t - townhouse", 
                 "Price in Dollars",
                 "S - property sold; SP - property sold prior; 
                 PI - property passed in; PN - sold prior not disclosed;
                 SN - sold not disclosed; NB - no bid;
                 VB - vendor bid; W - withdrawn prior to auction; 
                 SA - sold after auction;
                 SS - sold after auction price not disclosed;
                 N/A - price or highest bid not available",
                 "Real Estate Agent", 
                 "Date sold",
                 "Distance from CBD",
                 "Postcode address number",
                 "Number of bedrooms (from different source)",
                 "Number of bathrooms",
                 "Number of car spots",
                 "Land size",
                 "Building size",
                 "Year built",
                 "Governing council for the area",
                 "Latitude coordinates of houses",
                 "Longtitude coordinates of houses",
                 "General Region (West, North West, North, North east, etc)",
                 "Number of properties that exist in the suburb")
tab1 <- data.frame(Variables,Description)
kbl(tab1, booktabs = T, caption = "Data description") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))
#2.2 Data Cleaning
# Summary of character variables
Character_variables <- c("Suburb","Address","Type","Method","SellerG",
                         "Date","CouncilArea","Regionname")
Number_of_categories <- c(n_distinct(dat$Suburb),n_distinct(dat$Address),
                          n_distinct(dat$Type), n_distinct(dat$Method),
                          n_distinct(dat$SellerG),n_distinct(dat$Date),
                          n_distinct(dat$CouncilArea),n_distinct(dat$Regionname))
tab2 <- data.frame(Character_variables,Number_of_categories)
kbl(tab2, booktabs = T, caption = "Summary of character variables") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Summary of numeric variables
Numeric_variables <- c("Rooms","Price","Distance","Postcode","Bedroom2",
                       "Bathroom","Car","Landsize","BuildingArea","YearBuilt",
                       "Lattitude", "Longtitude","Propertycount")
Min <- c(min(dat$Rooms),min(dat$Price),min(dat$Distance),min(dat$Postcode),
         min(dat$Bedroom2),min(dat$Bathroom),min(dat$Car,na.rm=TRUE),
         min(dat$Landsize),min(dat$BuildingArea,na.rm=TRUE),
         min(dat$YearBuilt,na.rm=TRUE),min(dat$Lattitude),
         min(dat$Longtitude),min(dat$Propertycount))
Median <- c(median(dat$Rooms),median(dat$Price),median(dat$Distance),
            median(dat$Postcode),median(dat$Bedroom2),median(dat$Bathroom),
            median(dat$Car,na.rm=TRUE),median(dat$Landsize),
            median(dat$BuildingArea,na.rm=TRUE),median(dat$YearBuilt,na.rm=TRUE),
            median(dat$Lattitude),median(dat$Longtitude),
            median(dat$Propertycount))
Mean <- c(mean(dat$Rooms),mean(dat$Price),mean(dat$Distance),mean(dat$Postcode),
          mean(dat$Bedroom2),mean(dat$Bathroom),mean(dat$Car,na.rm=TRUE),
          mean(dat$Landsize),mean(dat$BuildingArea,na.rm=TRUE),
          mean(dat$YearBuilt,na.rm=TRUE),mean(dat$Lattitude),
          mean(dat$Longtitude),mean(dat$Propertycount))
Max <- c(max(dat$Rooms),max(dat$Price),max(dat$Distance),max(dat$Postcode),
         max(dat$Bedroom2),max(dat$Bathroom),max(dat$Car,na.rm=TRUE),
         max(dat$Landsize),max(dat$BuildingArea,na.rm=TRUE),
         max(dat$YearBuilt,na.rm=TRUE),max(dat$Lattitude),
         max(dat$Longtitude),max(dat$Propertycount))
Number_of_NA <- c(sum(is.na(dat$Rooms)),sum(is.na(dat$Price)),sum(is.na(dat$Distance)),
                  sum(is.na(dat$Postcode)),sum(is.na(dat$Bedroom2)),
                  sum(is.na(dat$Bathroom)),sum(is.na(dat$Car)),
                  sum(is.na(dat$Landsize)),sum(is.na(dat$BuildingArea)),
                  sum(is.na(dat$YearBuilt)),sum(is.na(dat$Lattitude)),
                  sum(is.na(dat$Longtitude)),sum(is.na(dat$Propertycount)))
tab3 <- data.frame(Numeric_variables,Min,Median,Mean,Max,Number_of_NA)
kbl(tab3, booktabs = T,digits = 2,caption = "Summary of numeric variables") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))
#3 variables have missing values: Building Area, Year Built and Car

#Remove variable Building Area and Year Built:
data <- dat[,-15]
data <- data[,-15]
summary(data)

# Handle missing data of Car variable: 62 N/A = 0.46% data set
62/nrow(dat) * 100

# Imputation method for N/A values = median imputation method
data$Car[is.na(data$Car)] <- median(data$Car, na.rm=TRUE)
summary(data$Car)

# Find outliers in Price and Landsize
# Draw Boxplot of Price
boxplot(data$Price, xlab="Price", col="blue")
# There may be some outliers in Price, but we will not remove them

# Draw Boxplot of Landsize
boxplot(data$Landsize, xlab="Land Size", col="blue")

# Calculate percentage of Landsize = 0 in data set
sum(data$Landsize==0)/nrow(data)
#therefore, we should remove Landsize variable

# Draw boxplot of Rooms variable
boxplot(data$Rooms, xlab="Rooms", col="blue")

# Draw boxplot of Bedrooms variable
boxplot(data$Bedroom2, xlab="Bedroom", col="blue")

# Explore the extreme value of Rooms variable
table1 <- data %>% select(Rooms, Type, Bedroom2, Bathroom, Car, Landsize) %>%
  filter(data$Rooms %in% c(8:10))
kbl(table1, booktabs = T, caption ="Explore highest values of Rooms") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Explore the highest value of Bedroom2
table2 <- data %>% select(Rooms, Type, Bedroom2, Bathroom, Car, Landsize) %>%
  filter(data$Bedroom2 %in% c(8:20))
kbl(table2, booktabs = T, caption ="Explore highest values of Bedroom2") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Remove all data points that number of bedrooms > number of rooms
sum(data$Bedroom2 > data$Rooms)
outlier <- which(data$Bedroom2 > data$Rooms)
data <- data[-outlier,]

#-----------------------Data Exploration and Visualization----------------------
#3. Data exploration and visualization
#3.1. House Price Exploration
# Features of top highest and lowest price houses

# Top 10 highest price houses
top_highest <- data %>% select(Price,Suburb,Rooms,Type,Bedroom2,Bathroom,Car,Landsize) %>%
  top_n(10,Price) %>% arrange(desc(Price))  
kbl(top_highest, booktabs = T, caption = "The top 10 highest price houses") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Top 10 lowest price houses
top_lowest <- data %>% select(Price,Suburb,Rooms,Type,Bedroom2,Bathroom,Car,Landsize) %>%
  top_n(-10,Price) %>% arrange(desc(Price))  
kbl(top_lowest, booktabs = T, caption = "The top 10 lowest price houses") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Histogram of Housing Price
data %>% ggplot(aes(x = Price, fill = ..count..)) +
  geom_histogram() +
  ggtitle("Histogram of Price") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))

# Transform Price into log form and draw histogram
data <- data %>% mutate(Price_log = log(Price))
data %>% ggplot(aes(x = Price_log, fill = ..count..)) +
  geom_histogram() +
  ggtitle("Histogram of log Price") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))
# Nearly normal distribution 
# We will use Price_log instead of Price in prediction model

#3.2. House price exploration by Type
# Create table of Type only
house_type <- data %>% select(Type,Price) %>%
  group_by(Type) %>% summarize(Total = length(Type), Max_Price = max(Price),
                               Min_Price = min(Price), Average_Price=mean(Price))
kbl(house_type, booktabs = T,caption = "House price and type") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Draw plot of Type and Price
ggplot(house_type, aes(x= Type, y=Average_Price)) +
  geom_bar(stat='identity', fill="blue",width=0.5) + 
  coord_flip() +
  labs(x="", y="Average house price") +
  ggtitle("Average Housing Price by Type")

#3.3. House price exploration by suburb
# Explore suburb by total houses, max price, min price, average price
house_suburb <- data %>% select(Suburb,Price) %>%
  group_by(Suburb) %>% summarize(Total = length(Suburb), Max_Price = max(Price),
                                 Min_Price = min(Price), Average_Price=mean(Price))

# Top ten suburbs with highest average price
top_10_suburb <- house_suburb %>% select(Suburb,Average_Price) %>%
  top_n(10,Average_Price) %>% arrange(desc(Average_Price))  
ggplot(top_10_suburb, aes(x=reorder(Suburb, Average_Price), y=Average_Price)) +
  geom_bar(stat='identity', fill="blue") + 
  coord_flip() +
  labs(x="", y="Average Price") +
  geom_text(aes(label=round(Average_Price, digits = 0)), 
            hjust=1.2, size=3, col="white") +
  ggtitle("Top highest average house price by suburb")

# Top ten suburbs with lowest average price
top_lowest_suburb <- house_suburb %>% select(Suburb,Average_Price) %>%
  top_n(-10,Average_Price) %>% arrange(desc(Average_Price))  
ggplot(top_lowest_suburb, aes(x=reorder(Suburb, Average_Price), y=Average_Price)) +
  geom_bar(stat='identity', fill="blue") + 
  coord_flip() +
  labs(x="", y="Average Price") +
  geom_text(aes(label=round(Average_Price, digits = 0)), 
            hjust=1.2, size=3, col="white") +
  ggtitle("Top lowest average house price by suburb")

# Compare the highest average price and lowest average price among suburbs
max(house_suburb$Average_Price)/min(house_suburb$Average_Price)

#3.4. House price exploration by other variables
# Create table of Rooms and Price
house_room <- data %>% select(Rooms,Price) %>%
  group_by(Rooms) %>% summarize(Total = length(Rooms), Max_Price = max(Price),
                                Min_Price = min(Price), Average_Price=mean(Price))
kbl(house_room, booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Draw plot of Rooms and Price
p1 <- ggplot(house_room, aes(x = reorder(Rooms,Average_Price), y = Average_Price)) +
  geom_bar(stat='identity', fill="blue",width=0.7) + 
  coord_flip() +
  labs(x="Number of rooms", y="Average house price") +
  geom_text(aes(label=round(Average_Price, digits = 0)), 
            hjust= 1.1, size=3,col="white") +
  ggtitle("Average Housing Price by Room")

# Explore Price by Bedroom2
# Create table of Bedroom and Price
house_bedroom <- data %>% select(Bedroom2,Price) %>%
  group_by(Bedroom2) %>% summarize(Total = length(Bedroom2), Max_Price = max(Price),
                                   Min_Price = min(Price), Average_Price=mean(Price))
kbl(house_bedroom, booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Draw plot of Bedroom and Price
p2 <- ggplot(house_bedroom, aes(x = reorder(Bedroom2,Average_Price), 
                                y = Average_Price)) +
  geom_bar(stat='identity', fill="blue",width=0.7) + 
  coord_flip() +
  labs(x="Number of bedrooms", y="Average house price") +
  geom_text(aes(label=round(Average_Price, digits = 0)), 
            hjust= 1.1, size=3,col="white") +
  ggtitle("Average Housing Price by Bedroom")

# Explore Price by Bathroom
# Create table of Bathroom and Price
house_bathroom <- data %>% select(Bathroom,Price) %>%
  group_by(Bathroom) %>% summarize(Total = length(Bathroom), Max_Price = max(Price),
                                   Min_Price = min(Price), Average_Price=mean(Price))
kbl(house_bathroom, booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Draw plot of Bathroom and Price
p3 <- ggplot(house_bathroom, aes(x = reorder(Bathroom,Average_Price), y = Average_Price)) +
  geom_bar(stat='identity', fill="blue",width=0.7) + 
  coord_flip() +
  labs(x="Number of bathrooms", y="Average house price") +
  geom_text(aes(label=round(Average_Price, digits = 0)), 
            hjust= 1.1, size=3,col="white") +
  ggtitle("Average Housing Price by Bathroom")

# Explore Price by Car
# Create table of Car and Price
house_car <- data %>% select(Car,Price) %>%
  group_by(Car) %>% summarize(Total = length(Car), 
                              Max_Price = max(Price),
                              Min_Price = min(Price), 
                              Average_Price=mean(Price))
kbl(house_car, booktabs = T) %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Draw plot of Car and Price
p4 <- ggplot(house_car, aes(x = reorder(Car,Average_Price), y = Average_Price)) +
  geom_bar(stat='identity', fill="blue",width=0.7) + 
  coord_flip() +
  labs(x="Number of cars", y="Average house price") +
  geom_text(aes(label=round(Average_Price, digits = 0)), 
            hjust= 1.1, size=3,col="white") +
  ggtitle("Average Housing Price by Car")

# Combine 4 plots (Rooms, Bedroom, Bathroom and Car)
grid.arrange(p1,p2,p3,p4)

# House Price with distance
# Draw plot of Distance and Price
ggplot(data, aes(x=Distance, y=Price)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Scatter plot of Housing Price and Distance") +
  theme(plot.title = element_text(hjust = 0.4))

#--------------------------Methodology and Result--------------------------------
#4. Methodology
# Using Knn, linear regression, Random Forest model to predict housing price
# Choose the optimal model based on the smallest RMSE
# Select suitable variables in data set, then create data set to use in prediction model
set.seed(1,sample.kind="Rounding")
data_model <- data %>% select(Rooms, Type, Distance, Bedroom2, Bathroom,
                              Car, Price_log)

# count number of Suburb that appear only 1 time
sum(house_suburb$Total==1)

# Change class of Type from character to factor
data_model$Type <- as.factor(data_model$Type)

# Check again data
summary(data_model)

# Separate data set into train set and validation set
index <- createDataPartition(y=data_model$Price_log, times=1,p=0.2, list=FALSE)
validation_set <- data_model[index,]
train_set <- data_model[-index,]

#-------------------------------Knn Model---------------------------------------
#4.1. knn method
# Set k nearest number from 2 to 50 with 10-fold cross validation
n_try <- seq(2,50,1)
k <- 10

# Randomly assign data into k folds
folds <- sample (1:k,nrow(train_set),replace =TRUE)
# Create a matrix of size k=49 to store test RMSE
cv_rmse <- matrix (0,k,49)
# Do for loops to calculate test MSE for each k and each models
for(j in 1:k){
  rmse_knn_cv <- sapply(n_try,function(n){
    fit_knn_cv <- kknn(Price_log~.,train_set[folds!=j,],
                       train_set[folds==j,],k=n)
    y_hat_knn <- fit_knn_cv$fitted.values
    sqrt(mean((y_hat_knn - train_set[folds==j,]$Price_log)^2))
  })
  cv_rmse[j,] <- rmse_knn_cv
}

# Calculate mean of each column
mean.cv.rmse <- colMeans(cv_rmse)

# Find value of p with minimum value of test MSE
k_optimal <- n_try[which.min(mean.cv.rmse)]

# Find minimum value of RMSE
rmse_knn <- min(mean.cv.rmse)

# Plot all value of k and RMSE corresponding
plot(mean.cv.rmse, main="RMSE of Knn Model",
     ylab="Mean RMSE", xlab="Value of k",
     col="blue")

# Table of optimal result
result1 <- data_frame(Method = "Knn Method", 
                      RMSE = rmse_knn,
                      k_optimal = k_optimal)
kbl(result1, booktabs = T, caption = "Result of knn method") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Creat table of Knn method with optimal result
knn_result <- data_frame(Method = "Knn Method",
                         RMSE = rmse_knn)

# ---------------------------- Linear regression method -------------------------
#4.2. Linear regression method
# Linear regression model with 10-fold cross validation
fit_lm_cv <- train(Price_log ~ ., train_set,
                   method = "lm", trControl = trainControl(method = "cv", number = 10))

# Fit to find predicted results
y_hat_lm <- predict(fit_lm_cv, train_set)

# Calculate RMSE
rmse_lm <- sqrt(mean((y_hat_lm - train_set$Price_log)^2))
lm_result <- data_frame(Method = "Linear Regression Method",
                        RMSE = rmse_lm)

# Create table of lm method with optimal result
kbl(lm_result, booktabs = T, caption ="Result of linear regression method") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# ---------------------------- Random Forest method -------------------------
#4.3. Random Forest method
# Use randomForest function for random forest model
fit_rf <- randomForest(Price_log ~ .,data=train_set,importance=TRUE)

# Calculate RMSE
rmse_rf <- sqrt(mean((fit_rf$predicted - train_set$Price_log)^2))
rf_result <- data_frame(Method = "Random Forest Method",
                        RMSE = rmse_rf)

# Create table of Random Forest with optimal result
kbl(rf_result, booktabs = T, caption ="Result of random forest method") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Combined result of 3 methods 
result <- bind_rows(knn_result,lm_result,rf_result)
kbl(result, booktabs = T, caption ="Combined result of three methods") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# ----------------------- Final result on validation data set ----------------
#4.4. Final result on validation data set
# Fit Random Forest model
fit_rf2 <- randomForest(Price_log ~ .,data=validation_set,importance=TRUE)

# Calculate RMSE
rmse_rf2 <- sqrt(mean((fit_rf2$predicted - validation_set$Price_log)^2))
rf_result2 <- data_frame(Method = "Random Forest Method - Final Result",
                         RMSE = rmse_rf2)

# Create table of final result on validation data set
kbl(rf_result2, booktabs = T, 
    caption ="Result of chosen model on validation set") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Draw QQ Plot
qqnorm(fit_rf2$predicted, pch = 1, frame = FALSE)
qqline(fit_rf2$predicted, col = "red", lwd = 2)

# Examine variable importance
varImpPlot(fit_rf2)
varImp(fit_rf2)
