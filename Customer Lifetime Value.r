# Libraries used 
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)


# Changing name of given dataset to data and checking structure and summary of data
setwd("F:/Projects/Customer Value Analysis in Insurance Marketing")
data <- read.csv("Insurance_Marketing_Customer_Value_Analysis.csv")
str(data)
summary(data)


# Converting character variables into Factor variables
data$State <- as.factor(data$State)
data$Response <- as.factor(data$Response)
data$Coverage <- as.factor(data$Coverage)
data$Education <- as.factor(data$Education)
data$EmploymentStatus <- as.factor(data$EmploymentStatus)
data$Gender <- as.factor(data$Gender)
data$Location_Code  <- as.factor(data$Location_Code )
data$Marital_Status  <- as.factor(data$Marital_Status)
data$Policy_Type  <- as.factor(data$Policy_Type)
data$Renew_Offer_Type  <- as.factor(data$Renew_Offer_Type)
data$Policy  <- as.factor(data$Policy)
data$Sales_Channel  <- as.factor(data$Sales_Channel)
data$Vehicle_Class  <- as.factor(data$Vehicle_Class)
data$Vehicle_Size  <- as.factor(data$Vehicle_Size)


# Converting two numerical variables as factors
data$Number_of_Open_Complaints <- as.factor(data$Number_of_Open_Complaints)
data$Number_of_Policies <- as.factor(data$Number_of_Policies)
str(data)


# Checking for the missing values : No missing values found
sapply(data, function(x) sum(is.na(x)))
summary(data)


# Checking for the presence of Outliers in Dependent variable and removing them
boxplot(data$Customer_Lifetime-Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Customer_Lifetime_Value <15628.739, ]
boxplot(data$Customer_Lifetime_Value)

boxplot(data$Customer_Lifetime-Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Customer_Lifetime_Value <14956.300, ]
boxplot(data$Customer_Lifetime_Value)

boxplot(data$Customer_Lifetime-Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Customer_Lifetime_Value <14956.300, ]
boxplot(data$Customer_Lifetime_Value)


# Checking the presence of Outliers in other independent variables and removing them
boxplot(data$Income)


boxplot(data$Monthly_Premium_Auto)
quantile(data$Monthly_Premium_Auto, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Monthly_Premium_Auto <167, ]
boxplot(data$Months_Since_Policy_Inception)


boxplot(data$Total_Claim_Amount)
quantile(data$Total_Claim_Amount, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data <- data[data$Total_Claim_Amount <944.994931, ]
boxplot(data$Total_Claim_Amount)

nrow(data)
names(data)

# Preparing the Regression model :

#Iteration 1 :

fit<- lm(Customer_Lifetime_Value ~ 	State+Response+	Coverage +
           Education+Effective_To_Date +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class+Vehicle_Size, data=data)

summary(fit)


#Iteration 2 :

fit<- lm(Customer_Lifetime_Value ~ Response+	Coverage +
           Education+Effective_To_Date +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class+Vehicle_Size, data=data)

summary(fit)


#Iteration 3 :

fit<- lm(Customer_Lifetime_Value ~ Coverage +
           Education+Effective_To_Date +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class+Vehicle_Size, data=data)

summary(fit)


#Iteration 4 :

fit<- lm(Customer_Lifetime_Value ~
           Education+Effective_To_Date +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class+Vehicle_Size, data=data)

summary(fit)


#Iteration 5 :

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class+Vehicle_Size, data=data)

summary(fit)


#Iteration 6 :

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount+ 
           Vehicle_Class, data=data)

summary(fit)


#Iteration 7 :

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Months_Since_Last_Claim+Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount
           , data=data)

summary(fit)


#Iteration 8 :

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy+Sales_Channel+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount
         , data=data)

summary(fit)


#Iteration 9 : Sales_channel

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+Location_Code+Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy+
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount
         , data=data)

summary(fit)


#Iteration 10 :

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+ I(Location_Code == "Suburban")+Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy +
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount
         , data=data)

summary(fit)

#Iteration 11 :

fit<- lm(Customer_Lifetime_Value ~
           Education +EmploymentStatus+Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy +
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount
         , data=data)

summary(fit)



#Iteration 12 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy +
           Number_of_Open_Complaints + Number_of_Policies +  
           Renew_Offer_Type +  Total_Claim_Amount
         , data=data)

summary(fit)



#Iteration 13 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy +
           Number_of_Open_Complaints + Number_of_Policies +  
           I(Renew_Offer_Type == "2") +  Total_Claim_Amount
         , data=data)

summary(fit)

#Iteration 14 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+ Policy_Type+
           Monthly_Premium_Auto + Policy +
           Number_of_Open_Complaints + Number_of_Policies +  
           Total_Claim_Amount
         , data=data)

summary(fit)


#Iteration 15 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+ I(Policy_Type == "Personal Auto") +
           Monthly_Premium_Auto + Policy +
           Number_of_Open_Complaints + Number_of_Policies +  
           Total_Claim_Amount
         , data=data)

summary(fit)



#Iteration 16 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+ I(Policy_Type == "Personal Auto") +
           Monthly_Premium_Auto + I(Policy == "Corporate L2") + 
           I(Policy == "Corporate L3") +
           Number_of_Open_Complaints + Number_of_Policies +  
           Total_Claim_Amount
         , data=data)

summary(fit)


#Iteration 17 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+
           Monthly_Premium_Auto + I(Policy == "Corporate L2") + 
           I(Policy == "Corporate L3") +
           Number_of_Open_Complaints + Number_of_Policies +  
           Total_Claim_Amount
         , data=data)

summary(fit)


#Iteration 18 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+
           Monthly_Premium_Auto + 
           Number_of_Open_Complaints + Number_of_Policies +  
           Total_Claim_Amount
         , data=data)

summary(fit)

#Iteration 19 :

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+
           Monthly_Premium_Auto + 
           I(Number_of_Open_Complaints == "1") + I(Number_of_Open_Complaints == "3") +
          Number_of_Policies +I(Number_of_Open_Complaints == "4") +  I(Number_of_Open_Complaints == "5")
           + Total_Claim_Amount
         , data=data)

summary(fit)


#Iteration 20 : FINAL MODEL

fit<- lm(Customer_Lifetime_Value ~
           Education + I(EmploymentStatus == "Employed") +  
           I(EmploymentStatus == "Unemployed") + Gender+
           Income+ Months_Since_Policy_Inception+
           Marital_Status+
           Monthly_Premium_Auto + 
           I(Number_of_Open_Complaints == "1") + I(Number_of_Open_Complaints == "3")+
           Number_of_Policies + I(Number_of_Open_Complaints == "4") +  
           I(Number_of_Open_Complaints == "5"), data=data)

summary(fit)

# Checking the Assumptions :
# checking Multicollinearity Assumption :
vif(fit)


# checking Normality Assumption :
resids <- fit$residuals
ad.test(resids)


# checking Homoscedasticity Assumption :
# Breusch-Pagan test
bptest(fit)  


# checking Auto-Correlation Assumption :
dwt(fit)


# Get the predicted or fitted values
fitted(fit)
data$pred <- fitted(fit)


# Calculating MAPE
attach(data)
(sum((abs(Customer_Lifetime_Value-pred))/Customer_Lifetime_Value))/nrow(data)

