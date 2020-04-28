#Group 02
#Team Members   : Amit Verma, Sparsh Sharma, Simardeep Kaur
#Dataset        : US Adult Income

#Install Packages
install.packages("plyr")
installed.packages("dplyr")
install.packages("lmtest")
installed.packages("car")
install.packages("genralhoslem")
install.packages("MASS")
install.packages("tidyverse")
install.packages("rpart")
install.packages("caret")
install.packages("rpart.plot")
install.packages("sqldf")
install.packages("reshape2")
install.packages("knitr")
install.packages("Hmisc")
install.packages("pROC")

# Library
library(plyr)
library(dplyr)
library(lmtest)
library(car)
library(generalhoslem)
library(MASS)
library(tidyverse)
library(rpart)
library(caret)
library(rpart.plot)
library(sqldf) #For SQL Functions
library(reshape2) #For plots
library(knitr) # For tables
library(Hmisc)
library(pROC)

# Overview Of Data
#Read the file
adult_data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",sep=",")

#Check first 6 rows
head(adult_data)

#Check structure of dataset
str(adult_data)

#Summarise the dataset
summary(adult_data)

#Data Cleaning
# Step 1: Replace the " ?" with NA using na.strings argument
adult_data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",sep=",",header = FALSE,na.strings = " ?")
# Step 2: add column headers
names(adult_data)<-c('Age','Workclass','fnlgwt','Education','Education num','Marital Status',
                     'Occupation','Relationship','Race','Sex','Capital Gain','Capital Loss',
                     'Hours/Week','Native country','Income')
# Step 3: Delete NA values using omit
adult_data<-na.omit(adult_data)

# Renumbering the rows
row.names(adult_data) <-1:nrow(adult_data)

#Combining the Categories

#Work Class
summary(adult_data$Workclass)

# Removing the Never Worked Category using droplevels function
adult_data$Workclass<-droplevels(adult_data$Workclass)

#Occupation
summary(adult_data$Occupation)
PrimaryOcc<-c(" Craft-repair"," Farming-fishing"," Handlers-cleaners"," Priv-house-serv", " Transport-moving" )
SecondaryOcc<-c( " Armed-Forces"," Adm-clerical"," Machine-op-inspct"," Sales"," Tech-support")
TertiaryOcc<-c(" Exec-managerial"," Prof-specialty"," Protective-serv")
Ohters<-c(" Other-service")

adult_data<-mutate(adult_data,
                   occupation_status = ifelse(Occupation %in% PrimaryOcc, "PrimaryOcc",
                                              ifelse(Occupation %in% SecondaryOcc, "SecondaryOcc",
                                                     ifelse(Occupation %in% TertiaryOcc, "TertiaryOcc",
                                                            ifelse(Occupation %in% Ohters, "Others","Others")))))

adult_data$occupation_status<-factor(adult_data$occupation_status,levels = c("PrimaryOcc","SecondaryOcc","TertiaryOcc","Others"))

#Education
summary(adult_data$Education)
Primary<-c(" 10th"," 11th"," 12th"," 1st-4th"," 5th-6th"," 7th-8th"," 9th"," Preschool")
Secondary<-c(" Bachelors"," Some-college"," Prof-school")
Intermediate<-c(" Assoc-acdm"," Assoc-voc"," HS-grad")
Advanced<-c(" Doctorate"," Masters")

adult_data<-mutate(adult_data,
                   education_level = ifelse(Education %in% Primary, "Primary",
                                            ifelse(Education %in% Secondary, "Secondary",
                                                   ifelse(Education %in% Intermediate, "Intermediate",
                                                          ifelse(Education %in% Advanced, "Advanced","Others")))))

#Convert the education_levelinto factor
adult_data$education_level<-factor(adult_data$education_level,levels = c("Primary","Intermediate","Secondary","Advanced"))
summary(adult_data$`Native country`)

#Marital Status
Single<-c(" Divorced"," Never-married"," Separated"," Widowed")
Married<-c(" Married-AF-spouse"," Married-civ-spouse"," Married-spouse-absent")

adult_data<-mutate(adult_data,
                   Marital_Status= ifelse(adult_data$`Marital Status` %in% Single, "Single",
                                          ifelse(adult_data$`Marital Status` %in% Married, "Married",'Others'
                                          )))

# Convert the Marital_Status into factor
adult_data$Marital_Status<-factor(adult_data$Marital_Status,levels = c("Single","Married"))
summary(adult_data$Marital_Status)                   

#Country
Asia<- c(" Cambodia", " China", " Hong", " Laos", " Thailand"," Japan", " Taiwan", " Vietnam"," India", " Iran")
Central_America <- c(" Cuba", " Guatemala", " Jamaica", " Nicaragua", " Puerto-Rico",  " Dominican-Republic", " El-Salvador", " Haiti", " Honduras", " Mexico", " Trinadad&Tobago"," Ecuador", " Peru", " Columbia")
Europe<- c(" England", " Germany", " Holand-Netherlands", " Ireland", " France", " Greece", " Italy", " Portugal", " Scotland"," Poland"," Yugoslavia", " Hungary")
adult_data <- mutate(adult_data, native_region = ifelse(adult_data$`Native country`%in% Asia, "Asia",
                                                        ifelse(adult_data$`Native country` %in% Central_America, "Central_America",
                                                               ifelse(adult_data$`Native country` %in% Europe, "Europe",
                                                                      ifelse(adult_data$`Native country` == " United-States", " United-States", " Outlying-US" )))))
# Convert The native_region column into factor
adult_data$native_region<-factor(adult_data$native_region,ordered = FALSE)
summary(adult_data$native_region)

# 2 Sample T Test (for quantative variable)

#Age
summary(adult_data$Income)
var.test(adult_data$Age[adult_data$Income == " >50K"],adult_data$Age[adult_data$Income == " <=50K"])
t.test(adult_data$Age[adult_data$Income == " >50K"],adult_data$Age[adult_data$Income == " <=50K"],var.equal = FALSE)

#fnlgwt
var.test(adult_data$fnlgwt[adult_data$Income == " >50K"],adult_data$fnlgwt[adult_data$Income == " <=50K"])
t.test(adult_data$fnlgwt[adult_data$Income == " >50K"],adult_data$fnlgwt[adult_data$Income == " <=50K"],var.equal = FALSE)

#Education Num
var.test(adult_data$`Education num`[adult_data$Income == " >50K"],adult_data$`Education num`[adult_data$Income == " <=50K"])
t.test(adult_data$`Education num`[adult_data$Income == " >50K"],adult_data$`Education num`[adult_data$Income == " <=50K"],var.equal = FALSE)

#Capital Gain
var.test(adult_data$`Capital Gain`[adult_data$Income == " >50K"],adult_data$`Capital Gain`[adult_data$Income == " <=50K"])
t.test(adult_data$`Capital Gain`[adult_data$Income == " >50K"],adult_data$`Capital Gain`[adult_data$Income == " <=50K"],var.equal = FALSE)

#Capital Loss
var.test(adult_data$`Capital Loss`[adult_data$Income == " >50K"],adult_data$`Capital Loss`[adult_data$Income == " <=50K"])
t.test(adult_data$`Capital Loss`[adult_data$Income == " >50K"],adult_data$`Capital Loss`[adult_data$Income == " <=50K"],var.equal = FALSE)

#Hours/Week
var.test(adult_data$`Hours/Week`[adult_data$Income == " >50K"],adult_data$Age[adult_data$Income == " <=50K"])
t.test(adult_data$`Hours/Week`[adult_data$Income == " >50K"],adult_data$Age[adult_data$Income == " <=50K"],var.equal = FALSE)

#Chi Square Test (for Categorical data)

#Workclass
summary(adult_data$Workclass)
tbl_Workclass=table(adult_data$Income,adult_data$Workclass)
tbl_Workclass
chisq.test(tbl_Workclass)

#Occupation_Status
summary(adult_data$occupation_status)
tbl_Occupation=table(adult_data$Income,adult_data$occupation_status)
tbl_Occupation
chisq.test(tbl_Occupation)

#Marital Status
summary(adult_data$Marital_Status)
tbl_MaritalStatus=table(adult_data$Income,adult_data$Marital_Status)
tbl_MaritalStatus
chisq.test(tbl_MaritalStatus)

#Education
summary(adult_data$education_level)
tbl_educationlevel=table(adult_data$Income,adult_data$education_level)
tbl_educationlevel
chisq.test(tbl_educationlevel)

#Native Region
summary(adult_data$native_region)
tbl_nativeregion=table(adult_data$Income,adult_data$native_region)
tbl_nativeregion
chisq.test(tbl_nativeregion)

#Relationship
summary(adult_data$Relationship)
tbl_Relationship=table(adult_data$Income,adult_data$Relationship)
tbl_Relationship
chisq.test(tbl_Relationship)

#Race
summary(adult_data$Race)
tbl_Race=table(adult_data$Income,adult_data$Race)
tbl_Race
chisq.test(tbl_Race)

#Sex
summary(adult_data$Sex)
tbl_Sex=table(adult_data$Income,adult_data$Sex)
tbl_Sex
chisq.test(tbl_Sex)

#Variable Selection

# Fit the model
adult<-data.frame(adult_data$Age, adult_data$Workclass,adult_data$fnlgwt,adult_data$education_level,adult_data$`Education num`,adult_data$Marital_Status,adult_data$occupation_status,adult_data$Relationship,adult_data$Race,adult_data$Sex,adult_data$`Capital Gain`,adult_data$`Capital Loss`,adult_data$`Hours/Week`,adult_data$native_region,adult_data$Income)
adult_stepwise<- glm (adult$adult_data.Income ~., family = binomial, data = adult) %>%
  stepAIC(trace = FALSE)

# Summarize the final selected model
summary(adult_stepwise)

#Tree Algorithm
#Figuring out important variables

tree <- rpart(adult$adult_data.Income ~ .,data = adult, method = "class")
imp <- varImp(tree)
rownames(imp)[order(imp$Overall, decreasing=TRUE)]

#Data after selecting significant variables
adultdata<-read.csv("/cloud/project/adultdata1.csv",header=TRUE)
head(adultdata)

# Likelihood Ratio Test for significant variable
null_model <- glm(Income ~1 , family=binomial(link = "logit"), data=adultdata)
full_model <- glm(Income ~Capital.Gain  , family=binomial(link = "logit"), data=adultdata)
anova(null_model,full_model,test="Chisq")

#Calculating G*2
null_model$deviance-full_model$deviance
#Calculating df
null_model$df.residual-full_model$df.residual

#Likelihood ratio teat for non-significant variable

#fnlgwt
null_model1 <- glm(Income ~1 , family=binomial(link = "logit"), data=adult_data)
full_model1 <- glm(Income ~fnlgwt, family=binomial(link = "logit"), data=adult_data)
anova(null_model1,full_model1,test="Chisq")

#Calculating G*2
null_model1$deviance-full_model1$deviance

#Calculating df
null_model1$df.residual-full_model1$df.residual

#Splitting data
trainIndex = sample(1:nrow(adultdata), size = round(0.7*nrow(adultdata)), replace=FALSE)
adult_train = adultdata[trainIndex ,]
adult_test = adultdata[-trainIndex ,]
nrow(adult_train)
nrow(adult_test)

#Non-interaction model
fit<-glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss,family = binomial(link='logit'),data = adult_train)
summary(fit)

#Interaction model
fit1<-glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss+Marital_Status:Capital.Gain,family = binomial,data = adult_train)
summary(fit1)

#Hosmer-Lameshow Test
hosmer_fit <- glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss, family = binomial(link='logit'), data = adult_train )
logitgof(adult_train$Income, fitted(hosmer_fit))

#Checking multicollinearity

#VIF for categorical variables
vif(glm(Income~occupation_status+Marital_Status,family=binomial(link = "logit"), data=adult_train ))

#VIF for quantitative variables
vif(glm(Income~Capital.Gain+Capital.Loss,family=binomial(link = "logit"), data=adult_train ))

#AIC(For Interaction Model)
Model1 <- glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss+Marital_Status:Capital.Gain,family = binomial, data = adult_train)
fitLM <- stepAIC(Model1)

#LIkelihood ratio test

# Non Interaction Model
model_non_interaction <- glm(Income ~occupation_status+Marital_Status+Capital.Gain+Capital.Loss , family=binomial(link = "logit"), data=adult_train)

# Interaction Model
model_interaction <- glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss+Marital_Status:Capital.Gain, family=binomial(link = "logit"), data=adult_train)
anova(model_non_interaction,model_interaction,test="Chisq")

# Calculating Deviance
model_non_interaction$deviance-model_interaction$deviance

#Final Model in R
Model_interaction <- glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss+Marital_Status:Capital.Gain, family=binomial(link = "logit"), data=adult_train)
summary(Model_interaction)

#Classification Table For Models selected using SAS/R

#Mean for income column for full data                   
adult_data$Income_full<-with(adult_data, ifelse(adult_data$Income == ' >50K', 1, 0))
prop<-sum(adult_data$Income_full)/nrow(adult_data)
prop

#Creating column for income based 0,1 values
adult_test$income_test<-with(adult_test, ifelse(adult_test$Income == ' >50K', 1, 0))

head(predict(fit1,adult_test, type = "response"))
predicted_r<-as.numeric(predict(fit1,adult_test, type = "response")>prop)

#Classification Table for R model
xtabs(~adult_test$income_test+predicted_r)

#Classification Table for SAS model
head(predict(SAS_model,adult_test, type = "response"))
predicted_sas<-as.numeric(predict(SAS_model,adult_test, type = "response")>prop)
xtabs(~adult_test$income+predicted_sas)

#ROC Curve (R Model)
rocplot<-roc(Income~fitted(Model_interaction),data=adult_train)
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)

#ROC Curve (SAS Model)
SAS_model<-  glm(Income~occupation_status+Marital_Status+Capital.Gain+Capital.Loss+
                   +Capital.Gain:occupation_status+Capital.Gain:Marital_Status+Marital_Status:occupation_status, family=binomial(link = "logit"), data=adult_train)      

rocplot1<-roc(Income~fitted(SAS_model),data=adult_train)
plot.roc(rocplot1,legacy.axes = TRUE)
auc(rocplot1)

#Comparing 2 ROC curves
list(rocplot,rocplot1)
ggroc(list(rocplot,rocplot1),legacy.axes = TRUE)