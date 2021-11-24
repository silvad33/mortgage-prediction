#-----------------------------
# Set Working Directory
#-----------------------------
setwd("C:\\Users\\imh3\\OneDrive - Medtronic PLC")
#-----------------------------
# Install Required Packages
#-----------------------------
install.packages("GGally", dependencies = TRUE)
install.packages("cluster", dependencies = TRUE)
install.packages("corrplot", dependencies = TRUE)
install.packages("corrgram", dependencies = TRUE)
install.packages("elasticnet", dependencies = TRUE)
install.packages("ISLR", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("mice", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("naniar", dependencies = TRUE)
install.packages("VIM", dependencies = TRUE)
install.packages("DMwR", dependencies = TRUE)
install.packages("ROSE", dependencies = TRUE)
install.packages("parallel", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("MASS", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("plotly", dependencies = TRUE)
install.packages("doParallel", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("missForest", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("psych")
install.packages("factoextra", dependencies = TRUE)
install.packages("ggbiplot", dependencies = TRUE)
install.packages("skimr", dependencies = TRUE)
library(skimr)
library(naniar)
library(mice)
library(MASS)
library(ISLR)
library(caret)
library(cluster)
library(psych)
library(data.table)
library(tidyverse)
library(corrgram)
library(ggplot2)
library(corrplot)
library(GGally)
library(plyr)
library(dplyr)
library(VIM)
library(elasticnet)
library(ROSE)
library(DMwR)
library(Hmisc)
library(nnet)
library(car)
library(missForest)
library(rpart)
library(factoextra)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(parallel)
library(doParallel)

#---------------------------------------
#### Step 1: Load in the Data
#---------------------------------------
hrdata <- read.csv("qualcomm_recoded_data.csv", stringsAsFactors=TRUE, header = T)


#---------------------------------------
#### Step 2: Data Exploration
#---------------------------------------

## look at the variables and data types
attach(hrdata)
dplyr::tibble(hrdata)
skim(hrdata)

## Check if the dependent variable is evenly distributed
d.table <- table(as.numeric(hrdata$Turnover.Status))
unique(hrdata$Turnover.Status)
d.table 


## Plot the distribution of the class of dependent variable
par(las=2)
par(mar=c(1,14,1,1))
nn <- barplot(table(hrdata$Turnover.Status), horiz=TRUE, col = heat.colors(12), cex.names=0.7) #Fig1
text(x=d.table, y=nn, labels = d.table, pos = 4, cex = 0.8, col = "black")
text(x=d.table[5], y=nn[5], labels = d.table[5], pos = 2, cex = 0.8, col = "black")

## Find NA values in dataframe
colSums(is.na(hrdata))

# remove columns if necessary (blank datasets or IDs or single values)
# hrdata[,c("agency_name",  #removed as it 100% corresponds with 'agency_abbr'
#            "rate_spread"  #removed as all empty'
# )] <- list(NULL)

# Verify no NA values in dataframe
colSums(is.na(hrdata))

dim(hrdata) # 10109 * 23

## Normalize the missing values
mortgagedata <- mortgagedata %>%
  replace_with_na(replace = list(tract_to_msamd_income = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 population = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 minority_population = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 number_of_owner_occupied_units = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 number_of_1_to_4_family_units = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 loan_amount_000s = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 hud_median_family_income = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_income_000s = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 purchaser_type_name  = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 preapproval_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 owner_occupancy_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 lien_status_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 edit_status_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 denial_reason_name_3 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 denial_reason_name_2 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 denial_reason_name_1 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_sex_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_race_name_1 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_race_name_2 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_race_name_3 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_race_name_4 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_race_name_5 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 co_applicant_ethnicity_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 as_of_year = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 application_date_indicator = c('N/A', "", "na", "not applicable", "Not Applicable", 0),
                                 applicant_sex_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_race_name_1 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_race_name_2 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_race_name_3 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_race_name_4 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_race_name_5 = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 applicant_ethnicity_name = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 agency_abbr = c('N/A', "", "na", "not applicable", "Not Applicable"),
                                 action_taken_name = c('N/A', "", "na", "not applicable", "Not Applicable")
  ))

# Find NA values in dataframe
colSums(is.na(mortgagedata)) >0
## Plot NA Values
dev.off()
par(las=2)
par(mar=c(1,14,1,2))
na <- (colSums(is.na(mortgagedata))/nrow(mortgagedata))
na
n <- round(na[na > 0],2)
nn <- barplot(n, main = "% of NA", horiz=TRUE, 
              col = heat.colors(12))
text(x=n[1:5], y=nn[1:5], labels = n[1:5], pos = 4, cex = 0.7, col = "black") #Fig2
text(x=n[6:18], y=nn[6:18], labels = n[6:18], pos = 2, cex = 0.7, col = "black") #Fig2

## Remove columns with 90% or more values missing
na.data <- (colSums(is.na(mortgagedata))/nrow(mortgagedata)) > .90
na.data
names.na.data <- which(na.data, arr.ind = TRUE, useNames = TRUE)
names(names.na.data)
mortgagedata[, c(names(names.na.data))] <- list(NULL)

names(mortgagedata)
colSums(is.na(mortgagedata)) >0

colsToImpute <- colSums(is.na(mortgagedata)) >0
which(colsToImpute, arr.ind = TRUE, useNames = TRUE)

str(mortgagedata)


#-----------------------------
#### Step 3: Data  Cleaning
#-----------------------------
# use more cores for computing
#numcores <- detectCores()
#registerDoParallel(numcores)
dev.off()
par(las=1)
par(mar=c(1,2,1,2))
md.pattern(mortgagedata, rotate.names = TRUE) #Fig3
nrow(cc(mortgagedata)) # 95704
nrow(ic(mortgagedata)) # 1153 incomplete cases
mortgagedata %>% select(tract_to_msamd_income, population) %>% marginplot
pbox(mortgagedata, pos=2) #Fig3-1
# comparing variable with others seems there's no pattern (it's random) in missing data, okay to impute

## Missing value and impuatation with Mice
memory.limit()
mice_plot <- aggr(mortgagedata, col=c('navyblue','yellow'), # Fig4
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(mortgagedata), cex.axis=.7,
                  gap=2, ylab=c("Missing data","Pattern"))

imp.col <- c("number_of_1_to_4_family_units",
             "number_of_owner_occupied_units",
             "tract_to_msamd_income",
             "loan_amount_000s",
             "applicant_income_000s")

mortgagedata.i <- mice(mortgagedata[, imp.col], m=5, maxit = 1, method = 'pmm', seed = 749)
densityplot(mortgagedata.i) # Fig4-1
#stripplot(mortgagedata.i)   # Fig4-2

saveRDS(mortgagedata.i, "mortgagedatai.rds")
mortgagedata.i <- readRDS("mortgagedatai.rds")

mortgagedata.i.df <- mice::complete(mortgagedata.i, 1)
mortgagedata <- mortgagedata[, !(names(mortgagedata) %in% imp.col)]
mortgagedata <- cbind(mortgagedata, mortgagedata.i.df)


## Look at mice plot after imputation
mice_plot <- aggr(mortgagedata, col=c('navyblue','yellow'), #Fig5
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(mortgagedata), cex.axis=.7,
                  gap=1, ylab=c("Missing data","Pattern"))

#densityplot(mortgagedata.i) # Fig5-1
nrow(cc(mortgagedata))
nrow(ic(mortgagedata)) # now all rows are 'complete cases'


# write csv file after data cleaning
write.csv(mortgagedata, "mortdata_afterimputation.csv")
saveRDS(mortgagedata, "mortdata_afterimputation.rds")
mortgagedata <- read.csv("mortdata_afterimputation.csv")


## merge 8 categories into 4
Action <- data.table(Action = c('Approved',
                                'Withdrawn',
                                'Denied',
                                'Purchased',
                                'Approved',
                                'Withdrawn',
                                'Approved',
                                'Denied'))
Action <- cbind(distinct(data.frame(mortgagedata$action_taken_name)), Action)
# category from 8 to 4
mortgagedata <- merge(mortgagedata, Action, 
                      by.x = "action_taken_name",
                      by.y = "mortgagedata.action_taken_name")

dev.off()
#write.csv(mortgagedata, "mortdata_aftermerge.csv")
par(mar=c(2,2,2,2))
plot(table(mortgagedata$Action)) #Fig6
table(mortgagedata$Action)

write.csv(mortgagedata, "mortdata_aftermerge.csv")
saveRDS(mortgagedata, "mortgagedata.rds")
mortgagedata <- readRDS("mortgagedata.rds")

## check 'complete cases' 
nrow(cc(mortgagedata)) #96857
nrow(ic(mortgagedata)) #0
dim(mortgagedata) #96857 * 22


y.mortgagedata <- dplyr::select(mortgagedata, Action, action_taken_name)
str(y.mortgagedata)

# Additional Removal of variables
mortgagedata[,c("action_taken_name",  #y
                "X",  #ID 
                "X.1",
                "Action")  #y
             ] <- list(NULL)
names(mortgagedata)

## Correlation
dev.off()
m.cor <- cor(as.data.frame(lapply(mortgagedata, as.numeric)))
corrgram(m.cor, 
         lower.panel=panel.shade, upper.panel=panel.pie, cex.labels = 1) #Fig7-1

ggcorr(m.cor, 
       label = TRUE, 
       label_alpha = TRUE, palette = "PuOr",hjust = 0.75, angle = -0, cex=3) #Fig7-2

# filter out highly correlated features
tooHigh <- findCorrelation(m.cor, .9, names=FALSE)
tooHigh
mortgagedata <- mortgagedata[, -tooHigh] 

# check high-correlated features again
m.cor <- cor(as.data.frame(lapply(mortgagedata, as.numeric)))
tooHigh <- findCorrelation(m.cor, .9, names=FALSE)
tooHigh
str(mortgagedata)

# combine y.numerical and mortgagedata
y.factor <- as.factor(y.mortgagedata$Action)
mortgagedata <- cbind(y.factor, mortgagedata)
str(mortgagedata)
# Check collinearity
lm.model <- lm(as.numeric(y.factor) ~ ., data=mortgagedata)
vif(lm.model)
alias(lm.model)
# remove columns 
mortgagedata$co_applicant_sex_name <- NULL
mortgagedata$co_applicant_ethnicity_name <- NULL
mortgagedata$applicant_ethnicity_name <- NULL
# Check collinearity
lm.model <- lm(as.numeric(y.factor) ~ ., data=mortgagedata)
vif(lm.model)
alias(lm.model)

write.csv(mortgagedata, "mortdata_afterCLEAN.csv")
saveRDS(mortgagedata, "mortgagedata_afterCLEAN.rds")
mortgagedata <- readRDS("mortgagedata_afterCLEAN.rds")


#double check complete cases
nrow(cc(mortgagedata)) # 96857
nrow(ic(mortgagedata)) # 0
table(mortgagedata$y.factor)
#confirm datatype and columns
mortgagedata$X <- NULL
str(mortgagedata)

# Checking if there is any feature(s) with near zero variance
nzv <- nearZeroVar(mortgagedata, saveMetrics = TRUE)
sum(nzv$nzv == TRUE)
# to check if any of the features used for modeling consists of near zero variance in them.

library(ggpubr)
# population with mortgage loan
pop_mor <- group_by(mortgagedata, y.factor) %>% 
  summarise(
    count = n(), 
    mean = mean(population, na.rm = TRUE),
    sd = sd(population, na.rm = TRUE)
  ) 
pop_mor <- ggstripchart(mortgagedata, x = "y.factor", y = "population", #Fig 50
             color = "y.factor",
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "blue"),
             add = "mean_sd")

# minority population with mortgage loan
minpop_mor <- group_by(mortgagedata, y.factor) %>% 
  summarise(
    count = n(), 
    mean = mean(minority_population, na.rm = TRUE),
    sd = sd(minority_population, na.rm = TRUE)
  ) 
min_mor <- ggstripchart(mortgagedata, x = "y.factor", y = "minority_population", #Fig 51
             color = "y.factor",
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "blue"),
             add = "mean_sd")

# applicant_income with mortgage loan
income_mor <- group_by(mortgagedata, y.factor) %>% 
  summarise(
    count = n(), 
    mean = mean(applicant_income_000s, na.rm = TRUE),
    sd = sd(applicant_income_000s, na.rm = TRUE)
  ) 

inc_mor <- ggplot(mortgagedata, aes(x=y.factor, y=applicant_income_000s)) +   #Fig 53
  geom_boxplot(outlier.colour=NA, 
               color = c("#00AFBB", "#E7B800", "#FC4E07", "blue")) + 
  coord_cartesian(ylim = c(0, 500)) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.01)


# loan amount with mortgage loan
amo_mor <- group_by(mortgagedata, y.factor) %>% 
  summarise(
    count = n(), 
    mean = mean(loan_amount_000s, na.rm = TRUE),
    sd = sd(loan_amount_000s, na.rm = TRUE)
  ) 
amount_mor <-ggplot(mortgagedata, aes(x=y.factor, y=loan_amount_000s)) +   #Fig 54
  geom_boxplot(outlier.colour=NA, 
               color = c("#00AFBB", "#E7B800", "#FC4E07", "blue")) + 
  coord_cartesian(ylim = c(0, 1000)) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.01)


# number of family with mortgage loan
famnum_mor <- group_by(mortgagedata, y.factor) %>% 
  summarise(
    count = n(), 
    mean = mean(number_of_1_to_4_family_units, na.rm = TRUE),
    sd = sd(number_of_1_to_4_family_units, na.rm = TRUE)
  ) 
fam_mor <- ggplot(mortgagedata, aes(x=y.factor, y=number_of_1_to_4_family_units)) + #Fig 55
  geom_boxplot(outlier.colour=NA, 
               color = c("#00AFBB", "#E7B800", "#FC4E07", "blue")) + 
  coord_cartesian(ylim = c(0, 5000)) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.01)


# race with mortgage loan
races_mor <- group_by(mortgagedata, y.factor) %>% 
  summarise(
    count = n(), 
    mean = mean(co_applicant_race_name_1, na.rm = TRUE),
    sd = sd(co_applicant_race_name_1, na.rm = TRUE)
  ) 
race_mor <- ggstripchart(mortgagedata, x = "y.factor", y = "co_applicant_race_name_1", #Fig 56
             color = "y.factor",
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "blue"),
             add = "mean_sd")

#---------------------------
#### Step 4: Model Planning
#---------------------------


## create 70:30 train and test set
set.seed(749)
trainIndex <- createDataPartition(mortgagedata$y.factor, p=.7, list=F) #random stratified sampling 


m.train <- mortgagedata[trainIndex ,] 
m.test <- mortgagedata[-trainIndex ,] 
table(m.train$y.factor)

saveRDS(m.train, "m_train_beforeBALANCing.rds")
saveRDS(m.test, "m_test_beforeBALANCing.rds")

#install.packages("DataExplorer")
library(DataExplorer)
DataExplorer::create_report(m.train)



ctrl <- trainControl(method = "cv", number=5)

## balance the categories
# UpSample
set.seed(749)
up_train <- upSample(x = m.train[,-1],
                     y = m.train$y.factor, yname = "Class")                
table(up_train$Class)
y.factor <- up_train$Class
up_train$Class <- NULL
up_train <- cbind(y.factor, up_train)

# DownSample
set.seed(749)
down_train <- downSample(x = m.train[,-1],
                         y = m.train$y.factor, yname = "Class")  
table(down_train$Class)
y.factor <- down_train$Class
down_train$Class <- NULL
down_train <- cbind(y.factor, down_train)


# Check balanced
par(mfrow=c(1,2))
plot(table(up_train$y.factor))
plot(table(down_train$y.factor))
nrow(cc(up_train))
nrow(cc(down_train))
nrow(ic(up_train))
nrow(ic(down_train))

#calculate performance on different sampled training set

set.seed(749)
orig_fit <- train(y.factor ~ ., data = m.train,
                  method = "glmnet", #Boosted Classification Trees or glmnet glm
                  tuneLength=2,
                  trControl = ctrl)   
confusionMatrix(predict(orig_fit, m.test), m.test$y.factor)

saveRDS(orig_fit, "orig_fit.rds")

set.seed(749)
up_fit <- train(y.factor ~ ., data = up_train, 
                method = "glmnet",
                tuneLength=2,
                trControl = ctrl)
saveRDS(up_fit, "up_fit.rds")
confusionMatrix(predict(up_fit, m.test), m.test$y.factor)

set.seed(749)
down_fit <- train(y.factor ~ ., data = down_train, 
                  method = "glmnet",
                  tuneLength=2,
                  trControl = ctrl)
saveRDS(down_fit, "down_fit.rds")
confusionMatrix(predict.train(down_fit, m.test), m.test$y.factor)


#compare the performance of all models 
orig_fit <- readRDS("orig_fit.rds")
up_fit <- readRDS("up_fit.rds")
down_fit <- readRDS("down_fit.rds")

rValues <- resamples(list(orig=orig_fit, up=up_fit, down=down_fit))
bwplot(rValues) #Fig33


# Choose the best performance dataset and save
m.train <- up_train
saveRDS(m.train, "m_train.rds")
saveRDS(m.test, "m_test.rds")
write.csv(m.train, "m_train.csv")
write.csv(m.test, "m_test.csv")

m.train <- readRDS("m_train.rds") #187684     16
m.test <- readRDS("m_test.rds")   #29056    16
colnames(m.train)

## Create test data split for test
y.m.test <- m.test[, 1]
x.m.test <- m.test[, -1]
table(y.m.test)




#---------------------------
#### Step 5: Model Building
#---------------------------
## define parameters
# 10 fold CV
ctrl <- trainControl(method="cv", number=10,
                     allowParallel=TRUE, savePredictions=TRUE) 
# RMSE function
rmse <- function(error)
{
  sqrt(mean(as.numeric(error)^2))
}


### Decision Tree Model
#-----------------------------------
set.seed(749)
model.rpart <- rpart(y.factor ~ ., data=m.train, method="class")
library(rpart.plot)
rpart.plot(model.rpart) #Fig31
saveRDS(model.rpart, "model_rpart.rds")

print(model.rpart, digits = 3)
pred.rpart <- predict(model.rpart, x.m.test, type="class")
saveRDS(pred.rpart, "pred_rpart.rds")
pred.rpart <- readRDS("pred_rpart.rds")
confusionMatrix(pred.rpart, y.m.test)
rmse(as.numeric(pred.rpart) - as.numeric(y.m.test)) #1.078311

set.seed(749)
train.rpart <- train(y.factor ~ ., data=m.train, 
                     method="rpart",tuneLength=4,
                     trControl=ctrl)
train.rpart
saveRDS(train.rpart, "train_rpart.rds")

### Bagging model
#-----------------------------------
set.seed(749)
model.bag <- randomForest(y.factor ~ ., data=m.train, mtry=14)
pred.bag <- predict(model.bag, x.m.test, type = "class")
saveRDS(model.bag, "model_bag.rds")
confusionMatrix(pred.bag, y.m.test)


set.seed(749)
train.bag <- train(y.factor ~ ., data=m.train, 
                   method="treebag",tuneLength=4,
                   trControl=ctrl)
train.bag
saveRDS(train.bag, "train_bag.rds")



### RandomForest model
#-----------------------------------
set.seed(749)
model.rf <- randomForest(y.factor ~ ., data=m.train, mtry=4)
pred.rf <- predict(model.rf, x.m.test, type="class")
saveRDS(model.rf, "model_rf.rds")
confusionMatrix(pred.rf, y.m.test)

set.seed(749)
train.rf <- train(y.factor ~ ., data=m.train, 
                  method="rf", tuneLength=4,
                  trControl=ctrl)
train.rf
saveRDS(train.rf, "train_rf.rds")



### Boosting model
#-----------------------------------
library(gbm)
set.seed(749)
model.boo <- gbm(y.factor ~ ., data=m.train)
summary(model.boo) #Fig23
saveRDS(model.boo, "model_boo.rds")
pred.boo <- predict(model.boo, x.m.test, n.trees = 100)
pred.boo <- data.frame(pred.boo) %>% 
  mutate(max = max.col(., ties.method = "last"), label = as.numeric(y.m.test))
confusionMatrix(as.factor(pred.boo$max), factor(as.numeric(y.m.test)))
rmse(pred.boo$max - as.numeric(y.m.test)) #1.024883


set.seed(749)
train.boo <- train(y.factor ~ ., data=m.train, 
                   method="gbm",tuneLength=4,
                   trControl=ctrl)
train.boo
saveRDS(train.boo, "train_boo.rds")



#### 0. DT Models Summary
#------------------------
train.rpart <- readRDS("train_rpart.rds")
train.bag <- readRDS("train_bag.rds")
train.rf <- readRDS("train_rf.rds")
train.boo <- readRDS("train_boo.rds")

dt.models<- list("DT"=train.rpart, "RF"=train.rf,
                 "BaggingTree"=train.bag, 
                 "BoostingTree" = train.boo)

dt.resamples<- resamples(dt.models)
summary(dt.resamples)

#plot performances
bwplot(dt.resamples) #Fig 35
saveRDS(dt.resamples, "dt_resamples.rds")




## Dummyfy Variables
#-----------------------------------------------------
# remove y.numerical variable to generate dummy dataset
y.numerical <- as.numeric(m.train$y.factor)
m.train$y.factor <- NULL
m.train$as_of_year <- factor(m.train$as_of_year)
# Encode categorical values
m.train.dmy <- dummyVars(" ~ purchaser_type_name+
                              preapproval_name+
                              owner_occupancy_name+
                              lien_status_name+
                              co_applicant_race_name_1+
                              as_of_year+
                              applicant_sex_name+
                              applicant_race_name_1+
                              agency_abbr",
                         m.train, fullRank = T)
m.train.dmy <- data.frame(predict(m.train.dmy, newdata = m.train))
table(is.na(m.train.dmy)) #187684 * 35
nrow(cc(m.train.dmy)) #187684
nrow(ic(m.train.dmy)) #0

# Combine with the y variable back
m.train.dmy <- cbind(y.numerical ,m.train.dmy)
# Collinearity
lm.model <- lm(y.numerical ~ ., data=m.train.dmy)
vif(lm.model)
alias(lm.model)
#co_applicant_race_name_1.Not.applicable
m.train.dmy[,"co_applicant_race_name_1.Not.applicable"] <- NULL
#m.train.dmy[,"purchaser_type_name.Farmer.Mac..FAMC."] <- NULL

nrow(cc(m.train.dmy)) #187684
nrow(ic(m.train.dmy)) #0
dim(m.train.dmy) #187684*35
str(m.train.dmy)

# Do the same with the test set
y.m.test.numerical <- as.numeric(y.m.test)
x.m.test$as_of_year <- factor(x.m.test$as_of_year)
x.m.test.dmy <- dummyVars(" ~ purchaser_type_name+
                              preapproval_name+
                              owner_occupancy_name+
                              lien_status_name+
                              co_applicant_race_name_1+
                              as_of_year+
                              applicant_sex_name+
                              applicant_race_name_1+
                              agency_abbr",
                          x.m.test, fullRank = T)
x.m.test.dmy <- data.frame(predict(x.m.test.dmy, newdata = x.m.test))
x.m.test.dmy[,"co_applicant_race_name_1.Not.applicable"] <- NULL
#x.m.test.dmy[,"purchaser_type_name.Farmer.Mac..FAMC."] <- NULL

## recheck collinearity 
lm.model <- lm(y.numerical ~ ., data=m.train.dmy)
vif(lm.model)
alias(lm.model)
m.cor <- cor(as.data.frame(lapply(m.train.dmy, as.numeric)))
tooHigh <- findCorrelation(m.cor, .9, names=FALSE)
tooHigh

m.train.dmy <- m.train.dmy[, -tooHigh]
x.m.test.dmy <- x.m.test.dmy[, -(tooHigh-1)]
dim(m.train.dmy) #187684     34
dim(x.m.test.dmy) #29056    33


#save dataset before smote
write.csv(m.train.dmy, 'm_train_afterDUMMY.csv')
saveRDS(m.train.dmy, "m_train_afterDUMMY.rds")
m.train.dmy <- readRDS("m_train_afterDUMMY.rds")
table(m.train.dmy$y.numerical)

write.csv(x.m.test.dmy, "m_test_afterDUMMY.csv")
saveRDS(x.m.test.dmy, "m_test_afterDUMMY.rds")
x.m.test.dmy <- readRDS("m_test_afterDUMMY.rds")
y.m.test.lin <- as.numeric(y.m.test)
saveRDS(y.m.test.lin, "y_m_test_afterDUMMY.rds")
y.m.test.lin <- readRDS("y_m_test_afterDUMMY.rds")


## Principal Component Analysis
m.pca <- prcomp(m.train.dmy, center = TRUE,scale. = TRUE)
fviz_eig(m.pca)
ggbiplot(m.pca, ellipse=TRUE,  groups=m.train.dmy$y.numerical) #Fig43


### Linear Regression
#------------------------
set.seed(749)
#regular regression

model.lm <- lm(y.numerical ~ ., data=m.train.dmy, type='response')
summary(model.lm)
round(model.lm$coefficients, 2)
rmse(resid(model.lm)) # 0.920968
qqnorm(model.lm$residuals) # A quantile normal plot - good for checking normality
qqline(resid(model.lm)) #Fig12
varImp(model.lm)

pred.lm <- predict(model.lm, newdata=x.m.test.dmy, type='response')
saveRDS(model.lm, "model_lm.rds")
plot(pred.lm, y.m.test.lin) #Fig15

pred.lm <- data.frame(pred.lm) %>% mutate(max=max.col(0), label=y.m.test.lin)
dim(pred.lm)
table.lm <- table(factor(as.integer(pred.lm$pred.lm)), factor(y.m.test.lin))
table.lm
saveRDS(table.lm, "table_lm.rds")

set.seed(749)
train.lm <- train(y.numerical ~ ., data=m.train.dmy, 
                  method="lm",
                  trControl=ctrl)
perf.lm <-getTrainPerf(train.lm)
saveRDS(train.lm, "train_lm.rds")



### Ridge Regression
#------------------------

set.seed(749)
train.ridge <- train(y.numerical ~ ., 
                     preProcess=c("scale"),
                     data= m.train.dmy, 
                     method = "ridge", tuneLength=4, trControl=ctrl)
summary(train.ridge)
rmse(resid(train.ridge)) # 0.920968
plot(density(resid(train.ridge))) #A density plot
qqnorm(resid(train.ridge)) # A quantile normal plot - good for checking normality
qqline(resid(train.ridge)) #Fig12
varImp(train.ridge)
perf.ridge <-getTrainPerf(train.ridge)
saveRDS(train.ridge, "train_ridge.rds")
saveRDS(perf.ridge, "perf_ridge.rds")

pred.ridge <- predict(train.ridge, newdata = x.m.test.dmy)
plot(pred.ridge, y.m.test.lin) #Fig15


pred.ridge <- data.frame(pred.ridge) %>% mutate(max=max.col(0), label=y.m.test.lin)
dim(pred.ridge)
table.ridge <- table(factor(as.integer(pred.ridge$pred.ridge)), factor(y.m.test.lin))
table.ridge
saveRDS(table.ridge, "table_ridge.rds")




### Lasso Regression
#---------------------
set.seed(749)
train.lasso <- train(y.numerical ~ ., data=m.train.dmy, 
                     method="lasso",tuneLength=4,
                     trControl=ctrl)

summary(train.lasso)
rmse(resid(train.lasso)) # 0.9209896
plot(density(resid(train.lasso))) #A density plot
qqnorm(resid(train.lasso)) # A quantile normal plot - good for checking normality
qqline(resid(train.lasso)) #Fig12
varImp(train.lasso)
perf.lasso <-getTrainPerf(train.lasso)
saveRDS(train.lasso, "train_lasso.rds")
saveRDS(perf.lasso, "perf_lasso.rds")

pred.lasso <- predict(train.lasso, newdata = x.m.test.dmy)
plot(pred.lasso, y.m.test.lin) #Fig15


pred.lasso <- data.frame(pred.lasso) %>% mutate(max=max.col(0), label=y.m.test.lin)
dim(pred.lasso)
table.lasso <- table(factor(as.integer(pred.lasso$pred.lasso)), factor(y.m.test.lin))
table.lasso
saveRDS(table.lasso, "table_lasso.rds")


### Smooth Spline
#------------------------
set.seed(749)
train.ss <- train(y.numerical ~ ., data=m.train.dmy, 
                  method="gamSpline",tuneLength=4,
                  trControl=ctrl)
summary(train.ss)
rmse(resid(train.ss)) # 0.942762
plot(density(resid(train.ss))) #A density plot
qqnorm(resid(train.ss)) # A quantile normal plot - good for checking normality
qqline(resid(train.ss)) #Fig12
varImp(train.ss)
perf.ss <-getTrainPerf(train.ss)
saveRDS(train.ss, "train_ss.rds")
saveRDS(perf.ss, "perf_ss.rds")

pred.ss <- predict(train.ss, newdata = x.m.test.dmy)
plot(pred.ss, y.m.test.lin) #Fig15


pred.ss <- data.frame(pred.ss) %>% mutate(max=max.col(0), label=y.m.test.lin)
dim(pred.ss)
table.ss <- table(factor(as.integer(pred.ss$pred.ss)), factor(y.m.test.lin))
table.ss
saveRDS(table.ss, "table_ss.rds")



#### 0. Linear Models Summary
#------------------------
train.lm <- readRDS("train_lm.rds")
table.ridge <- readRDS("table_ridge.rds")
table.lasso <- readRDS("table_lasso.rds")
train.ss <- readRDS("train_ss.rds")

lin.models<- list("linear"=train.lm, "lasso"=train.lasso, 
                  "ridge"=train.ridge, "smooth"=train.ss)
 
lin.resamples<- resamples(lin.models)
summary(lin.resamples)

#plot performances
bwplot(lin.resamples, metric="RMSE") #Fig 41
bwplot(lin.resamples, metric="Rsquared") #Fig 42



### Logistic Regression
#------------------------
set.seed(749)
model.log <- multinom(y.numerical ~ ., data = m.train.dmy)

summary(model.log)
round(coef(model.log), 4)
exp(coef(model.log))
varImp(model.log)

saveRDS(model.log, "model_log.rds")
model.log <- readRDS("model_log.rds")

pred.log <- predict(model.log, newdata = x.m.test.dmy)
table.log <- table(pred.log, y.m.test.lin)
confusionMatrix(table.log)

set.seed(749)
train.log <-  train(factor(y.numerical) ~ ., data=m.train.dmy, method="multinom", 
                    family="binomial", trControl=ctrl)
train.log
saveRDS(train.log, "train_log.rds")
train.log <- readRDS("train_log.rds")



### Linear Discriminant Analysis
#------------------------
set.seed(749)
model.lda <- lda(y.numerical ~ ., data = m.train.dmy)
plot(model.lda) #Fig16
varImp(model.lda)


saveRDS(model.lda, "model_lda.rds")
model.lda <- readRDS("model_lda.rds")

pred.lda <- predict(model.lda, newdata = x.m.test.dmy, type = "response")
table.lda <- table(pred.lda$class, y.m.test.lin)
table.lda
plot(table.lda) #Fig16-2
saveRDS(table.lda, "table_lda.rds")


set.seed(749)
train.lda <-  train(factor(y.numerical) ~ ., data=m.train.dmy, 
                    method="lda", trControl=ctrl)
train.lda
saveRDS(train.lda, "train_lda.rds")
train.lda <- readRDS("train_lda.rds")


### KNN

#------------------------
set.seed(749)

wss <- numeric(10)
for (k in 1:10)
{ 
  clust <- kmeans(m.train.dmy, centers=k, nstart=25)
  wss[k] <- sum(clust$withness)
}

plot(1:10, wss, type='b', xlab="Number of Clusters", ylab="within Sum of Squares") #Fig 17
library(cluster)
set.seed(749)
# k = 4
train.kmeans.4 <- kmeans(m.train.dmy, 4, nstart=20)
clusplot(m.train.dmy, train.kmeans.4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) #Fig 18
test.kmeans.4 <- kmeans(x.m.test.dmy, 4, nstart=20)
table(test.kmeans.4$cluster, y.m.test.lin)

saveRDS(train.kmeans.4, "model_knn.rds")
train.kmeans.4 <- readRDS("model_knn.rds")
saveRDS(test.kmeans.4, "test_knn.rds")

set.seed(749)
train.knn <-  train(factor(y.numerical) ~ ., data=m.train.dmy, 
                    method="knn", trControl=ctrl)
train.knn
saveRDS(train.knn, "train_knn.rds")
train.knn <- readRDS("train_knn.rds")


#### 0. Linear Models Summary
#------------------------
train.log <- readRDS("train_log.rds")
train.lda <- readRDS("train_lda.rds")
table.knn <- readRDS("train_knn.rds")

non.models<- list("logistic"=train.log, "LDA"=train.lda)
 
non.resamples<- resamples(non.models)
summary(non.resamples)
saveRDS(non.resamples, "non_resamples.rds")
#plot performances
bwplot(non.resamples) #Fig 44


#stopCluster(c1)
#registerDoSEQ()


