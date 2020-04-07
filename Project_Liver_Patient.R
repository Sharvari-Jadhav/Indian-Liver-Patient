### Importing the dataset ###
liver_data <- read.csv("Indian Liver Patient Dataset (ILPD).csv")

### Changing column names are easy understanding ###
colnames(liver_data) <- c("Age","Sex","Total_Bil","Dir_Bil","Alkphos","Alamine","Asparate","Tot_Prot","Albumin","A_G_Ratio","Disease")

### Short form of Male and Female ###
liver_data$Sex <- (ifelse(liver_data$Sex == "Male","M","F"))

### Converting the disease prediction from 1s and 2s to 0s and 1s ###
liver_data$Disease <- as.numeric(ifelse(liver_data$Disease == 2,0,1))

### Summay of the dataset ###
summary(liver_data)

### Finding how many NAs are there in dataset ###
sapply(liver_data,function(x) sum(is.na(x)))

### Table to find the number of people who have the diesease ###
table(liver_data$Disease)

### Imputing NA values in A_G_Ratio to 1 ###
liver_data$A_G_Ratio[is.na(liver_data$A_G_Ratio)] <- mean(liver_data$A_G_Ratio,na.rm = TRUE)

### Find the values of mean,median,minimum,maximum and standard deviation ###
data.frame(mean=sapply(liver_data, mean),
           sd=sapply(liver_data, sd),
           min=sapply(liver_data, min),
           max=sapply(liver_data, max),
           median=sapply(liver_data, median))

### Finding the column data types ###
split(names(liver_data),sapply(liver_data, function(x) paste(class(x), collapse=" ")))

### Finding Correlations between variables ###
liver_data_1 <- liver_data[,-c(2)] ### Removed Gender column to find correlation
corrl <- round(cor(liver_data_1),2)
corrl

### Plot for correlation ###
library(plyr)
library(psych)
corPlot(liver_data_1)

highCorrCols <- findCorrelation(corrl, cutoff = 0.75, names=TRUE)
highCorrCols

liver_data_1 = liver_data_1[, !(names(liver_data_1) %in% highCorrCols)]
liver_data = liver_data[, !(names(liver_data) %in% highCorrCols)]

### Viewing the histograms ###
multi.hist(liver_data[,sapply(liver_data, is.numeric)])

hist(log(liver_data$Total_Bil))
hist((liver_data$Total_Bil))


### Finding PCA for better analysis ###
pcs <- prcomp(na.omit(liver_data_1))
summary(pcs)

### Splitting training data and validation data ###
library(caTools)
set.seed(123)
split = sample.split(liver_data$Disease, SplitRatio = 0.7)
training_set = subset(liver_data, split == TRUE)
validation_set = subset(liver_data, split == FALSE)

### Decision tree creation for training set ###
library(rpart)
library(rpart.plot)
decitree <- rpart(Disease ~ ., data = training_set , method = "class" , cp = 0 , maxdepth = 7)

prp(decitree , type = 1,extra = 1, under = TRUE , box.col = "blue")

### Confusion Matrix for training set ###
default.train.data <- predict(decitree,training_set,type = "class")
library(caret)
confusionMatrix(default.train.data,as.factor(training_set$Disease))

### Decision tree creation for validation set ###
#decitree_test <- rpart(Disease ~ ., data = validation_set , method = "class" , cp = 0 , maxdepth = 7)

#prp(decitree_test , type = 1,extra = 1, under = TRUE , box.col = "blue")

### Confusion Matrix for validation set ###
default.valid.data <- predict(decitree,validation_set,type = "class")

confusionMatrix(default.valid.data,as.factor(validation_set$Disease))

#################### Splitting and normalizing ###############
set.seed(455)
liver_data$Splits <- sample.split(liver_data, SplitRatio = 0.7)
library(dplyr)

liver_data <- liver_data %>% mutate_each(funs(log), -Age, -Sex, -Albumin, -A_G_Ratio, 
                                         -Disease, -Splits)

training_set_logic <-  subset(liver_data,split == TRUE)
validation_set_logic <-  subset(liver_data,split == FALSE)

######################## training ##########################

logit.reg <- glm(Disease ~ Age + Sex + Total_Bil + Dir_Bil + Alkphos + Alamine + Asparate + 
                   Tot_Prot + Albumin + A_G_Ratio, data = training_set_logic , family = binomial(link = "logit"))


#training_set_logic$Disease <- as.numeric(ifelse(training_set_logic$Disease == 2,0,1))
logit.reg.pred.train <- predict(logit.reg,training_set_logic)
confusionMatrix(as.factor(ifelse(logit.reg.pred.train>0.5,1,0)),as.factor(training_set_logic$Disease))

######################## validation ##########################

#logit.reg_valid <- glm(Disease ~ Age + Sex + Total_Bil + Dir_Bil + Alkphos + Alamine + Asparate + 
#                         Tot_Prot + Albumin + A_G_Ratio, data = validation_set_logic , family = binomial(link = "logit"))


#validation_set_logic$Disease <- as.numeric(ifelse(validation_set_logic$Disease == 2,0,1))
logit.reg.pred.valid <- predict(logit.reg,validation_set_logic)
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid>0.5,1,0)),as.factor(validation_set_logic$Disease))

##########################################################

### ROC ###
library(pROC)
r <- roc(validation_set_logic$Disease,logit.reg.pred.valid)
plot.roc(r)

auc(r)

r1 <- roc(training_set_logic$Disease,logit.reg.pred.train)
plot.roc(r1)

auc(r1)

#########################################################
