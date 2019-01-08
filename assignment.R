`
setwd("~/BSFI_Assignment");

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

##########################################################################

## Model Building   

##---------Logistic Regression----------#

# Required Packages

library(caret)
library(caTools)
library(dummies)

bank_data <- bank_data[, -21]


#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

train1<-train[,-45]

library(MASS)

library(car)


#---------------------------------------------------------    

### Model 1: Logistic Regression

logistic_1_1 <- glm(response ~.,family = 'binomial',data = train1)

summary(logistic_1_1)

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1_1, direction = "both")
summary(logistic_2)

# stepAIC has removed some variables and only the following ones remain
vif(logistic_2)


#Removing least significant variable
logistic_3 <- glm(formula = response ~ educationPrimary_Education+contactcellular+monthaug + 
                    monthdec+monthjun+monthmar
                    +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure + poutcomenonexistent + emp.var.rate 
                    +cons.price.idx + cons.conf.idx + euribor3m + nr.employed 
                    +maritalsingle + monthsep +jobservices+jobunknown +`jobblue-collar`+ day_of_weekwed +housingyes , family = "binomial", data = train1)

summary(logistic_3)

vif(logistic_3)

#Removing housingyes as it is least significant
logistic_4 <- glm(formula = response ~ educationPrimary_Education+contactcellular+monthaug + 
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure + poutcomenonexistent + emp.var.rate 
                  +cons.price.idx + cons.conf.idx + euribor3m + nr.employed 
                  +maritalsingle + monthsep +jobservices+jobunknown +`jobblue-collar`+day_of_weekwed, family = "binomial", data = train1)

summary(logistic_4)
vif(logistic_4)

#removing least significant variable educationPrimary_Education
logistic_5 <- glm(formula = response ~  +monthaug + contactcellular+
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure + poutcomenonexistent + emp.var.rate 
                  +cons.price.idx + cons.conf.idx + euribor3m + nr.employed 
                  +maritalsingle + monthsep +jobservices+jobunknown +`jobblue-collar`+day_of_weekwed, family = "binomial", data = train1)
summary(logistic_5)
vif(logistic_5)

#removing least significant variable poutcomenonexistent
logistic_6 <- glm(formula = response ~  +monthaug + contactcellular+
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure  + emp.var.rate 
                  +cons.price.idx + cons.conf.idx + euribor3m + nr.employed 
                  +maritalsingle + monthsep +jobservices+jobunknown +`jobblue-collar`+day_of_weekwed, family = "binomial", data = train1)
summary(logistic_6)
vif(logistic_6)

#removing least significant element day_of_weekwed
logistic_7 <- glm(formula = response ~  +monthaug + contactcellular+
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure  + emp.var.rate 
                  +cons.price.idx + cons.conf.idx + euribor3m + nr.employed 
                  +maritalsingle + monthsep +jobservices+jobunknown +`jobblue-collar`, family = "binomial", data = train1)
summary(logistic_7)
vif(logistic_7)
#removing least significant element euribor3m

logistic_8 <- glm(formula = response ~  +monthaug + contactcellular+
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure  + emp.var.rate 
                  +cons.price.idx + cons.conf.idx   + nr.employed 
                  +maritalsingle + monthsep +jobservices+jobunknown +`jobblue-collar`, family = "binomial", data = train1)
summary(logistic_8)
vif(logistic_8)
#removing least significant element jobunknown
logistic_9 <- glm(formula = response ~  +monthaug + contactcellular+
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure  + emp.var.rate 
                  +cons.price.idx + cons.conf.idx   + nr.employed 
                  +maritalsingle + monthsep +jobservices+`jobblue-collar`, family = "binomial", data = train1)
summary(logistic_9)
vif(logistic_9)


#removing least significant element jobservices
logistic_10 <- glm(formula = response ~  +monthaug + contactcellular+
                    monthdec+monthjun+monthmar
                  +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure  + emp.var.rate 
                  +cons.price.idx + cons.conf.idx   + nr.employed 
                  +maritalsingle + monthsep +`jobblue-collar`, family = "binomial", data = train1)
summary(logistic_10)
vif(logistic_10)

#removing least significant element maritalsingle
logistic_11 <- glm(formula = response ~  +monthaug + contactcellular+
                     monthdec+monthjun+monthmar
                   +monthnov+ day_of_weekmon + campaign + pdays + poutcomefailure  + emp.var.rate 
                   +cons.price.idx + cons.conf.idx   + nr.employed 
                    + monthsep +`jobblue-collar`, family = "binomial", data = train1)
summary(logistic_11)
vif(logistic_11)

predictions_logit <- predict(logistic_11, newdata = test[, -61], type = "response")
predictions_logit_2<-predict(logistic_11, newdata =train[,-61],type = "response")

summary(predictions_logit)


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))
predicted_response_2 <- factor(ifelse(predictions_logit_2 >= 0.50, "yes", "no"))


# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf_2 <- confusionMatrix(predicted_response_2, train$response, positive = "yes")

conf
conf_2


perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
perform_fn_2 <- function(cutoff) 
{
  predicted_response_2 <- factor(ifelse(predictions_logit_2 >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response_2, train$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
for(i in 1:100)
{
  OUT[i,] = perform_fn_2(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 12% for final model

predicted_response<- factor(ifelse(predictions_logit >= 0.128, "yes", "no"))

predicted_response_2<- factor(ifelse(predictions_logit_2 >= 0.128, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf_final_2 <- confusionMatrix(predicted_response_2, train$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
acc_2 <- conf_final_2$overall[1]

sens_2 <- conf_final_2$byClass[1]

spec_2 <- conf_final_2$byClass[2]

acc
acc_2
sens
sens_2
spec
spec_2

varImp(logistic_11,scale = FALSE)
imp <- as.data.frame(varImp(logistic_11))
imp <- data.frame(names   = rownames(imp), overall = imp$Overall)
imp[order(imp$overall,decreasing = T),]
#names   overall
#9             pdays 16.278861
#5          monthmar 14.725732
#12   cons.price.idx 14.532166
#11     emp.var.rate 14.529991
#2   contactcellular 14.311715
#13    cons.conf.idx 10.342295
#10  poutcomefailure  9.305595
#14      nr.employed  8.551855
#1          monthaug  8.216708
#4          monthjun  6.545492
#7    day_of_weekmon  5.394758
#8          campaign  4.259339
#15         monthsep  4.252285
#16 `jobblue-collar`  3.906371
#6          monthnov  3.572966
#3          monthdec  3.486316
#---------------------------------------------------------    
# ------Model Evaluation----------------------------------

# Appending the probabilities and response variables to the test data

test$predicted_probs <- predictions_logit

test$predicted_response <- predicted_response
train$predicted_probs <- predictions_logit_2

train$predicted_response <- predicted_response_2

#--------------------------------------------------------- 
#Calculation of call
Cost_per_call<- 0.033*test$duration + 0.8
test$Cost_per_call<-Cost_per_call
test$ID <- seq.int(nrow(test))

Cost_per_call_2<- 0.033*train$duration + 0.8
train$Cost_per_call<-Cost_per_call_2
train$ID <- seq.int(nrow(train))



# Creating new dataframe "test_predictions  
#prospect ID, actual response, predicted response, predicted probability of response, duration of call in seconds, and cost of call"

test_predictions_1 <- test[, c("ID","response", "predicted_probs", "predicted_response","duration","Cost_per_call")]
test_predictions_2 <- train[, c("ID","response", "predicted_probs", "predicted_response","duration","Cost_per_call")]
prediction <- rbind(test_predictions_1, test_predictions_2)

# sorting the probabilities in decreasing order 
prediction <- prediction[order(prediction$predicted_probs, decreasing = T), ]

#Downloading the data 
write.csv(prediction,"prediction.csv")

summary(prediction$response[1:6800])
summary(prediction$predicted_response[1:6800])


# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}
top80_prospect_count <- 4119 + 4119 +4119 +4119 +4118 #20594
avg_cost <- sum(prediction[1:20594, 6])/3793 # 53.20 #(Total cost of campaign for top 80%/ Number prospects achieved ) 
#Avg call duration for targeting top 80% propsects 
avg_duration <- mean(prediction[1:20594, 5]) #272.6972 seconds


prediction$response <- as.factor(ifelse(prediction$response=="yes",1,0))

LG = lift(prediction$response, prediction$predicted_probs, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")



# The Cumulative Lift of 1.624569 for top 5  deciles,
# means that when selecting 80% of the records based on the model, 
# one can expect 1.5 times the total number of targets (events) found by randomly 
# selecting 80%-of-records without a model.

### Analyzing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers. For example,
# by contacting only 10% of customers based on the predictive model we will reach 
# 4.409483 times as many respondents as if we use no model.

#To achieve Business objective to target 80% of prospects, one has to  target top 5 deciles that will cover 81.7% prospects

# Avg call duration will be the average of the time taken by top 5 deciles -- 272.6972 seconds




