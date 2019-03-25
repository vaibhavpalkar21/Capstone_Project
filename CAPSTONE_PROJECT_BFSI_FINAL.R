##########CAPSTONE BFSI PROJECT BY - Pradnya Paithankar, Vaibhav Palkar, Dr Rajendra Warke##########################

########Loading Libraries############33333
library("dplyr")
library("ggplot2")
library("Information")
library("caTools")
library(corrplot)
library("woe")
library("woeBinning")
library("scorecard")
library("MASS")
library(dummies)
library(caret)
library(randomForest)

##########Read files#####
credit<-read.csv("Credit Bureau data.csv", header=TRUE,sep=",", strip.white=TRUE ,na.strings= c("NA",""))
demographic<-read.csv("Demographic data.csv", header=TRUE,sep=",",strip.white=TRUE ,na.strings= c("NA",""))

##############DATA EXPLORATION AND CLEANING######################

#Check for duplicate application IDs
credit[which(duplicated(credit$Application.ID)),1]
#765011468 653287861 671989187
demographic[which(duplicated(demographic$Application.ID)),1]
#765011468 653287861 671989187
## remove duplicate app ids
credit<-credit[-which(credit$Application.ID %in% c(765011468, 653287861, 671989187)),]
demographic<-demographic[-which(demographic$Application.ID %in% c(765011468, 653287861, 671989187)),]
# check if the application ID from both the data frames are exactly same
sum(credit$Application.ID!=demographic$Application.ID)
#0 Means same application ID

# merger for creating master data frame
master<-merge(demographic,credit)
length(which(duplicated(customer$Application.ID)))
str(master)
#71289 observations of 29 variables
unique(master$Performance.Tag)
#0 NA 1   --> NA represents the applications which were not approved for credit cards
# 1 is default

sum(is.na(master$Performance.Tag))
#1425 rejected applications


table(master$Performance.Tag)
# 0     1 
# 66917  2947 

# Removing rejected applications prior to further processing
customer<-dplyr::filter(master,!is.na(Performance.Tag ))
#69864 records


############## Basic data cleaning

length(row.names(customer[apply(customer,1,anyNA),]))
##1171 rows have some NA records

# checking for individual fields

# Age
unique(customer$Age)
# some records have age as 0 and -3
length(which(customer$Age<=0))
# 20 records. very less. So replacing with MODAL category
table(customer$Age)

# modal age is 39

#Replace with records with 0 and -3 age with modal category age
customer[which(customer$Age<=0),"Age"]<-39

## Checking for NA values for columns (blank / missing)
colnames(customer[apply(customer,2,anyNA)])
# [1] "Gender"                                     
# [2] "Marital.Status..at.the.time.of.application."
# [3] "No.of.dependents"                           
# [4] "Education"                                  
# [5] "Profession"                                 
# [6] "Type.of.residence"                          
# [7] "Avgas.CC.Utilization.in.last.12.months"     
# [8] "No.of.trades.opened.in.last.6.months"       
# [9] "Presence.of.open.home.loan"                 
# [10] "Outstanding.Balance"   


#Gender
sum(is.na(customer$Gender))
# Just 2 record so remove
customer<-customer[-which(is.na(customer$Gender)),]


# Education
sum(is.na(customer$Education))
#118 NA records


# Checking other values
table(customer$Education)
# Bachelor      Masters       Others          Phd Professional 
# 17298        23481          119         4463        24383 

# replacing education NA with MODAL class that is Professional
##Merge with professionals

customer[which(is.na(customer$Education)),"Education"]<-"Professional"
table(customer$Education)


#marital status
sum(is.na(customer$Marital.Status..at.the.time.of.application.))
## Just 6 NA records
table(customer$Marital.Status..at.the.time.of.application.)
# Married  Single 
# 59540   10316 
# Replacing NA records MODAL status MARRIED
customer[which(is.na(customer$Marital.Status..at.the.time.of.application.)),"Marital.Status..at.the.time.of.application."]<-"Married"


#type of residence
sum(is.na(customer$Type.of.residence))
# 8 records
table(customer$Type.of.residence)
# Company provided Living with Parents              Others               Owned 
# 1628                1818                 199               14235 
# Rented 
# 53378 
#Replacing NA type of residence records with MODAL category Rented

customer[which(is.na(customer$Type.of.residence)),"Type.of.residence"]<-"Rented"


# Dependents
sum(is.na(customer$No.of.dependents))
# Just 3 records
table(customer$No.of.dependents)
# 1     2     3     4     5 
# 15214 15120 15638 11993 11871 
#Removing 3 records with NA dependents
customer<-customer[-which(is.na(customer$No.of.dependents)),]


# Profession
sum(is.na(customer$Profession))
# Just 13 records
table(customer$Profession)
# SAL      SE SE_PROF 
# 40424   14303   16524 
#Replacing NA with MODAL profession
customer[which(is.na(customer$Profession)),"Profession"]<-"SAL"
## profession NA -merge with mode


# Averge CC Utiliization
sum(is.na(customer$Avgas.CC.Utilization.in.last.12.months))
#1023 significant number of records (1.5%)
### These applicants are not having any other credit card
# Not to be deleted. To be treated separately or with WOE transformation
### OR understand its link with others and take average
## Understanding distribution of CC utiliization
table(customer$Avgas.CC.Utilization.in.last.12.months)
quantile(customer$Avgas.CC.Utilization.in.last.12.months,probs=seq(0,1,0.01),na.rm=TRUE)
# No outliers BUT signifinat number of records / concentration at the highest value of CC utillization 113-- TO be understood and treated separately

## Checking values of other related variables where CC utillization is NA

no_cc_cust<-customer[which(is.na(customer$Avgas.CC.Utilization.in.last.12.months)),c("Total.No.of.Trades","Outstanding.Balance","Presence.of.open.home.loan","Presence.of.open.auto.loan","Performance.Tag")]
summary(no_cc_cust)
#272 NA records found in each of Outstanding.Balance,  Presence.of.open.home.loan
# Checking if both these NA are for same records
summary(filter(no_cc_cust,is.na(no_cc_cust$Outstanding.Balance))$"Presence.of.open.home.loan")
## ALL 272 NA. Means records match
table(filter(no_cc_cust,is.na(no_cc_cust$Outstanding.Balance))$"Total.No.of.Trades")
## All these records do not have any trades
#######272 records do not have any record of loans with credit bureau
table(filter(customer,is.na(customer$Outstanding.Balance))$"Performance.Tag")
# 0   1 
# 264   8 

table(filter(customer,Avgas.CC.Utilization.in.last.12.months==13)$"Performance.Tag")
# 0    1 
# 2166   61 

# No of trades opened in last 6 months
sum(is.na(customer$No.of.trades.opened.in.last.6.months))
# Just 1 record - so delete
customer<-customer[-which(is.na(customer$No.of.trades.opened.in.last.6.months)),]

# Presense of open home loan
table(as.factor(customer$Presence.of.open.home.loan))
# 0     1 
# 52776 18202 
sum(is.na(customer$Presence.of.open.home.loan))
#272 records - These could be same as records having total no of trades and outstanding balance as 0
table(filter(customer,is.na(customer$Presence.of.open.home.loan))$"Total.No.of.Trades")
#  0 
#272 
## Confirmed. These records to be treated separately


# Income
# replacing negative income (-0.5) with average income

summary(customer$Income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.5    14.0    27.0    27.2    40.0    60.0 
quantile(customer$Income,probs=seq(0,1,0.01))
#0% quantile is -0.5
length(which(customer$Income<0))
customer[which(customer$Income<0),"Income"]
##81 records having income as -0.5
#Checking age and profession for these records
table(customer[which(customer$Income == -0.5 ),c("Age","Profession")])
## No specific pattern observed
summary(customer[which(customer$Income == -0.5 ),"Outstanding.Balance"])
#No NA outstanding
# Replacing  with mean value
customer[which(customer$Income<0),"Income"]<-mean(customer$Income)


# number of months in current residence
summary(customer$No.of.months.in.current.residence)
quantile(customer$No.of.months.in.current.residence,probs=seq(0,1,0.01))
# no outliers


#number of months in current company
summary(customer$No.of.months.in.current.company)
quantile(customer$No.of.months.in.current.company,probs=seq(0,1,0.01))
# Outlier (133) at 100 percentile, understanding in depth
quantile(customer$No.of.months.in.current.company,probs=seq(.99,1,0.0005))
# Outlier at exact 100 percentile
length(which(customer$No.of.months.in.current.company>75))
#10 outlier records
# CApping outlier in No.of.months.in.current.company with the value at 99.95 percentile
customer[which(customer$No.of.months.in.current.company>75),"No.of.months.in.current.company"] = 75

str(customer)
nrow(customer)
## 69858 rows after basic cleaning
table(customer$Performance.Tag)
# 0     1 
# 66911  2947 


################ EXPLORATORY DATA ANALYSIS#################

# univariate and bivariate analysis analysis
table(customer$Performance.Tag)
# 0     1 
# 66911  2947 

avg_default_rate<-sum(customer$Performance.Tag)*100/length(customer$Performance.Tag)
avg_default_rate
#4.218558
#default rate is 4.22%

#############Understanding distribution of factor variables and linkage with default rate#######

plot_basic<-function(cat_var, var_name){
  ggplot(customer, aes(cat_var)) + 
    geom_bar( fill="steelblue") +
    geom_text(stat="count",aes(label=..count..), size =3, vjust = 1) + xlab(var_name)
}
plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, customer, sum)
  b <- aggregate(Performance.Tag~cat_var, customer, length)
  colnames(a)[2] <- "total"
  colnames(b)[2] <- "count"
  ab <- merge(a,b)
  colnames(ab) <- c(var_name,"total","count")
  ab$Default.Rate <- round(ab$total*100/ab$count,2)
  ggplot(ab, aes(a[,1], Default.Rate, label = Default.Rate)) + 
    geom_bar(stat = 'identity',fill='plum') + geom_hline(yintercept = 4.22, color ="red") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = 2) + xlab(var_name)
  
}
plot_basic(customer$Gender, "Gender")
plot_response(customer$Gender, "Gender") # Female has default 4.35 rate above total default rate
plot_basic(customer$Marital.Status..at.the.time.of.application., "Marital Status")
plot_response(customer$Marital.Status..at.the.time.of.application., "Marital Status") #single has default rate 4.31 above total default rate
plot_response(customer$No.of.dependents, "Number of Dependents")
plot_basic(customer$No.of.dependents, "Number of Dependents") #dependent with 1,3&5 has default rate above total default rate
plot_response(customer$Education, "Education") # Others has default above average(6.72)
plot_basic(customer$Education, "Education")
plot_response(customer$Profession, "Profession") #SE has default rate 4.6 above total default rate
plot_basic(customer$Profession, "Profession")
plot_response(customer$Type.of.residence, "Type of Residence") #company provided(4.56) & living with parents(4.5) has default rate above total default rate
plot_basic(customer$Type.of.residence, "Type of Residence")
plot_response(customer$Presence.of.open.home.loan, "Presence of Open Home Loan")#people with NO open home loan has default rate above total default rate
plot_basic(customer$Presence.of.open.home.loan, "Presence of Open Home Loan")
plot_response(customer$Presence.of.open.auto.loan, "Presence of Open Auto Loan") #people with NO open auto loan has default rate above total default rate
plot_basic(customer$Presence.of.open.auto.loan, "Presence of Open Auto Loan")
plot_response(customer$Total.No.of.Trades, "Total Number of Trades") # with total trades as 6 to 15, 24,26,27, 31,32,33,39 has default rate above total default rate
plot_basic(customer$Total.No.of.Trades, "Total Number of Trades")
plot_response(factor(customer$No.of.times.90.DPD.or.worse.in.last.6.months), "DPD 90 days 6 months") # 1,2,3
plot_basic(factor(customer$No.of.times.90.DPD.or.worse.in.last.6.months), "DPD 90 days 6 months")
plot_response(factor(customer$No.of.times.90.DPD.or.worse.in.last.12.months), "DPD 90 days 12 months")# 1 to 5
plot_basic(factor(customer$No.of.times.90.DPD.or.worse.in.last.12.months), "DPD 90 days 12 months")
plot_response(factor(customer$No.of.times.30.DPD.or.worse.in.last.6.months), "DPD 30 days 6 months") #1 to 7
plot_basic(factor(customer$No.of.times.30.DPD.or.worse.in.last.6.months), "DPD 30 days 6 months")
plot_response(factor(customer$No.of.times.30.DPD.or.worse.in.last.12.months), "DPD 30 days 12 months") # 1 to 8
plot_basic(factor(customer$No.of.times.30.DPD.or.worse.in.last.12.months), "DPD 30 days 12 months")
plot_response(factor(customer$No.of.times.60.DPD.or.worse.in.last.6.months), "DPD 60 days 6 months") # 1 to 5
plot_basic(factor(customer$No.of.times.60.DPD.or.worse.in.last.6.months), "DPD 60 days 6 months") 
plot_response(factor(customer$No.of.times.60.DPD.or.worse.in.last.12.months), "DPD 60 days 12 months") # 1 to 6
plot_basic(factor(customer$No.of.times.60.DPD.or.worse.in.last.12.months), "DPD 60 days 12 months")
plot_response(factor(customer$No.of.trades.opened.in.last.6.months), "No of trades opened in last 6 months") # 5 to 9 and 11
plot_basic(factor(customer$No.of.trades.opened.in.last.6.months), "No of trades opened in last 6 months") 
plot_response(factor(customer$No.of.trades.opened.in.last.12.months), "No of trades opened in last 12 months") # 2 to 6, 8, 13, 14, 17, 23 to 28
plot_basic(factor(customer$No.of.trades.opened.in.last.12.months), "No of trades opened in last 12 months")
plot_response(factor(customer$No.of.PL.trades.opened.in.last.6.months), "No of PL trades opened in last 6 months") # 1 to 5
plot_basic(factor(customer$No.of.PL.trades.opened.in.last.6.months), "No of PL trades opened in last 6 months")
plot_response(factor(customer$No.of.PL.trades.opened.in.last.12.months), "No of PL trades opened in last 12 months") # 3 to 12
plot_basic(factor(customer$No.of.PL.trades.opened.in.last.12.months), "No of PL trades opened in last 12 months")
plot_response(factor(customer$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), "Inquiries last 6 months") # 1, 3 to 6
plot_basic(factor(customer$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), "Inquiries last 6 months")
plot_response(factor(customer$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), "Inquiries last 12 months") # 2,3,9,12,14to 19
plot_basic(factor(customer$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), "Inquiries last 12 months") # 2,3,9,12,14to 19

##########Understanding distribution of numeric variables and linkage with default rate#######

plot_num_basic<-function(num_var, var_name){
  ggplot(customer, aes(num_var)) + 
    geom_histogram( fill="steelblue", bins= 15, col="grey") + 
    #scale_x_continuous(breaks = round(seq(min(num_var),max(num_var),(max(num_var)-min(num_var))/15),0)) 
    xlab(var_name)
}
plot_num_basic(customer$Income,"Income")
plot_num_basic(customer$Age,"Age") 
plot_num_basic(customer$No.of.months.in.current.residence,"No of months in current residence") 
plot_num_basic(customer$No.of.months.in.current.company,"No of months in current company")
plot_num_basic(customer$Avgas.CC.Utilization.in.last.12.months,"Avg CC utilization") 
plot_num_basic(customer$Outstanding.Balance/10000,"Outstanding balance_10k") 
plot_num_basic(customer$Total.No.of.Trades,"Total no of trades") 


library(scales)
num_response<-function(num_var, var_name){
  a<-data.frame(tapply(customer$Performance.Tag, cut(num_var, 10,include.lowest=0,right=FALSE,ordered_result = FALSE), function(x)
  { round(sum(x)*100/length(x),2)}))
  d<-data.frame(cbind(rownames(a),a[,1]))
  colnames(d)<-c("bins","default_rate")
  ggplot(d, aes(factor(bins,levels=unique(bins)), as.numeric(as.character(default_rate)), label = default_rate)) + 
    geom_bar(stat = 'identity',fill='plum') + geom_hline(yintercept = 4.22, color ="red") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = 2) + xlab(var_name) + ylab("default_rate") + scale_y_continuous()
}


num_response(customer$Income,"Income") # income upto 30 has high default rate than total default
num_response(customer$Age,"Age") # for age 30-40 and 55-60 has highest default rate than total default rate
num_response(customer$No.of.months.in.current.residence,"No of months in current residence") #for age 18-54,55-66, 78-90, has highest default rate than total default rate
num_response(customer$No.of.months.in.current.company,"No of months in current company")# for 2.93-17.4, 31.8-39, 67.8-75.1
num_response(customer$Avgas.CC.Utilization.in.last.12.months,"Avg CC utilization") # for 22.6-113
num_response(customer$Outstanding.Balance/10000,"Outstanding balance_10k") # for outstanding balance(in 10k) 52-207, 313-522 has highest default rate thta total default rate
num_response(customer$Total.No.of.Trades,"Total no of trades") # for range 4.4-17.6, 30.8-35.2 have highest default rate that total default rate

#####Understanding correlation among variables#########3
customer_numeric<-customer[,c(3,6,7,11:25,27,28)]
column_names<-colnames(customer_numeric)
colnames(customer_numeric)<-seq(1,20,1)
correl_calc <- round(cor(customer_numeric,method= "spearman",use="pairwise.complete.obs"),1)

#configure color combination for the plot
plot.new()

col <- colorRampPalette(c("red","white","blue"))(20)
#plot correlation matrix
corrplot(correl_calc,method="color",tl.cex = 0.8,number.cex = 0.7,tl.col = "red",col=col,
         order = "FPC",type="upper",addCoef.col = "black",diag = T)


############ WOE and IV analysis for understanding variable importance#######
IV1 <- create_infotables(data=customer, y="Performance.Tag", bins=20, parallel=FALSE)
print(IV$Summary, row.names=FALSE)
### Lowest IV values denote least significant variable
# Variable           IV
# Avgas.CC.Utilization.in.last.12.months 3.255287e-01
# No.of.trades.opened.in.last.12.months 3.032720e-01
# No.of.PL.trades.opened.in.last.12.months 3.000029e-01
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 2.973641e-01
# Outstanding.Balance 2.867805e-01
# Total.No.of.Trades 2.521255e-01
# No.of.times.30.DPD.or.worse.in.last.6.months 2.435167e-01
# No.of.PL.trades.opened.in.last.6.months 2.217298e-01
# No.of.times.30.DPD.or.worse.in.last.12.months 2.173184e-01
# No.of.times.90.DPD.or.worse.in.last.12.months 2.141444e-01
# No.of.times.60.DPD.or.worse.in.last.6.months 2.095906e-01
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 2.081323e-01
# No.of.trades.opened.in.last.6.months 1.899855e-01
# No.of.times.60.DPD.or.worse.in.last.12.months 1.875938e-01
# No.of.times.90.DPD.or.worse.in.last.6.months 1.602474e-01
# No.of.months.in.current.residence 8.143580e-02
# Income 4.760147e-02
# No.of.months.in.current.company 2.800625e-02
# Presence.of.open.home.loan 1.722774e-02
# Age 9.159817e-03
# Application.ID 4.024345e-03
# No.of.dependents 2.820865e-03
# Profession 2.016834e-03
# Presence.of.open.auto.loan 1.692924e-03
# Type.of.residence 9.514340e-04
# Education 7.560776e-04
# Gender 3.523490e-04
# Marital.Status..at.the.time.of.application. 8.110856e-05

##### Plotting WOE values####
plot_infotables(IV1, IV1$Summary$Variable[1:6], same_scale=TRUE)
plot_infotables(IV1, IV1$Summary$Variable[7:12], same_scale=TRUE)
plot_infotables(IV1, IV1$Summary$Variable[13:18], same_scale=TRUE)
plot_infotables(IV1, IV1$Summary$Variable[19:24], same_scale=TRUE)
plot_infotables(IV1, IV1$Summary$Variable[25:length(IV1$Summary$Variable)], same_scale=TRUE)


##### END OF DATA EXPLORATION and CLEANING####################

#######DATA PREPARATION FOR MODEL BUILDING###################

## converting applicable columns to factor
customer$Presence.of.open.auto.loan<-as.factor(as.character(customer$Presence.of.open.auto.loan))
customer$Presence.of.open.home.loan<-as.factor(as.character(customer$Presence.of.open.home.loan))
customer$Performance.Tag<-as.factor(as.character(customer$Performance.Tag))



###############################################################3
######
######    LOGISTIC REGRESSION     #############################
######
######
##################################################################

### Logistic Regressoin model for Demographic data ####

# model building for demographic data
# columns for demographic datasets

# "Application.ID"
# "Age"                                        
# "Gender" 
# "Marital.Status..at.the.time.of.application."
# "No.of.dependents"
# "Income"    
# "Education"    
# "Profession"                                 
# "Type.of.residence"  
# "No.of.months.in.current.residence"          
# "No.of.months.in.current.company" 
# "Performance.Tag" 


# arrange columns
demographic_final <- customer[, c("Application.ID", "Age", "Gender", "Marital.Status..at.the.time.of.application.", "No.of.dependents", "Income",
                                  "Education", "Profession", "Type.of.residence", "No.of.months.in.current.residence", "No.of.months.in.current.company", "Performance.Tag")]
sum(is.na(demographic_final))
##0  No NA values

#Check for duplicate application IDs
sum(demographic_final[which(duplicated(demographic_final$Application.ID)),1])
# no duplicate records found

demographic_final <- demographic_final[, -1]
str(demographic_final)
#standardization of numeric data
demographic_final[,c(1,4,5,9,10)] <- sapply(demographic_final[,c(1,4,5,9,10)], function(x){scale(x)})

#creating dummy variables

demographic_final_dummy <- dummy.data.frame(demographic_final[,-11])

demographic_final_dummy$Performance.Tag <- demographic_final$Performance.Tag

# as.factor(ifelse(demographic_final_dummy$Performance.Tag == 1, "yes", "no"))

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(demographic_final_dummy$Performance.Tag, SplitRatio = 0.70)

train <- demographic_final_dummy[split_indices, ]

test <- demographic_final_dummy[!split_indices, ]

nrow(train)/nrow(demographic_final_dummy)
#70%
nrow(test)/nrow(demographic_final_dummy)
#30%

## generating models logistic regression
demo_model_1 = glm(Performance.Tag ~., data=train, family="binomial")
summary(demo_model_1)

# we will use step-wise function to remove the extremely insignificant variables
demo_model_2<- stepAIC(demo_model_1, direction="both")
summary(demo_model_2)

# all variables have a p value below 0.05, and the number of variables is also good.
# we can stop now

# significant variables are:
#  Income                            -0.19614    0.02324   -8.439  < 2e-16 ***
#  No.of.months.in.current.residence  0.05812    0.02209    2.632   0.0085 ** 
#  No.of.months.in.current.company   -0.11483    0.02288   -5.019 5.21e-07 ***

final_model <-demo_model_2

## MODEL EVALUATION

#Prediciton on test data
test_pred = predict(final_model, type = "response", newdata = test[,-23])

summary(test_pred)

# Putting prediction in test data frame
test$pred_probability <- test_pred

# range of prediction is 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02150 0.03504 0.04123 0.04213 0.04835 0.07254  

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of mean

test_predicted_perf_tag <- factor(ifelse(test_pred >= 0.04213, "yes", "no"))

test$Performance.Tag <- as.factor(ifelse(test$Performance.Tag == 1, "yes", "no"))

levels(test_predicted_perf_tag)
levels(test$Performance.Tag)
table(test$Performance.Tag)
# # Creating confusion matrix for identifying the model evaluation.
library(caret)

conf <- confusionMatrix(test_predicted_perf_tag, test$Performance.Tag, positive = "yes")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  test_predicted_perf_tag <- factor(ifelse(test_pred >= cutoff, "yes", "no"))
  conf <- confusionMatrix(test_predicted_perf_tag, test$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#--------------------------------------------------------- 
summary(test_pred)
# Creating cutoff values from 00.02150 to 0.07254 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.02150,.07254,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    
# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=TRUE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.70,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------  
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.03)]
cutoff

# Let's choose a cutoff value of 4.263778 % for final model

test_predicted_perf_tag <- factor(ifelse(test_pred >= 0.04263778, "yes", "no"))

conf_final <- confusionMatrix(test_predicted_perf_tag, test$Performance.Tag, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 0.5605764 

sens # 0.5542986

spec # 0.5608529


##############################################################
####Logistic Regression on damographic + Credit data

##Creating data frames for models, removing application ID field
cust_LR_raw<-customer[,-1]
cust_LR_raw_bal<-customer[,-1]
cust_LR_woe_raw<-customer[,-1]
cust_LR_woe_bal<-customer[,-1]
cust_RF_raw<-customer[,-1]
cust_RF_bal<-customer[,-1]
cust_RF_woe_raw<-customer[,-1]
cust_RF_woe_bal<-customer[,-1]


##############  COMMON FUNCTIONS REQUIRED IN MOST OF THE MODELS   ########
### CLean credit NA records
cleanCreditNArecords<-function(x){
  #remove 272 credit NA records
  x<-x[-which(is.na(x$Outstanding.Balance)),]
  #Replace CC util with mean
  meanCC<-mean(x$Avgas.CC.Utilization.in.last.12.months,na.rm = T)
  x[which(is.na(x$Avgas.CC.Utilization.in.last.12.months)),]$Avgas.CC.Utilization.in.last.12.months <- meanCC
  x<-x
}

### Create Train and test datasets with same percentage of default rate in each
createTestTrain<-function(x,y,sRatio){
  library("caTools")
  set.seed(100)
  split_indices<-sample.split(x,SplitRatio = sRatio)
  split_indices
  train<<-y[split_indices,]
  test<<-y[!split_indices,]
  fOut<-list(train, test)
}

## Dummy of factor columns
createDummyCols<-function(x){
  facts<-data.frame(unlist(lapply(x,is.factor)))
  colnames(facts)<-"factcolumns"
  facts<-data.frame(cbind(facts$factcolumns,row.names(facts)))
  colnames(facts)<-c("val","factcolname")
  fact_cols<-facts[which(facts$val==TRUE),]$factcolname
  fact_cols<-as.character(fact_cols)
  fact_cols<-fact_cols[-1]
  df_factcols<-dummy.data.frame(x[,fact_cols])
  df_othercols<-x[,!colnames(x)%in%fact_cols]
  x<-data.frame(df_othercols,df_factcols)
}
##Find numeric columns
findNumCols<-function(x){
  nums <- data.frame(unlist(lapply(x, is.numeric)))
  colnames(nums)<-"numcolname"
  nums<-data.frame(cbind(nums$numcolname,row.names(nums)))
  colnames(nums)<-c("val","numcolname")
  num_cols<-nums[which(nums$val==TRUE),]$numcolname
  num_cols<-as.character(num_cols)
  
}

## Scaling of neumeric columns
scaleNumericCols<-function(x){
  nums <- data.frame(unlist(lapply(x, is.numeric)))
  colnames(nums)<-"numcolname"
  nums<-data.frame(cbind(nums$numcolname,row.names(nums)))
  colnames(nums)<-c("val","numcolname")
  num_cols<-nums[which(nums$val==TRUE),]$numcolname
  num_cols<-as.character(num_cols)
  x[,num_cols]<-data.frame(sapply(x[,num_cols],function(x) {scale(x)}))
  x<-x
}


## Prediction on test data with model
predictwithModel<-function(finalmodel,testdf){
  #### predictions
  predicted_defaults <- predict(finalmodel, newdata = testdf[, -1], type = "response")
  
  predicted_perf_tag<- factor(ifelse(predicted_defaults >= mean(predicted_defaults), "yes", "no"))
  PerformanceTag<- as.factor(ifelse(testdf$Performance.Tag == 1, "yes", "no"))
  conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
  fOut<-list(conf,predicted_defaults,predicted_perf_tag )
}

perform_fn <- function(cutoff,predictions_default,test_perf_tag) 
{
  predicted_perf_tag <- factor(ifelse(predictions_default >= cutoff, "yes", "no"))
  test_perf_tag<-factor(ifelse(test_perf_tag ==1, "yes", "no"))
  conf <- confusionMatrix(predicted_perf_tag, test_perf_tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

## Get and Plot cutoff
getPlotCutoff<-function(predicted_defaults,test_perf_tag,diff_ss){
  ## X is confusion matrix basic, Y1 is predicted default value, y2 is performance tag from test, Z is desired difference between Sens and spec
  #Creating cutoff values from Min and Max values of probable responses for plotting and initiallizing a matrix of 1000 X 4.
  s = seq(min(predicted_defaults),max(predicted_defaults),length=100)
  OUT = matrix(0,100,3)
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i],predicted_defaults,test_perf_tag)
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
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<diff_ss)]}


calculateKStat<-function( predicted_perf_tag,test_perf_tag,kstat){
  library(ROCR)
  predicted_perf_tag <- ifelse(predicted_perf_tag=="yes",1,0)
  test_perf_tag <- ifelse(test_perf_tag=="yes",1,0)
  pred_object_test<- prediction(predicted_perf_tag, test_perf_tag)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  kstat<-max(ks_table_test)
}

### lift and gain
lift <- function(labels , predicted_prob,groups=10) {
  
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
##### End of common functions


########################### Logistic regression on base data##########

cust_LR_raw<-cleanCreditNArecords(cust_LR_raw)
nrow(cust_LR_raw) #69586

train_test_list<-createTestTrain(cust_LR_raw$Performance.Tag,cust_LR_raw,0.8)
train<-train_test_list[[1]]
test<-train_test_list[[2]]

nrow(train) # 55669
nrow(test) # 13917

## scaling of numeric columns
train<-scaleNumericCols(train)
test<-scaleNumericCols(test)

## Dummy variables creation for factor columns
train<-createDummyCols(train)
test<-createDummyCols(test)

### Model building

model_1<-glm(Performance.Tag~.,data=train,family="binomial")
summary(model_1)
model_2<-stepAIC(model_1,direction ="both")
summary(model_2)
vif(model_2)
model_3<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
               No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Total.No.of.Trades, family = "binomial", data = train)

#remove insignificant variables and multicollinear ones from the model on the basis of VIF and p-value
vif(model_3)
summary(model_3)

# "No.of.PL.trades.opened.in.last.12.months" variable. has very high VIF value so remove it
# build model 4 excluding "No.of.PL.trades.opened.in.last.12.months"

model_4<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
               No.of.PL.trades.opened.in.last.6.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Total.No.of.Trades, family = "binomial", data = train)
vif(model_4)
# "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans." variable. has very high VIF value so remove it
# build model 4 excluding "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
summary(model_4)

model_5<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
               No.of.PL.trades.opened.in.last.6.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
               Total.No.of.Trades, family = "binomial", data = train)
vif(model_5)
summary(model_5)

# "Total.No.of.Trades" variable. has very high VIF and lowest P-value so remove it
# build model 4 excluding "Total.No.of.Trades"

model_6<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
               No.of.PL.trades.opened.in.last.6.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., family = "binomial", data = train)
vif(model_6)
summary(model_6)

# we have reached optimum values of VIF, so next base on P-value we will consider the variables
# No.of.months.in.current.residence has p value 0.05457, so insignificant. Removing
model_7<-glm(formula = Performance.Tag ~ Income + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
               No.of.PL.trades.opened.in.last.6.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., family = "binomial", data = train)
summary(model_7)

# No.of.months.in.current.compy has p value 0.05457, so insignificant. Removing
an
model_8<-glm(formula = Performance.Tag ~ Income + 
               No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
               No.of.PL.trades.opened.in.last.6.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., family = "binomial", data = train)
summary(model_8)

# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)                                                    -3.26654    0.02384 -137.045  < 2e-16 ***
#   Income                                                         -0.06436    0.02244   -2.868 0.004125 ** 
#   No.of.times.90.DPD.or.worse.in.last.12.months                   0.08329    0.02907    2.865 0.004173 ** 
#   No.of.times.30.DPD.or.worse.in.last.12.months                   0.19190    0.02816    6.815 9.41e-12 ***
#   Avgas.CC.Utilization.in.last.12.months                          0.23072    0.02082   11.080  < 2e-16 ***
#   No.of.PL.trades.opened.in.last.6.months                         0.17081    0.02537    6.732 1.67e-11 ***
#   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.  0.08476    0.02525    3.357 0.000789 ***
#   AIC: 18748
#   Number of Fisher Scoring iterations: 6
Model_LR_raw<-model_8

## Prediction
conf_matrix<- predictwithModel(Model_LR_raw,test)[[1]]
conf_matrix$table
# Reference
# Prediction   no  yes
# no  8072  211
# yes 5257  377

conf_matrix$overall[["Accuracy"]]
#0.6068118
conf_matrix$byClass
# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
# 0.64115646           0.60559682           0.06691516           0.97452614           0.06691516 
# Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
# 0.64115646           0.12118290           0.04225049           0.02708917           0.40482863 
# Balanced Accuracy 
# 0.62337664 


predicted_defaults<-predictwithModel(Model_LR_raw,test)[[2]]
predicted_perf_tag<-predictwithModel(Model_LR_raw,test)[[3]]

### Find optimal cutoff to maximize sensitivity and specificity
cutoff<-getPlotCutoff(predicted_defaults,test$Performance.Tag,0.04)
cutoff
cutoff <- cutoff[1]
cutoff
#0.04257107
predicted_perf_tag<-factor(ifelse(predicted_defaults >= cutoff, "yes", "no"))
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))

conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy 
# 0.6055184 
# Sensitivity 
# 0.6428571 
# Specificity 
# 0.6038713

conf$byClass
# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
# 0.64285714           0.60387126           0.06680806           0.97457319           0.06680806 
# Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
# 0.64285714           0.12103746           0.04225049           0.02716103           0.40655314 
# Balanced Accuracy 
# 0.62336420 

### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.2467284
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults, test$Performance.Tag, groups = 10)
default_decile

# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  1392      65.5    65.5  11.2    1.12
# 2      2  1392      58.7   124.   21.2    1.06
# 3      3  1392      58.0   182.   31.0    1.03
# 4      4  1391      57.5   240.   40.8    1.02
# 5      5  1392      58.9   299.   50.9    1.02
# 6      6  1392      58.4   357.   60.8    1.01
# 7      7  1391      57.1   414.   70.6    1.01
# 8      8  1392      58.4   473.   80.5    1.01
# 9      9  1392      57.4   530.   90.3    1.00
# 10     10  1391      57.0   587.  100.     1.00

### Scrutinizing applications from top 5 buckets (1392*5) will identify 50.9 % of the predicted defaults


##################################################################
######   Logistic regression on WOE converted dataframe   #####
###############################################################
cust_LR_woe_raw<-customer[,-1]
nrow(cust_LR_woe_raw)


## using scorecard package
library(scorecard)
## Create WOR bins
woe_bins<-woebin(cust_LR_woe_raw,y="Performance.Tag",bin_num_max= 15)
## Convert data frame to woe values
cust_LR_woe_raw <- woebin_ply(cust_LR_woe_raw, woe_bins,
                              replace_blank_na = TRUE)
plotlist = woebin_plot(woe_bins)
print(plotlist)
# # save binning plot
for (i in 1:length(plotlist)) {
  ggplot2::ggsave(
    paste0(names(plotlist[i]), ".png"), plotlist[[i]],
    width = 15, height = 9, units="cm" )
}
train_test_list<-createTestTrain(cust_LR_woe_raw$Performance.Tag,cust_LR_woe_raw,0.8)
train<-train_test_list[[1]]
test<-train_test_list[[2]]


model_1<-glm(Performance.Tag~.,data=train,family="binomial")
summary(model_1)
model_2<-stepAIC(model_1,direction ="both")
summary(model_2)
vif(model_2)
# all below 3 so good
model_3<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
               No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
             family = "binomial", data = train)
summary(model_3)
## Removing insignificant variable Income_woe with p value 0.106065
model_4<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
               No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
             family = "binomial", data = train)
summary(model_4)
## Removing insignificant variable Profession_woe with p value 0.062190
model_5<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
               No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
             family = "binomial", data = train)
summary(model_5)
## Removing insignificant variable No.of.trades.opened.in.last.12.months_woe with p value 0.047807
model_6<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
             family = "binomial", data = train)
summary(model_6)
## Removing insignificant variable No.of.dependents_woe  with p value 0.003484

model_7<-glm(formula = Performance.Tag ~ Age_woe +  
               No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
             family = "binomial", data = train)
summary(model_7)

## Removing insignificant variable Age_woe  with p value 0.001630
model_8<-glm(formula = Performance.Tag ~  No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
             family = "binomial", data = train)
summary(model_8)

# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)                                                         -3.11956    0.02210 -141.152  < 2e-16 ***
#   No.of.months.in.current.company_woe                                  0.42683    0.12378    3.448 0.000564 ***
#   No.of.times.30.DPD.or.worse.in.last.12.months_woe                    0.43549    0.05127    8.494  < 2e-16 ***
#   Avgas.CC.Utilization.in.last.12.months_woe                           0.45861    0.05253    8.730  < 2e-16 ***
#   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe  0.55038    0.05690    9.673  < 2e-16 ***

#AIC 18728

Model_LR_woe_raw<-model_8

## Prediction
conf_matrix<- predictwithModel(Model_LR_woe_raw,test)[[1]]
conf_matrix$table
#       Reference
# Prediction   no  yes
# no  7497  150
# yes 5885  439
conf_matrix$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
# 0.56803378     0.05404203     0.55977069     0.57626853     0.95784124 
# AccuracyPValue  McnemarPValue 
# 1.00000000     0.00000000 
conf_matrix$byClass
# Sensitivity          Specificity       Pos Pred Value 
# 0.74533107           0.56023016           0.06941809 
# Neg Pred Value            Precision               Recall 
# 0.98038446           0.06941809           0.74533107 
# F1           Prevalence       Detection Rate 
# 0.12700709           0.04215876           0.03142223 
# Detection Prevalence    Balanced Accuracy 
# 0.45265192           0.65278061 

predicted_defaults<-predictwithModel(Model_LR_woe_raw,test)[[2]]
predicted_perf_tag<-predictwithModel(Model_LR_woe_raw,test)[[3]]
### Find optimal cutoff to maximize sensitivity and specificity
cutoff<-getPlotCutoff(predicted_defaults,test$Performance.Tag,0.018)
cutoff
# 0.04775926

predicted_perf_tag<-factor(ifelse(predicted_defaults >= cutoff, "yes", "no"))
levels(predicted_perf_tag)
levels(test$Performance.Tag)
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))

conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy 
# 0.6452652
# Sensitivity 
# 0.6281834 
# Specif.icity 
# 0.646017
### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.2742004
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults, test$Performance.Tag, groups = 10)
default_decile
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  1398      68.0    68.0  11.6    1.16
# 2      2  1397      57.7   126.   21.5    1.08
# 3      3  1397      56.9   183.   31.3    1.04
# 4      4  1397      56.5   239.   40.9    1.02
# 5      5  1397      58.5   298.   50.9    1.02
# 6      6  1397      57.3   355.   60.7    1.01
# 7      7  1397      57.7   413.   70.6    1.01
# 8      8  1397      57.6   470.   80.5    1.01
# 9      9  1397      58.7   529.   90.5    1.01
# 10     10  1397      55.4   584.  100.     1.00

### Scrutinizing applications from top 5 buckets (1398*5) will identify 50.9 % of the predicted defaults

#####################################################333
#####     Logistic regression with data balancing   ####
#######################################################

cust_LR_raw_bal<-customer[,-1]
str(cust_LR_raw_bal)
cust_LR_raw_bal<-cleanCreditNArecords(cust_LR_raw_bal)
nrow(cust_LR_raw_bal)
#69658

table(cust_LR_raw_bal$Performance.Tag)
# 0     1 
# 66647 2939

train_test_list<-createTestTrain(cust_LR_raw_bal$Performance.Tag,cust_LR_raw_bal,0.8)
train<-train_test_list[[1]]
test<-train_test_list[[2]]
nrow(train) #55669
nrow(test) # 13917

##### Balancing data by adding more records of defaulted applications
library("ROSE")
train<-ovun.sample(Performance.Tag~., data=train,method="over",N=65000)$data
Default_rate_bal<-sum(train$Performance.Tag==1)/length(train$Performance.Tag)
Default_rate_bal
#0.1797231   (Default Rate increased from 4.21% to 17.9% )
nrow(train) #65000


## scaling of numeric columns
train<-scaleNumericCols(train)
test<-scaleNumericCols(test)

## Dummy variables creation for factor columns
train<-createDummyCols(train)
test<-createDummyCols(test)

### Model building

model_1<-glm(Performance.Tag~.,data=train,family="binomial")
summary(model_1)
model_2<-stepAIC(model_1,direction ="both")
summary(model_2)
vif(model_2)
# Outstanding.Balance VIF 351.602778 - Remove
model_3<-  glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                 No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Total.No.of.Trades + EducationMasters + 
                 EducationOthers + EducationPhd + ProfessionSE + Type.of.residenceLiving.with.Parents + 
                 Type.of.residenceOthers + Presence.of.open.home.loan0 + Presence.of.open.auto.loan0, 
               family = "binomial", data = train)
vif(model_3)
#No.of.times.30.DPD.or.worse.in.last.6.months 16.356390  remove
model_4<-
  glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
        No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
        No.of.times.90.DPD.or.worse.in.last.12.months + 
        No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
        Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
        No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
        No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
        Outstanding.Balance + Total.No.of.Trades + EducationBachelor + 
        EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
        Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)
vif(model_4)
# Total.No.of.Trades 8.036758 remove
summary(model_4)

model_5<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
               No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Outstanding.Balance + EducationBachelor + 
               EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
               Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)


vif(model_5)


model_6<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
               No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Outstanding.Balance +  EducationBachelor + 
               EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
               Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)


vif(model_6)
#No.of.PL.trades.opened.in.last.6.months 6.669288 Remove

model_7<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
               No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Outstanding.Balance +  EducationBachelor + 
               EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
               Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)


vif(model_7)
#No.of.times.60.DPD.or.worse.in.last.12.months 6.071715 remove

model_8<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
               No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
               Outstanding.Balance +  EducationBachelor + 
               EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
               Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)



vif(model_8)
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 5.831950 remove

model_9<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
               No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
               No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.times.30.DPD.or.worse.in.last.12.months + 
               Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
               No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
               Outstanding.Balance +  EducationBachelor + 
               EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
               Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)


vif(model_9)
#No.of.times.90.DPD.or.worse.in.last.12.months 5.293851

model_10<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +  EducationBachelor + 
                EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
                Type.of.residenceOthers + Presence.of.open.auto.loan0, family = "binomial", data = train)


vif(model_10)
# All below 4 so OK
summary(model_10)
#Presence.of.open.auto.loan0 P value-  0.735907 Insignificant, remove.
model_11<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.6.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +  EducationBachelor + 
                EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
                Type.of.residenceOthers , family = "binomial", data = train)


summary(model_11)
#No.of.trades.opened.in.last.6.months P value-  0.628634 Insignificant, remove.
model_12<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +  EducationBachelor + 
                EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
                Type.of.residenceOthers , family = "binomial", data = train)

summary(model_12)
#EducationBachelor  P value-  0.62153 Insignificant, remove.
model_13<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +   
                EducationMasters + EducationOthers + ProfessionSAL + Type.of.residenceCompany.provided + 
                Type.of.residenceOthers , family = "binomial", data = train)

summary(model_13)
#Type.of.residenceCompany.provided  P value-  0.26643 Insignificant, remove.

model_14<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +   
                EducationMasters + EducationOthers + ProfessionSAL + 
                Type.of.residenceOthers , family = "binomial", data = train)

summary(model_14)
#ProfessionSAL   P value-  0.2099 Insignificant, remove.

model_15<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +   
                EducationMasters + EducationOthers + 
                Type.of.residenceOthers , family = "binomial", data = train)

summary(model_15)
# Type.of.residenceOthers P value-  0.04386 Insignificant, remove.
model_16<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +   
                EducationMasters + EducationOthers , family = "binomial", data = train)

summary(model_16)
#No.of.times.90.DPD.or.worse.in.last.6.months P value-  0.04078 Insignificant, remove.

model_17<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +   
                EducationMasters + EducationOthers , family = "binomial", data = train)

summary(model_17)
#EducationMasters P value-  0.0156 Insignificant, remove.
model_18<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance +   
                EducationOthers , family = "binomial", data = train)

summary(model_18)
#EducationOthers P value-  0.01320 Insignificant, remove.


model_19<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                Outstanding.Balance  , family = "binomial", data = train)
summary(model_19)
# Outstanding.Balance  p value 0.00614 remove

model_20<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                No.of.months.in.current.company + 
                No.of.times.30.DPD.or.worse.in.last.12.months + 
                Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + 
                No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. , family = "binomial", data = train)
summary(model_20)

# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)                                                    -1.627506   0.011179 -145.580  < 2e-16 ***
#   Income                                                         -0.079561   0.011145   -7.139 9.41e-13 ***
#   No.of.months.in.current.residence                              -0.058006   0.011286   -5.140 2.75e-07 ***
#   No.of.months.in.current.company                                -0.091827   0.010686   -8.594  < 2e-16 ***
#   No.of.times.30.DPD.or.worse.in.last.12.months                   0.267317   0.009915   26.962  < 2e-16 ***
#   Avgas.CC.Utilization.in.last.12.months                          0.245498   0.010865   22.596  < 2e-16 ***
#   No.of.PL.trades.opened.in.last.12.months                        0.225426   0.013767   16.374  < 2e-16 ***
#   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.  0.053285   0.013158    4.050 5.13e-05 ***

Model_LR_bal<-model_20


## Prediction
conf_matrix<- predictwithModel(Model_LR_bal,test)[[1]]
conf_matrix$table
#             Reference
# Prediction   no  yes
# no  7671  208
# yes 5658  380

conf_matrix$overall[["Accuracy"]]
#0.5785011
conf_matrix$byClass
# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
# 0.64625850           0.57551204           0.06293475           0.97360071           0.06293475 
# Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
# 0.64625850           0.11469967           0.04225049           0.02730474           0.43385787 
# Balanced Accuracy 
# 0.61088527 


predicted_defaults<-predictwithModel(Model_LR_bal,test)[[2]]
predicted_perf_tag<-predictwithModel(Model_LR_bal,test)[[3]]

### Find optimal cutoff to maximize sensitivity and specificity
cutoff<-getPlotCutoff(predicted_defaults,test$Performance.Tag,0.02)
cutoff
# 0.1903347
predicted_perf_tag<-factor(ifelse(predicted_defaults >= cutoff, "yes", "no"))
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))

conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy 
# 0.6124165  
# Sensitivity 
# 0.6207483 
# Specificity 
# 0.6120489 

### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.2327972
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults, test$Performance.Tag, groups = 10)
default_decile
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  1392      275.    275.  11.0    1.10
# 2      2  1392      250.    525.  21.0    1.05
# 3      3  1392      249.    774.  30.9    1.03
# 4      4  1391      247.   1021.  40.8    1.02
# 5      5  1392      250.   1271.  50.8    1.02
# 6      6  1392      251.   1522.  60.8    1.01
# 7      7  1391      246.   1768.  70.6    1.01
# 8      8  1392      249.   2017.  80.6    1.01
# 9      9  1392      244.   2261.  90.3    1.00
# 10     10  1391      242.   2503. 100.     1.00
### Scrutinizing applications from top 5 buckets (1392*5) will identify 50.8 % of the predicted defaults


###################################################################
#####     Logistic regression on balanced dataframe of WOE values ####
#######################################################################

cust_LR_woe_bal<-customer[,-1]
nrow(cust_LR_woe_bal) # 69858

library(scorecard)
## using scorecard package
woe_bins<-woebin(cust_LR_woe_bal,y="Performance.Tag",bin_num_max= 15)
cust_LR_woe_bal <- woebin_ply(cust_LR_woe_bal, woe_bins,
                              replace_blank_na = TRUE)
plotlist = woebin_plot(woe_bins)
print(plotlist)
# # save binning plot
for (i in 1:length(plotlist)) {
  ggplot2::ggsave(
    paste0(names(plotlist[i]), ".png"), plotlist[[i]],
    width = 15, height = 9, units="cm" )
}
train_test_list<-createTestTrain(cust_LR_woe_bal$Performance.Tag,cust_LR_woe_bal,0.8)
train<-train_test_list[[1]]
test<-train_test_list[[2]]
library("ROSE")
train<-ovun.sample(Performance.Tag~., data=train,method="over",N=65000)$data
Default_rate_bal<-sum(train$Performance.Tag==1)/length(train$Performance.Tag)
Default_rate_bal
#0.1764769
nrow(train) # 65000

model_1<-glm(Performance.Tag~.,data=train,family="binomial")
summary(model_1)
model_2<-stepAIC(model_1,direction ="both")
summary(model_2)

model_3<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.90.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
               No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
               No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
vif(model_3)
summary(model_3)

#remove insignificant variables and multicollinear ones from the model on the basis of VIF and p-value

# "No.of.times.30.DPD.or.worse.in.last.6.months_woe" variable. has very high  10.545190 VIF value so remove it
# build model 4 excluding "No.of.times.30.DPD.or.worse.in.last.6.months_woe"

model_4<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.90.DPD.or.worse.in.last.6.months_woe + 
               No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
               No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
vif(model_4)

# "No.of.PL.trades.opened.in.last.12.months_woe" variable. has very high 8.877819 VIF value so remove it
# build model 4 excluding "No.of.PL.trades.opened.in.last.12.months_woe"

model_5<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.90.DPD.or.worse.in.last.6.months_woe + 
               No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
               No.of.trades.opened.in.last.12.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
vif(model_5)

# "No.of.trades.opened.in.last.12.months_woe" variable. has very high 5.691961 VIF value so remove it
# build model 4 excluding "No.of.trades.opened.in.last.12.months_woe"

model_6<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.90.DPD.or.worse.in.last.6.months_woe + 
               No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
vif(model_6)

# "No.of.times.90.DPD.or.worse.in.last.12.months_woe 4.934426" variable. has very high 4.934426 VIF value so remove it
# build model 4 excluding "No.of.times.90.DPD.or.worse.in.last.12.months_woe 4.934426"

model_7<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.90.DPD.or.worse.in.last.6.months_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
vif(model_7)
summary(model_7)
# we have reached optimum values of VIF, so next base on P-value we will consider the variables
# No.of.times.90.DPD.or.worse.in.last.6.months_woe has p value 0.95226, so insignificant. Removing

model_8<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
summary(model_8)

# No.of.trades.opened.in.last.6.months_woe has p value 0.947734, so insignificant. Removing

model_9<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
               Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
               No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
               Avgas.CC.Utilization.in.last.12.months_woe +  
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
               Outstanding.Balance_woe + Total.No.of.Trades_woe + Presence.of.open.auto.loan_woe, 
             family = "binomial", data = train)
summary(model_9)

# Total.No.of.Trades_woe has p value 0.546178, so insignificant. Removing

model_10<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
                Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
                No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                Avgas.CC.Utilization.in.last.12.months_woe +  
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                Outstanding.Balance_woe + Presence.of.open.auto.loan_woe, 
              family = "binomial", data = train)
summary(model_10)

# Presence.of.open.auto.loan_woe has p value 0.546178, so insignificant. Removing

model_11<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
                Income_woe + Profession_woe + No.of.months.in.current.company_woe + 
                No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                Avgas.CC.Utilization.in.last.12.months_woe +  
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                Outstanding.Balance_woe, 
              family = "binomial", data = train)
summary(model_11)

# Income_woe has p value 0.546178, so insignificant. Removing

model_12<-glm(formula = Performance.Tag ~ Age_woe + No.of.dependents_woe + 
                Profession_woe + No.of.months.in.current.company_woe + 
                No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                Avgas.CC.Utilization.in.last.12.months_woe +  
                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                Outstanding.Balance_woe, 
              family = "binomial", data = train)
summary(model_12)

# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)                                                         -1.53832    0.01082 -142.165  < 2e-16 ***
#   Age_woe                                                              0.90752    0.12989    6.987 2.81e-12 ***
#   No.of.dependents_woe                                                 1.26836    0.20981    6.045 1.49e-09 ***
#   Profession_woe                                                       0.86456    0.22130    3.907 9.35e-05 ***
#   No.of.months.in.current.company_woe                                  0.35273    0.06166    5.720 1.06e-08 ***
#   No.of.times.30.DPD.or.worse.in.last.12.months_woe                    0.44825    0.02587   17.325  < 2e-16 ***
#   Avgas.CC.Utilization.in.last.12.months_woe                           0.41966    0.02718   15.440  < 2e-16 ***
#   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe  0.47799    0.02975   16.069  < 2e-16 ***
#   Outstanding.Balance_woe                                              0.13033    0.03207    4.064 4.83e-05 ***
#AIC: 57015

Model_LR_woe_bal<-model_12

## Prediction
conf_matrix<- predictwithModel(Model_LR_woe_bal,test)[[1]]
conf_matrix$table
# Reference
# Prediction   no  yes
# no  7436  150
# yes 5946  439
conf_matrix$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 0.56366760     0.05277378     0.55539580     0.57191287     0.95784124     1.00000000     0.00000000  
conf_matrix$byClass
# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
# 0.74533107           0.55567180           0.06875489           0.98022673           0.06875489 
# Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
# 0.74533107           0.12589619           0.04215876           0.03142223           0.45701811 
# Balanced Accuracy 
# 0.65050143 

predicted_defaults<-predictwithModel(Model_LR_woe_bal,test)[[2]]
predicted_perf_tag<-predictwithModel(Model_LR_woe_bal,test)[[3]]
### Find optimal cutoff to maximize sensitivity and specificity
cutoff<-getPlotCutoff(predicted_defaults,test$Performance.Tag,0.01)
cutoff
# 0.1957099

predicted_perf_tag<-factor(ifelse(predicted_defaults >= cutoff, "yes", "no"))
levels(predicted_perf_tag)
levels(test$Performance.Tag)
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))

conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
conf
#     Reference
# Prediction   no  yes
# no  8551  211
# yes 4831  378
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy 
# 0.6391096 
# Sensitivity 
# 0.6417657 
# Specificity 
# 0.6389927

conf$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 0.63910958     0.05911597     0.63108187     0.64707932     0.95784124     1.00000000     0.00000000 
conf$byClass
# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
# 0.64176570           0.63899268           0.07256671           0.97591874           0.07256671 
# Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
# 0.64176570           0.13038979           0.04215876           0.02705604           0.37284375 
# Balanced Accuracy 
# 0.64037919 

### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.2807584
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults, test$Performance.Tag, groups = 10)
default_decile

# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  1398      269.    269.  11.5    1.15
# 2      2  1397      232.    501.  21.4    1.07
# 3      3  1397      230.    731.  31.1    1.04
# 4      4  1397      227.    958.  40.8    1.02
# 5      5  1397      236.   1194.  50.8    1.02
# 6      6  1397      231.   1425.  60.7    1.01
# 7      7  1397      232.   1657.  70.6    1.01
# 8      8  1397      232.   1889.  80.5    1.01
# 9      9  1397      235.   2124.  90.4    1.00
# 10     10  1397      224.   2348. 100.     1.00

### Scrutinizing applications from top 5 buckets (1397*5) will identify 50.8 % of the predicted defaults

##############
#   Conclusion : Logistic regression
#   Model on balanced data set with WOE values has highest accuracy, sensitivity and specificity
#   Also has marginally more descriminative pwoer
##############

################################################################
###
###         RANDOM FOREST MODELS                ################
###
################################################################


################  Random forest on base data ####
library(randomForest)
cust_RF_raw<-customer[,-1]
cust_RF_raw<-cleanCreditNArecords(cust_RF_raw)
nrow(cust_RF_raw) #69586

table(cust_RF_raw$Performance.Tag)
# 0     1 
# 66647 2939 

train_test_list<-createTestTrain(cust_RF_raw$Performance.Tag,cust_RF_raw,0.8)
train<-train_test_list[[1]]
test<-train_test_list[[2]]

nrow(train) #55669
nrow(test)   #13917
str(train)

## REMOVING COLUMNS WITH HIGH MULTICOLLINEARITY
numcols<-findNumCols(train)
train_num<-train[,numcols]
str(train_num)
train.cor<-cor(train_num)
high_cor_vars<-findCorrelation(train.cor,cutoff=0.9,name=TRUE,exact = TRUE)
high_cor_vars
train<-train[,!colnames(train)%in% high_cor_vars]
test<-test[,!colnames(test)%in%high_cor_vars]
table(train$Performance.Tag)
# 0     1 
# 53318  2351 
table(test$Performance.Tag)
# 0     1 
# 13329   588 
### Model building
### Selected sq root of variables as mtry 
### model OOB getting stabilized after 90 trees. So selected 100 trees
model_rf_raw<- randomForest(Performance.Tag ~ ., data=train, 
                            ntree=100, mtry=5, do.trace=TRUE, importance = TRUE,na.action=na.omit)
varImpPlot(model_rf_raw)
## Prediction
predicted_defaults <- data.frame(predict(model_rf_raw, newdata = test[, -1],type="prob"))

summary(predicted_defaults$X1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.01000 0.04000 0.05046 0.08000 0.38000 
predicted_perf_tag<- factor(ifelse(predicted_defaults$X1>= mean(predicted_defaults$X1), "yes", "no"))
PerformanceTag<- as.factor(ifelse(test$Performance.Tag == 1, "yes", "no"))
table(PerformanceTag)
# PerformanceTag
# no   yes 
# 13329   588 
table(predicted_perf_tag)
# predicted_perf_tag
# no  yes 
# 8746 5171 
conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
conf$table
# Reference
# Prediction   no  yes
# no  8479  267
# yes 4850  321

conf$overall[["Accuracy"]]
#0.6323202
conf$byClass
# Sensitivity          Specificity       Pos Pred Value 
# 0.54591837           0.63613174           0.06207697 
# Neg Pred Value            Precision               Recall 
# 0.96947176           0.06207697           0.54591837 
# F1           Prevalence       Detection Rate 
# 0.11147769           0.04225049           0.02306532 
# Detection Prevalence    Balanced Accuracy 
# 0.37155996           0.59102506 


### Find optimal cutoff to maximize sensitivity and specificity
cutoff<-getPlotCutoff(predicted_defaults$X1,test$Performance.Tag,0.06)
cutoff[1]
#0.04222222 
cutoff<-cutoff[1]

predicted_perf_tag<-factor(ifelse(predicted_defaults$X1 >= cutoff, "yes", "no"))
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))
table(PerformanceTag)
# no   yes 
# 13329   588 
conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy 
# 0.5669325 
# > sens
# Sensitivity 
# 0.6207483 
# > spec
# Specificity 
# 0.5645585 
conf$byClass
# Sensitivity          Specificity       Pos Pred Value 
# 0.62074830           0.56455848           0.05916680 
# Neg Pred Value            Precision               Recall 
# 0.97121838           0.05916680           0.62074830 
# F1           Prevalence       Detection Rate 
# 0.10803611           0.04225049           0.02622692 
# Detection Prevalence    Balanced Accuracy 
# 0.44327082           0.59265339 

### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.1853068
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults$X1, test$Performance.Tag, groups = 10)
default_decile
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  1392      82.4    82.4  11.7    1.17
# 2      2  1392      69.5   152.   21.6    1.08
# 3      3  1392      71.4   223.   31.8    1.06
# 4      4  1391      67.7   291.   41.5    1.04
# 5      5  1392      71.8   363.   51.7    1.03
# 6      6  1392      69.6   432.   61.6    1.03
# 7      7  1391      68.6   501.   71.4    1.02
# 8      8  1392      67.0   568.   80.9    1.01
# 9      9  1392      67.7   636.   90.5    1.01
# 10     10  1391      66.5   702.  100.     1.00

### Scrutinizing applications from top 5 buckets (1392*5) will identify 51.7 % of the predicted defaults

####################################
######  Randowm forest on WOE data ######
###########################################

cust_RF_woe_raw<-customer[,-1]
nrow(cust_RF_woe_raw)


## using scorecard package
woe_bins<-woebin(cust_RF_woe_raw,y="Performance.Tag",bin_num_max= 15)
cust_RF_woe_raw <- woebin_ply(cust_RF_woe_raw, woe_bins,
                              replace_blank_na = TRUE)

train_test_list<-createTestTrain(cust_RF_woe_raw$Performance.Tag,cust_RF_woe_raw,0.8)
train<-train_test_list[[1]]
test<-train_test_list[[2]]


model_rf_raw_woe <- randomForest(Performance.Tag ~ ., data=train, proximity=FALSE,
                                 ntree=130, mtry=5,strata=Performance.Tag, do.trace=TRUE, importance = TRUE,na.action=na.omit)
varImpPlot(model_rf_raw_woe)
predicted_defaults <- data.frame(predict(model_rf_raw_woe, newdata = test[, -1],type="prob"))
summary(predicted_defaults$X1)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.007692 0.023077 0.040245 0.061539 0.500000 
predicted_perf_tag<- factor(ifelse(predicted_defaults$X1>= mean(predicted_defaults$X1), "yes", "no"))
PerformanceTag<- as.factor(ifelse(test$Performance.Tag == 1, "yes", "no"))
table(PerformanceTag)
# no   yes 
# 13382   589 
table(predicted_perf_tag)
# no  yes 
# 8838 5133 
conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
conf$table
# Reference
# Prediction   no  yes
# no  8579  259
# yes 4803  330
conf$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
# 0.63767805     0.04295513     0.62964380     0.64565492     0.95784124 
# AccuracyPValue  McnemarPValue 
# 1.00000000     0.00000000
### Find optimal cutoff to maximize sensitivity and specificity
cutoff<-getPlotCutoff(predicted_defaults$X1,test$Performance.Tag,0.05)
cutoff
# 0.03535354

predicted_perf_tag<-factor(ifelse(predicted_defaults$X1 >= cutoff, "yes", "no"))
table(predicted_perf_tag)
levels(test$Performance.Tag)
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))
table(PerformanceTag)

conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy 
# 0.5856417
# Sensitivity 
# 0.6264856 
# Specif.icity 
# 0.583844
### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.2103295
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults$X1, test$Performance.Tag, groups = 10)
default_decile
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  1398      66.2    66.2  11.8    1.18
# 2      2  1397      53.9   120.   21.4    1.07
# 3      3  1397      55.1   175.   31.2    1.04
# 4      4  1397      55.8   231.   41.1    1.03
# 5      5  1397      56.6   288.   51.2    1.02
# 6      6  1397      56.8   345.   61.3    1.02
# 7      7  1397      55.1   400.   71.1    1.02
# 8      8  1397      54.5   454.   80.8    1.01
# 9      9  1397      55.9   510.   90.7    1.01
# 10     10  1397      52.2   562.  100.     1.00

### Scrutinizing applications from top 5 buckets (1397*5) will identify 51.2 % of the predicted defaults

############################################
###   Random forest on balanced data    #######
###################################################

cust_RF_bal<-customer[,-1]
str(cust_RF_bal)
cust_RF_bal<-cleanCreditNArecords(cust_RF_bal)
nrow(cust_RF_bal)

table(cust_RF_bal$Performance.Tag)
# 0     1 
# 66647  2939  
train_test_list<-createTestTrain(cust_RF_bal$Performance.Tag,cust_RF_bal,0.7)
train<-train_test_list[[1]]
test<-train_test_list[[2]]
nrow(train)
library("ROSE")
train<-ovun.sample(Performance.Tag~., data=train,method="over",N=58000)$data
Default_rate_bal<-sum(train$Performance.Tag==1)/length(train$Performance.Tag)
Default_rate_bal
#0.1956379

nrow(train)
#58000
nrow(test)
#20876

## REMOVING COLUMNS WITH HIGH MULTICOLLINEARITY
numcols<-findNumCols(train)
train_num<-train[,numcols]
str(train_num)
train.cor<-cor(train_num)
#corrplot(train.cor)
high_cor_vars<-findCorrelation(train.cor,cutoff=0.85,name=TRUE,exact = TRUE)
high_cor_vars

train<-train[,!colnames(train)%in% high_cor_vars]
test<-test[,!colnames(test)%in%high_cor_vars]
table(train$Performance.Tag)
# 0     1 
# 46653 11347 
table(test$Performance.Tag)
# 0     1 
# 19994   882 

### Model building

model_rf_raw_bal <- randomForest(Performance.Tag ~ ., data=train, proximity=FALSE,
                                 ntree=130, mtry=4,strata=Performance.Tag, do.trace=TRUE, importance = TRUE,na.action=na.omit)

varImpPlot(model_rf_raw_bal)

predicted_defaults <- data.frame(predict(model_rf_raw_bal, newdata = test[, -1],type="prob"))
str(predicted_defaults)
summary(predicted_defaults$X1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.02308 0.05385 0.06827 0.10000 0.40769 
predicted_perf_tag<- factor(ifelse(predicted_defaults$X1>= mean(predicted_defaults$X1), "yes", "no"))
PerformanceTag<- as.factor(ifelse(test$Performance.Tag == 1, "yes", "no"))
table(PerformanceTag)
# no   yes 
# 19994   882
table(predicted_perf_tag)
# predicted_perf_tag
# no   yes 
# 11605  9271 
conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
conf$table
#   Reference
# Prediction    no   yes
# no  11296   309
# yes  8698   573
conf$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
# 0.56854761     0.03870068     0.56179597     0.57528016     0.95775053 
# AccuracyPValue  McnemarPValue 
# 1.00000000     0.00000000 
conf$byClass
# Sensitivity          Specificity       Pos Pred Value 
# 0.64965986           0.56496949           0.06180563 
# Neg Pred Value            Precision               Recall 
# 0.97337355           0.06180563           0.64965986 
# F1           Prevalence       Detection Rate 
# 0.11287304           0.04224947           0.02744779 
# Detection Prevalence    Balanced Accuracy 
# 0.44409849           0.60731468 


### Find optimal cutoff to maximize sensitivity and specificity

cutoff<-getPlotCutoff(predicted_defaults$X1,test$Performance.Tag,0.01)
cutoff
#0.07000777 0.07412587
cutoff<-cutoff[1]

predicted_perf_tag<-factor(ifelse(predicted_defaults$X1 >= cutoff, "yes", "no"))
PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))
table(PerformanceTag)
# no   yes 
# 19994   882 
conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
acc
sens
spec
# Accuracy
#0.6115635 
# > sens
# Sensitivity 
# 0.6088435 
# > spec
# Specificity 
# 0.6116835 
conf$byClass
# Sensitivity          Specificity       Pos Pred Value 
# 0.60884354           0.61168351           0.06469100 
# Neg Pred Value            Precision               Recall 
# 0.97256461           0.06469100           0.60884354 
# F1           Prevalence       Detection Rate 
# 0.11695524           0.04224947           0.02572332 
# Detection Prevalence    Balanced Accuracy 
# 0.39763365           0.61026352 

### Kstatistics for descriminatory power
Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
Kstat
#0.220527
##For a good model, KS statistic would be more than 40% and would lie in the top few deciles

#### Lift and gain
default_decile = lift(predicted_defaults$X1, test$Performance.Tag, groups = 10)
default_decile
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2088      163.    163.  11.4    1.14
# 2      2  2088      140.    303.  21.3    1.06
# 3      3  2087      144.    447.  31.4    1.05
# 4      4  2088      141.    588.  41.3    1.03
# 5      5  2087      143.    731.  51.3    1.03
# 6      6  2088      136.    868.  60.9    1.01
# 7      7  2088      142.   1010.  70.8    1.01
# 8      8  2087      138.   1148.  80.5    1.01
# 9      9  2088      139.   1286.  90.3    1.00
# 10     10  2087      139.   1425. 100.     1.00

### Scrutinizing applications from top 5 buckets (2088*5) will identify 51.3 % of the predicted defaults

#### DOING TUNING OF THE MODEL TO CHECK IF PERFORMANCE IMPROVES

mtry <- tuneRF(train[,-1],train$Performance.Tag, ntreeTry=130,
                          stepFactor=1.5,improve=0.005, trace=TRUE, plot=TRUE)
 best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
 print(mtry)
 # mtry     OOBError
 # 3.OOB    3 0.0010344828
 # 4.OOB    4 0.0008620690
 # 6.OOB    6 0.0008793103
 print(best.m)
#4
 
 ### USed value of mtry is best. So no further tuning necessary
 
 
###############################################################
####  Random forest on WOE data of balanced dataset  #########
###############################################################
 cust_RF_woe_bal<-customer[,-1]
 nrow(cust_RF_woe_bal)
 
 
 ## using scorecard package
 woe_bins<-woebin(cust_RF_woe_bal,y="Performance.Tag",bin_num_max= 15)
 cust_RF_woe_bal <- woebin_ply(cust_RF_woe_bal, woe_bins,
                               replace_blank_na = TRUE)
 
 train_test_list<-createTestTrain(cust_RF_woe_bal$Performance.Tag,cust_RF_woe_bal,0.8)
 train<-train_test_list[[1]]
 test<-train_test_list[[2]]
 
 library("ROSE")
 train<-ovun.sample(Performance.Tag~., data=train,method="over",N=65000)$data
 Default_rate_bal<-sum(train$Performance.Tag==1)/length(train$Performance.Tag)
 Default_rate_bal
 #0.1764769
 model_rf_woe_bal <- randomForest(Performance.Tag ~ ., data=train, proximity=FALSE,
                                  ntree=130, mtry=5,strata=Performance.Tag, do.trace=TRUE, importance = TRUE,na.action=na.omit)
 
 predicted_defaults <- data.frame(predict(model_rf_woe_bal, newdata = test[, -1],type="prob"))
 summary(predicted_defaults$X1)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.00000 0.01538 0.05385 0.07528 0.11538 0.85385 
 predicted_perf_tag<- factor(ifelse(predicted_defaults$X1>= mean(predicted_defaults$X1), "yes", "no"))
 PerformanceTag<- as.factor(ifelse(test$Performance.Tag == 1, "yes", "no"))
 table(PerformanceTag)
 # no   yes 
 # 13382   589 
 table(predicted_perf_tag)
 # no  yes 
 # 8165 5806 
 conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
 conf$table
 # Reference
 # Prediction   no  yes
 # no  7941  224
 # yes 5441  365
 conf$overall
 # Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
 # 0.59451721     0.04071703     0.58632096     0.60267408     0.95784124 
 # AccuracyPValue  McnemarPValue 
 # 1.00000000     0.00000000 
 ### Find optimal cutoff to maximize sensitivity and specificity
 cutoff<-getPlotCutoff(predicted_defaults$X1,test$Performance.Tag,0.07)
 cutoff
 # 0.07762238
 
 predicted_perf_tag<-factor(ifelse(predicted_defaults$X1 >= cutoff, "yes", "no"))
 table(predicted_perf_tag)
 levels(test$Performance.Tag)
 PerformanceTag<-factor(ifelse(test$Performance.Tag ==1, "yes", "no"))
 table(PerformanceTag)
 
 conf <- confusionMatrix(predicted_perf_tag, PerformanceTag, positive = "yes")
 acc <- conf$overall[1]
 sens <- conf$byClass[1]
 spec <- conf$byClass[2]
 acc
 sens
 spec
 # Accuracy 
 # 0.6267268
 # Sensitivity 
 # 0.5670628 
 # Specif.icity 
 # 0.583844
 
 ### Kstatistics for descriminatory power
 Kstat<-calculateKStat(predicted_perf_tag,PerformanceTag)
 Kstat
 #0.1964157
 ##For a good model, KS statistic would be more than 40% and would lie in the top few deciles
 
 #### Lift and gain
 # bucket total totalresp Cumresp  Gain Cumlift
 # <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
 #   1      1  1398     122.     122.  11.6    1.16
 # 2      2  1397     101.     223.  21.2    1.06
 # 3      3  1397     102.     325.  30.9    1.03
 # 4      4  1397     105.     429.  40.8    1.02
 # 5      5  1397     107.     536.  51.0    1.02
 # 6      6  1397     104.     641.  60.9    1.02
 # 7      7  1397     105.     746.  70.9    1.01
 # 8      8  1397     102.     848.  80.7    1.01
 # 9      9  1397     107.     955.  90.8    1.01
 # 10     10  1397      96.7   1052. 100.     1.00
 
 ### Scrutinizing applications from top 5 buckets (1397*5) will identify 51.0 % of the predicted defaults
 
 
 
 ######################################################################
 #####      MODEL SELECTION BASED ON EVALUATION ###############
 
 ##SELECTED MODEL IS LOGISTIC REGRESSION ON BALANCED WOE DATA based on 
 ## highest accuracy/sensitivity and specificity
 #################################################################
 
 
 ##################################################################
 ####  APPLICATION SCORECARD BASED ON SELECTED MODEL  ###############
 ###################################################################
 
 ## Basis of scorecard :
 ##good to bad odds of 10 to 1 at a score of 400 doubling every 20 points.  
 # goods	bads	Total	p good	    p bad	      odds	ln odds	    SCORE
 # 10	    1	    11	  0.909090909	0.090909091	10	  2.302585093	400
 # 20	    1	    21	  0.952380952	0.047619048	20	  2.995732274	420
 ### Solving equations
 ## 400 = m*2.302585093  + c
 ## 420 = m*2.995732274  + c
 ## m = multifuling factor = 28.8539
 ## C = offset / intercept = 333.56144
 ## Equation of scorecard
 ## Score = 28.8539* logodds + 333.56144
 
 ### Using selected model for building application score card
 predicted_defaults_total<-predict(Model_LR_woe_bal, type = "response", 
                                   
                                   newdata = cust_LR_woe_bal[,-1])
 summary(predicted_defaults_total)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.04764 0.08261 0.15402 0.16904 0.23559 0.39667 
 
 m<- 28.8539
 c<-333.56144
 Pbad<-predicted_defaults_total
 Pgood<-1-Pbad
 odds_model<-Pgood/Pbad
 ln_odds<-log(odds_model)
 summary(ln_odds)
 score_app<-(m*ln_odds)+c
 plot(score_app)
 min(score_app)
 #345.6618
 max(score_app)
 #419.9896
 #### Initital score cut off used corresponds to the bad probability cut off used in model
 score_cutoff<-(m*log((1-0.19570994)/0.19570994)) + c
 score_cutoff
 #374.314
 Application_scorecard<-data.frame(customer$Application.ID,cust_LR_woe_bal$Performance.Tag,predicted_defaults_total,Pgood,Pbad,odds_model,ln_odds,score_app)
 plot (Application_scorecard$score_app,Application_scorecard$predicted_defaults_total,xlab = "Scores", ylab="Predicted default probability", main = "Mapping scores to default probability")
 hist(Application_scorecard$score_app,xlab = "Scores",  main = "Score Distribution")
 
 #######################################################
 ## TUNING score cutoff based on financial calculation##
 #######################################################
 ### ASSUMPTIONS
 ## AVerage credit limit per customer = 100000
 ##Avg credit utillization per customer = 50%
 # Bank Earning per transaction = 1.5%
 ## Business gain for bank = Crdit limit* avg credit util*earning per transaction*good customers
 ## credit loss for bank = bad customers * 4 months(120 days) credit util
  cutoff_score <- 374  
 ##### Findinf the optimal cutoff where net gain in business would be maximum
 j<-1
 business_lost<-vector(mode="numeric", length=0)
 credit_loss_saved<-vector(mode="numeric", length=0)
 net_gain<-vector(mode="numeric", length=0)
 
 score_range<-vector(mode="numeric", length=0)
 #### Checking for a range of cutoff score
 for (i in seq(cutoff_score-30,cutoff_score+30, by=1)){
   bad_rejected<-length(which(Application_scorecard$cust_LR_woe_bal.Performance.Tag==1 & Application_scorecard$score_app< i))
   reject_count<-length(which(Application_scorecard$score_app<=i))
   business_lost[j]<-(reject_count)*100000*0.5*1.5*12/100 
   credit_loss_saved[j]<-(bad_rejected)*100000*0.5*4
   net_gain[j] <- credit_loss_saved[j]-business_lost[j]
   score_range[j]<-i
   j<-j+1
 }
 net_gain <- credit_loss_saved-business_lost
 ideal_score_cutoff<-cutoff_score-30+which(net_gain==max(net_gain))-1
 plot(score_range, net_gain, type="l", xlab="Cut off score",ylab="Net Gain",cex = 5, col = "dark red")+abline(v=ideal_score)+title(main= "Cut off score for maximum business gain")
 ideal_score_cutoff
 #376
 Good_count<-length(which(Application_scorecard$score_app>ideal_score_cutoff))
 Good_count
 #41402
 Bad_count<-length(which(Application_scorecard$score_app<=ideal_score_cutoff))
 Bad_count
 #28456
 ### With scorecard, acceptance ratio is
 acceptance_ratio <- Good_count*100/(Good_count+Bad_count)
 #59.26594
 ## Original acceptance ratio from dataset
 original_acceptance_ratio<-length(which(!is.na(master$Performance.Tag)))*100/length(master$Performance.Tag)
 #98.00109
 ### Acceptance proccess would be automatic and more stringent
 
 #############################################################
 ###### Checking model effectiveness for rejected customers################
 #####################################################################
 
 customer_rejected <- subset(master, is.na(master$Performance.Tag))
 str(customer_rejected)
 customer_rejected <- customer_rejected[,-1]
  #Check for duplicate application IDs
 sum(customer_rejected[which(duplicated(customer_rejected$Application.ID)),1])
 # no duplicate records found
 
 # na values
 sum(is.na(customer_rejected))
 # 1462 values found (1425 are form Performance.Tag columns)
 colnames(customer_rejected[apply(customer_rejected,2,anyNA)])
 # [1] "Performance.Tag"                        
 # [2] "Education"                             
 # [3] "Profession"                             
 # [4] "Avgas.CC.Utilization.in.last.12.months"
 
 # NA treatment Education
 sum(is.na(customer_rejected$Education))
 #1 NA records
 
 # Checking other values
 table(customer_rejected$Education)
 # Bachelor      Masters       Others          Phd Professional 
 # 395          489            2           85          453 
 
 # replacing education NA with MODAL class that is Professional
 ##Merge with professionals
 
 customer_rejected[which(is.na(customer_rejected$Education)),"Education"]<-"Professional"
 table(customer_rejected$Education)
 
 # Profession
 sum(is.na(customer_rejected$Profession))
 # Just 1 records
 table(customer_rejected$Profession)
 # SAL      SE SE_PROF 
 # 765     380     279  
 
 #Replacing NA with MODAL profession
 customer_rejected[which(is.na(customer_rejected$Profession)),"Profession"]<-"SAL"
 table(customer_rejected$Profession)
 ## profession NA -merge with mode
 
 # Averge CC Utiliization
 
 sum(is.na(customer_rejected$Avgas.CC.Utilization.in.last.12.months))
 #35 significant number of records (2.45%)
 ### These applicants are not having any other credit card
 # Not to be deleted. To be treated separately or with WOE transformation
 ### OR understand its link with others and take average
 ## Understanding distribution of CC utiliization
 table(customer_rejected$Avgas.CC.Utilization.in.last.12.months)
 quantile(customer_rejected$Avgas.CC.Utilization.in.last.12.months,probs=seq(0,1,0.01),na.rm=TRUE)
 # No outliers BUT signifinat number of records / concentration at the highest value of CC utillization 101-- TO be understood and treated separately
 
 meanCC<-mean(customer_rejected$Avgas.CC.Utilization.in.last.12.months,na.rm = T)
 customer_rejected[which(is.na(customer_rejected$Avgas.CC.Utilization.in.last.12.months)),]$Avgas.CC.Utilization.in.last.12.months <- meanCC
 
 summary(customer_rejected$Avgas.CC.Utilization.in.last.12.months)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 1.00   36.00   51.08   51.08   66.00  101.00 
                             
 customer_rejected <- scaleNumericCols(customer_rejected)
 
 customer_rejected <- dummy.data.frame(customer_rejected)
 
 
 
 # lets predict rejected dataset on Model_RawLR_bal
 levels(customer_rejected$Performance.Tag)
 #NULL
 
 ## Prediction
 rejected_pred <- predict(Model_LR_bal,newdata = customer_rejected[,-1], type = "response")
 summary(rejected_pred)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.03319 0.12497 0.16371 0.17370 0.21460 0.39236
 m<- 28.8539
 c<-333.56144
 Pbad<-rejected_pred
 Pgood<-1-Pbad
 odds_model<-Pgood/Pbad
 ln_odds<-log(odds_model)
 summary(ln_odds)
 score_app<-(m*ln_odds)+c
 summary(score_app)
 
 possible_accepts<-length(which(score_app >= 374)) 
 # 966 customers from customer rejected are good customer
 possible_rejects<-length(which(score_app <= 374)) 
 #  459 customers from customer rejected are bad customer
 
 approval_rate <- (966/1425)*100
 approval_rate
 #67.78947
 
 ### Possible annual business gain by implementation of model
 ## average credit limit * avg cc util * charges per transactions *12months 
 ##966*100000*0.5*1.5*12/100 = 8694000
 
 
 #######################EOF############################