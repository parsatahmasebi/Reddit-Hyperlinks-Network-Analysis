setwd("~/Desktop/Winter Quarter/Customer & Social Analytics/Final Project Reddit")
library("dplyr")
library("tidyverse")
library("igraph")
library("readr")
library("tidyr")
install.packages("threejs")
library("threejs")
install.packages("writexl")

install.packages("GGally")
library(GGally)
library("dplyr")
library("tidyverse")

install.packages("pastecs")
library("pastecs")
library("psych")
library("sqldf")
install.packages("MatchIt")
library(MatchIt)
library("ggplot2")
library(dplyr)
library(ggplot2)
install.packages("oddsratio")
library(oddsratio)


#importing the data
df_body2 <- read.csv("log-reg-finalexam277.csv", sep = ",") 

head(df_body2)
colnames(df_body2)

sum(is.na(df_body2))
str(df_body2)
summary(df_body2)
# Removing any NAs
ompleterecords <- na.omit(df_body2) 



#Changing the -1 nd 1 values of Linksentiment column to 0 and 1

df_body2$LINK_SENTIMENT [df_body2$LINK_SENTIMENT  > 1] <- 1
df_body2$LINK_SENTIMENT [df_body2$LINK_SENTIMENT < 1] <- 0

library("psych")


# Only selecting a subset of 86 columns which expressed the features of the posts in our data
df_body2 %>%
  select(LINK_SENTIMENT, LIWC_Quant,	,LIWC_Numbers,	LIWC_Social,	LIWC_Family,	
         LIWC_Friends,	LIWC_Humans,	LIWC_Affect,	LIWC_CogMech,	LIWC_Cause,	LIWC_Discrep,	LIWC_Tentat,	
         LIWC_Certain,	LIWC_Percept,	LIWC_See,	LIWC_Hear,	LIWC_Feel,	LIWC_Body,	LIWC_Health,	LIWC_Sexual,	
         LIWC_Time,	LIWC_Work,	
         LIWC_Home,	LIWC_Money,	LIWC_Relig,	LIWC_Assent,	LIWC_Nonflu,	LIWC_Filler) -> df_reg





# Logistic regression before any feature processing

log_reg_bef<-glm(LINK_SENTIMENT ~ LIWC_Quant +	LIWC_Numbers  +	LIWC_Social  +	LIWC_Family  +LIWC_Friends +	LIWC_Humans  +
      LIWC_Affect  +	LIWC_CogMech  +		LIWC_Cause  +		LIWC_Discrep  +		LIWC_Tentat  +	LIWC_Certain  +	
      LIWC_Percept  +	LIWC_See  +		LIWC_Hear  +		LIWC_Feel  +	LIWC_Body  +	LIWC_Health  +	LIWC_Sexual +	
      LIWC_Time  +	LIWC_Work  +LIWC_Home+LIWC_Money +LIWC_Relig  +	LIWC_Assent +LIWC_Nonflu  + 
        LIWC_Filler,family = binomial(), data = df_reg)

summary(log_reg_bef)

# Only Significant variables from above logisitic regression

log_reg_bef<-glm(LINK_SENTIMENT ~ LIWC_Quant +	LIWC_Numbers   +	LIWC_Family  +	LIWC_Humans  +
                   LIWC_Affect  +	LIWC_CogMech  +		LIWC_Cause  +		LIWC_Discrep  +		LIWC_Tentat  +	LIWC_Certain  +	
                   LIWC_Percept  +	LIWC_See  +		LIWC_Hear    +	LIWC_Body  +	LIWC_Health  +	
                   LIWC_Time  +	LIWC_Work  +LIWC_Home +LIWC_Relig  +	LIWC_Assent +LIWC_Nonflu  + 
                   LIWC_Filler,family = binomial(), data = df_reg)

summary(log_reg_bef)
exp(coef(log_reg_bef))


# Let's create a correlation matrix


corrplot::corrplot(cor(df_reg))

#Let's remove the variable with high multicollinearity assent, LIWC_Percept


df_body2 %>%
  select(LINK_SENTIMENT,	,LIWC_Numbers,	LIWC_Social,	LIWC_Family,	LIWC_Friends,	LIWC_Humans,	
         LIWC_Affect,	LIWC_CogMech,	LIWC_Cause,	LIWC_Discrep,	LIWC_Tentat,	LIWC_Certain,	LIWC_See,
         LIWC_Hear,	LIWC_Feel,	LIWC_Body,	LIWC_Health,	LIWC_Sexual,	LIWC_Time,	LIWC_Work,	
         LIWC_Home,	LIWC_Money,	LIWC_Relig,	,	LIWC_Nonflu,	LIWC_Filler) -> df_reg_removed_multicol

corrplot::corrplot(cor(df_reg_removed_multicol))

### Second round of removal : ,	LIWC_Hear,,	LIWC_Certain

df_body2 %>%
  select(LINK_SENTIMENT,LIWC_Numbers,	LIWC_Social,	LIWC_Family,	LIWC_Friends,	LIWC_Humans,	LIWC_Affect,	LIWC_CogMech,	
         LIWC_Cause,	
         LIWC_Discrep,	LIWC_Tentat,	LIWC_See,	LIWC_Feel,	LIWC_Body,	LIWC_Health,	LIWC_Sexual,	LIWC_Time,	LIWC_Work,	
         LIWC_Home,	LIWC_Money,	LIWC_Relig,	,	LIWC_Nonflu,	LIWC_Filler) -> df_reg_removed_multicol2

#Let's see the distributions

boxplot(df_reg_removed_multicol2[,-1], horizontal=TRUE, main="Boxplots")

# Multiple histograms
par(mfrow=c(13, 2))
par("mar")
par(mar=c(1,1,1,1))
hist(df_reg_removed_multicol2, main = "Distributions")


# Check for 0 values in columns:
# Check for columns that contain infinity after getting logged
apply(df_reg_removed_multicol2, 2, function(x) any(is.null(x)))

# we will take log from all the variables
df_reg_removed_multicol2 %>%
  mutate(LIWC_Numbers=log(LIWC_Numbers+1),LIWC_Social = log(LIWC_Social+1),LIWC_Family=log(LIWC_Family+1),
         LIWC_Friends=log(LIWC_Friends+1), LIWC_Humans = log(LIWC_Humans+1), LIWC_Affect = log(LIWC_Affect+1),
         LIWC_CogMech = log(LIWC_CogMech+1), LIWC_Cause = log(LIWC_Cause+1) , LIWC_Discrep = log(LIWC_Discrep+1),
         LIWC_Tentat = log(LIWC_Tentat+1) , LIWC_See  = log(LIWC_See+1) , LIWC_Feel = log(LIWC_Feel+1) ,
         LIWC_Body = log(LIWC_Body+1) , LIWC_Health = log(LIWC_Health+1), LIWC_Sexual= log(LIWC_Sexual+1),
         LIWC_Time = log(LIWC_Time+1) , LIWC_Work = log(LIWC_Work+1) , LIWC_Home = log(LIWC_Home+1), LIWC_Money = log(LIWC_Money+1),
         LIWC_Relig = log(LIWC_Relig+1), LIWC_Nonflu = log(LIWC_Nonflu+1) , LIWC_Filler = log(LIWC_Filler+1)  ) -> df_reg_removed_multicol2_log

# Check for columns that contain inifinity after getting logged
apply(df_reg_removed_multicol2_log, 2, function(x) any(is.infinite(x)))


## Now let's do logistic regression on the processed variables
df_reg_removed_multicol2_log$LINK_SENTIMENT [df_reg_removed_multicol2_log$LINK_SENTIMENT  > 1] <- 1
df_reg_removed_multicol2_log$LINK_SENTIMENT [df_reg_removed_multicol2_log$LINK_SENTIMENT < 1] <- 0



log_reg<- glm(LINK_SENTIMENT ~ LIWC_Social +	LIWC_Family +	LIWC_Friends +	LIWC_Humans +	LIWC_Affect +	
                LIWC_CogMech +	LIWC_Cause+	
              LIWC_Discrep +		LIWC_Tentat +		LIWC_See +		LIWC_Feel +		LIWC_Body +		LIWC_Health +		
                LIWC_Sexual +		LIWC_Time +		LIWC_Work +	
              LIWC_Home +		LIWC_Money +		LIWC_Relig + LIWC_Nonflu +		LIWC_Filler ,	family = binomial(), 
              data = df_reg_removed_multicol2_log)

summary(log_reg)

# Log Reg for only significant variables

log_reg2<- glm(LINK_SENTIMENT ~ LIWC_Social +	LIWC_Affect +	LIWC_CogMech +	LIWC_Cause+	
              +		LIWC_Tentat +		LIWC_See +		LIWC_Body   +		LIWC_Work 
                 +		LIWC_Filler ,	family = binomial(), 
              data = df_reg_removed_multicol2_log)

summary(log_reg2)

##Except LIWC_Money is not significant this time so we do not interpret its coefficients

exp(coef(log_reg2))


## Test- Train

# Let's see the histogram of Link Sentiment
prop.table(table(df_body2$LINK_SENTIMENT))
barplot(prop.table(table(df_body2$LINK_SENTIMENT)), col = rainbow(2) , ylim=c (0,1), main = "Class Distribution For LinkSentiment")



require(caTools)
install.packages(caTools)
library(caTools)


train_index <- sample.split(Y = df_reg_removed_multicol2_log$LINK_SENTIMENT , SplitRatio = 0.7)
train_data <- df_reg_removed_multicol2_log[train_index, ]
test_data <- df_reg_removed_multicol2_log[!train_index, ]



model <- glm(LINK_SENTIMENT ~.,family=binomial(link='logit'),data=train_data)

summary(model)
anova(model)


fitted.results <- predict(model, test_data, type='response')
                          
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test_data$LINK_SENTIMENT)
print(paste('Accuracy',1-misClasificError))

"Accuracy 0.92641447980644"


# ROC Curve
require(ROCR)
install.packages("ROCR")
library(ROCR)

# tpr is true positive rate but there are whole lots of other measures --> look at the the documentation.

p <- predict(model, newdata= test_data, type="response")
pr <- prediction(p, test_data$LINK_SENTIMENT)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

0.655816


#Interpreting the ROC curve

#ROC curve
#The ROC curve shows the trade-off between sensitivity (or TPR) and specificity (1 – FPR). 
#Classifiers that give curves closer to the top-left corner indicate a better performance. 
#As a baseline, a random classifier is expected to give points lying along the diagonal (FPR = TPR). 
#The closer the curve comes to the 45-degree diagonal of the ROC space, the less accurate the test.
#Note that the ROC does not depend on the class distribution. This makes it useful for evaluating classifiers predicting rare events such as diseases or disasters. In contrast, evaluating performance using accuracy (TP +
# TN)/(TP + TN + FN + FP) would favor classifiers that always predict a negative outcome for rare events.


#Logistic Regression after Oversampling in order to create a more balanced data

#over sampling

#install packages
install.packages("ROSE")
library(ROSE)

table(df_reg_removed_multicol2_log$LINK_SENTIMENT)
data_balanced_over <- ovun.sample(LINK_SENTIMENT ~ ., data = train_data, method = "over")$data
table(data_balanced_over$LINK_SENTIMENT)



log_reg_balanced_1<- glm(LINK_SENTIMENT ~ LIWC_Social +	LIWC_Family +	LIWC_Friends +	LIWC_Humans +	LIWC_Affect +	LIWC_CogMech +	LIWC_Cause+	
                LIWC_Discrep +		LIWC_Tentat +		LIWC_See +		LIWC_Feel +		LIWC_Body +		LIWC_Health +		LIWC_Sexual +		LIWC_Time +		LIWC_Work +	
                LIWC_Home +		LIWC_Money +		LIWC_Relig + LIWC_Nonflu +		LIWC_Filler ,	family = binomial(), 
              data = data_balanced_over)

summary(log_reg_balanced_1)
exp(coef(log_reg_balanced_1))


######## Doing the same analysis for the title file
setwd("~/Desktop/Winter Quarter/Customer & Social Analytics/Final Project Reddit")
df_title2 <- read.table(file = "soc-redditHyperlinks-title.tsv", sep = "\t", header = TRUE)

df_title2 %>%
  separate(PROPERTIES , c( "Char_num" , "char_num_no_space", "alpha_char_frac" , "digits_frac" , "upper_frac" , "whitespace_frac",
                           "special_char_frac" , "word_cnt" , "unique_word_cnt" , "longword_cnt" , "avg_word_length" , "unique_stopword_cnt",
                           "sentences_cnt" , "longsentences_cnt" , "avg_char_persentence" , "avg_word_sentence" , "auto_readable_index",
                           "pos_sent_vad" , "neg_sent_vad" , "component_sent_vad", "LIWC_Funct",	"LIWC_Pronoun",	"LIWC_Ppron",	
                           "LIWC_I",	"LIWC_We",	"LIWC_You",	"LIWC_SheHe",	"LIWC_They",	
                           "LIWC_Ipron",	"LIWC_Article",	"LIWC_Verbs",	"LIWC_AuxVb",	
                           "LIWC_Past",	"LIWC_Present",	"LIWC_Future",	"LIWC_Adverbs",	"LIWC_Prep",	
                           "LIWC_Conj",	"LIWC_Negate",	
                           "LIWC_Quant",	"LIWC_Numbers",	"LIWC_Swear",	"LIWC_Social",	"LIWC_Family",	
                           "LIWC_Friends",	"LIWC_Humans",	"LIWC_Affect",	"LIWC_Posemo",	"LIWC_Negemo",	"LIWC_Anx",
                           "LIWC_Anger",	"LIWC_Sad",	"LIWC_CogMech",	"LIWC_Insight",	"LIWC_Cause",	"LIWC_Discrep",	
                           "LIWC_Tentat",	"LIWC_Certain",	"LIWC_Inhib",	"LIWC_Incl",	"LIWC_Excl",	"LIWC_Percept",	
                           "LIWC_See",	"LIWC_Hear",	"LIWC_Feel",	"LIWC_Bio",	"LIWC_Body",	"LIWC_Health",	
                           "LIWC_Sexual",	"LIWC_Ingest",	"LIWC_Relativ",	"LIWC_Motion",	"LIWC_Space",	"LIWC_Time",	
                           "LIWC_Work",	"LIWC_Achiev",	"LIWC_Leisure",	"LIWC_Home",	"LIWC_Money",	"LIWC_Relig",	
                           "LIWC_Death",	"LIWC_Assent",	"LIWC_Dissent",	"LIWC_Nonflu", "LIWC_Filler")) -> df_title2


str(df_title2)                          



head(df_title2)
colnames(df_title2)

sum(is.na(df_title2))

df_title2 <- na.omit(df_title2) 

summary(df_reg_title)




df_title2$LINK_SENTIMENT [df_title2$LINK_SENTIMENT  > 1] <- 1
df_title2$LINK_SENTIMENT [df_title2$LINK_SENTIMENT < 1] <- 0
as.factor(df_title2$LINK_SENTIMENT)


df_title2 %>%
  select(LINK_SENTIMENT, LIWC_Quant,	,LIWC_Numbers,	LIWC_Social,	LIWC_Family,	LIWC_Friends,	LIWC_Humans,	LIWC_Affect,	LIWC_CogMech,	LIWC_Cause,	LIWC_Discrep,	LIWC_Tentat,	LIWC_Certain,	LIWC_Percept,	LIWC_See,	LIWC_Hear,	LIWC_Feel,	LIWC_Body,	LIWC_Health,	LIWC_Sexual,	LIWC_Time,	LIWC_Work,	
         LIWC_Home,	LIWC_Money,	LIWC_Relig,	LIWC_Assent,	LIWC_Nonflu,	LIWC_Filler) -> df_reg_title





# Log reg before removing multicolinearity

log_reg_bef_title<-glm(LINK_SENTIMENT ~ LIWC_Quant +	LIWC_Numbers  +	LIWC_Social  +	LIWC_Family  +LIWC_Friends +	LIWC_Humans  +
                   LIWC_Affect  +	LIWC_CogMech  +		LIWC_Cause  +		LIWC_Discrep  +		LIWC_Tentat  +	LIWC_Certain  +	
                   LIWC_Percept  +	LIWC_See  +		LIWC_Hear  +		LIWC_Feel  +	LIWC_Body  +	LIWC_Health  +	LIWC_Sexual +	
                   LIWC_Time  +	LIWC_Work  +LIWC_Home+LIWC_Money +LIWC_Relig  +	LIWC_Assent +LIWC_Nonflu  + LIWC_Filler,family = binomial(), data = df_title2)

summary(log_reg_bef_title)

# Only Significants

log_reg_bef_title<-glm(LINK_SENTIMENT ~ LIWC_Quant +	LIWC_Numbers   +	LIWC_Family  +	LIWC_Humans  +
                   LIWC_Affect  +	LIWC_CogMech  +		LIWC_Cause  +		LIWC_Discrep  +		LIWC_Tentat  +	LIWC_Certain  +	
                   LIWC_Percept  +	LIWC_See  +		LIWC_Hear    +	LIWC_Body  +	LIWC_Health  +	
                   LIWC_Time  +	LIWC_Work  +LIWC_Home +LIWC_Relig  +	LIWC_Assent +LIWC_Nonflu  + LIWC_Filler,family = binomial(), data = df_reg_title)

summary(log_reg_bef_title)
exp(coef(log_reg_bef_title))


# Let's create a correlation matrix



corrplot::corrplot(cor(df_title2))

#Let's remove the multicolinearity assent, LIWC_Percept


df_body2 %>%
  select(LINK_SENTIMENT,	,LIWC_Numbers,	LIWC_Social,	LIWC_Family,	LIWC_Friends,	LIWC_Humans,	LIWC_Affect,	LIWC_CogMech,	LIWC_Cause,	LIWC_Discrep,	LIWC_Tentat,	LIWC_Certain,	LIWC_See,	LIWC_Hear,	LIWC_Feel,	LIWC_Body,	LIWC_Health,	LIWC_Sexual,	LIWC_Time,	LIWC_Work,	
         LIWC_Home,	LIWC_Money,	LIWC_Relig,	,	LIWC_Nonflu,	LIWC_Filler) -> df_reg_removed_multicol

corrplot::corrplot(cor(df_reg_removed_multicol))

### Second round of removal : ,	LIWC_Hear,,	LIWC_Certain

df_body2 %>%
  select(LINK_SENTIMENT,LIWC_Numbers,	LIWC_Social,	LIWC_Family,	LIWC_Friends,	LIWC_Humans,	LIWC_Affect,	LIWC_CogMech,	LIWC_Cause,	
         LIWC_Discrep,	LIWC_Tentat,	LIWC_See,	LIWC_Feel,	LIWC_Body,	LIWC_Health,	LIWC_Sexual,	LIWC_Time,	LIWC_Work,	
         LIWC_Home,	LIWC_Money,	LIWC_Relig,	,	LIWC_Nonflu,	LIWC_Filler) -> df_reg_removed_multicol2

#Let's see the distributions
#HISTOGRAN CODE COMES HERE
# Check for 0 values in columns:
# Check for columns that contain inifinity after getting logged
apply(df_reg_removed_multicol2, 2, function(x) any(is.null(x)))

# we will take log from all the variables
df_reg_removed_multicol2 %>%
  mutate(LIWC_Numbers=log(LIWC_Numbers+1),LIWC_Social = log(LIWC_Social+1),LIWC_Family=log(LIWC_Family+1),
         LIWC_Friends=log(LIWC_Friends+1), LIWC_Humans = log(LIWC_Humans+1), LIWC_Affect = log(LIWC_Affect+1),
         LIWC_CogMech = log(LIWC_CogMech+1), LIWC_Cause = log(LIWC_Cause+1) , LIWC_Discrep = log(LIWC_Discrep+1),
         LIWC_Tentat = log(LIWC_Tentat+1) , LIWC_See  = log(LIWC_See+1) , LIWC_Feel = log(LIWC_Feel+1) ,
         LIWC_Body = log(LIWC_Body+1) , LIWC_Health = log(LIWC_Health+1), LIWC_Sexual= log(LIWC_Sexual+1),
         LIWC_Time = log(LIWC_Time+1) , LIWC_Work = log(LIWC_Work+1) , LIWC_Home = log(LIWC_Home+1), LIWC_Money = log(LIWC_Money+1),
         LIWC_Relig = log(LIWC_Relig+1), LIWC_Nonflu = log(LIWC_Nonflu+1) , LIWC_Filler = log(LIWC_Filler+1)  ) -> df_reg_removed_multicol2_log

# Check for columns that contain inifinity after getting logged
apply(df_reg_removed_multicol2_log, 2, function(x) any(is.infinite(x)))




## Now let's do logistic regression
df_reg_removed_multicol2_log$LINK_SENTIMENT [df_reg_removed_multicol2_log$LINK_SENTIMENT  > 1] <- 1
df_reg_removed_multicol2_log$LINK_SENTIMENT [df_reg_removed_multicol2_log$LINK_SENTIMENT < 1] <- 0



log_reg<- glm(LINK_SENTIMENT ~ LIWC_Social +	LIWC_Family +	LIWC_Friends +	LIWC_Humans +	LIWC_Affect +	LIWC_CogMech +	LIWC_Cause+	
                LIWC_Discrep +		LIWC_Tentat +		LIWC_See +		LIWC_Feel +		LIWC_Body +		LIWC_Health +		LIWC_Sexual +		LIWC_Time +		LIWC_Work +	
                LIWC_Home +		LIWC_Money +		LIWC_Relig + LIWC_Nonflu +		LIWC_Filler ,	family = binomial(), 
              data = df_reg_removed_multicol2_log)

summary(log_reg)

# Log Reg for only significant variables

log_reg2<- glm(LINK_SENTIMENT ~ LIWC_Social +	LIWC_Affect +	LIWC_CogMech +	LIWC_Cause+	
                 +		LIWC_Tentat +		LIWC_See +		LIWC_Body   +		LIWC_Work +	LIWC_Money 
               +		LIWC_Filler ,	family = binomial(), 
               data = df_reg_removed_multicol2)

summary(log_reg2)

##Except LIWC_Money is not significant this time so we do not interpret its coefficients

exp(coef(log_reg2))


## Test- Train

# Let's see the histogram of Link Sentiment
prop.table(table(df_body2$LINK_SENTIMENT))
barplot(prop.table(table(df_body2$LINK_SENTIMENT)), col = rainbow(2) , ylim=c (0,1), main = "Class Distribution For LinkSentiment")



require(caTools)
install.packages(caTools)


train_index <- sample.split(Y = df_reg_removed_multicol2_log$LINK_SENTIMENT , SplitRatio = 0.7)
train_data <- df_reg_removed_multicol2_log[train_index, ]
test_data <- df_reg_removed_multicol2_log[!train_index, ]



model <- glm(LINK_SENTIMENT ~.,family=binomial(link='logit'),data=train_data)

summary(model)
anova(model)

# Original code below
#fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- predict(model, test_data, type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test_data$LINK_SENTIMENT)
print(paste('Accuracy',1-misClasificError))

"Accuracy 0.92641447980644"




# ROC Curve
require(ROCR)
install.packages("ROCR")
library(ROCR)
# tpr is true positive rate but there are whole lots of other measures --> look at the the documentation.
p <- predict(model, newdata= test_data, type="response")
pr <- prediction(p, test_data$LINK_SENTIMENT)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

0.655816


# Oversampling


#install packages
install.packages("ROSE")
library(ROSE)

table(df_reg_removed_multicol2_log$LINK_SENTIMENT)
data_balanced_over <- ovun.sample(LINK_SENTIMENT ~ ., data = train_data, method = "over")$data
table(data_balanced_over$LINK_SENTIMENT)



log_reg_balanced_1<- glm(LINK_SENTIMENT ~ LIWC_Social +	LIWC_Affect +	LIWC_CogMech +	LIWC_Cause+	
                           +		LIWC_Tentat +		LIWC_See +		LIWC_Body   +		LIWC_Work 
                         +		LIWC_Filler ,	family = binomial(), 
                         data = data_balanced_over)

summary(log_reg_balanced_1)
exp(coef(log_reg_balanced_1))










