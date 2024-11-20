library(dplyr)
library(dendextend)
library(RColorBrewer)
library(reshape2)
library(ggthemes)
#install.packages("ggthemes") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'ggthemes'"
library(GGally)
library(hdrcde)
#install.packages("hdrcde") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'hdrcde'"
library(KernSmooth)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(wordcloud)
#install.packages("wordcloud") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'wordcloud'"
library(wordcloud2)
#install.packages("wordcloud2") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'wordcloud2'"
library(wesanderson)
#install.packages("wesanderson") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'wesanderson'"
library(plotly)
library(kernlab)
library(vscc)
library(caret)

diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's Path to the Data Set

diabetes_data <- read.csv(data)


df <- diabetes_data
## quick look
head(df)
str(df)

# Dimensions of the data
dim(df)


#summary statistics
summary(df)

# finds the count of missing values 
sum(is.na(df))
# No N/A values

# finds duplicated rows
sum(duplicated(df))
# No duplicated rows

###pair plot 
ggpairs(df[,-c(9)], aes(colour=as.factor(Outcome), alpha=0.4),lower=list(continuous="points"),
        axisLabels="none", switch="both")

### bar plot 
# one variable, Tells us how many people do and do not have diabetes
outcomes <- df$Outcome
outcomes <- as.factor(outcomes)
plot(outcomes, xlab = "Diabetes", ylab = "Number of Individuals", 
     main = "Diabetes Dataset Outcomes Distribution", col = "lightgreen")


# Age distribution
ages <- df$Age
hist(ages, xlab = "Ages", main = "Age Distribution in Diabetes Dataset", col="skyblue")


## Correlations 

data2<-df[,-c(9)]
corr1<-cor(data2)
corrplot(corr1, bg = "white", 
         type="lower", tl.cex = 0.75, 
         tl.col="black", tl.srt = 45)


# Principal Component Analysis (PCA)

diabetes_pca<-prcomp(diabetes_data,scale=TRUE)
diabetes_pca

summary(diabetes_pca) # 80 percent rules suggests that we should take 5 principle components.
sum(diabetes_pca$sdev^2)

eigenvals <- diabetes_pca$sdev^2
eigenvals

cumsum(eigenvals)/sum(eigenvals) # same as last line in summary(heptathlon_pca2) "Cumulative Proportion"

plot(eigenvals,xlab="Principal Component",ylab="Eigenvalue",main="Eigenvalue vs. Principal) Component",type ="l" ) #Eigenvalues suggest that we should take 3 principle components.


# Normality Test for Factor Analysis. 

shapiro.test(diabetes_data$Pregnancies) # Not normal, p= 2.2e-16

shapiro.test(diabetes_data$Glucose) # Not normal

shapiro.test(diabetes_data$BloodPressure) # Not normal

shapiro.test(diabetes_data$SkinThickness) # Not normal

shapiro.test(diabetes_data$Insulin) # Not normal

shapiro.test(diabetes_data$BMI) # Not normal

shapiro.test(diabetes_data$Age) # Not normal

# Using the normal QQ-Plot to confirm that the dataset is not normally distributed
qqnorm(diabetes_data[,1])
qqline(diabetes_data[,1])

## Split Train/Test
set.seed(2024118)
train.index <- createDataPartition(diabetes_data$Outcome, p=.75, list = FALSE)
y <- diabetes_data[train.index,9]
x <- diabetes_data[train.index,-9]
y <- as.factor(y)

#############################################################################################################################################

#word cloud plot
library(kernlab)
data(spam)
attach(spam)
head(spam)
dim(spam)
names(spam)

s.data= spam[spam$type == "spam",]
ns.data=spam[spam$type == "nonspam",]
frq=function(x)round(mean(x), digits=2)
n.spam.frq= sapply(ns.data[,-58],frq)
spam.frq=sapply(s.data[,-58],frq)
##find highest freq
ns<-sort(n.spam.frq, decreasing = T)
sp<-sort(spam.frq,decreasing = T)
#make cloud words
#nonspam
#par(mfrow = c(1, 2))
u<-ns[4:57]
xx1<-names(u)
ns2<-as.data.frame(u)
set.seed(1234)
wordcloud(xx1,ns2$u,scale=c(5,.5),min.freq = 0,colors = brewer.pal(10,"Paired"),rot.per=.15,max.words=Inf, random.order=FALSE)
#spam
v<-sp[4:57]
xx<-names(v)
ns3<-as.data.frame(v)
set.seed(4321)
wordcloud(xx,ns3$v,scale=c(5,.5),min.freq = 0,colors = brewer.pal(10,"Paired"),rot.per=.40,max.words=Inf, random.order=FALSE)
par(mfrow = c(1, 1))
barplot(ns[4:13], col=rainbow(10), ylab = "Percentage Relative")

########################################
## clean data, replace missing value  ##
########################################
dfn<-data.frame(id=seq(1,10), X1=c(10,9,8,7,NA,NA,20,15,12,NA), 
           X2=factor(c("A","B","A","A","","B","A","B","","A")),
           X3=factor(c("","BB","CC","BB","BB","CC","AA","BB","","AA")),
           X4=c(NA,20,18,22,18,17,19,NA,17,23)
)
head(dfn)
# finds the count of missing values 
sum(is.na(dfn))
## replace (imputation) missing values? OR remove??
## remove? (NOT recommended)
new_df<-  na.omit(dfn) 
sum(is.na(new_df))
# replace with ( with the median)  ONLY for cont var
dfnew_med<- dfn %>% mutate_at(vars(X1,X4),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
# replace with ( with the mean )  ONLY for cont var (recommended more)
dfnew_mean <- dfn %>% mutate_at(vars(X1,X4),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
# replace with ( with the mode)   for catg var
dfnew_mean %>% mutate_at(vars(X2,X3),~ifelse(is.na(.x), mode(.x, na.rm = TRUE), .x))

##############################################
### Choosing important variables/features ####
##############################################

data(coffee, package="pgmm")
coffee <- within(coffee, Type <- ifelse(Variety==1,"Arabica", "Robusta"))
names(coffee) <- abbreviate(names(coffee), 8)
## correlation??
data(coffee, package="pgmm")
ggcorr(coffee[,-c(1,2)], label = TRUE, label_size = 3,
       label_round = 2, label_alpha = TRUE)

## separation??
data(coffee, package="pgmm")
coffee <- within(coffee, Type <- ifelse(Variety==1,"Arabica", "Robusta"))

ggpairs(coffee[,-c(1,2)], aes(colour=Type, alpha=0.4)) 
ggparcoord(coffee[order(coffee$Type),], columns=3:14,
           groupColumn="Type", scale="uniminmax") +
  xlab("") +  ylab("") +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("blue","red")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
## experience ??
## significant??
## function??
require("mclust")
data(coffee, package="pgmm")
X<-coffee[,-c(1,2)]
coffeerun <- vscc(X)
plot(coffeerun)

##########################################
######## Split data train/test ###########
##########################################
# stratified splitting/sampling (recommended more!) use %  for splitting/sampling. e.g 75% of data, test data:  25% of data

data("iris")
df=iris
set.seed(2344)
train.index <- createDataPartition(df$Outcome, p = .75, list = F)
train <- df[ train.index,]
test  <- df[-train.index,]
# random  splitting/sampling
set.seed(2344)
train_ind_raw <- sample(nrow(df), size = floor(0.75 * nrow(df)))
train<- as.data.frame(df[train_ind_raw, ])
test <- as.data.frame(df[-train_ind_raw, ])

