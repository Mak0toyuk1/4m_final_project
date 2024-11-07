library(dplyr)
library(dendextend)
library(RColorBrewer)
library(reshape2)
library(ggthemes)
library(GGally)
library(hdrcde)
library(KernSmooth)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(wordcloud)
library(wordcloud2)
library(wesanderson)
library(RColorBrewer)
library(plotly)
library(kernlab)#
library(vscc)
library(caret)



data("iris")
df=iris
## quick look
head(df)
attach(df)
str(df)
dim(df)


#quick summary
summary(df)
# finds the count of missing values 
sum(is.na(df))

###pair plot 
ggpairs(df[,-c(5)], aes(colour=as.factor(Species), alpha=0.4),lower=list(continuous="points"),
        axisLabels="none", switch="both")
### bar plot 

# one variable

ggplot(df,aes(x=Species,y=Sepal.Length,fill=factor(Species)))+
  geom_bar(stat="identity")+theme_minimal()+
  scale_fill_brewer(palette="RdPu",name="Species")+
  theme_minimal()+ xlab("")+ylab("")
# all variables
gg <- melt(df,id="Species")   # df is your original table
ggplot(gg, aes(x=variable, y=value, fill=factor(Species))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  theme_minimal()+
  scale_fill_brewer(palette="PuBuGn",name="Species")+
  theme_minimal()+ xlab("")+ylab("")

#### pie chart
data <- df %>% 
  group_by(Species) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(Species))
data$label <- scales::percent(data$per)
ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=Species), stat="identity", width = 1)+
  coord_polar("y", start=0)+scale_fill_brewer("Species")+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)
  )

## correlation 

data2<-df[,-c(5)]
corr1<-cor(data2)
corrplot(corr1, method = "circle",bg = "white", 
         type="lower", order="hclust", tl.cex = 0.75, 
         tl.col="black", tl.srt = 45)

## another corr
ggcorr(data2, label = TRUE, label_size = 3,
       label_round = 2, label_alpha = TRUE)

### grid  plots 
ggparcoord(iris, columns=1:4, groupColumn="Species")

a <- ggplot(iris, aes("Boxplot for all", Sepal.Width)) +
  xlab("")  + geom_boxplot() +
  scale_x_discrete(breaks=NULL) 
b <- ggplot(iris, aes(Species, Sepal.Width)) + 
  geom_boxplot() +  xlab("")
grid.arrange(a, b, nrow=1, widths=c(1,2))

a <- ggplot(iris, aes("Boxplot for all", Petal.Width)) +
  xlab("")  + geom_boxplot() +
  scale_x_discrete(breaks=NULL) 
b <- ggplot(iris, aes(Species, Petal.Width)) + 
  geom_boxplot() +  xlab("")
grid.arrange(a, b, nrow=1, widths=c(1,2))

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
train.index <- createDataPartition(df$Species, p = .75, list = F)
train <- df[ train.index,]
test  <- df[-train.index,]
# random  splitting/sampling
set.seed(2344)
train_ind_raw <- sample(nrow(df), size = floor(0.75 * nrow(df)))
train<- as.data.frame(df[train_ind_raw, ])
test <- as.data.frame(df[-train_ind_raw, ])
