library(cluster)
diabetes<- read.csv("/Users/xinyichen/Desktop/diabetes_dataset.csv")
head(diabetes) 


x<-diabetes[,-9]
sum(is.na(x)) 
str(x) 


# diabetes, k-means, k=2
x<-diabetes[,-9]
diabetes_kmeans2<-kmeans(x,2)
plot(x, col = diabetes_kmeans2$cluster)
table(diabetes[,9],diabetes_kmeans2$cluster)
plot(x, col = diabetes[,9])
si2 <- silhouette(diabetes_kmeans2$cluster, dist(x))
plot(si2, nmax= 80, cex.names=0.6)
plot(si2, nmax= 80, cex.names=0.6, col=c("pink", "skyblue"), main = "")

# diabetes, k-means, k=3

x<-diabetes[,-9]
sum(is.na(x)) 
str(x) 
diabetes_kmeans<-kmeans(x,3)
plot(x, col = diabetes_kmeans$cluster)
table(diabetes[,9],diabetes_kmeans$cluster)
plot(x, col = diabetes[,9])
si1 <- silhouette(diabetes_kmeans$cluster, dist(x))
summary(si1)
plot(si1, nmax= 50, cex.names=0.6)
plot(si1, nmax= 50, cex.names=0.6, col=c("pink", "skyblue","green"), main = "")
