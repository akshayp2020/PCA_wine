#install.packages("gdata")
library(gdata)
#install.packages("cluster")
library(cluster)
wine = read.csv(choose.files())
View(wine)
View(wine[-1])
data <-wine[,-1]
attach(data)
cor(data)

#correlation
pca <-princomp(data,cor = TRUE,covmat = NULL,scores = TRUE)
summary(pca)
plot(pca)

#PCA scores of top three components
pca$scores[,1:6]

#Add PCA Scores of the three components to wine data
data1 <- cbind(wine,pca$scores[,1:6])
View(data1)

#considering only pca scores as they represent the data
clust_data <-data1[,15:20]


#Normalize the data
norm_clust<-scale(clust_data) 
dist1<-dist(norm_clust,method = "euclidean") 


# Clustering (Hierarchical)
fit<-hclust(dist1,method="complete") 
plot(fit) 
rect.hclust(fit, k=8, border="red")
bag1<-cutree(fit,8)
cluster <-as.matrix(bag1)
View(cluster)
final <-cbind(cluster,data1)
View(final)

#kmean
install.packages("plyr")
library(plyr)
View(final)
data2 <-scale(final[,15:20])

#scree plot
wss=(nrow(data2)-1)*sum(apply(data2, 2, var))
for(i in 1:7) wss[i] = sum(kmeans(data2, centers=i)$withinss)
plot(1:7,wss,type = "b",xlab = "Number of clusters", ylab = "Sum of squares")

#K-means using value of k = 7
install.packages("animation")
library(animation)
kn<-kmeans.ani(data2,7)
table(kn$cluster)

#using euclidean distance and complet linkage

dist3<-dist(norm_clust,method = "euclidean")
fit2<-hclust(dist3,method="average") #Hierarchical clustering Using average linkage
plot(fit2)

rect.hclust(fit2, k=6, border="red")
groups<-cutree(fit2,6)
#Form the clusters
cluster1 <-as.matrix(groups)
View(cluster1)
final1 <-cbind(cluster1,data1)
View(final1)

#k means 
library(plyr)
norm_data1 <- scale(final1[,15:17])

#Scree Plot 
wss1 =(nrow(norm_data1)-1)*sum(apply(norm_data1, 2, var)) 
for (i in 1:6) wss1[i] = sum(kmeans(norm_data1, centers=i)$withinss)
plot(1:6, wss1, type="b", xlab="Number of Clusters", ylab="Sum of squares")

#k means using k =6
library(animation)
ka <- kmeans.ani(norm_data1,6)
table(ka$cluster)


