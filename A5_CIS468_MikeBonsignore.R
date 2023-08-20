# Mike Bonsignore
# CIS 468
# Assignment 5
# 5/7/23

library(cluster)
library(fpc)
library(scales)
library(ggplot2) 

#### DATA EXPLORATION ####
winedata <- read.csv(file = "wine-clustering.csv", stringsAsFactors = T)

View(winedata)
names(winedata)
summary(winedata)

plot(winedata)

winenorm <- winedata

View(winenorm)

#### NORMALIZATIONS ####
winenorm$Alcohol <- rescale(winedata$Alcohol, to = c(0,1))
winenorm$Malic_Acid <- rescale(winedata$Malic_Acid, to = c(0,1))
winenorm$Ash <- rescale(winedata$Alcohol, to = c(0,1))
winenorm$Ash_Alcanity <- rescale(winedata$Alcohol_Alcanity, to = c(0,1))
winenorm$Magnesium <- rescale(winedata$Alcohol, to = c(0,1))
winenorm$Total_Phenols <- rescale(winedata$Total_Phenols, to = c(0,1))
winenorm$Flavanoids <- rescale(winedata$Flavanoids, to = c(0,1))
winenorm$Nonflavanoid_Phenols <- rescale(winedata$Nonflavanoid_Phenols, to = c(0,1))
winenorm$Proanthocyanins <- rescale(winedata$Proanthocyanins, to = c(0,1))
winenorm$Color_Intensity <- rescale(winedata$Color_Intensity, to = c(0,1))
winenorm$Hue <- rescale(winedata$Hue, to = c(0,1))
winenorm$OD280 <- rescale(winedata$OD280, to = c(0,1))
winenorm$Proline <- rescale(winedata$Proline, to = c(0,1))

View(winenorm)

#plot correlations in the data
pairs(winedata)
pairs(winenorm)

#### RUNNING CLUSTERING ALGORITHM ####
km3 <- kmeans(winenorm, centers = 3)
km3
summary(as.factor(km3$cluster))

km4 <- kmeans(winenorm, centers = 4)
km4
summary(as.factor(km4$cluster))

# Visualizing clustering #
pairs(winedata, col=km3$cluster)
pairs(winedata, col=km4$cluster)
ggplot(winedata, aes(x = OD280, y = Alcohol)) + geom_point(color = km3$cluster)
ggplot(winedata, aes(x = OD280, y = Alcohol)) + geom_point(color = km4$cluster)

# Visualize k3 clustering
winedata$cluster <- as.factor(km3$cluster)
ggplot(winedata, aes(x=cluster, y=Alcohol, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("cluster") +
  ylab("Alcohol")

ggplot(winedata, aes(x=Alcohol, color=cluster, linetype=cluster, fill=cluster)) +  
  geom_density(alpha=0.5) + 
  scale_color_brewer(palette="Set1")

# Visualize k6 clustering
winedata$cluster <- as.factor(km4$cluster)
ggplot(winedata, aes(x=cluster, y=Alcohol, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("cluster") +
  ylab("Alcohol")

ggplot(winedata, aes(x=Alcohol, color=cluster, linetype=cluster, fill=cluster)) +  
  geom_density(alpha=0.5) + 
  scale_color_brewer(palette="Set1")

#### EVALUATION ####

# Distance Matrix
distm <- dist(winenorm)
distm

# Cluster Stats
cstatskm3 = cluster.stats(distm,km3$cluster)
cstatskm4 = cluster.stats(distm,km4$cluster)

cstatskm3$average.between
cstatskm3$average.within
cstatskm4$average.between
cstatskm4$average.within

# Finding optimal number of clusters with elbow method
sse <- 0
count <- 1
for (k in 2:10){
  km <- kmeans(winenorm, centers=k)
  sse[count] <- km$tot.withinss
  count <- count+1
}

plot(c(2:10),sse)
lines(c(2:10),sse)

# Evaluating model with sillhouette method
avg_sil <- 0
count <- 1
for (k in 2:10){
  km <- kmeans(winenorm, centers=k)
  sil <- silhouette(km$cluster,distm)
  avg_sil[count] <- mean(sil[, 3])
  count <- count+1
}

plot(c(2:10),avg_sil)
lines(c(2:10),avg_sil)


# Silhouette plots
sil3 <- silhouette(km3$cluster,distm)
avg_sil3 <- mean(sil3[,3])
avg_sil3
plot(sil3)

sil4 <- silhouette(km4$cluster,distm)
avg_sil4 <- mean(sil3[,3])
avg_sil4
plot(sil4)
