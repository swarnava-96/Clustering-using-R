################## Hierarchial Clustering #########################

# Loading the data set
dataset <- read.csv("utilities.csv")
str(dataset)
head(dataset)

# Visualize the features
plot(dataset)

# Scatterplot
plot(Fuel_Cost~Sales,dataset)
with(dataset,text(Fuel_Cost~Sales,labels=Company,pos=4,cex=0.3))

plot(RoR~Sales,dataset)
with(dataset,text(RoR~Sales,labels=Company,pos=4,cex=0.3))

# Standardization
z <- dataset[,-c(1,1)]
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)

# Calculate Euclidean Distance
distance <- dist(z)
distance
print(distance,digits=3)

# Clustering Dendrogram
hc1 <- hclust(distance)
plot(hc1,labels = dataset$Company,hang = -1)


# Clustering Dendrogram(average)
hc1 <- hclust(distance,method = "average")
plot(hc1,labels=dataset$Company,hang = -1)

# Clustering Membership
member.1 <- cutree(hc1,3)
aggregate(z,list(member.1),mean)

# Actual values
aggregate(dataset[,-c(1,1)],list(member.1),mean)