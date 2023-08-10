# Thong ke du lieu
summary(iris)
# result:
# Sepal.Length    Sepal.Width     Petal.Length  
# Min.   :4.300   Min.   :2.000   Min.   :1.000  
# 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600  
# Median :5.800   Median :3.000   Median :4.350  
# Mean   :5.843   Mean   :3.057   Mean   :3.758  
# 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100  
# Max.   :7.900   Max.   :4.400   Max.   :6.900  
# Petal.Width          Species  
# Min.   :0.100   setosa    :50  
# 1st Qu.:0.300   versicolor:50  
# Median :1.300   virginica :50  
# Mean   :1.199                  
# 3rd Qu.:1.800                  
# Max.   :2.500    

#1. Boxplot
# Theo thuoc tinh
par(mar=c(7,5,1,1))
boxplot(iris,las=2)

#Theo loai
irisVer <- subset(iris, Species == "versicolor")
irisSet <- subset(iris, Species == "setosa")
irisVir <- subset(iris, Species == "virginica")
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(irisVer[,1:4], main="Versicolor",ylim = c(0,8),las=2)
boxplot(irisSet[,1:4], main="Setosa",ylim = c(0,8),las=2)
boxplot(irisVir[,1:4], main="Virginica",ylim = c(0,8),las=2)

#2. Histogram
#Petal.Length chia theo loai
par(mfrow=c(1,3))
hist(irisVer$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))
hist(irisSet$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))
hist(irisVir$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40)
     
#Petal.Width chia theo loai
par(mfrow=c(1,3))
hist(irisVer$Petal.Width,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,50))
hist(irisSet$Petal.Width,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,50))
hist(irisVir$Petal.Width,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,50)

#Sepal.Length chia theo loai
par(mfrow=c(1,3))
hist(irisVer$Sepal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,30))
hist(irisSet$Sepal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,30))
hist(irisVir$Sepal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,30)

#Sepal.Width
par(mfrow=c(1,3))
hist(irisVer$Sepal.Width,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,30))
hist(irisSet$Sepal.Width,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,30))
hist(irisVir$Sepal.Width,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,30))

#3.Scatterplot
#x= Sepal.Length va y= Sepal.Width
pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))


##### XAY DUNG MO HINH ########
#buoc 1: Cai dat thu vien
install.packages(C50)
library(C50)

#buoc 2:Load dataset 
input <- iris[,1:4]
output <- iris[,5]

#buoc 3:Xay dung mo hinh Decision Tree 
model1 <- C5.0(input, output, control = C5.0Control(noGlobalPruning = TRUE, 
                                                    minCases = 1))
model2 <- C5.0(input, output, control = C5.0Control(noGlobalPruning = FALSE))
#buoc 4: Ve Decision Tree
plot(model1, main = "C5.0 Decision Tree - Unpruned")
plot(model2, main = "C5.0 Decision Tree - Pruned")

#buoc 5: xem ket qua cua mo hinh
summary(model2)

##### SO SANH MO HINH DECISION VOI MO HINH K-MEAN #####
#buoc 1: Cai dat thu vien
install.packages("dplyr")
install.packages("ggplot2")
install.packages(“corrplot”)
library(dplyr)
library(ggplot2)
library(corrplot
#buoc 2:Load dataset 
# Tao clone dataset iris
iris2 <- iris
#lam sach data
iris2$Species <- NULL


#buoc 3:Xac dinh so luong cum K
#K=3

#buoc 4: Phan cum

set.seed(8593)

kmeans.result <- kmeans(iris2, 3)


#buoc 5: xem ket qua cua mo hinh
kmeans.result
table(iris$Species, kmeans.result$cluster)

#buoc 6: ve do thi bieu dien cac cum
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)

points(kmeans.result$centers[c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 2)

###SO SANH ####

