#Installing Libraries
library(readxl)
library(cluster)
library(data.table)#Data. table is an extension of data. frame package in R. It is widely used for fast aggregation of large datasets,
library(Hmisc)#data analysis funs
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)
library(ggthemes)
library(psych)
library(relaimpo)
library(e1071)
library(corrplot)
library(factoextra)
library(fpc)
library(cowplot)
library(regclass)
library(caret)
library(pRoc)
library(ROCR)
library(memisc)
library(MASS)
library(scales)
library(gridExtra)
#install.packages("klaR")
library(klaR)
library(tidyverse)
library(caret)

#Reading the file from the directory
Canine_Data <- read_excel("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/MVAFinalExamToUpload_Kals/Final_Data_MVA.xlsx")

#Exploratory data analysis:
#View(Canine_Data)
head(Canine_Data)
dim(Canine_Data)
attach(Canine_Data)
names(Canine_Data)

Canine_Data <-  data.frame(Canine_Data)
#Numerical data only
Canine_num <- Canie_Data[2:10]
str(Canine_Data)
#Converting the 2 character variables into categorical variables
Canine_Data$CanineGroup <- as.factor(Canine_Data$CanineGroup)
Canine_Data$Gender <- as.factor(Canine_Data$Gender)
str(Canine_Data)
#Printing Descriptive statistics
summary(Canie_Data)
unique(CanineGroup)
#There are 5 Canine groups
unique(Gender)
#Gender is UNKNOWN for Thai dogs
#Checking for null/missing values
grep('NA',Final_Data)
#There are NO null values

#Looking for the outliers 
boxplot(Canine_Data$X1)
boxplot(Canine_Data$X2)
boxplot(Canine_Data$X3)
boxplot(Canine_Data$X4)
boxplot(Canine_Data$X5)
boxplot(Canine_Data$X6)
boxplot(Canine_Data$X7)
boxplot(Canine_Data$X8)
boxplot(Canine_Data$X9)
#X8 and x9 have some outliers but they are not extreme others look ok
#So no need to remove outliers 

# Computing the means of each variable in data frame 
colMeans(Canine_num)

# Covariance matrix 
cov(Canine_num)
# Finding correlation -Correlation matrix takes units out and gives normalized values 
cor.PT<-cor(Canine_num)
cor.PT
#Plotting correlation
corrplot(cor.PT,method="number")
#As per above Correlation plot, there is High Positive correlation between variables
#X1 and X2 are very much correlated with almost all other variables.

#@@@@@@@@@@@ Question 1 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#1. Using suitable graphical method, compare the distribution of the nine variables for the prehistoric and modern Thai dog.
#a. Create a Draftsman plot for the 9 variables showing each species as a different color
#First Trying to plot for all the Canine groups
pairs(Canine_Data[2:10], main = "Draftman's Plot for All the Canine Groups", col = as.numeric(Canine_Data$CanineGroup), 
      bg = c("red", "green2","black", "green3", "blue")[unclass(Canine_Data$CanineGroup)])
legend("bottomright", fill = unique(Canine_Data$CanineGroup),legend = c(levels(Canine_Data$CanineGroup)))

#unclassturns the list of canine grps from a list of categories (a "factor" data type in R terminology) into a list of ones, twos and threes, 4, 5:

#Plotting Draftsman plot only for 2 groups Moderndog and Thaigdog below
group <- NA
group[Canine_Data$CanineGroup == 'ModernDog'] <- 1
group[Canine_Data$CanineGroup == 'ThaiDogs'] <- 2

pairs(Canine_Data[,2:10],
      col = c("red", "blue")[group],   # Change color by group
      pch = c(5, 3)[group],                            # Change points by group
      main = "Draftman's Plot of Modern and Thai Dogs")
legend("bottomright", fill = c("red","blue"), legend = c("ModenDog","ThaiDogs"),col=c("red","blue"))

#Doing ggpairs for only 2 grps 
datapr<-(Canine_Data$CanineGroup %in% c("ModernDog","ThaiDogs"))
datapr1<- Canie_Data %>% filter(Canine_Data$CanineGroup %in% c("ModernDog","ThaiDogs"))
head(datapr1)
ggpairs(datapr1,column=2:10,ggplot2::aes(colour=CanineGroup),title="Draftsman Plot for Modern and Thai Dogs")
##Interpretation: Red= Modern dog and Green is Thai dogs
#For Modern dog = x1 is correlated with X2 and X8
#For Thai dogs= X7 and x8 , X8 and x9 are highly corelated
#There is Left skewed distributions for almost all fields from X1 to X9 for both canine groups
#==============Question 1 end =======================

#@@@@@@@@@@@ Question 2 - Distance Matrix @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#2. Create a distance matrix between the 5 canine groups

#Creating Distance matrix after scaling
#By default is euclidean dist
x<- dist(scale(Canine_Data[2:10],center = FALSE))
x
plot(x)

#Distance matrix with original data
dist.mat <- dist(Canine_Data[2:10])
dist.mat
plot(dist.mat)

#Both show similar distributions of distances
#==============Question 1 end =======================

#@@@@@@@@@@@ Question 2 - PCA @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#3. Use principal components analysis to investigate the relationships between the species on the basis of these variables

#PC generation
pca <- prcomp(Canine_Data[2:10],scale=TRUE)
pca
#Total 9 principal components are generated
#PC1 has all the variables positively contributing to it
#PC2 -ve contribution of of x7 which is length of first to third molar inclusive 
#PC3 has positive contribution of X3 i.e.breadth of articular condyle
summary(pca)
#Reading from the summary of pca table we can see that upto pc3 about 92% of variance is captured
plot(pca)
#Above plot shows that PC1 explains majority of variace whichis 78%
biplot(pca,scale=0)
#$x gives the new dataset #u need to rename these columns
head(pca$x)
#Modify comments
#From Summary of Pincipal components,
#Proportion of Variance, PC1, PC2 and PC3 explain 78%,7% and 6% of variance respectively.
#'Cumulative Proportion' field, 92% of Cummulative variance is explained by PC1, PC2, PC3
#So I will include PC1,PC2 and PC3 in my data input
#So My input variables will be reduced from 11 to 3 

(eigen_dog <- pca$sdev^2) #singular values (square roots of eigenvalues) stored in sparrow_pca$sdev 
names(eigen_dog) <- paste("PC",1:9,sep="") #Naming PC components
eigen_dog
sumlambdas <- sum(eigen_dog)
sumlambdas #sum of genvalues is total var of ur dataset
propvar <- eigen_dog/sumlambdas
#Printing Proper variance per PC
propvar
#Percentage of total variance
percentvar <- (eigen_dog/sumlambdas) *100
percentvar
#Bar plot of Percentage variance 
barplot(percentvar, main = "Bar Plot", xlab = "Principal Component", ylab = "Percentage Variance")
#As per above graph, PC1 holds 78% of ur total var, PC2 14% and so on
#Cummulative variance
cumvar_dog <- cumsum(propvar)
cumvar_dog
#Bar plot of Cummulative Percentage variance 
barplot(cumvar_dog, main = "Bar Plot", xlab = "Principal Component", ylab = "Percentage Variance")

#Plotting Scree diagram
plot(eigen_dog, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
#Plotting log scree diagram
plot(log(eigen_dog), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
# Scree diagram suggests to use 4 PCs but I beleive 92% variance by PC1,2,3 is enough for question at hand.

#Printing our new Dataset after PCA 
#Binding with categorical columns from the original dataset
pca.cty <- cbind(data.frame(CanineGroup,Gender),pca$x)
head(pca.cty)
#Renaming 1st 3 Principal components as we have decided to seect 1st 3 components
names(pca.cty)[names(pca.cty) == 'PC1'] <- 'Mix_all'
names(pca.cty)[names(pca.cty) == 'PC2'] <- 'Neg_len_1_3molar'
names(pca.cty)[names(pca.cty) == 'PC3'] <- 'Postv_articular_condyle'
#This is our new dataset that can be passed to models
head(pca.cty)

#PCA Conclusion:
#Principal Component analysis is a statistical technique that uses Orthogonal Transformation.
#It helps in reducing the number of input variables to be passed to a model.
#The principal componens are Non-correlated with each other.
#After performing PCA on this dataset, it can be concluded that:
#Contents of Principal Components:
#PC1 has all factors contributing to it
#PC2 is dominated by Negative effect of x7 which is length of first to third molar inclusive 
#PC3 is dominated by positive effect of breadth of articular condyle
#PC components renamed accordingly.
#From Summary of Pincipal components,
#Proportion of Variance, PC1 and PC2 explain 78% , 7% and 6% of variance respectively.
#'Cumulative Proportion' field, 92% of Cummulative variance is explained by PC1, PC2, PC3
#So I will include PC1 and PC2 , PC3 in my data input to models.
#So My input variables will be reduced from 11 to 3 
#As there is high correlation between measurements of different canine groups, they must be
#related to each other which can furthur be conformed with the help of cluster analysis.

#==============Question 3 end =======================

#@@@@@@@@@@@ Question 4 - Cluster Analysis @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#4. Carry out cluster analysis to study relation between different specifies.
#a. Who is Indian Wolf related to?

#DG_Clust_1 <- read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/lastyearmidterm/Butterfly_colonies_Updated.csv",row.names=1, fill = TRUE)
#DG_Clust_1 <- read_excel("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/MVAFinal_old/Final_Data.xlsx",row.names=1, fill = TRUE)

head(Canine_Data)
######## Hierarchical Clustering ##########
#Scaling 
matstd.dog <- scale(Canie_Data[,2:10])
head(matstd.dog)

#Complete linkage method 
# Creating a (Euclidean) distance matrix of the standardized data 
dist.PT_Clust_1 <- dist(matstd.dog, method="euclidean")

#Default - Complete Linkage
clusPT.fn <- hclust(dist.PT_Clust_1) 
plot(clusPT.fn,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Farthest neighbor linkage",labels=Canine_Data$CanineGroup)#can add for labels instead of row numbers
##As per this dendogram , if you cut aroud level 4, Indian Wolves are realted to 
#Modern Dog and Thai dog as they are clustered in same group.
#Dendogrm shows that canines have have roughly 4 main group which are subdivided into smalled grps.

#Single linkage
# Invoking hclust command (cluster analysis by single linkage method)      
clusPT.nn <- hclust(dist.PT_Clust_1, method = "single") 
# Plotting vertical dendrogram      
# create extra margin room in the dendrogram, on the bottom 
par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(clusPT.nn),ylab="Distance between Countries-Single Linkage",ylim=c(0,2.5),main="Dendrogram of Single linkage canines")

#Average
clusPT.avl <- hclust(dist.PT_Clust_1,method="average")
plot(clusPT.avl,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Group average linkage")
#Dendogrm shows that canines have roughly 4 main group which are subdivided into smalled grps.

#Lazy option --> agnes is 1 liner command for clustering 
# We will use agnes function as it allows us to select option for data standardization, the distance measure and clustering algorithm in one single function
(agn.PT <- agnes(dist.PT_Clust_1, metric="euclidean", stand=TRUE, method = "single"))
#  Description of cluster merging
agn.PT$merge

#Dendogram 
plot(as.dendrogram(agn.PT), xlab= "Distance between Countries",xlim=c(8,0),
     horiz = TRUE,main="Agnes Dendrogram  \n Canines")

######## Non Hierarchical clustering -- K-Means Clustering##########
##DG_Clust_1 <- read_excel("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/MVAFinal_old/finaloldCluster.xlsx",row.names=1)#, fill = TRUE)
##DG_Clust_1 <- read_xlsx("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/MVAFinal_old/finaloldCluster.xlsx",rowNames=TRUE)#, fill = TRUE)

#Converted xlsx into csv and importing it with row names for cluster analysis (csv has row id column)
DG_Clust_1 <- read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/MVAFinal_old/finaloldClustercsvform.csv",row.names=1, fill = TRUE)
head(DG_Clust_1)
#names(Final_Data)
#Imporing without row names the csv (csv has ro ID column)
Dg_Norowname <- read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/MVAFinal_old/finaloldClustercsvform.csv") #,row.names=1, fill = TRUE)
#View(Dg_Norowname)
names(Dg_Norowname)

# Standardizing the data with scale()
matstd.dog <- scale(DG_Clust_1[,2:10])#quantitative
head(matstd.dog)
# Creating a (Euclidean) distance matrix of the standardized data 
#dist.PT_Clust_1 <- dist(matstd.dog, method="euclidean")

#Implementing K-Means Clustering with different values of k.
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen
#k=2
(kmeans2.dog <- kmeans(matstd.dog,2,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.dog$betweenss/kmeans2.dog$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2
#46% variance with k=2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3.dog <- kmeans(matstd.dog,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.dog$betweenss/kmeans3.dog$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3
#28% variance with k=3

# Computing the percentage of variation accounted for. Four clusters
(kmeans4.dog <- kmeans(matstd.dog,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.dog$betweenss/kmeans4.dog$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4
#21%

# Computing the percentage of variation accounted for. Five clusters
(kmeans5.dog <- kmeans(matstd.dog,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.dog$betweenss/kmeans5.dog$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5

(kmeans6.dog <- kmeans(matstd.dog,6,nstart = 10))
# Computing the percentage of variation accounted for. Six clusters
perc.var.6 <- round(100*(1 - kmeans6.dog$betweenss/kmeans6.dog$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6
#
#It can be seen that Variance goes down as K increases...
#To Identify the Best number of K Clusters, plotting Elbow Plot
wss=c()########## empty vector to hold wss
for(i in 2:10)#### from 2 to 10 cluster
{
  km = kmeans(matstd.dog[,1:9],i)
  wss[i-1]=km$tot.withinss
}
wss
#Creating a 'elbowdt' data table with column names num and wss with the contents of wss
elbowdt = data.table(num=2:10,wss)
elbowdt
#Plotting
ggplot(elbowdt,aes(x=num,y=wss)) + geom_line()
#For k = 6 the between sum of square/total sum of square ratio tends to change slowly 
#and remain less changing as compared to others.
#Also this dataset has only 77 rows so more than 6/7 clusters would not make much sense to me.
#Therefore, k = 6 should be a good choice for the number of clusters.
#For 6 clusters, k-means = 6
# Centers (k's) are numbers thus, 10 random sets are chosen
(kmeans6.dog <- kmeans(matstd.dog,6,nstart = 10))
perc.var.6 <- round(100*(1 - kmeans6.dog$betweenss/kmeans6.dog$totss),1)
names(perc.var.6) <- "Perc. 3 clus"
perc.var.6
kmeans6.dog
kmeans6.dog$cluster
#typeof(kmeans6.dog$cluster)
#plotting output of kmeans for 6 clusters
fviz_cluster(kmeans6.dog,data=matstd.dog)
#Clusters plotting in another way to see them more clearly
plotcluster(matstd.dog,kmeans6.dog$cluster)
#Clear 6 groups can be seen after Plotting the clusters.
#Creating separate matrices for clusters
##b<-names(kmeans6.dog$cluster[kmeans6.dog$cluster == 1])
##b
##(kmeans6.dog$cluster[kmeans6.dog$cluster == 1])

clus1 <- matrix(names(kmeans6.dog$cluster[kmeans6.dog$cluster == 1]), 
                ncol=1, nrow=length(kmeans6.dog$cluster[kmeans6.dog$cluster == 1]))
colnames(clus1) <- "Cluster 1"
clus1

clus2 <- matrix(names(kmeans6.dog$cluster[kmeans6.dog$cluster == 2]), 
                ncol=1, nrow=length(kmeans6.dog$cluster[kmeans6.dog$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus2

clus3 <- matrix(names(kmeans6.dog$cluster[kmeans6.dog$cluster == 3]), 
                ncol=1, nrow=length(kmeans6.dog$cluster[kmeans6.dog$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus3

clus4 <- matrix(names(kmeans6.dog$cluster[kmeans6.dog$cluster == 4]), 
                ncol=1, nrow=length(kmeans6.dog$cluster[kmeans6.dog$cluster == 4]))
colnames(clus4) <- "Cluster 4"
clus4

clus5 <- matrix(names(kmeans6.dog$cluster[kmeans6.dog$cluster == 5]), 
                ncol=1, nrow=length(kmeans6.dog$cluster[kmeans6.dog$cluster == 5]))
colnames(clus5) <- "Cluster 5"
clus5

clus6 <- matrix(names(kmeans6.dog$cluster[kmeans6.dog$cluster == 6]), 
                ncol=1, nrow=length(kmeans6.dog$cluster[kmeans6.dog$cluster == 6]))
colnames(clus6) <- "Cluster 6"
clus6

#Displaying all the Clolonies as in their respective clusters
list(clus1,clus2,clus3,clus4,clus5,clus6)

#Making Subsets for 6 clusters using Row filtering from the Original dataset
#(Not the scaled one)
#So below are the 6 cluster sets of Original entire dataset
#Using original dataframe to capture Row ID as clusters are identified based on these IDs
head(DG_Clust_1)
#head(Final_Data)
#Dg_Norowname$ï..ID_C
#Dg_Norowname
#Dg_Norowname$ï..ID_C %in% clus4
DG_Cl1_Dt<-subset(Dg_Norowname,Dg_Norowname$ï..ID_C %in% clus1)
#DG_Cl1_Dt
DG_Cl2_Dt<-subset(Dg_Norowname,Dg_Norowname$ï..ID_C %in% clus2)
#DG_Cl2_Dt
DG_Cl3_Dt<-subset(Dg_Norowname,Dg_Norowname$ï..ID_C %in% clus3)
#DG_Cl3_Dt
DG_Cl4_Dt<-subset(Dg_Norowname,Dg_Norowname$ï..ID_C %in% clus4)
#DG_Cl4_Dt
DG_Cl5_Dt<-subset(Dg_Norowname,Dg_Norowname$ï..ID_C %in% clus5)
#DG_Cl5_Dt
DG_Cl6_Dt<-subset(Dg_Norowname,Dg_Norowname$ï..ID_C %in% clus6)
#DG_Cl6_Dt

#Printing all the columns of the Clusters formed 
#Original observations after clustering with all the variables
list(DG_Cl1_Dt,DG_Cl2_Dt,DG_Cl3_Dt,DG_Cl4_Dt,DG_Cl5_Dt,DG_Cl6_Dt)

#Interpretation:
#Cluster analysis is a technique that groups the observations into clusters based on similarities
#Clustering types: Hierarchical and nonhierarchical
#Hierarchical and non hierarchical clustering has been performed on Protein consumptiondataset above.
#Clear 6 groups can be seen after Plotting the clusters.
#In one of the cluster (cluster 1), Indian wolves, Moden dog and Thai dogs are clubbed together.
#So Indian Wolves are related to Thai dogs and Modern Dogs

#==============Question 4 end =======================

#@@@@@@@@@@@ Question 5 - Factor Analysis @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#5. Identify the important factors underlying the Skull measurement
#a. Is there a relationships between the species with respect to these factors?

#library(psych)
fa.parallel(Canine_Data[2:10]) # See factor recommendation

#we can see that after 2 factors the eigen value crosses at 1 and hence 1 is the number of recommended factors

#Do an eigen value decomposition removing the non numeric columns
fcdg <- principal(Canine_Data[2:10], nfactors=2, rotate="varimax")
fcdg
summary(fcdg)

#From the summary we can see that upto 4 factors the variables explain about 83% of the variance
round(fcdg$values, 3)
fcdg$loadings

# Communalities
fcdg$communality

# Plotting the relationship and mapping between variables and factors with weights
fa.diagram(fcdg) 
#Above, output gives weigths going in RCs

#fa.graph(fcdg)
#cluster.plot(fcdg)
#plot(fcdg)
#Now lets rename these factors as per their contributing variables as per above graph
colnames(fcdg$loadings) <- c("x_42963","x_7851")
#fcdg

#Factor Analysis Conclusion:
#Factor analysis is a technique used to reduce number of columns.
#Factor analysis tries to find if there is any underlying latent variable in your input columns.
#After performing Factor analysis on Canine dataset, it can be concluded that:
#Based on per Measurements for the canine groups,
#Total 2 factors have been formed with common variance of different Measurements contributing to them.
#RC1 is made up with Positive contributions of Breadh and Height mearurements.
#RC2 is made up with Positive contributions of Length mearurements 
#This shows that there is an underlying latent variable within Breadth and Height related skull measurements for anine dogs.
#Also there is an underlying latent variable within length related measurements.
#As per above diagram, almost all the factors have significant contribution and so 
#So, its better not to loose any of 2 factors
#Hence I will take All 2 Factors, RC1 and RC2 as inputs for our models
#Above factor analysis, we can conclude to reduce number of 9 Measurements to 2 in our input dataset.
#==============Question 5 end =======================

#@@@@@@@@@@@ Question 6 - Discriminant Function Analysis @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#6. Carry out a discriminant function analysis to see how well it is possible to separate the groups using the measurements.

#library(MASS)
#library(scales)
#library(gridExtra)
#install.packages("klaR")
#library(klaR)
#library(tidyverse)
#library(caret)

#splitting the data into training and testing sets with 80% and 20% respectively.
set.seed(123)
training.samples <- Canine_Data$CanineGroup %>% createDataPartition(p = 0.8, list = FALSE)
#So sample has a random mix of canine groups
train.data <- Canine_Data[training.samples, ] 
test.data <- Canine_Data[-training.samples, ] 
dim(Canine_Data) #Total data  77 11
dim(train.data) #Training data  63 11
dim(test.data) #Test data 14 11

#Estimating preprocessing parameters
#Pre-processing transformation (centering, scaling etc.) can be estimated from the training data and applied to any data set with the same variables.
#So the properties of training and test data remain the same
preproc.param <- train.data %>% preProcess(method = c("center", "scale"))
#preproc.param

# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)
head(train.transformed)
head(test.transformed)

#Fitting the LDA model with Canie group as dependent variable and all the measurements as predictors
#Fitting with Training data
fit_lda <- lda(CanineGroup ~ X1+X2+X3+X4+X5+X6+X7+X8+X9, data = train.transformed)
fit_lda
#We get 4 functions LD1 to LD4 as there are canine 5 groups 
plot(fit_lda)

summary(fit_lda)
#chk coeffs they r gd
print(fit_lda)
fit_lda$counts #IT gives no of observations put in respective groups
fit_lda$means
#Here we can see how means r different for the measurements in different canine grps
#For example Indian wolves have higher (Positive) values of means for all the measurements.
#On the contrary, Golden Jackal has the smallest values (negative) for all the measurements.
#This shows that there is a large difference between the measurements of Indian wolves and Golden Jackal.
#This is how the Canine groups are being separated using themeasurements.
fit_lda$scaling
fit_lda$prior
#above gives prior probabilities
fit_lda$lev
fit_lda$svd
#singular values (svd) that gives the ratio of the between- and within-group standard deviations on the linear discriminant variables.
#fit_lda$N
#fit_lda$call

#Predictions for test data==> 
predictions <- model %>% predict(test.transformed)
names(predictions)

#Linear discriminants -- Functions
head(predictions$x, 9) 

#Plotting Trainng data predictions 
lda.dataTrn <- cbind(train.transformed, predict(model)$x)
ggplot(lda.dataTrn, aes(LD1, LD2,LD3,LD4)) +
  geom_point(aes(color = CanineGroup))

#Plotting for Testing data predictions 
lda.dataTst <- cbind(test.transformed, predictions$x)
ggplot(lda.dataTst, aes(LD1, LD2,LD3,LD4)) +
  geom_point(aes(color = CanineGroup))

#Both the Training and Testing prediction plots clearly shows 5 canine groups without any overlap
#So Model predictions look good.

# Lets try to compare Actual and predicted groups
#For training data
lda.train <- predict(model,train.transformed)
lda.train
train.transformed$lda <- lda.train$class
train.transformed$lda
table(train.transformed$lda,train.transformed$CanineGroup)
# running accuracy on the training set shows how good the model is. 
#It predicted 2 values wron out of 63 observations (2 Modern dogs are incorrectly predicted as Thai dogs)
#It is not an indication of "true" accuracy. We will use the test set to approximate accuracy
#For test data
lda.test <- predict(model,test.transformed)
test.transformed$lda <- lda.test$class
table(test.transformed$lda,test.transformed$CanineGroup)
#This shows that out of 4 Moderndogs, 3 are predicted correctly but 1 is incorrectly predicted as a Thai dog

#Fig margin too large to produce graph
#par(mar = c(5, 5, 5, 5))
#partimat(CanineGroup ~ X1+X2+X3+X4+X5+X6+X7+X8+X9, data=train.transformed, method="lda")

#Now lets check accuracy %
mean(predictions$class==test.transformed$CanineGroup)
#Model Accuracy for train and test is 92.85%

#Discriminant Analysis Conclusion:
#Discriminant analysis is used when dependant variable is categorical with more than 2 values.
#Multiple discriminant analysis helps to classify the observations using functions.
#As per LDA model means -
#Here we can see how means r different for the measurements in different canine grps
#For example Indian wolves have higher (Positive) values of means for all the measurements.
#On the contrary, Golden Jackal has the smallest values (negative) for all the measurements.
#This shows that there is a large difference between the measurements of Indian wolves and Golden Jackal.
#This is how the Canine groups are being separated using themeasurements.
#As per LDA model Plots - 
#Both the Training and Testing prediction plots clearly shows 5 canine groups without any overlap
#So Model predictions look good.
# running accuracy on the training set shows how good the model is. 
#For training set, It predicted 2 values wron out of 63 observations (2 Modern dogs are incorrectly predicted as Thai dogs)
#For test set, This shows that out of 4 Moderndogs, 3 are predicted correctly but 1 is incorrectly predicted as a Thai dog
#With above outputs you can see that the canine groups are very well separated with LDA
#with 92% accuracy
#==============Question 6 end =======================

#@@@@@@@@@@@ Question 7 - Logistic Regression @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#7. investigate each canine group separately to see whether logistic regression shows a significant difference between males and females for the measurements. Note that in view of the small sample sizes available for each group, it is unreasonable to expect to fit a logistic function involving all nine variables with good estimates of parameters. Therefore, consideration should be given to fitting functions using only a subset of the variables.
#
#library(regclass)
#Sample size is very small for the individual canine groups 
#So to avoid curse of dimentionality, I am trying to fit logistic regression model using few measure variables 
#For Modern Dogs
moderncanine <- Canine_Data[1:16,]
xtabs(~Gender + CanineGroup, data=moderncanine)
#using the variables X1, X2, X3 for the logistic model
logistic_modern <- glm(Gender~ X2+X3+X4, data=moderncanine, family="binomial")
#Viewing the summary statistics
summary(logistic_modern)
#AIC = 22
#plotting the confusion matrix
confusion_matrix(logistic_modern)
#X2, X3 and X4 give Good predictions for Modern Dogs. 
#@@@@@@
#predicted.data <- data.frame(probability.of.gender=logistic_modern$fitted.values,X2=moderncanine$X2)
#predicted.data #finding hrt desease prob based on sex for each data pt
## we can use a table to summarize the predicted probabilities.
#xtabs(~ probability.of.gender + X2, data=predicted.data)
#@@@@@@@@@
#Computing the logistic model for the 2nd group of canines using the X7 variable
#For Golden Jackals
can2<- Canine_Data[17:36,]
xtabs(~Gender + CanineGroup, data=can2)
lo2 <- glm(Gender~ X1+X5, data=can2, family="binomial")
summary(lo2)
confusion_matrix(lo2)
#X5 is more significant role to predict gender of Golden jackals
#However, X1 and X5 do not have significant difference between them and so do not predict Golden jackal gender very well.

#For Cuons
#Now for the 3rd group, using the X4 and X7 predictors
can3<- Canine_Data[37:53,]
xtabs(~Gender + CanineGroup, data=can3)
lo3 <- glm(Gender~ X4+X7, data=can3, family="binomial")
summary(lo3)
confusion_matrix(lo3)

#Similarly for the 4th group, i have used the X1, X6, X7, X8 and X9 predictors
#For Indian wolves 
can4<- Canine_Data[64:77,]
xtabs(~Gender + CanineGroup, data=can4)
lo4 <- glm(Gender~ X1+X6+X7+X8+X9, data=can4, family="binomial")
summary(lo4)
confusion_matrix(lo4)

#Logistic regression Conclusion:
#Sample size is very small for the individual canine groups 
#So to avoid curse of dimentionality, I am trying to fit logistic regression model using few measure variables 
#As per confusion matrices above, most of the logistic regression models are working well
#Because measurements are different for different Canine Groups so they predict gender correctly for given measurements.
#X2, X3 and X4 very well classify Modern Dogs into Male and Female groups. 
#X5 is more significant role to predict gender of Golden jackals
#However, X1 and X5 do not have significant difference between them and so do not predict Golden jackal gender very well.
#Hence, the above logistic regression models shows that there is significant difference between males and females for the measurements. 
#And so they classify observations into correct Male / Female groups given the values of Measurements.

#==============Question 7 end =======================

#@@@@@@@@@@@ Question 8 - ROC @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#8. Show ROC containing both your discriminant and logistic function for gender classification for the Prehistoric Thai Dog
#For Thai dogs: 
Canine_Data$Gender[53:63] <- c("Female", "Male", "Female", "Male", "Male", "Male", "Female","Female","Male","Male","Female")
Canine_Data$Gender <- as.factor(Canine_Data$Gender)
dataThai <- Canine_Data[c(54:63),]
dataThai
lo_Thai <- glm(Gender ~ .,data=dataThai[c(-1)], family="binomial")
summary(lo_Thai)

#library(pROC)
roc(dataThai$Gender,lo_Thai$fitted.values,plot=TRUE)
par(pty = "s")
#The Area under curve is 1 which is overfitting which might be due to the small smaple size

#Fitting lda model on thai dog data
lin_thai <- lda(Gender ~ ., data = dataThai[c(-1)])#, family="binomial")
lin_thai
lin_thai$prior
summary(lin_thai)

#roc(data$hd, logistic_simple$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
roc(dataThai$Gender,lo_Thai$fitted.values,plot=TRUE)

# Lets add the other graph
#plot.roc(dataThai$Gender, lin_thai$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
#legend("bottomright", legend=c("Simple", "Non Simple"), col=c("#377eb8", "#4daf4a"), lwd=4) # Make it user friendly

#Conclusion for Thai dogs ROC:
#The Area under curve is 1 which is overfitting which might be due to the small smaple size

#==============Question 8 end =======================

#@@@@@@@@@@@ Question 9 - Gender prediction for prehistoric thai dogs @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#9. Predict the Gender for the Prehistoric Thai Dog
data9 = Canine_Data[-c(54:63),-1] #removing the unknown gender rows and the canine group column
data9$Gender <- as.factor(data9$Gender)
#Fitting model on Thai dog data
logistic_T <- glm(Gender ~ ., data=data9, family="binomial")
summary(logistic_T)

#a. Explain the reason for choosing the MVA technique for prediction
#The Logistic regression techinque is chosen because
#We want to predict the Gender of Thai dogs, which is a categorical variable having 2 values as 'Male' & 'Female'
#This falls under the Binomial classification problem which works on the maximum likelihood principle
#The Linear discriminant analysis or random forest will also work for this problem.

#b. What is the Hit Ratio (Accuracy) of your classification technique?

#To check accuracy
pdata <- predict(logistic_T,newdata=data9,type="response")
pdata
#Above are the probabilities
pdataF <- as.factor(ifelse(test=as.numeric(pdata>0.5) == 0, yes="Female", no="Male"))
pdataF
#if Probability >0.5 then classified as male, else female
#install.packages("e1071", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#library(e1071)
confusionMatrix(pdataF, data9$Gender)

#If error comment below 4 lines
data9$Gender <- "Female"
data9$Gender[2] <- "Male"
#finalthai$Gender = as.factor(finalthai$Gender)
head(data9)

#Conclusion for gender prediction for prehistoric dog gender predictions:
#As per balanced class output, the Accuracy is 68.79%
#It can also be seen that for Female class sensitivoty is more and for male class specificity is more

#//////Q 9 
#Predicting the gender of thai dogs
#install.packages("caret", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#library(caret)
#finalthai <- Canine_Data[c(54:63),-1]
#finalthai$Gender <- "Female"
#finalthai$Gender[2] <- "Male"
#finalthai$Gender = as.factor(finalthai$Gender)
#head(finalthai)
#Using a threshold of 0.5 for determining the gender
#pdata <- predict(logistic,newdata=finalthai,type="response")
#Printing probabilities
#pdata
#If Prob > 0 then male else female
#pdataF <- as.factor(ifelse(test=as.numeric(pdata>0.5) == 0, yes="Female", no="Male"))
#pdataF

#==============Question 9 end =======================

#@@@@@@@@@@@ Question 10 - Multiple Regression @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#10.    Create a model to predict length of the Mandible length for Prehistoric Thai Dog.
#a. What is the accuracy of your model

#From the above prediction, we are filling the unknown values with the predicted values.
Canie_Data$Gender[53:63] <- c("Female", "Male", "Female", "Male", "Male", "Male", "Female","Female","Male","Male","Female")
Canine_Data$Gender <- as.factor(Canine_Data$Gender)

#Dataset with only thai dog data as asked in question
datamult <- Canine_Data[c(54:63),]

#X1 is the madible length
head(datamult)
#View(data10)
#View(data10[c(-1)])
#multiple regression with all the predictors
Regrn_all <-lm(X1~.,data = datamult[c(-1)])
summary(Regrn_all)

#Output not good so trying with different variables
#The significance of X8,X2 is very less and, we can comupte the model by removing these factors.

Regrn_2 <- lm(X1~X3+X4+X5+X6+X7+X9+Gender,data = datamult[c(-1)])
summary(Regrn_2)
#plot(Regrn_2) #plots residuals
#The adjusted R-squared is nearly 1 (99.73) which indicates over-fitting. 
#This is due the small sample size.

#dropping x5 as it is not significant
Regrn_3 <- lm(X1~X3+X4+X6+X7+X9+Gender,data = datamult[c(-1)])
summary(Regrn_3)
#Predictions 
predk <- predict(Regrn_3,datamult[c(-1)])
predk #Predictions
datamult$X1 #Actual Data
#If you compare visually, te actual and predicted data,, the predictions are quiet impressive.

#Here accuracy is 91.06

##step<-stepAIC(Regrn_2,direction = "both") we can also use stepaic if needed but here we already got 91 % accuracy

#Conclusion for Multiple Regression:
#Multiple Regression model is used to predict length of the Mandible length for Prehistoric Thai Dogs.
#Multiple regression is used because the dependent variable is quantitative here.
#Different combinations of predictors were tried in order to achieve better accuracy
#The predictions were made
#The model with predictors - 'X3+X4+X6+X7+X9+Gender'is selected.
#Accuracy of this model is 91.06% 
#==============Question 10 end =======================
