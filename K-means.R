install.packages("readxl")
install.packages("VIM")
install.packages("cluster")
install.packages("Ecdat")
install.packages("compareGroups")
install.packages("mice")
install.packages("SNPassoc")
install.packages("haplo.stats")
install.packages("survival")
install.packages("mvtnorm")
install.packages("parallel")

# Loading
library(readxl)
library(cluster)
library(Ecdat)
library(compareGroups)
library(mice)
library(lattice)

# Read excel files
Chipotle <- read_excel("Chipotle.xlsx")

data("Chipotle")
str(Chipotle)
md.pattern(Chipotle)

#Imputing missing values using mice
mice_imputes = mice(Chipotle, m=5, maxit = 40)

#What methods were used for imputing
mice_imputes$method

#Imputed dataset
Imputed_data = complete(mice_imputes,5)
Imputed_data

df <- na.omit(Imputed_data)
md.pattern(df)

str(df)

#Convert Age to factor
df$top1 = as.factor(df$top1)

#Chose variable
df2 <- df[c("importanthealthy", "healthyimportanttome")]

disMat <- daisy(df2, metric = "gower")

# 
set.seed(123)
mixedClusters <- kmeans(disMat, centers = 2)

# check the numbers for each cluster
table(mixedClusters$cluster)
plot(mixedClusters)

# combine the cluster to original table
df$cluster <- mixedClusters$cluster

# group by cluster
library(dplyr)
group <- df %>% group_by(cluster)
group

group<-compareGroups(cluster~.,data=df2)
mixedClusters
