library(readxl)
Cororna_data <- read_excel("DB.xlsx")
View(Cororna_data)


# select Random sample #

library(dplyr)
set.seed(123)
Sample <- sample_frac(Cororna_data,size = 0.90)
View(Sample)



# Factor analysis #
Sample_factor_variables <- Sample[,15:38]
View(Sample_factor_variables)

# Determination number of extracting factors
# scree plot
library(EFA.dimensions)
SCREE_PLOT(Sample_factor_variables)

# applying factor analysis on the same variables in the paper #
# Retain 5 factors.
# Without Rotation .

library(psych)
fa_result_Unrotated <- fa(Sample_factor_variables, nfactors = 5, rotate = "none", scores = "regression", 
                        fm = "pa", cor = "cor")
fa_result_Unrotated
factor_loadings_Unrotated <- fa_result_Unrotated$loadings
round(cor(factor_loadings_Unrotated),2)

# applying factor analysis on the same variables in the paper #
# With Oblique Rotation.
# retain 5 factors.


fa_result_Rotated <- fa(Sample_factor_variables, nfactors = 5, rotate = "oblimin", scores = "regression", 
                fm = "pa", cor = "cor")
fa_result_Rotated
factor_loadings_Rotated <- fa_result_Rotated$loadings
round(cor(factor_loadings_Rotated),2)

# Without Rotation .
# retain 4 factors.


fa_result_Unrotated1 <- fa(Sample_factor_variables, nfactors = 4, rotate = "none", scores = "regression", 
                          fm = "pa", cor = "cor")
fa_result_Unrotated1
factor_loadings_Unrotated1 <- fa_result_Unrotated1$loadings
round(cor(factor_loadings_Unrotated1),2)


# applying factor analysis on the same variables in the paper #
# With Oblimin Rotation.
# retain 4 factors.


fa_result_Rotated1 <- fa(Sample_factor_variables, nfactors = 4, rotate = "oblimin", scores = "regression", 
                        fm = "pa", cor = "cor")
fa_result_Rotated1
factor_loadings_Rotated1 <- fa_result_Rotated1$loadings
round(cor(factor_loadings_Rotated1),2)

# applying factor analysis on the same variables in the paper #
# With Varimax Rotation.
# retain 4 factors.

fa_result_Rotated_varimax <- fa(Sample_factor_variables, nfactors = 4, rotate = "varimax", scores = "regression",fm = "pa", cor = "cor") 
fa_result_Rotated_varimax
factor_loadings_varimax <- fa_result_Rotated_varimax$loadings
round(cor(factor_loadings_varimax),2)


# Clustering analysis #
library('factoextra') # for elbow curve
library(ggplot2)
library("mvtnorm") # Prerequsite package to use mvnTest package
library('mvnTest') # To test Multivariate normality
library("MVN")     # to use mardiaTest for normality
library("heplots")# Required package for box-m test
library(car)
library(MASS)

clustering_df = Sample[, c('Transcendence','Interpersonal','Openness','Restraint')] # select the required variables to cluster upon and standardize them

stand_clustering_df = scale(Sample[, c('Transcendence','Interpersonal','Openness','Restraint')]) # select the required variables to cluster upon and standardize them

fviz_nbclust(clustering_df, kmeans, method = "wss") # 2 is the optimal number of clusters


pca <- prcomp(stand_clustering_df) # perform PCA on standardized variables to obtain initial number of clusters

summary(pca) # first principal component since it explains 56% of the variations

ggplot(clustering_df, aes(x = pca$x[,1], y = pca$x[,2], )) +
  geom_point() +
  ggtitle("Scatter Plot of data points") +
  labs(title = 'Principal Components scatterplot',x = "PC 1 ", y = "PC 2")+
  scale_color_discrete(name = "Mental Health clusters")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))



#### number of clusters k = 3 ####

set.seed(123) # set the random seed for reproducability  
kmeans_result3 <- kmeans(clustering_df, centers = 3)


cluster3_df <- data.frame(clustering_df, cluster = kmeans_result3$cluster) # combining the obtained cluster to the df
ggplot(cluster3_df, aes(x = pca$x[,1], y = pca$x[,2], color = factor(cluster))) +
  geom_point() +
  ggtitle("Scatter Plot of Clusters") +
  labs(x = "PC 1 ", y = "PC 2")+
  scale_color_discrete(name = "Character Strength" , labels = c('Low' , 'Moderate' , 'High'))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

### validation(through discriminant analysis)

# testing equal means
fit <- manova(cbind(Transcendence, Interpersonal, Openness, Restraint) ~ cluster, data = cluster3_df)
summary(fit, test = "Pillai")

# testing homogenous var-cov matrices
boxM(cluster3_df[,1:4], group = cluster3_df$cluster) # Applying box-m test 


# testing normality
mvn(cluster3_df[,1:4] , mvnTest = "mardia") 

# DA
fit <- lda(cluster ~ Transcendence + Interpersonal + Openness + Restraint, data = cluster3_df) # Linear discriminant function
predicted <- predict(fit, newdata = cluster3_df)
table(cluster3_df$cluster, predicted$class) # classification table


#### number of clusters k = 2 ####

set.seed(123) # set the random seed for reproducability  
kmeans_result2 <- kmeans(clustering_df, centers = 2)


cluster2_df <- data.frame(clustering_df, cluster = kmeans_result2$cluster) # combining the obtained cluster to the df
ggplot(cluster2_df, aes(x = pca$x[,1], y = pca$x[,2], color = factor(cluster))) +
  geom_point() +
  ggtitle("Scatter Plot of Clusters") +
  labs(x = "PC 1 ", y = "PC 2")+
  scale_color_discrete(name = "Character Strength ", labels =c("Low", "High") )+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

### validation(through discriminant analysis)

# testing equal means
cluster1 <- cluster2_df[cluster2_df$cluster == 1, 1:4]
cluster2 <- cluster2_df[cluster2_df$cluster == 2, 1:4]
ICSNP::HotellingsT2(cluster1,cluster2)

# testing homogenous var-cov matrices
boxM(cluster2_df[,1:4], group = cluster2_df$cluster) # Applying box-m test 


# testing normality
mvn(cluster2_df[,1:4] , mvnTest = "mardia") 

# DA
fit2 <- lda(cluster ~ Transcendence + Interpersonal + Openness + Restraint, data = cluster2_df) # Linear discriminant function
predicted2 <- predict(fit2, newdata = cluster2_df)
table(cluster2_df$cluster, predicted2$class) # classification table
##############################################################################
Cororna_data <- read_excel("D:/ass data science/project multy/DB.xlsx")
View(Cororna_data)

#select variables
data<-Cororna_data[,c("DASS_depression","GHQ_12","SEC","Age","Gender","Student")]

#transform type of variable
data$Student<-as.factor(data$Student)
data$Gender<-as.factor(data$Gender)

#impute missing
library("dplyr")
data$Student<-recode_factor(data$Student,"NA"="Other")

# Fit Multivariate regression model
mv_model<-lm(cbind(DASS_depression,GHQ_12,SEC)~Age+Gender+Student,data=data)
fit <- summary(mv_model)
print(fit)
library(car)
#the results among the three variable use Manova
Anova(mv_model)
