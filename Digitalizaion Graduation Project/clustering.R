######libraries #######
library(ggplot2)





full_df = read.csv('D:/feps  fourth year curriculum/grad project/clustering/transformed_scores.csv')

clustering_variables = full_df[ , c('fir25' , 'cor16' , 'fir10' , 
                                   'fir17' , 'fir19' , 'fir8_2',"normalized_scores")]
table(clustering_variables$fir25)
table(clustering_variables$cor16)
table(clustering_variables$fir10)
table(clustering_variables$fir17)
table(clustering_variables$fir19)
table(clustering_variables$fir8_2)


clustering_variables$fir10 <- as.integer(clustering_variables$fir10)
clustering_variables$fir17 <- as.integer(clustering_variables$fir17)
clustering_variables$fir8_2 <- as.integer(clustering_variables$fir8_2)

str(clustering_variables)

#######impute fir10 , fir17 , fir8_2 #####

library(mice)


mice_object <- mice(clustering_variables, maxit = 5)  # You can adjust maxit as needed

# Specify the variables to be imputed
variables_to_impute <- c("fir10", "fir17", "fir8_2")

# Set the method for imputation (e.g., "pmm" for predictive mean matching)
mice_object$method[variables_to_impute] <- "pmm"

# Perform imputation
imputed_data <- complete(mice_object)

# Now 'imputed_data' contains the imputed values for the specified variables

###### 2024-fir8_2 #####

imputed_data$fir8_2 = 2024 - imputed_data$fir8_2


###### check dist b4 and after imputation#####


# check for fir10


# Extract the original and imputed values
original_values <- clustering_variables$fir10
imputed_values <- imputed_data$fir10

# Create a data frame for visualization
df <- data.frame(
  Status = rep(c("Before Imputation", "After Imputation"), each = length(original_values)),
  Value = c(original_values, imputed_values)
)

# Create a histogram
ggplot(df, aes(x = Value, fill = Status)) +
  geom_density(alpha = 0.9) +
  labs(title = "Density Plot of 'fir10' Before and After Imputation",
       x = "Value",
       y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


###### checking variables' correlations #####

library(Hmisc)

rcorr(as.matrix(imputed_data[,c('fir25' , 'fir10' , 'fir17' ,'fir8_2' , 'normalized_scores')]))			



####### using nominal variables that form the index ####


alldata <- cbind(imputed_data, full_df[, c("cor11","cor12","cor13",
                                                        "cor14","fir12","fir14",
                                                        "fir16","fir22",
                                                        "fir20","fir18")])

alldata$normalized_scores = NULL
summary(alldata)


# check correlations for numeric variables

rcorr(as.matrix(alldata[,c('fir25' , 'fir10' , 'fir17' ,'fir8_2' )]))			


# check correlations for categorical variables
df_categorical <- alldata[sapply(alldata, is.character)]



library(vcd)



# Get the column names
cols <- colnames(df_categorical)

# Initialize a matrix to store the p-values
p_values <- matrix(NA, ncol(df_categorical), ncol(df_categorical))
rownames(p_values) <- cols
colnames(p_values) <- cols

# Calculate the p-values
for(i in seq_along(cols)) {
  for(j in seq_along(cols)) {
    if(i != j) {
      test <- chisq.test(df_categorical[,i], df_categorical[,j])
      p_values[i,j] <- test$p.value
    }
  }
}

# Print the p-values
print(p_values)


# remove cor16 & fir19 from ur df


######## correlation between categorical and quantitative variables ######## 

data1 = read.csv('D:/feps  fourth year curriculum/grad project/clustering/imputed_data_nominal_variables(not index).csv')

colnames(data1)

data1[ , c("X" , "cor16","fir19")] = NULL

str(data1)

# change variables' types
library(dplyr)

data1 <- data1 %>%
  mutate_at(vars("cor11" , "cor12" , "cor13" , "cor14" ,
                 "fir12" , "fir14" , "fir16",  "fir22"  ,"fir20" , "fir18"), as.factor)


## check normality

numerical_vars <- c("fir25" , "fir10",  "fir17" , "fir8_2")

# Apply the Shapiro-Wilk test to each numerical variable
test_results <- lapply(data1[numerical_vars], shapiro.test)

# Print the results
print(test_results)
# we can't use ANOVA because the normality assumption isn't satisfied




## check correlation between categorical and numerical variables

# testing between categorical variables that has 2 levels

numerical_vars <- c("fir25","fir10","fir17","fir8_2")

categorical_vars <- c("cor11","cor12", "cor13" , "cor14" ,
"fir12" , "fir14" , "fir16" , "fir22" , "fir18")



mw_results <- list()

# Perform a Mann-Whitney U test for each combination of a numerical variable and a categorical variable
for (num_var in numerical_vars) {
  for (cat_var in categorical_vars) {
    # Split the numerical variable by the levels of the categorical variable
    group1 <- data1[data1[[cat_var]] == levels(data1[[cat_var]])[1], num_var]
    group2 <- data1[data1[[cat_var]] == levels(data1[[cat_var]])[2], num_var]
    
    # Perform the Mann-Whitney U test
    res.mw <- wilcox.test(group1, group2)
    
    # Store the result in the list
    mw_results[[paste(num_var, cat_var, sep = "&")]] <- res.mw
  }
}

# Print the Mann-Whitney U test results
print(mw_results)



# test numerical variables and categorcial variable with more than 2 levels

categorical_vars <- c('fir20') 

# Initialize a list to store the Kruskal-Wallis test results
kw_results <- list()

# Perform a Kruskal-Wallis test for each combination of a numerical variable and a categorical variable
for (num_var in numerical_vars) {
  for (cat_var in categorical_vars) {
    # Perform the Kruskal-Wallis test
    res.kw <- kruskal.test(as.formula(paste(num_var, "~", cat_var)), data = data1)
    
    # Store the result in the list
    kw_results[[paste(num_var, cat_var, sep = "&")]] <- res.kw
  }
}

# Print the Kruskal-Wallis test results
print(kw_results)



###### clustering #######



#using the 10 nominal variables that forms the index

cluster_data10 = data1[ , c("cor11","cor12","cor13",
                            "cor14","fir12","fir14",
                            "fir16","fir22",
                            "fir20","fir18")]
str(cluster_data10)

library(dplyr)

cluster_data10 <- cluster_data10 %>%
  mutate_at(vars("cor11" , "cor12" , "cor13" , "cor14" ,
                 "fir12" , "fir14" , "fir16",  "fir22"  ,"fir20" , "fir18"), as.factor)

# preprocess the variables
library(fastDummies)

df_dummy1 <- dummy_cols(cluster_data10, select_columns = c("cor11" , "cor12" , "cor13" , "cor14" ,
                                                           "fir12" , "fir14" , "fir16",  "fir22"  ,"fir20" , "fir18"))
colnames(df_dummy1) # will remove the first 16 columns since they are redundant and character

df_dummy1[c(                        
            "cor11","cor12"  ,                            
            "cor13" ,"cor14" ,                             
            "fir12","fir14"    ,                          
            "fir16"  ,        "fir22"    ,                          
            "fir20","fir18")] = NULL # remove redundant variables


# perform the clustering

library('klaR') # for k-modes

# Perform k-modes clustering

set.seed(123)  # for reproducibility
kmodes_result <- kmodes(df_dummy1, 2 , iter.max = 25) 


kmodes_resultW <- kmodes(df_dummy1, 2 , iter.max = 25 , weighted = TRUE) 


# calculate evaluation metrics
library('fpc')
library(cluster)

#metircs WITH weighted = FALSE
WSS <- sum(kmodes_result$withindiff)
total_ss <- sum(scale(df_dummy1)^2)
BSS = total_ss - WSS
sil_score <- silhouette(kmodes_result$cluster, dist(df_dummy1))
mean(sil_score[, 3]) # avg sil score
#metircs WITH weighted = TRUE
WSSW <- sum(kmodes_resultW$withindiff)
BSSW = total_ss - WSSW
sil_scoreW <- silhouette(kmodes_resultW$cluster, dist(df_dummy1))
mean(sil_scoreW[, 3]) # avg sil score


# the weighted k-modes is better since it has greater BSS and lower WSS



#using the 16 numerical- & nominal variables that forms the index

cluster_data16 = data1[,c("fir25","cor16","fir10","fir17","fir19","fir8_2","cor11","cor12" , "cor13" 
                          ,"cor14","fir12","fir14","fir16","fir22","fir20","fir18")]
str(cluster_data16)

cluster_data16 <- cluster_data16 %>%
  mutate_at(vars("cor11" , "cor12" , "cor13" , "cor14" ,
                 "fir12" , "fir14" , "fir16",  "fir22"  ,"fir20" , "fir18", 'fir19','cor16'), as.factor)

# preprocess the variables 

cluster_data16[c('fir25' , 'fir10' , 'fir17' ,'fir8_2')] <- scale(cluster_data16[c('fir25' , 'fir10' , 'fir17' ,'fir8_2')])


# will normalize the variables instead to make them between 0 & 1 

num_vars <- cluster_data16 %>% select('fir25' , 'fir10' , 'fir17' ,'fir8_2')

num_vars <- as.data.frame(lapply(num_vars, function(x) (x - min(x)) / (max(x) - min(x))))

# Preprocess categorical variables

library(fastDummies)

df_dummy1 <- dummy_cols(cluster_data16, select_columns = c("cor16" ,"fir19", "cor11","cor12","cor13","cor14" ,
"fir12","fir14","fir16","fir22","fir20","fir18"))
colnames(df_dummy1) # will remove the first 16 columns since they are redundant and character

df_dummy1[c("fir25","cor16"                              
            ,"fir10","fir17"    ,                          
            "fir19" ,    "fir8_2"     ,                        
            "cor11","cor12"  ,                            
            "cor13" ,"cor14" ,                             
            "fir12","fir14"    ,                          
        "fir16"  ,        "fir22"    ,                          
          "fir20","fir18")] = NULL
# Combine preprocessed numerical and categorical variables
df_preprocessed <- cbind(num_vars, df_dummy1)

# perform k-means analysis

library(cluster)
library(factoextra)

set.seed(123)  # for reproducibility
k.max <- 5  # maximum number of clusters to consider
wss <- sapply(1:k.max, 
              function(k){kmeans(df_preprocessed, k, nstart=50, iter.max = 15 )$tot.withinss})

# Plot the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = 'Elbow Curve')


# calculate the clusters and vizualization

# Load necessary libraries
library(ggplot2)

# Perform k-means clustering
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(df_preprocessed, centers = 2, nstart = 25)

# Add cluster assignments to the data
df_preprocessed$cluster <- as.factor(kmeans_result$cluster)

# Create a scatter plot of the data colored by cluster assignment
ggplot(df_preprocessed, aes(x = fir17, y = fir10, color = cluster)) +
  geom_point(alpha = 0.5, size = 3) +
  theme_minimal() +
  labs(color = "Cluster")


# calculate evaluation metrics

wss <- sum(kmeans_result$withinss)

# Calculate BSS
df_preprocessed$cluster = NULL # remove it to calculate bss

total_ss <- sum((df_preprocessed - colMeans(df_preprocessed))^2)
bss <- total_ss - wss

# Calculate silhouette score
sil_score <- silhouette(kmeans_result$cluster, dist(df_preprocessed))
mean(sil_score[, 3]) 