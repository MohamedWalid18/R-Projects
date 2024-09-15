full_data <- read.csv("D:/feps  fourth year curriculum/grad project/Data(egypt,jordan,morroco)/E-firms-Combined_Egy-Jor-Mar_Stata.csv",header = T)



factor_analysis <- full_data[, c("country","serial","cor11","cor12","cor13",
                                 "cor14","cor16","fir12","fir14",
                                 "fir16","fir22",
                  "fir20","fir18","fir19")]
dim(factor_analysis)

class(factor_analysis$cor11)
table(factor_analysis$cor11)

library(dplyr)
factor_analysis <- factor_analysis %>% mutate_at(c("country","cor11","cor12","cor13",
                                       "cor14","cor16","fir12","fir14",
                                       "fir16","fir22",
                                       "fir20","fir18","fir19"), as.factor)

str(factor_analysis)



###################adding the index to df_dummy

# to create the index columns
INDEX <- full_data[, c("country","serial","cor11","cor12","cor13","cor14","cor16","fir12","fir14","fir16","fir22",
                  "fir20","fir18","fir19")]
View(INDEX)
dim(INDEX)

class(INDEX$cor11)
table(INDEX$cor11)
INDEX$cor11 <- as.factor(INDEX$cor11)
levels(INDEX$cor11)
library(plyr)
INDEX$cor11 <- mapvalues(INDEX$cor11, from = c("Yes","No"),to= c(1,0))
class(INDEX$cor11)
INDEX$cor11 <- as.numeric(INDEX$cor11)
class(INDEX$cor11)
INDEX["cor11"][INDEX["cor11"] == 1] <- 0
INDEX["cor11"][INDEX["cor11"] == 2] <- 1

class(INDEX$cor12)
table(INDEX$cor12)
INDEX$cor12 <- as.factor(INDEX$cor12)
levels(INDEX$cor12)
INDEX$cor12 <- mapvalues(INDEX$cor12, from = c("Yes","No"),to= c(1,0))
class(INDEX$cor12)
INDEX$cor12 <- as.numeric(INDEX$cor12)
class(INDEX$cor12)
INDEX["cor12"][INDEX["cor12"] == 1] <- 0
INDEX["cor12"][INDEX["cor12"] == 2] <- 1

class(INDEX$cor13)
table(INDEX$cor13)
INDEX$cor13 <- as.factor(INDEX$cor13)
levels(INDEX$cor13)
INDEX$cor13 <- mapvalues(INDEX$cor13, from = c("Yes","No"),to= c(1,0))
class(INDEX$cor13)
INDEX$cor13 <- as.numeric(INDEX$cor13)
class(INDEX$cor13)
INDEX["cor13"][INDEX["cor13"] == 1] <- 0
INDEX["cor13"][INDEX["cor13"] == 2] <- 1

class(INDEX$cor14)
table(INDEX$cor14)
INDEX$cor14 <- as.factor(INDEX$cor14)
levels(INDEX$cor14)
INDEX$cor14 <- mapvalues(INDEX$cor14, from = c("Yes","No"),to= c(1,0))
class(INDEX$cor14)
INDEX$cor14 <- as.numeric(INDEX$cor14)
class(INDEX$cor14)
INDEX["cor14"][INDEX["cor14"] == 1] <- 0
INDEX["cor14"][INDEX["cor14"] == 2] <- 1

class(INDEX$cor16)
table(INDEX$cor16)
INDEX$cor16 <- as.factor(INDEX$cor16)
levels(INDEX$cor16)
INDEX$cor16 <- mapvalues(INDEX$cor16, from = c("Yes","No"),to= c(1,0))
class(INDEX$cor16)
INDEX$cor16 <- as.numeric(INDEX$cor16)
class(INDEX$cor16)
INDEX["cor16"][INDEX["cor16"] == 1] <- NA
INDEX["cor16"][INDEX["cor16"] == 2] <- 0
INDEX["cor16"][INDEX["cor16"] == 3] <- 1

class(INDEX$fir12)
table(INDEX$fir12)
INDEX$fir12 <- as.factor(INDEX$fir12)
levels(INDEX$fir12)
INDEX$fir12 <- mapvalues(INDEX$fir12, from = c("Yes","No"),to= c(1,0))
class(INDEX$fir12)
INDEX$fir12 <- as.numeric(INDEX$fir12)
class(INDEX$fir12)
INDEX["fir12"][INDEX["fir12"] == 1] <- NA
INDEX["fir12"][INDEX["fir12"] == 2] <- 0
INDEX["fir12"][INDEX["fir12"] == 3] <- 1

class(INDEX$fir14)
table(INDEX$fir14)
INDEX$fir14 <- as.factor(INDEX$fir14)
levels(INDEX$fir14)
INDEX$fir14 <- mapvalues(INDEX$fir14, from = c("Yes","No"),to= c(1,0))
class(INDEX$fir14)
INDEX$fir14 <- as.numeric(INDEX$fir14)
class(INDEX$fir14)
INDEX["fir14"][INDEX["fir14"] == 1] <- NA
INDEX["fir14"][INDEX["fir14"] == 2] <- 0
INDEX["fir14"][INDEX["fir14"] == 3] <- 1

class(INDEX$fir16)
table(INDEX$fir16)
INDEX$fir16 <- as.factor(INDEX$fir16)
levels(INDEX$fir16)
INDEX$fir16 <- mapvalues(INDEX$fir16, from = c("Yes","No"),to= c(1,0))
class(INDEX$fir16)
INDEX$fir16 <- as.numeric(INDEX$fir16)
class(INDEX$fir16)
INDEX["fir16"][INDEX["fir16"] == 1] <- NA
INDEX["fir16"][INDEX["fir16"] == 2] <- 0
INDEX["fir16"][INDEX["fir16"] == 3] <- 1

class(INDEX$fir18)
table(INDEX$fir18)
INDEX$fir18 <- as.factor(INDEX$fir18)
levels(INDEX$fir18)
INDEX$fir18 <- mapvalues(INDEX$fir18, from = c("Yes","No"),to= c(1,0))
class(INDEX$fir18)
INDEX$fir18 <- as.numeric(INDEX$fir18)
class(INDEX$fir18)
INDEX["fir18"][INDEX["fir18"] == 1] <- NA
INDEX["fir18"][INDEX["fir18"] == 2] <- 0
INDEX["fir18"][INDEX["fir18"] == 3] <- 1

class(INDEX$fir19)
table(INDEX$fir19)
INDEX$fir19 <- as.factor(INDEX$fir19)
levels(INDEX$fir19)
INDEX$fir19 <- mapvalues(INDEX$fir19, from = c("Yes","No"),to= c(1,0))
class(INDEX$fir19)
INDEX$fir19 <- as.numeric(INDEX$fir19)
class(INDEX$fir19)
INDEX["fir19"][INDEX["fir19"] == 1] <- NA
INDEX["fir19"][INDEX["fir19"] == 2] <- 0
INDEX["fir19"][INDEX["fir19"] == 3] <- 1

class(INDEX$fir22)
table(INDEX$fir22)
INDEX$fir22 <- as.factor(INDEX$fir22)
levels(INDEX$fir22)
INDEX$fir22 <- mapvalues(INDEX$fir22, from = c("Yes","No"),to= c(1,0))
class(INDEX$fir22)
INDEX$fir22 <- as.numeric(INDEX$fir22)
class(INDEX$fir22)
INDEX["fir22"][INDEX["fir22"] == 1] <- NA
INDEX["fir22"][INDEX["fir22"] == 2] <- 0
INDEX["fir22"][INDEX["fir22"] == 3] <- 1

class(INDEX$fir20)
table(INDEX$fir20)
INDEX$fir20 <- as.factor(INDEX$fir20)
levels(INDEX$fir20)
INDEX$fir20 <- mapvalues(INDEX$fir20, from = c("Yes, one person","Yes, we have an IT department",
                                               "No","digital tasks are outsourced"),  to= c(1,1,0,0))
class(INDEX$fir20)
INDEX$fir20 <- as.numeric(INDEX$fir20)
class(INDEX$fir20)
INDEX["fir20"][INDEX["fir20"] == 1] <- NA
INDEX["fir20"][INDEX["fir20"] == 2] <- 0
INDEX["fir20"][INDEX["fir20"] == 3] <- 1

INDEX$index <- (INDEX$cor11 + INDEX$cor12 + INDEX$cor13 + INDEX$cor14 + INDEX$cor16 + INDEX$fir19 +
                  INDEX$fir12 + INDEX$fir14 + INDEX $fir16 + INDEX$fir22 + INDEX$fir20 + INDEX$fir18)/12

summary(INDEX)

new_data <- INDEX[INDEX$index > 0,]
View(new_data)
new_data <- na.omit(new_data)
View(new_data)
dim(new_data)
summary(new_data)
table(new_data$country)

# get the matching firms between factor_analysis df & new_data
new_data1 = new_data[c('country','serial' , 'index')]


df3 <- merge(factor_analysis, new_data1, by = c('country','serial'))

write.csv(df3, file = "D:/feps  fourth year curriculum/grad project/CODING/specified columns & index.csv")


#### preparing data types 
library(dplyr)
df3 <- df3 %>% mutate_at(c("country","cor11","cor12","cor13",
                                                   "cor14","cor16","fir12","fir14",
                                                   "fir16","fir22",
                                                   "fir20","fir18","fir19"), as.factor)

str(df3)

df3['serial'] = NULL

str(df3)



########## calculating website age ########
data1 = read.csv('D:/feps  fourth year curriculum/grad project/CODING/specified columns & index.csv')

date_df = full_data[c("serial",'fir21_2' ,'country' )]

df3 <- merge(data1, date_df, by = c('country','serial'))
# we have empty cells(not applicable) and character values

df3 <- df3 %>%
  mutate(website_age = ifelse(is.na(as.numeric(fir21_2)), fir21_2, 2024 - as.numeric(fir21_2)))
colnames(df3)

df3 <- df3 %>% rename(website_age_years = website_age)

df3[c("X" , "fir21_2")] = NULL

write.csv(df3,'D:/feps  fourth year curriculum/grad project/CODING/data.csv',row.names = FALSE)

# convert the factor variables into dummies in another df called df_dummy
library(fastDummies)

df_dummy <- dummy_cols(df3, select_columns = c("country","cor11","cor12","cor13",
                                               "cor14","cor16","fir12","fir14",
                                               "fir16","fir22",
                                               "fir20","fir18","fir19"))
colnames(df_dummy) # will remove the first 13 columns since they are redundant and character

df_dummy[c("country","cor11","cor12","cor13",
           "cor14","cor16","fir12","fir14",
           "fir16","fir22",
           "fir20","fir18","fir19")] = NULL

colnames(df_dummy)

write.csv(df_dummy, file = "D:/feps  fourth year curriculum/grad project/CODING/dummy_variables & index for FA.csv")

#######factor analysis#######
library(polycor) # to calculate the correlateion matrix 

# to calculate the polyserial correlation matrix,because we have numeric & dummy variables
cor_matrix <- hetcor(df_dummy)$correlations


library(EFA.dimensions) # for scree plot 
SCREE_PLOT(df_dummy)


library(psych)

fa_result <- fa(cor_matrix , nfactors = 1)
fa_result




library(ltm) # for cronbach alpha

alpha_result <- cronbach.alpha(df3)

# alpha is 0.718 which is acceptable


####### Comparing Factors fir8,10,17,25,26 with & without index #######

index_WebsiteAge = read.csv('D:/feps  fourth year curriculum/grad project/CODING/data.csv')

colnames(index_WebsiteAge)

firs_variables = full_data[c("country",   "serial",'fir8_2' , 'fir17' , 'fir10' , 'fir26',
                             'fir25')]

all_variables <- merge(firs_variables, index_WebsiteAge, by = c('country','serial'))


data_without_index = all_variables[c('country',"cor11","cor12",            
                                 "cor13","cor14","cor16" ,"fir12" ,           
                                "fir14","fir16","fir22","fir20",            
                                  "fir18","fir19",'fir8_2' , 'fir17' , 'fir10' ,
                                'fir25')]
str(data_without_index)
summary(data_without_index)

table(data_without_index$fir25)
table(data_without_index$fir26)
table(data_without_index$fir10) # can't impute the non-numeric values
table(data_without_index$fir17) # can't impute the non-numeric values
table(data_without_index$fir8_2)

library(dplyr)
data_without_index <- data_without_index %>% mutate_at(c("country","cor11","cor12","cor13",
                           "cor14","cor16","fir12","fir14",
                           "fir16","fir22",
                           "fir20","fir18","fir19"), as.factor)

data_without_index <- data_without_index %>% mutate_at(c('fir8_2' , 
                                                         'fir17' , 'fir10'
                                                         ), as.numeric)


 # subtract 2024 from fir8_2 to get the years of having high speed internet 
data_without_index$fir8_2 = 2024 - data_without_index$fir8_2



# converting to dummy variables 
library(fastDummies)

df_dummy1 <- dummy_cols(data_without_index, select_columns = c("country","cor11","cor12","cor13",
                                               "cor14","cor16","fir12","fir14",
                                               "fir16","fir22",
                                               "fir20","fir18","fir19"))
colnames(df_dummy1) # will remove the first 13 columns since they are redundant and character

df_dummy1[c("country","cor11","cor12","cor13",
           "cor14","cor16","fir12","fir14",
           "fir16","fir22",
           "fir20","fir18","fir19")] = NULL



# run FA on data_without_index
library(polycor)
cor_matrix <- hetcor(df_dummy1)$correlations


library(EFA.dimensions) # for scree plot 
SCREE_PLOT(cor_matrix)


library(psych)

fa_result <- fa(cor_matrix , nfactors = 1)
fa_result




library(ltm) # for cronbach alpha

alpha_result <- cronbach.alpha(df_dummy1)



#####index with fir variables#####
data_with_index = index_WebsiteAge[c("country",'serial','index')]


firs_data = full_data[c("country",'serial','fir8_2' , 'fir17' , 'fir10' , 'fir26',
                             'fir25')]

index_firs <- merge(data_with_index, firs_data, by = c('country','serial'))

summary(index_firs)

index_firs$fir26 = NULL # drop fir26 it has 510 NA's
index_firs$serial = NULL

str(index_firs)

# converting data types to be numeric
index_firs <- index_firs %>% mutate_at(c('fir8_2' ,'fir17' , 'fir10'), as.numeric)
index_firs <- index_firs %>% mutate_at(c('country'), as.factor)

# subtract 2024 from fir8_2 to get the years of having high speed internet 
index_firs$fir8_2 = 2024 - index_firs$fir8_2


#convert factor variables to dummy variables
df_dummy2 <- dummy_cols(index_firs, select_columns = 'country')
colnames(df_dummy2) # will remove country since it's redundant

df_dummy2["country"] = NULL

# run FA 

cor_matrix <- hetcor(df_dummy2)$correlations


SCREE_PLOT(cor_matrix)

csv = merge(index_WebsiteAge , firs_data, by = c('country','serial'))



####### Run FA on nominal variables only######
df1 = read.csv('D:/feps  fourth year curriculum/grad project/CODING/nominal_DummyVariables.csv')
colnames(df1)

df1[c("cor16_No","cor16_Yes","fir19_No","fir19_Yes" , "country_Egypt","country_Jordan","country_Morocco")] = NULL

library(polycor)
cor_matrix <- hetcor(df1)$correlations


library(EFA.dimensions) # for scree plot 
SCREE_PLOT(cor_matrix)


library(psych)

fa_result <- fa(cor_matrix , nfactors = 1)
fa_result

cronbach.alpha(df1)


########
df2 = read.csv('D:/feps  fourth year curriculum/grad project/CODING/Nominal_index_websiteAge.csv')
colnames(df2)
df2[c("country","serial","index","website_age_years","cor16" ,"fir19" )] = NULL


df3 = read.csv('D:/feps  fourth year curriculum/grad project/CODING/nominal_DummyVariables.csv')
df3[c("cor16_No" ,"cor16_Yes" , 'fir19_No' , 'fir19_Yes',"country_Egypt" ,"country_Jordan"                     
      ,"country_Morocco" )] = NULL


cor_matrix <- hetcor(df2)$correlations
SCREE_PLOT(cor_matrix)

library(ltm) # for cronbach alpha

alpha_result <- cronbach.alpha(df2)


## extract factor scores

results2 <- fa(df3, nfactors = 1, rotate = "none", scores = "Bartlett", 
               fm = "pa", cor = "poly")

df2$scores = results2$scores

# test sufficiency of 1 factor

chi_square <- 110016.4  # from results2 output 


# Calculate the p-value
pchisq(chi_square, 209, lower.tail = FALSE)



# try different factors to observe the loadings

results3 <- fa(df3,nfactors = 10,rotate = "none", scores = "Bartlett", 
               fm = "pa", cor = "poly")




####### Run FA on 16 variables ######

data1 = read.csv('D:/feps  fourth year curriculum/grad project/clustering/imputed_data_nominal_variables(not index).csv')

colnames(data1)

data1[ , c("X")] = NULL

library(dplyr)

data1 <- data1 %>%
  mutate_at(vars("cor11" , "cor12" , "cor13" , "cor14" ,
                 "fir12" , "fir14" , "fir16",  "fir22"  ,"fir20" , "fir18"), as.factor)
library(fastDummies)

df_dummy1 <- dummy_cols(data1, select_columns = c("cor16" ,"fir19", "cor11","cor12","cor13","cor14" ,
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


# preprocess the quantitative variables 

num_vars <- data1[, c("fir25", "fir10", "fir17", "fir8_2")]


num_vars <- as.data.frame(lapply(num_vars, function(x) (x - min(x)) / (max(x) - min(x))))


# Combine preprocessed numerical and categorical variables
df_preprocessed <- cbind(num_vars, df_dummy1)


# running FA now 

library(polycor)
cor_matrix <- hetcor(df_preprocessed)$correlations


library(EFA.dimensions) # for scree plot 
SCREE_PLOT(cor_matrix)


library(psych)

fa_result <- fa(cor_matrix , nfactors = 1)
fa_result

cronbach.alpha(df_preprocessed)

#### removing redundant dummies
colnames(df_preprocessed)


df_preprocessed[c("cor16_Yes","fir19_Yes"                              
            ,"cor11_Yes","cor12_Yes"    ,                          
            "cor13_Yes" ,    "cor14_Yes"     ,                        
            "fir12_Yes","fir14_Yes"  ,                            
            "fir16_Yes" ,"fir22_Yes" ,                             
            "fir20_digital tasks are outsourced","fir20_No"    ,                          
            "fir20_Yes, we have an IT department"  ,        "fir18_Yes")] = NULL

cor_matrix <- hetcor(df_preprocessed)$correlations

SCREE_PLOT(cor_matrix)
