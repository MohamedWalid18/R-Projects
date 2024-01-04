################ Read the data ################
library(readxl)



# 1st data set
data_ass_1_1 <- read_excel("D:/feps  fourth year curriculum/DS INTRO/3rd assignment/data ass 1 1.xlsx", 
                           col_types = c("text", "text", "numeric", 
                                         "numeric", "numeric", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "numeric", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "numeric", "text")) # converting data types

#2nd data set
data_ass_1_2 <- read_excel("D:/feps  fourth year curriculum/DS INTRO/3rd assignment/data ass 1 2.xlsx", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "text"))


        ################ Combine the two excel sheets into one data frame ################

merged_df <- cbind(data_ass_1_1 , data_ass_1_2) # merged dfs before cleaning

              ################Understand the data
head(merged_df)
str(merged_df)
summary(merged_df)

            ################ Clean the data ################
library(dplyr)       # for data manipulation
library('bnstruct')  # 'bnstruct' package for imputation
library("DescTools") # for enhanced summary 
library('plyr') # to recode categorical variables


cleaned_data <- merged_df[ , 1:25]  # Drop the duplicated 'Order ID' column
cleaned_data <- cleaned_data %>% select(-one_of(c("Customer Name","Product Container","Country","Row ID",
                                                  "Product Sub-Category",'Postal Code',
                                                  'State or Province',"Order Priority","Unit Price"
                                                  ,"Shipping Cost","Product Base Margin","City"
                                                   ))) # Drop the selected columns

          ### Converting categorical data to factors

cleaned_data$`Ship Mode` <- as.factor(cleaned_data$`Ship Mode`)
cleaned_data$`Customer Segment` <- as.factor(cleaned_data$`Customer Segment`)
cleaned_data$`Product Category` <- as.factor(cleaned_data$`Product Category`)
cleaned_data$`Region` <- as.factor(cleaned_data$`Region`)




summary(cleaned_data)

sum(is.na(cleaned_data))  # Total NA's is 5

                                ###### Cleaning ###### 

                                #### Cleaning 'Profit'

                                ## Handling missing values

#i will use 5 nearest neighbors to impute missing values 

cleaned_data$`Profit` = knn.impute(as.matrix(cleaned_data$Profit),k=5) 

                                ## Handling outliers

# using boxplot to investigate outliers
boxplot(cleaned_data$Profit)



# i will replace the outliers with the min&max of non-outlier values
outlier_info <- boxplot.stats(cleaned_data$Profit) # to get the values needed for a boxplot(Q1,Q2,Q3...)

# Get the whiskers of the boxplot
lower_whisker <- outlier_info$stats[1] #lower whisker
upper_whisker <- outlier_info$stats[5] #upper whisker

# Get the max and min of the non-outlier values
max_non_outlier <- max(cleaned_data$Profit[!(cleaned_data$Profit %in% outlier_info$out)]) # get maximum of non-outlier value in profit
min_non_outlier <- min(cleaned_data$Profit[!(cleaned_data$Profit %in% outlier_info$out)]) # get minimum of non-outlier value in profit

# Replace outliers greater than the upper whisker with max_non_outlier
cleaned_data$Profit[cleaned_data$Profit > upper_whisker] <- max_non_outlier

# Replace outliers less than the lower whisker with min_non_outlier
cleaned_data$Profit[cleaned_data$Profit < lower_whisker] <- min_non_outlier



                                 #### Cleaning 'Quantity ordered new'

cleaned_data$`Quantity ordered new` = knn.impute(as.matrix(cleaned_data$`Quantity ordered new`),k=5)

                                  #### Cleaning 'Sales'

                                ### Handling missing data

## impute missing values
cleaned_data$`Sales` = knn.impute(as.matrix(cleaned_data$`Sales`),k=5)

                                  ### Handling outliers

boxplot(cleaned_data$Sales) # we have outliers greater than the upper whisker

# to calculate the benches
outlier_info1 <- boxplot.stats(cleaned_data$Sales) # to get the values needed for a boxplot(Q1,Q2,Q3...)

# Get the whiskers of the boxplot
lower_whisker <- outlier_info1$stats[1] #lower whisker
upper_whisker <- outlier_info1$stats[5] #upper whisker

# Get the max and min of the non-outlier values
max_non_outlier <- max(cleaned_data$Sales[!(cleaned_data$Sales %in% outlier_info1$out)]) # get maximum value  of non-outlier value in sales
min_non_outlier <- min(cleaned_data$Sales[!(cleaned_data$Sales %in% outlier_info1$out)]) # get minimum value of non-outlier value in sales

# Replace outliers greater than the upper whisker with max_non_outlier
cleaned_data$Sales[cleaned_data$Sales > upper_whisker] <- max_non_outlier

# Replace outliers less than the lower whisker with min_non_outlier
cleaned_data$Sales[cleaned_data$Sales < lower_whisker] <- min_non_outlier

boxplot(cleaned_data$Sales) # now we don't have outliers



summary(cleaned_data)



                           #### Cleaning 'Region'

table(cleaned_data$Region) # Sounds like 'Vest' should be 'West' to have 4 total regions

cleaned_data$`Region` <- recode_factor(cleaned_data$`Region`, "Vest" = "West") #recode 'Vest' to 'West



                    ####### cleaning 'Discount' #####
table(cleaned_data$Discount) # 40 could be considered as a mistake

cleaned_data$Discount <- replace(cleaned_data$Discount, cleaned_data$Discount == 40, Mode(cleaned_data$Discount))  # replace 40 with the mode


                 ################ Split the data based on the region ################

table(cleaned_data$Region)

central_region = cleaned_data[cleaned_data$Region == 'Central', ]
east_region = cleaned_data[cleaned_data$Region == 'East', ]
south_region = cleaned_data[cleaned_data$Region == 'South', ]
west_region = cleaned_data[cleaned_data$Region == 'West', ]



           ################Use codes with categorical variables instead of the categories names ################
table(cleaned_data$Country)
table(cleaned_data$`Customer Segment`)
table(cleaned_data$`Ship Mode`)
table(cleaned_data$Region)
table(cleaned_data$`Product Category`)

cleaned_data$`Customer Segment` <- recode_factor(cleaned_data$`Customer Segment`, "Consumer" = "1", "Corporate" = "2",
                                     "Home Office" = "3", "Small Business" = "4") # recode `Customer Segment`

cleaned_data$`Ship Mode` <- recode_factor(cleaned_data$`Ship Mode`, "Delivery Truck" = "1", "Express Air" = "2",
                                     "Regular Air" = "3") # recode `Ship Mode`

cleaned_data$Region <- recode_factor(cleaned_data$Region, "Central" = "1", "East" = "2",
                                     "South" = "3", "West" = "4") # recode 'Region'

cleaned_data$`Product Category` <- recode_factor(cleaned_data$`Product Category`, "Furniture" = "1", "Office Supplies" = "2",
                                     "Technology" = "3") # recode 'product Category'


             ################ Split the data into 70% train and 30% test ################

set.seed(123)  # to fix the random sample

train <- cleaned_data %>% dplyr::sample_frac(0.70)
test <- dplyr::anti_join(cleaned_data, train, by = "Customer ID") # test set is different from the training set



              ################ Calculate the mean profit per region ################

mean_profit_per_region <- tapply(cleaned_data$Profit, cleaned_data$Region, mean) # using tapply on a subset of a df where we have a factor variable

mean_profit_per_region_df <- data.frame(
  Region = factor(names(mean_profit_per_region)),
  Mean_Profit = mean_profit_per_region) # convert the result to a df

mean_profit_per_region_df

               ################ Quantity order is greater than 20 ################

Quantity_20 <- cleaned_data[cleaned_data$`Quantity ordered new` > 20, ]  # A df where Quantity > 20
