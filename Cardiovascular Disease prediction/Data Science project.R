## loading libraries
library('ggplot2')
library("DescTools")
library("dplyr")
library("GGally")
library('gridExtra')
library('cowplot')
library("car") #VIF
library("plotROC") #geom_roc
library('caret') # for confusion matrix
library("pROC")
original_data <- read.csv("D:/ass data science/project Data science/Cardiovascular_Disease_Dataset.csv")

original_data<- read.csv('D:/feps  fourth year curriculum/DS INTRO/group project/Cardiovascular_Disease_Dataset.csv') # read the full-variables data

colnames(original_data)


# selecting 6 variables

selected_variables <- c("age", "gender", "chestpain", "exerciseangia",
                        "restingBP", "maxheartrate", "serumcholestrol",
                        "noofmajorvessels","target")

red_data <- original_data[, selected_variables]

str(red_data)


# changing data type for (gender ,chestpain , target & exerciseangia) to factor

red_data$gender <- as.factor(red_data$gender)
red_data$chestpain <- as.factor(red_data$chestpain)
red_data$exerciseangia <- as.factor(red_data$exerciseangia)
red_data$target <- as.factor(red_data$target)

####### Descriptive analysis#######

# gender  & cardiovascular disease pie charts

my_palette_default <- scales::hue_pal()(length(levels(red_data$gender)))



gender_pie = red_data %>%
  count(gender) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = "", y = percent, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = my_palette_default,
                    breaks = levels(red_data$gender),
                    labels = c('Female' , 'Male')) +
  coord_polar(theta = "y") +
  labs(title = "Gender Distribution") +
  theme_void() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

HD_pie = red_data %>%
  count(target) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = "", y = percent, fill = target)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = my_palette_default,
                    breaks = levels(red_data$target),
                    labels = c('No Heart Disease' , 'Heart Disease')) +
  coord_polar(theta = "y") +
  labs(title = "Heart Disease Distribution") +
  theme_void() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(gender_pie, HD_pie, ncol = 2)



## barcharts for exerciseangia & chestpain

chestpain_colors <- c("#F18F01", "#048BA8", "#8d3fa1", "#99C24D")

chestpain_barchart <- red_data %>%
  count(chestpain) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = c("typical angina", "atypical angina",
                   "non-anginal pain",'asymptomatic'), y = percent, fill = chestpain)) +
  geom_bar(stat = "identity", width = 0.7, color = 'black') +
  scale_fill_manual(values = chestpain_colors ,
                    labels = c("typical angina", "atypical angina",
                               "non-anginal pain",'asymptomatic')) +
  labs(title = "Chest Pain type bar chart" , x = 'Chest Pain type') +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0, 0.45))   #chestpain bar chart



exerciseangia_colors <- c("#66c2a5", "#fc8d62", "#8da0cb") #color codes 

angia_barchart <- red_data %>%
  count(exerciseangia) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = c("No", "Yes"), y = percent, fill = exerciseangia)) +
  geom_bar(stat = "identity", width = 0.7, color = 'black') +
  scale_fill_manual(values = exerciseangia_colors ,
                    labels = c("No", "Yes")) +
  labs(title = "Exercise induced angina bar chart" , x = 'Exercise induced angina') +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0, 0.6)) # exerciseangia barchart

grid.arrange(chestpain_barchart, angia_barchart, ncol = 2) # combine the 2 histograms in 1 plot 



## 2-way tabe for gender & chestpain

table1 <- prop.table(table(red_data$gender,red_data$chestpain), margin = 1) * 100

colnames(table1) <- c("typical angina", "atypical angina",
                      "non-anginal pain",'asymptomatic')
rownames(table1) <- c("Female", "Male")


table1_percent <- red_data %>%
  group_by(gender, chestpain) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n))

ggplot(table1_percent, aes(x = gender, y = chestpain, fill = perc)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text( vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Gender", y = "Chest Pain type", fill = "Percentage",
       title = 'Gender VS Chest Pain type (2-Way table)') +
  geom_text(aes(label = scales::percent(perc)), size = 4)+
  scale_x_discrete(labels = c("Female", "Male"), name = "Gender") +
  scale_y_discrete(labels = c("typical angina", "atypical angina",
                              "non-anginal pain",'asymptomatic'), name = "Chest Pain type")



# scatterplot matrix for quantitative variables

num_vars <- c("age", "restingBP", "maxheartrate", "serumcholestrol", "noofmajorvessels") #selecting numeric variables

quan_data <- red_data[, num_vars] # df of only the numeric variables

Desc(quan_data)

ggpairs(quan_data,
        columns = num_vars,
        title = "Scatterplot Matrix with Correlation Coefficients",
        lower = list(continuous = "points", combo = "dot", aes(color = gender,alpha = 0.5)),
        diag = list(continuous = "density"),
        upper = list(continuous = "cor"),
        axisLabels = "show")+
  theme_bw()+
  theme(text = element_text(size = 10))

       
# Histograms for age , max heatrate,restingBP , serumcholestrol ,noofmajorvessels
age_histogram <- ggplot(red_data, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for age", x = "Age", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) #histogram for age

maxheart_histogram <- ggplot(red_data, aes(x = maxheartrate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for Maximum heart rate achieved", x = "Max heart rate", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

resting_histogram <- ggplot(red_data, aes(x = restingBP)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for resting Blood Pressure", x = "restingBP", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

serum_histogram <- ggplot(red_data, aes(x = serumcholestrol)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for Serum Cholestrol", x = "serumcholestrol", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(age_histogram, maxheart_histogram, ncol = 2) # combine the 2 histograms together 


grid.arrange(resting_histogram, serum_histogram, ncol = 2) # combine the 2 histograms together 

# barchart for number of major vessels


# Calculate the frequency and percentage of each value
major_df <- data.frame(table(quan_data$noofmajorvessels))
names(major_df) <- c("noofmajorvessels", "count")
major_df$percentage <- (major_df$count / sum(major_df$count)) * 100

# Create the bar chart
p <- ggplot(major_df, aes(x = noofmajorvessels, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of Major Vessels", y = "Percentage (%)", title = "Bar Chart for Number of Major Vessels") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, max(major_df$percentage) + 10)  # Increase the y-axis limits

# Add the percentage labels on top of the bars
p + geom_text(aes(label = round(percentage, 1)), vjust = -0.5)






########### Boxplot for quantitative variables
age_boxplot <- ggplot(quan_data, aes(x = "", y = age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for Age") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

rest_boxplot <- ggplot(quan_data, aes(x = "", y = restingBP)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for Resting blood pressure ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

maxheart_boxplot <- ggplot(quan_data, aes(x = "", y = maxheartrate)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for Maximum heart rate") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

serum_boxplot <- ggplot(quan_data, aes(x = "", y = serumcholestrol)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for  Serum cholesterol") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

major_boxplot <- ggplot(quan_data, aes(x = "", y = noofmajorvessels)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for Number of major vessels") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(age_boxplot, rest_boxplot, ncol = 2)

grid.arrange(maxheart_boxplot,serum_boxplot,major_boxplot, ncol = 3)


layout(matrix(c(1,2,3,4,5,5), ncol = 2, byrow = TRUE)) # layout function works with base R plots not ggplots

boxplot(quan_data$age, main = "Boxplot for Age", col = "skyblue")
boxplot(quan_data$restingBP, main = "Boxplot for Resting blood pressure", col = "skyblue")
boxplot(quan_data$maxheartrate, main = "Boxplot for Maximum heart rate", col = "skyblue")
boxplot(quan_data$serumcholestrol, main = "Boxplot for Serum cholesterol", col = "skyblue")
boxplot(quan_data$noofmajorvessels, main = "Boxplot for Number of major vessels", col = "skyblue")


####### fitting the logistic regression model

# split Data into train(70%) & test(30%)
set.seed(123)

train_indices <- sample(seq_len(nrow(red_data)), size = 0.7 * nrow(red_data))
train_data <- red_data[train_indices, ]
test_data <- red_data[-train_indices,]

# fitting on the train set
initial_model <-glm(target ~ age +
                      
                      gender +
                      chestpain +
                      exerciseangia+
                      restingBP+
                      maxheartrate+
                      serumcholestrol+
                      noofmajorvessels,
                    data = train_data,
                    family = 'binomial')


stepwise_model <- step(initial_model, direction = "both") # perform stepwise 
summary(stepwise_model)
# exponentiating the estimates to get the odds ratios
exp(stepwise_model$coefficients)

# Check for multicollinearity using VIF

vif(stepwise_model)


# Test the significance of the model as a whole
anova(stepwise_model, test = "Chisq")

test_data <- red_data[-train_indices,names(stepwise_model$model) ] # selecting the same number of columns in the final model


# Make predictions on the test set for the stepwise model
step_predictions_test <- predict(stepwise_model,newdata = test_data,  type = "response") # using the main effects model on test_data

# Create ROC curve for the stepwise model
roc_obj <- roc(test_data$target,step_predictions_test)

# Plot the ROC curve
D <- data.frame(D = as.numeric(test_data$target), M = step_predictions_test) 

ggplot(D,aes(d = D, m = M))+
  geom_roc(cutoffs.at = c(seq(0,1, by = 0.1)))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1))+
  labs(x = "1 - Specificity", y = "Sensitivity")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
  labs(x = "1 - Specificity", y = "Sensitivity", title = "ROC Curve")

#Area under the curve
roc_step <- roc(test_data$target, step_predictions_test)
auc(roc_step) 

# investigating performance of main effects model
roc_coords<-coords(roc_step, "best") 
# Extract the best cutoff point
roc_coords$threshold

# Create confusion matrix for test model

# Convert probabilities to predictions using the chosen cutoff point
step_preds <- ifelse(step_predictions_test > 0.403329, 1, 0)

# function to create a confusion matrix & calculate their own measure 
table1<-table(test_data$target,step_preds)
table1

calculate_sensitivity_specificity <- function(classification_table) {
  # Extract the values from the classification table
  true_positives <- classification_table[2, 2]
  false_negatives <- classification_table[2, 1]
  true_negatives <- classification_table[1, 1]
  false_positives <- classification_table[1, 2]
  # Calculate sensitivity and specificity and accuracy
  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  Accuracy<-sum(diag(classification_table)) / sum(classification_table)
  # Return the results
  return(list(sensitivity, specificity,Accuracy))
}

calculate_sensitivity_specificity(table1)


## Machine learning (Decision tree) ##
# Using libraries #

library(caTools)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)

# Display the model
attach(train_data)
model_tree <- rpart(target ~ ., data = train_data, method = "class")
model_tree

# Visualize the decision tree
# 1 -> has heart attack , 0 -> does not have heart attack

rpart.plot(model)

# the most important variable in spliting the data

importance <- varImp(model_tree)
importance %>%
  arrange(desc(Overall))

# predictions using the model

prediction <- predict(model_tree, newdata = test_data, type = "class")
prediction

## confusion matrix
confusionMatrix(test_data$target, prediction)
