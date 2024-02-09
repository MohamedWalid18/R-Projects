library(dplyr)
library(pROC) # for ROC curve
library(plotROC)# for ROC curve
library('caret') # for confusion matrix
library(writexl) #export dataframe to excel file



################ Performing binary logistic regression model


binary_data= read.csv('D:/feps  fourth year curriculum/CDA/mini project/Thailand resources/recoded_data(binary).csv')

str(binary_data)



# Rename variables names to shorter ones

new_names <- c('index' , "Feeling_of_happiness", "Freedom_Control", "Financial_Satisfaction",
               "Frequency_No_Cash_Income", "Science_Technology", "Gender")
names(binary_data) <- new_names

# Convert explanatory variables to factors

binary_data$Feeling_of_happiness <- recode(binary_data$Feeling_of_happiness ,'1' = '1', '2' = '0') # 0 is unhappy , 1 is happy
binary_data$index <- as.character(binary_data$index)
binary_data$Feeling_of_happiness <- as.numeric(binary_data$Feeling_of_happiness)
binary_data$Freedom_Control <- as.factor(binary_data$Freedom_Control)
binary_data$Financial_Satisfaction <- as.factor(binary_data$Financial_Satisfaction)
binary_data$Frequency_No_Cash_Income <- as.factor(binary_data$Frequency_No_Cash_Income)
binary_data$Science_Technology <- as.factor(binary_data$Science_Technology)
binary_data$Gender <- as.factor(binary_data$Gender)



apply(binary_data , 2 , table) #observe the value of different categories

 # split Data into train(70%) & test(30%)
set.seed(123)

train_indices <- sample(seq_len(nrow(binary_data)), size = 0.7 * nrow(binary_data))

train_data <- binary_data[train_indices, ]



 ####### fitting the logistic regression model


# fitting on the train set
initial_model <-glm(Feeling_of_happiness ~ Freedom_Control +
                  
                  Financial_Satisfaction +
                  Frequency_No_Cash_Income +
                  Science_Technology +
                  Gender,
                data = train_data,
                family = 'binomial')





stepwise_model <- step(initial_model, direction = "both") # perform stepwise 
summary(stepwise_model)

mod0 <- glm(Feeling_of_happiness ~ 1, family = binomial("logit"), data = train_data)# null model(interept only)


pseudoR2 <- 1 - logLik(stepwise_model) / logLik(mod0) # mcfadden pseudo R-squared

summary(step(initial_model , direction = 'back')) # perform backward selection

#both methods yield the same model,therefore, the stepwise_model is appropriate


# interaction model

interaction_model <- glm(formula = Feeling_of_happiness ~ Financial_Satisfaction + 
                           Frequency_No_Cash_Income + Science_Technology+
                           Financial_Satisfaction*Frequency_No_Cash_Income, 
    family = "binomial", data = train_data)

summary(interaction_model)

stepwise_inter_model <- step(interaction_model, direction = "both") # perform stepwise 
summary(stepwise_inter_model)  # sounds like the main effects model(stepwise model) is the best



####compare between interaction model & main effects model Through ROC curves & confusion matrix on test set

test_data <- binary_data[-train_indices,names(interaction_model$model) ] # selecting the same number of columns in the final model


# Make predictions on the test set for the stepwise model
step_predictions_test <- predict(stepwise_model,newdata = test_data,  type = "response") # using the main effects model on test_data

# Make predictions on the test set for the interaction model
interaction_predictions_test <- predict(interaction_model,newdata = test_data,  type = "response") # using the interaction model on test_data

# Create a data frame for the stepwise model
df.step <- data.frame(obs = test_data$Feeling_of_happiness, pred = step_predictions_test, model = "Main effects model")

# Create a data frame for the interaction model
df.interaction <- data.frame(obs = test_data$Feeling_of_happiness, pred = interaction_predictions_test, model = "Interaction Model")

# Combine the data frames
df <- rbind(df.step, df.interaction)

# Plot ROC curves for both models on test_data
ggplot(df, aes(d = obs, m = pred, color = model)) +
  geom_roc(cutoffs.at = c(seq(0,1, by = 0.1)))+
  theme(legend.position = "bottom") +
  labs(x = "1 - Specificity", y = "Sensitivity")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
  labs(x = "1 - Specificity", y = "Sensitivity", title = "ROC Curves on test set")+
  scale_color_manual(values = c("blue", "red"))+
  theme(plot.title = element_text(hjust = 0.5))


roc_step <- roc(test_data$Feeling_of_happiness, step_predictions_test)

roc_interaction <- roc(test_data$Feeling_of_happiness, interaction_predictions_test)


# AUC for main effects model & interaction model

auc(roc_step) #main effects model is greater than the interaction model
auc(roc_interaction) 

# to extract the best cutoff point from ROC curve for interaction model
roc_coords <- coords(roc_interaction, "best") #extract the coordinates of the ROC curve

# Extract the best cutoff point
roc_coords$threshold

coords(roc_step, "best") # investigating performance of main effects model




# Convert probabilities to predictions using the chosen cutoff point
interaction_preds <- ifelse(interaction_predictions_test > 0.3609382, 1, 0)

# Assuming that 'outcome' is the name of your response variable in the test set
# Create confusion matrix for interaction model
confusionMatrix(as.factor(interaction_preds), as.factor(test_data$Feeling_of_happiness)) 



# Convert probabilities to predictions using the chosen cutoff point
step_preds <- ifelse(step_predictions_test > 0.353695, 1, 0)

# Assuming that 'outcome' is the name of your response variable in the test set
# Create confusion matrix for maineffects model
confusionMatrix(as.factor(step_preds), as.factor(test_data$Feeling_of_happiness)) 

# main effects model is better in overall correct classification

exp(stepwise_model$coefficients) # exponentiating the estimates to get the odds ratios


################ Performing multinomial logistic regression model

library(nnet) 
library(MASS)# for multinomial log reg
library('VGAM')
library(lmtest) # to test proportional odds assumption

multi_data= read.csv('D:/feps  fourth year curriculum/CDA/mini project/Thailand resources/recoded_data(multinomial Y).csv')

# renaming the variables to shorter ones
new_names2 <- c("Feeling_of_happiness", "Freedom_Control", "Financial_Satisfaction",
               "Frequency_No_Cash_Income", "Science_Technology", "Gender")
names(multi_data) <- new_names2


# converting variables' types
multi_data$Feeling_of_happiness <- as.factor(multi_data$Feeling_of_happiness)
multi_data$Freedom_Control <- as.factor(multi_data$Freedom_Control)
multi_data$Financial_Satisfaction <- as.factor(multi_data$Financial_Satisfaction)
multi_data$Frequency_No_Cash_Income <- as.factor(multi_data$Frequency_No_Cash_Income)
multi_data$Science_Technology <- as.factor(multi_data$Science_Technology)
multi_data$Gender <- as.factor(multi_data$Gender)


#recode response variables
multi_data$Feeling_of_happiness <- recode(multi_data$Feeling_of_happiness ,'1' = 'Happy', '2' = 'Quite happy','3' = 'Unhappy')

table(multi_data$Feeling_of_happiness)

# test proportional odds assumption

# Define the function
test_proportional_odds <- function(formula, data) {
  # Fit a proportional odds model
  model_prop_odds <- vglm(formula, family = cumulative(parallel = TRUE), data = data)
  
  # Fit a non-proportional odds model
  model_non_prop_odds <- vglm(formula, family = cumulative(parallel = FALSE), data = data)
  
  # Perform a likelihood ratio test
  lr_test <- lrtest(model_prop_odds, model_non_prop_odds)
  
  # Return the test result
  return(lr_test)
}

test_result <- test_proportional_odds(Feeling_of_happiness ~ Freedom_Control + Financial_Satisfaction +
                                        Frequency_No_Cash_Income+Science_Technology+Gender, multi_data)

print(test_result)


# Fit multinomial logistic regression

multinom_model <- multinom(Feeling_of_happiness ~ Freedom_Control + Financial_Satisfaction +
                             Frequency_No_Cash_Income+Science_Technology+Gender,
                           data =multi_data)

# Display summary
multino_summary <- summary(multinom_model)

# to calculate p-values for each coefficient
fit_multinom <- function(formula, data) {
  # Fit the model
  model <- nnet::multinom(formula, data = data)
  
  # Get the summary of the model
  model_summary <- summary(model)
  
  # Extract the coefficients and their standard errors
  coefs <- coef(model)
  std_errors <- model_summary$standard.errors
  
  # Calculate z values
  z_values <- coefs / std_errors
  
  # Calculate p-values
  p_values <- 2 * (1 - pnorm(abs(z_values)))
  
  # Create a data frame to display the results
  results <- data.frame(coefs, std_errors, z_values, p_values)
  colnames(results) <- c("Coefficient", "Standard Error", "Z value", "P value")
  
  # Return the results
  return(results)
}


p_values_one_sided <- pf(multino_summary$coefficients/ multino_summary$standard.errors, 1, Inf, lower.tail = FALSE)

# Calculate p-values for two-sided test
p_values_two_sided <- pmin(1, 2*p_values_one_sided)

# Print the p-values
print(p_values_two_sided)



coefs <- multino_summary$coefficients
std_errors<- multino_summary$standard.errors

fit_multinom <- function(coefs, std_errors) {
  # Calculate z values
  z_values <- coefs / std_errors
  
  # Calculate p-values for a two-sided test, only for non-NA coefficients
  p_values <- ifelse(is.finite(z_values), 2 * (1 - pnorm(abs(z_values))), NA)
  
  # Create a data frame to display the results
  results <- data.frame(coefs, std_errors, z_values, p_values)
  colnames(results) <- c("Coefficient", "Standard Error", "Z value", "P value")
  
  # Ensure the knitr package is installed
  if (!require(knitr)) {
    install.packages("knitr")
  }
  
  # Print the results as a table
  print(knitr::kable(results))
  
  # Return the results
  return(results)
}


results <- fit_multinom(coefs, std_errors)

# Print the results
print(results)





