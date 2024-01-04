# Loading packages
library('ggplot2')
library(DescTools)
library(dplyr)
library('PerformanceAnalytics')
library("tidyr")
library('gridExtra') # to create a grid of multiple plot
library('summarytools')
library('formattable') # for customized tables





# Reading the previously cleaned data 
cleaned_data <- read.csv('D:/feps  fourth year curriculum/DS INTRO/4th assignment/cleaned_data.csv')

str(cleaned_data)

# changing data type for categorical variables to factor
cleaned_data$Ship.Mode <- as.factor(cleaned_data$Ship.Mode)
cleaned_data$Customer.Segment <- as.factor(cleaned_data$Customer.Segment)
cleaned_data$Product.Category <- as.factor(cleaned_data$Product.Category)


colnames(cleaned_data) # to determine the 6 chosen variables

# Selecting 3 quanitative variables & 3 qualitative variables
reduced_data <- cleaned_data[,c("Quantity.ordered.new" , "Profit" , "Sales",
                                             "Ship.Mode","Customer.Segment",
                                             "Product.Category")]

summary(reduced_data)
####### Describtive analysis for categorical data##         

cat_reduced_data <- reduced_data[ , c("Ship.Mode","Customer.Segment",
                                      "Product.Category")]

summary(cat_reduced_data)

#### ship.mode 

#pie chart with percentages & legend
my_palette_default <- scales::hue_pal()(length(levels(cat_reduced_data$Ship.Mode)))

cat_reduced_data %>%
  count(Ship.Mode) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = "", y = percent, fill = Ship.Mode)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = my_palette_default,
                    breaks = levels(cat_reduced_data$Ship.Mode),
                    labels = c("Delivery Truck", "Express Air" ,
                              "Regular Air")) +
  coord_polar(theta = "y") +
  labs(title = "Ship Mode Distribution") +
  theme_void() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

### product category

category_colors <- c("#66c2a5", "#fc8d62", "#8da0cb") #color codes 

cat_reduced_data %>%
  count(Product.Category) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = c("Furniture", "Office Supplies",
                   "Technology"), y = percent, fill = Product.Category)) +
  geom_bar(stat = "identity", width = 0.7, color = 'black') +
  scale_fill_manual(values = category_colors ,
                    labels = c("Furniture", "Office Supplies",
                    "Technology")) +
  labs(title = "Product Category Distribution" , x = 'Product Category') +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0, 0.6))


### customer segments

segment_colors <- c("#F18F01", "#048BA8", "#8d3fa1", "#99C24D")

cat_reduced_data %>%
  count(Customer.Segment) %>%
  mutate(percent = n/sum(n) * 100) %>%
  ggplot(aes(x = "", y = percent, fill = Customer.Segment)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = segment_colors,
                    labels = c("Consumer", "Corporate",
                               "Home Office", "Small Business")) +
  coord_polar(theta = "y") +
  labs(title = "Customer Segment Distribution") +
  theme_void() +
  geom_text(aes(label = paste0(round(percent), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))
  

# 2-way table
  

table1 <- prop.table(table(cat_reduced_data$Customer.Segment,cat_reduced_data$Product.Category), margin = 1) * 100

colnames(table1) <- c("Furniture", "Office Supplies",
                     "Technology")
rownames(table1) <- c("Consumer", "Corporate",
                     "Home Office", "Small Business")


table1_percent <- cat_reduced_data %>%
  group_by(Customer.Segment, Product.Category) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n))

ggplot(table1_percent, aes(x = Product.Category, y = Customer.Segment, fill = perc)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text( vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Product Category", y = "Customer Segment", fill = "Percentage",
       title = 'Product category VS Customer Segment (2-Way table)') +
  geom_text(aes(label = scales::percent(perc)), size = 4)+
  scale_x_discrete(labels = c("Furniture", "Office Supplies",
                              "Technology"), name = "Product Category") +
  scale_y_discrete(labels = c("Consumer", "Corporate",
                              "Home Office", "Small Business"), name = "Customer Segment")




####### Describtive analysis for quantitative data##     


quan_reduced_data <- cleaned_data[,c("Quantity.ordered.new" , "Profit" , "Sales")]

## sales 
sales_histogram <- ggplot(quan_reduced_data, aes(x = Sales)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for Sales", x = "Sales", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0, 0.004))



sales_boxplot <- ggplot(quan_reduced_data, aes(x = "", y = Sales)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for Sales") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(sales_histogram, sales_boxplot, ncol = 2) # combine histogram & boxplot 

Desc(quan_reduced_data$Sales)
## profit 

Freq(quan_reduced_data$Profit)

profit_histogram <- ggplot(quan_reduced_data, aes(x = Profit)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for Profit", x = "Profit", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

profit_boxplot <- ggplot(quan_reduced_data, aes(x = "", y = Profit)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for Profit") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(profit_histogram, profit_boxplot, ncol = 2) # combine histogram & boxplot 

Desc(quan_reduced_data$Profit)


## Quantitiy ordered


Freq(quan_reduced_data$Quantity.ordered.new)

ggplot(quan_reduced_data, aes(x = Quantity.ordered.new)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for Quantitiy ordered", x = "Quantity Ordered", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


Desc(quan_reduced_data$Quantity.ordered.new)

# scatterplot matrix
chart.Correlation(quan_reduced_data , method = 'pearson')


###### combinations between quantitative and qualitative variables

ggplot(reduced_data, aes(y = Sales, x = Customer.Segment)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  scale_x_discrete(labels = c("Consumer", "Corporate",
                              "Home Office", "Small Business"), name = "Customer Segment")+
  labs(title = "Sales VS Customer Segments") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

category1 = subset(reduced_data , Customer.Segment==1)
category2 = subset(reduced_data , Customer.Segment==2)
category3 = subset(reduced_data , Customer.Segment==3)
category4 = subset(reduced_data , Customer.Segment==4)

# summary statistics for all 4 categories
summary(category1)
summary(category2)
summary(category3)
summary(category4)

# get the Interquartile range for all 4 categories
IQR(category1$Sales)
IQR(category2$Sales)
IQR(category3$Sales)
IQR(category4$Sales)


# The summary statistics for category 2 & 3 are very similar