

# 1. Load the libraries for this session
library(tidyverse)    # A collection of essential packages for data manipulation (dplyr), visualization (ggplot2), and more.
library(janitor)      # Excellent for cleaning column names and other basic data cleaning tasks.
library(lubridate)    # Makes working with dates and times much easier.
library(DataExplorer) # Great for automated exploratory data analysis and visualizing missing data.
library(randomForest) # A popular and powerful machine learning algorithm for prediction.
library(caret)        # For splitting data into training and testing sets
library(xgboost)      # The core package for the XGBoost algorithm
library(dplyr)
library(doParallel)   # Library for parallel processing
library (RcppRoll)
        
# 2. Import Data
sales_data<-read.csv("/home/creation/Project/Sales Analysis Project/sales_data/train.csv")

# 3. Display the first few rows of the original data
head(sales_data)

# 4. Data Preparation & Cleaning
## Create a copy to work on, preserving the original import
df_clean <- sales_data

##Changing Column Name to more understandable Name
df_clean<-df_clean %>% 
  rename(No_of_Orders=X.Order)

## Renaming Columns and Cleaning Whitespace
# Why: Standardizing column names makes them easier to type and reference.
df_clean<-df_clean %>% 
  clean_names()

## Checking and Correcting Data Structure & Types
# Why: R needs to know if a column is a number, text, date, or category (factor) to perform
# the correct operations and create appropriate plots.
glimpse(df_clean)

## Convert data types appropriately
# Why: `id`, `store_type`, etc., are categorical. `date` needs to be a Date object.
# `holiday` and `discount` are also categorical.
df_clean=df_clean %>%
  mutate(
  store_id=as.factor(store_id),
  store_type=as.factor(store_type),
  location_type=as.factor(location_type),
  region_code=as.factor(region_code),
  holiday=as.factor(ifelse(holiday==1,"Yes","No")),
  discount=as.factor(discount),
  date=dmy(date),
  sales=as.numeric(sales),
  no_of_orders=as.numeric(no_of_orders),
  date=as.Date(date,format="%d/%m/%y"),
  )

## Rechecking corrected data structures
glimpse(df_clean)

## Check for Missing Values
# Why: Missing values (NA) can break calculations and models. We need to identify them
# and decide on a strategy (remove, impute, etc.).
colSums(is.na(df_clean))

##Visual Plot of Missing Data
plot_missing(df_clean)

## Remove Duplicate Entries
# Why: Duplicate rows can skew results (e.g., overstating sales).
# We check the number of rows before and after to see if any were removed.
cat("No of Rows before Removing Duplicates: ", nrow(df_clean), "\n")

duplicated_data=df_clean %>% 
  group_by(across(everything())) %>% #Group by all columns
  filter(n()>1) %>% # Keep only groups (rows) that appear more than once
  ungroup()

df_clean=df_clean %>% 
  distinct()

cat("No of Rows after Removing Duplicates: ", nrow(df_clean), "\n")

## Identify and Handle Outliers
# Why: Outliers can heavily influence statistical measures (like the mean) and model performance.
# We first visualize them to understand their distribution.

# Boxplot to visualize potential outliers in sales
ggplot(df_clean,aes(x=sales)) +
  geom_boxplot(fill="skyblue",outlier.colour = "red",outlier.shape = 18,outlier.size = 3)+
  labs(title="Boxplot of Sales to Identify Outliers",x="Sales Amount")+
  theme_minimal()

# Identify outliers using the Interquartile Range (IQR) method
# Why: The IQR method is a robust statistical way to define outliers.
# An outlier is typically a value that falls below Q1 - 1.5*IQR or above Q3 + 1.5*IQR.

Q1=quantile(df_clean$sales,0.25)
Q3=quantile(df_clean$sales,0.75)
IQR_value=IQR(df_clean$sales)
lower_bound<-Q1-1.5*IQR_value
upper_bound<-Q3+1.5*IQR_value

# Filter out the outliers
# Note: In a real analysis, you might choose to cap them or analyze them separately,in this analysis, we are including them as we are unaware of the reason for outliers.

##Cleaned and Prepared Data frame
final_df <- df_clean
glimpse(final_df)

# 5. Save the Cleaned Data 
# Why: This saves our cleaned data to a new file. It's a best practice that
# saves you from re-running the entire cleaning script every time you want to analyze the data.
write.csv(final_df, "/home/creation/Project/Sales Analysis Project/sales_data/sales_data_cleaned.csv", row.names = FALSE)


# 6. Calculation, Analysis & Visualization
# Get a quick overview of the entire dataset
# Why: `summary()` provides key descriptive stats (mean, median, quartiles) for numeric
# columns and frequency counts for factor columns.
summary(final_df)

## Distribution of Sales Amounts
# Why: To understand the most common sales values and the shape of the distribution.

ggplot(final_df,aes(x=sales))+
  geom_histogram(bins=30,fill="cornflowerblue",color="white")+
  labs(title="Distribution of Sales",x="Sales Amount",y="frequency")+
  theme_classic()

## Store Type Performance
# Why: To identify which store types generate the most revenue on average.

final_df %>% 
  group_by(store_type) %>% 
  summarise(
    average_sales=mean(sales),
    total_sales=sum(sales)
  ) %>% 
  ggplot(aes(x=reorder(store_type,-average_sales),y=average_sales,fill=store_type))+
  geom_bar(stat = "identity")+
  labs(title="Average sales by Store Type",x="Store_Type",y="Average Sales")

## Regional Performance Analysis
# Why: To see which regions are the most profitable.
final_df %>%
  group_by(region_code) %>% 
  summarise(
    average_sales=mean(sales),
    total_sales=sum(sales)
  ) %>% 
  ggplot(aes(x=reorder(region_code,-average_sales),y=average_sales,fill=region_code))+
  geom_bar(stat="identity")+
  labs(title="Average Sales by Region Code",x="Region Code", y="Average Sales")

## Impact of Discounts on Sales
# Why: To visually check if offering a discount corresponds to higher sales.
ggplot(final_df,aes(x=discount,y=sales,fill = discount))+
geom_boxplot()+
labs(title="Sales Performance: Discount vs. No Discount", x = "Discount Offered", y = "Sales Amount")

# 7. Statistical Analysis
## ANOVA to compare sales across store types
# Why: A boxplot shows a difference, but ANOVA tells us if that difference is
# statistically significant.
# Null Hypothesis (H0): The average sales are equal across all store types.
anova_result<-aov(sales~store_type,data = final_df)
summary(anova_result)

# Interpretation: Look at the p-value (Pr(>F)). If it's very small (e.g., < 0.05),
# we reject the null hypothesis and conclude that there is a significant difference
# in sales between at least two of the store types.

## Correlation between Orders and Sales
# Why: To quantify the strength and direction of the linear relationship between the
# number of orders and the total sales amount. We expect a strong positive correlation.
correlation<-cor(final_df$no_of_orders,final_df$sales)
cat("Correlation between Orders and Sales:", correlation, "\n")

# Visualize this relationship with a scatter plot
ggplot(final_df, aes(x = no_of_orders, y = sales)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Adds a linear regression line
  labs(title = "Sales vs. Number of Orders", x = "Number of Orders", y = "Sales Amount")

## Hypothesis Testing for Discount Effectiveness (T-test)
# Why: To statistically determine if offering a discount leads to a significant
# increase in average sales.
# Null Hypothesis (H0): The mean sales for discounted and non-discounted days are equal.
t_test_result <- t.test(sales ~ discount, data = final_df)
print(t_test_result)
# Interpretation: Again, look at the p-value. A small p-value (< 0.05) suggests that the observed difference in average sales is not due to random chance, and the discount has a significant effect.

#Advanced Analysis - Sales Prediction Modeling
## Multiple Linear Regression
# Why: To build a simple model that predicts sales based on multiple factors.
# This helps us understand the individual contribution of each factor.
regression_model <- lm(sales ~ no_of_orders + store_type + region_code + discount, data = final_df)
summary(regression_model)
# Interpretation:
# - Coefficients: Show the estimated change in sales for a one-unit change in a predictor, holding others constant.
# - R-squared: Tells you what percentage of the variation in sales is explained by your model. A higher value is better.
# - p-values (Pr(>|t|)): Indicate the significance of each predictor.


# 8. Machine Learning and Feature Engineering
# Step 1: Reverifying and Correcting data types
final_df <- final_df %>%
  mutate(
    date = as.Date(date),
    store_id = as.factor(store_id),
    store_type = as.factor(store_type),
    location_type = as.factor(location_type),
    region_code = as.factor(region_code),
    holiday = as.factor(holiday),
    discount = as.factor(discount)
  )
cat("--- Data loaded successfully. Initial structure: ---\n")
glimpse(final_df)

#Step 2: Efficient Feature Engineering
# We are strategically removing 'week_of_year' to prevent the memory explosion.
# Its predictive power is mostly captured by the 'month' feature and is not worth the computational cost.

df_model_ready <- final_df %>%
  arrange(store_id, date) %>%
  group_by(store_id) %>%
  mutate(
    # Time-based features
    year = as.factor(year(date)),
    month = as.factor(month(date, label = TRUE)),
    day_of_week = as.factor(wday(date, label = TRUE, week_start = 1)),
    
    # Powerful time-series features
    sales_lag_1 = lag(sales, 1),
    sales_roll_mean_7 = roll_mean(sales, n = 7, align = "right", fill = NA)
  ) %>%
  ungroup() %>%
  na.omit() %>% # Remove rows with NAs created by lag/roll features
  select(
    sales, no_of_orders, store_type, location_type, region_code,
    holiday, discount, year, month, day_of_week,
    sales_lag_1, sales_roll_mean_7
  )

cat("\n--- Data structure after EFFICIENT feature engineering ---\n")
glimpse(df_model_ready)

# Step 3: Robust Model Training
# Split the FULL dataset into Training and Testing sets
set.seed(123)
train_indices <- createDataPartition(
  y = df_model_ready$sales,
  p = 0.8,
  list = FALSE
)
train_data <- df_model_ready[train_indices, ]
test_data  <- df_model_ready[-train_indices, ]

# Step 4.1: Strategy 1: Strategic Sampling
# --- STRATEGY 1: STRATEGIC SUBSAMPLING ---
# This is the KEY step to prevent the freeze. We train on a smaller sample.
# 40,000 rows is more than enough to build an excellent model.
set.seed(123)
train_data_sample <- train_data %>% sample_n(40000)

cat("\nOriginal training data rows:", nrow(train_data), "\n")
cat("Rows in the sample we will use for training:", nrow(train_data_sample), "\n\n")

# Step 4.2: Strategy 2: Setup Parallel Processing
# This uses multiple CPU cores on your server to make training much faster.
cores_to_use <- detectCores() - 1 # Leave one core free for system processes
cl <- makePSOCKcluster(cores_to_use)
registerDoParallel(cl)

cat("--- Registered", cores_to_use, "cores for parallel processing. Starting model training. ---\n")

# Step 4.3: Define Training Control for Cross-Validation 
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE, # Shows progress
  allowParallel = TRUE # This tells caret to use our parallel setup
)

#Step 5: Train the XGBoost Model using smaller train data sample
# We use a simplified tuning grid to find good parameters quickly.
cat("\n--- Training and Tuning XGBoost Model ---\n")
xgb_tune_grid_fast <- expand.grid(
  nrounds = c(150, 250),
  max_depth = 6,
  eta = 0.05,
  gamma = 0,
  colsample_bytree = 0.7,
  min_child_weight = 1,
  subsample = 0.7
)

set.seed(123)

# We train on the SAMPLE data to ensure it completes successfully
xgb_tuned_model <- train(
  sales ~ .,
  data = train_data_sample, 
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_tune_grid_fast,
  verbose = FALSE
)

cat("\n--- XGBoost Tuning Complete ---\n")
print(xgb_tuned_model)

# --- IMPORTANT: Stop the Parallel Cluster ---
# Always do this after training is complete to release the CPU cores.
stopCluster(cl)
registerDoSEQ() # Unregister the parallel backend
cat("\n--- Parallel cluster stopped. ---\n")

#Step 6: Final Model Evaluation
# We evaluate the trained model on the FULL, unseen test set to get a true measure of performance.
cat("\n--- Final Model Evaluation on the Full, Unseen Test Set ---\n")
predictions_xgb <- predict(xgb_tuned_model, newdata = test_data)

# Calculate performance metrics
performance_metrics <- data.frame(
  RMSE = RMSE(predictions_xgb, test_data$sales),
  MAE = MAE(predictions_xgb, test_data$sales),
  Rsquared = R2(predictions_xgb, test_data$sales)
)
print(round(performance_metrics, 4))

# Visualize Actual vs. Predicted values for our model
results_df <- data.frame(actual = test_data$sales, predicted = predictions_xgb)

ggplot(results_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.2, color = "darkorange") +
  geom_abline(color = "blue", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs. Predicted Sales (Tuned XGBoost Model)",
    subtitle = paste("Test Set R-squared:", round(performance_metrics$Rsquared, 3)),
    x = "Actual Sales",
    y = "Predicted Sales"
  ) +
  theme_bw()

#Step 7 What if Scenarios
# Now we use our best model (xgb_tuned_model) as a business simulator.

# Analyze Feature Importance
# This tells us WHICH variables are the most powerful drivers of sales.
importance <- varImp(xgb_tuned_model, scale = FALSE)
plot(importance, top = 15, main = "Top 15 Most Important Features (XGBoost)")

# "What-If" Scenario 1: What is the impact of increasing order count?
# Define a typical scenario. We need to provide values for ALL features in the model.
base_scenario <- list(
  no_of_orders = 70, # This is the variable we will change
  store_type = "S1",
  location_type = "L1",
  region_code = "R1",
  holiday = "No",
  discount = "Yes",
  year = "2018", # Use a year present in your data
  month = "Jul",
  day_of_week = "Mon",
  sales_lag_1 = 45000,
  sales_roll_mean_7 = 48000
)

# Create a sequence of order counts to test
order_counts_to_test <- seq(from = 40, to = 150, by = 5)
predicted_sales_list <- c()

for (orders in order_counts_to_test) {
  current_scenario <- base_scenario
  current_scenario$no_of_orders <- orders
  
  # The `predict` function from caret handles all the data formatting for us!
  predicted_value <- predict(xgb_tuned_model, newdata = as.data.frame(current_scenario))
  predicted_sales_list <- c(predicted_sales_list, predicted_value)
}

scenario1_results <- data.frame(orders = order_counts_to_test, predicted_sales = predicted_sales_list)

# Plot the results of the simulation
ggplot(scenario1_results, aes(x = orders, y = predicted_sales)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "What-if Analysis: Impact of Order Count on Sales",
    subtitle = "Holding all other factors constant for a sample store",
    x = "Number of Orders",
    y = "Predicted Sales Amount"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# "What-If" Scenario 2: How effective are discounts across different store types? ---
# We use `expand.grid` to create every combination of store_type and discount to test
scenario2_df <- expand.grid(
  store_type = c("S1", "S2", "S3", "S4"), # Test all store types in your data
  discount = c("No", "Yes")
)

# Add the other constant information to our scenarios dataframe
scenario2_df <- scenario2_df %>%
  mutate(
    no_of_orders = 80, # Assume a fixed 80 orders for a fair comparison
    location_type = "L1",
    region_code = "R1",
    holiday = "No",
    year = "2018",
    month = "Jul",
    day_of_week = "Mon",
    sales_lag_1 = 45000,
    sales_roll_mean_7 = 48000
  )

# Get a prediction for each scenario (each row in the dataframe)
scenario2_df$predicted_sales <- predict(xgb_tuned_model, newdata = scenario2_df)

# Plot the results as a grouped bar chart for easy comparison
ggplot(scenario2_df, aes(x = store_type, y = predicted_sales, fill = discount)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = scales::dollar(predicted_sales, accuracy = 1)),
    position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5
  ) +
  labs(
    title = "What-if: Discount Effectiveness Across Store Types",
    subtitle = "Assuming a fixed 80 orders per day",
    x = "Store Type",
    y = "Predicted Sales Amount",
    fill = "Discount Offered?"
  ) +
  scale_fill_manual(values = c("No" = "#E57373", "Yes" = "#64B5F6")) +
  theme_minimal()