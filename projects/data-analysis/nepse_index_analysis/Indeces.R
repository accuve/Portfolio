#Par A: LOADING REQUIRED LIBRARY

## 1. Loading Library
library(forecast)
library(tseries)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(visdat)
library(prophet)


# Part B: DATA CLEANING & DATA PREPARATION

## 1. Setting Data File Path
file_path<-"/home/creation/Project/Machine Learning/Project File/Indeces.csv"

## 2. Read indeces.csv data using readr library
index_data<-read.csv(file_path)

## 3. Renaming Column
names(index_data)<-c("SN","Date","IndexValue","AbsoluteChange","PercentageChange")

## 4. Identify and List Duplicate Rows
duplicate_rows <- index_data %>%
  group_by(Date) %>%
  filter(n() > 1) %>%
  ungroup() %>% # Ungroup for clean output
  arrange(Date)   # Sort by date to see duplicates together

## 5. Handling Duplicates and Sorting
original_rows <- nrow(index_data)
### Storing original row count to show how many duplicates were removed

index_data <- index_data %>%
  distinct(Date, .keep_all = TRUE)
###Remove any duplicate rows based on the 'Date' column, keeping the first instance

index_data <- index_data %>%
  arrange(ymd(Date))
###Sort the data chronologically, which is essential for time series analysis

## 6. Handling Missing Data using visdat library
vis_miss(index_data)
### Visdat shows 100% data

## 7. Listing Missing Data
missing_values <- colSums(is.na(index_data))
print(missing_values)
###There is no missing values in all the columns

## 8. Correcting Data formats using lubridate library
index_data$Date <- as.Date(index_data$Date, format = "%Y/%m/%d")
index_data$PercentageChange<-as.numeric(sub("%","",index_data$PercentageChange))
index_data$IndexValue <- as.numeric(index_data$IndexValue)
index_data$AbsoluteChange <- as.numeric(index_data$AbsoluteChange)
index_data$SN <- as.integer(index_data$SN) #Just for fun not required though

## 9. Remove Data with NA value
index_data <- index_data %>%
  filter(!is.na(IndexValue) & !is.na(Date))
### Final check to remove any rows that might have NA in IndexValue after conversion

## 10. Indentifying Outliers using ggplot2 library
ggplot(index_data,aes(y=IndexValue))+
  geom_boxplot()+
  ggtitle("Boxplot of Index Value to Identify Outliers")

### No outlier identified, for current scenario as the Nepal Stock Market is
### volaties, identified outliers will help for better training of the index data

## 11. Display the cleaned data's structure and a summary
str(index_data)
summary(index_data)

# Part C: CREATING TIME SERIES OBJECT
## 1. Create time series object
index_ts<-ts(index_data$IndexValue, frequency=365) # Daily

## 2. Ploting Time Series data
ggplot(index_data, aes(x = Date, y = IndexValue)) +
  geom_line(color = "blue") +
  ggtitle("Index Value Over Time") +
  xlab("Year") +
  ylab("Index Value")

#PART D: TIME SERIES FORECASTING MODEL: ARIMA MODEL
### Autoregressive Integrated Moving Average (ARIMA MODEL) is a widely used
### statistical model for time series forecasting. It captures the relationships
### between an observation and its past values (autoregression), the use of
### differencing to make the series stationary (integration), and the 
### relationship between an observation and past forecast errors (moving average)

## 1. Check for stationarity
adf.test(index_ts)

### Result: Dickey-Fuller = -1.692, Lag order = 9, p-value = 0.7087
### alternative hypothesis: stationary

## 2. Fit an Arima Model
fit_arima<-auto.arima(index_ts, seasonal = TRUE)
summary(fit_arima)

## 3. Forecasting Future Values
forecast_arima<-forecast(fit_arima,h=180) ###Forecasting for next 180 periods

## 4. Plot the forecast
autoplot(forecast_arima) +
  ggtitle("ARIMA Forecast for Next 180 Trading Days") +
  xlab("Time (Observation Index)") +
  ylab("Index Value")

## 5. Create a dataframe to show the numerical forecast
arima_predictions <- data.frame(
  Trading_Day_Ahead = 1:180,
  Forecasted_Value = as.numeric(forecast_arima$mean)
)
print("--- ARIMA Model: Predicted Values for the Next 180 Trading Days ---")
print(arima_predictions)

# PART E: ALTERNATIVE FORECASTING MODEL: PROPHET
## 1. Prepare data for Prophet
prophet_data <- index_data %>%
  select(Date, IndexValue) %>%
  rename(ds = Date, y = IndexValue)
## 2. Fit the Prophet model
prophet_model <- prophet(prophet_data, daily.seasonality = TRUE)

## 3. Create a future dataframe for predictions
future_dates <- make_future_dataframe(prophet_model, periods = 180)

## 4. Generate forecasts
forecast_prophet <- predict(prophet_model, future_dates)

## 5. Plot the Prophet forecast
print("Plotting Prophet forecast...")
plot(prophet_model, forecast_prophet) +
  ggtitle("Prophet Forecast for Next 180 Calendar Days") +
  xlab("Date") +
  ylab("Index Value")

# Plot the components
prophet_plot_components(prophet_model, forecast_prophet)

## 6. Extract and display the daily predicted values
prophet_predictions <- forecast_prophet %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail(180)

print("--- Prophet Model: Predicted Values for the Next 180 Calendar Days ---")
print(prophet_predictions)