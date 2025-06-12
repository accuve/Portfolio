#required library
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(xts)
library(quantmod)
library(plotly)
library(tseries)
library(forecast)
library(rugarch)
library(readr)
library(visdat)
library(TTR)
library(DescTools)
library(randomForest)
library(PerformanceAnalytics)

#Data Import
bac_stock_original<-read_csv("/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/Github/2. Projects/portfolio/projects/data-analysis/bac-stock/dataset_by_muhammad_atif_latif_kaggle/BAC_1978-03-01_2025-04-17.csv")

View(bac_stock_original)

str(bac_stock_original)

#Data Preparation
##Date Formating
bac_stock_str_changed<-bac_stock_original %>% 
  mutate(
    date=as.Date(substr(date,1,10)), #Convert to date, strip timezone
    open=as.numeric(open),
    high=as.numeric(high),
    low=as.numeric(low),
    close=as.numeric(close)
  )

View(bac_stock_str_changed)

str(bac_stock_str_changed)

##Data Quality Check
###Missing Values
vis_miss(bac_stock_str_changed)

####Converting all 0 value in 'open' column to NA
bac_stock_str_changed_na<-bac_stock_str_changed %>% 
  mutate(open=na_if(open,0))

View(bac_stock_str_changed_na)

vis_miss(bac_stock_str_changed_na)
#####Reason: Zeros in open are not valid prices - treating them as missing data

####Impute missing NA values using LOCF (Last Observation Carried Forward, closing value of previous day used to impute missing opening of next day instead of opening value of previous day)
bac_stock_str_changed_na_impute_locf<-bac_stock_str_changed_na %>% 
  arrange(date) %>% #sorting date in ascending order
  mutate(open = na.locf(open, na.rm = FALSE))

View(bac_stock_str_changed_na_impute_locf)

bac_stock_str_changed_na_impute_locf$open <- na.approx(bac_stock_str_changed_na_impute_locf$open, rule=2)
View(bac_stock_str_changed_na_impute_locf)

####filtering NA in open column to see the frequency of occurrence post mutate
filtered_open_na<-bac_stock_str_changed_na_impute_locf %>% 
  filter(is.na(open))

View(filtered_open_na)

####Missing Value
vis_miss(bac_stock_str_changed_na_impute_locf)

#####Remarks on output: Only first date opening amount is NA which is acceptable

###Duplicate Dates
duplicated_bac_stock_str_changed_na_impute_locf<-bac_stock_str_changed_na_impute_locf[duplicated(bac_stock_str_changed_na_impute_locf), ]

View(duplicated_bac_stock_str_changed_na_impute_locf)

####Remarks on output: no duplicate value

###Outlier detection & handling
Q1<-quantile(bac_stock_str_changed_na_impute_locf$close,0.25,na.rm= TRUE)
View(Q1)
Q3<-quantile(bac_stock_str_changed_na_impute_locf$close,0.75,na.rm= TRUE)
View(Q3)

IQR_val<-Q3-Q1

bac_stock_str_changed_na_impute_locf_outliers<-bac_stock_str_changed_na_impute_locf %>% 
  filter(close>=(Q1-1.5*IQR_val) & close<=(Q3+1.5*IQR_val))
         
View(bac_stock_str_changed_na_impute_locf_outliers)

###Logical Consistency Check

bac_stock_str_changed_na_impute_locf_outliers_logical <- bac_stock_str_changed_na_impute_locf_outliers %>%
  filter(open >= low & open <= high & close >= low & close <= high)

View(bac_stock_str_changed_na_impute_locf_outliers_logical)

#clean_data
bac_stock_clean<-bac_stock_str_changed_na_impute_locf_outliers_logical
View(bac_stock_clean)

#Feature Engineering
##Calculate Daily Returns
bac_stock_clean_returns<-bac_stock_clean %>% 
  arrange(date) %>% 
  mutate(
    daily_return=(close-lag(close))/lag(close),
    log_return=log(close/lag(close))
  )

View(bac_stock_clean_returns)

##Price Range
bac_stock_clean_returns_price<-bac_stock_clean_returns %>% 
  mutate(price_range=high-low)

View(bac_stock_clean_returns_price)

##Volatility (Rolling Standard Deviation)
bac_stock_clean_returns_price_sd<-bac_stock_clean_returns_price %>% 
  mutate(roll_sd=rollapply(log_return,width=7,FUN=sd,fill=NA,align="right"))

View(bac_stock_clean_returns_price_sd)

##Moving Average
bac_stock_clean_returns_price_sd_ma<-bac_stock_clean_returns_price_sd %>% 
  mutate(
    moving_average_7=rollapply(close,width=7,FUN=mean,fill=NA,align="right"),
    moving_average_21=rollapply(close,width=21,FUN=mean,fill=NA,align="right")
  )
View(bac_stock_clean_returns_price_sd_ma)

##Relative Strength Index
bac_stock_clean_returns_price_sd_ma_rsi<-bac_stock_clean_returns_price_sd_ma
bac_stock_clean_returns_price_sd_ma_rsi$rsi_14<-RSI(bac_stock_clean_returns_price_sd_ma_rsi$close,n=14)

View(bac_stock_clean_returns_price_sd_ma_rsi)

####Ensures data integrity and creates derived metrics for trend analysis


#Complete Data ready for analysis
bac_stock_complete<-bac_stock_clean_returns_price_sd_ma_rsi

View(bac_stock_complete)

#Save complete data
write_csv(bac_stock_complete,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/Github/2. Projects/portfolio/projects/data-analysis/bac-stock/extracted-data-subsets/bac_stock_complete.csv")

#summary
##Mean and Median
summary_stats<-data.frame(
  Variable=c("open","high","low","close","volume"),
  mean_bac_stock_complete=c(
    mean(bac_stock_complete$open,na.rm = TRUE),
    mean(bac_stock_complete$high,na.rm = TRUE),
    mean(bac_stock_complete$low,na.rm = TRUE),
    mean(bac_stock_complete$close,na.rm = TRUE),
    mean(bac_stock_complete$volume,na.rm = TRUE)
  ),
  median_bac_stock_complete=c(
    median(bac_stock_complete$open, na.rm = TRUE),
    median(bac_stock_complete$high, na.rm = TRUE),
    median(bac_stock_complete$low, na.rm = TRUE),
    median(bac_stock_complete$close, na.rm = TRUE),
    median(bac_stock_complete$volume, na.rm = TRUE)
  )
  )

View(summary_stats)

#Time Series Analysis

##Stationarity Testing
time_series_data<-ts(bac_stock_complete$close,start = c(1978,3),frequency = 12)
adf_result<-adf.test(time_series_data)
capture.output(adf_result, file = "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/06. BAC Stock/bac-stock/adf_test_result.txt")

##Decomposition
decompose_time_series_data<-decompose(time_series_data)
plot(decompose_time_series_data)

##Correlation Analysis
acf(time_series_data)
pacf(time_series_data)
###Identifies underlying patterns and dependencies in price movements

#Visualization
##Price Trends
###Candlestick Chart
####For entire period
candleChart(as.xts(bac_stock_complete[, c("open","high","low","close")],
                   order.by = bac_stock_complete$date))


####For Last 7 days
last_60_bac_stock_complete<-tail(bac_stock_complete,60)

candleChart(as.xts(last_60_bac_stock_complete[, c("open","high","low","close")],
                   order.by = last_60_bac_stock_complete$date))
###Interactive Trendline
plotly_data<-ggplot(bac_stock_complete,aes(x=date,y=close))+
  geom_line()+
  geom_smooth(method="loess")

ggplotly(plotly_data)

##Distribution Analysis
###Histogram/density plot of returns
bac_stock_complete$daily_return<-c(NA,diff(log(bac_stock_complete$close)))

ggplot(na.omit(bac_stock_complete),aes(x=daily_return))+
  geom_histogram(binwidth = 0.01,fill="blue",alpha=0.5)+geom_density(color="red")

###Volatility Clustering
bac_stock_complete$roll_sd<-zoo::rollapply(bac_stock_complete$daily_return,width=20, FUN=sd, fill=NA)

ggplot(na.omit(bac_stock_complete),aes(x=date,y=roll_sd))+
  geom_line()
####Visualizes key patterns like trends, volatility regimes, and return distributions

#Statistical Analysis
##Volatility Modeling
returns_clean<-na.omit(bac_stock_complete$daily_return)*100
spec<-ugarchspec()
fit<-ugarchfit(spec,returns_clean)
plot(fit)

##Hypothesis Testing
hypothesis_testing<-t.test(bac_stock_complete$daily_return)
capture.output(hypothesis_testing,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/06. BAC Stock/bac-stock/hypothesis_testing.txt")
View(hypothesis_testing)

#Predictive Modeling
##ARIMA Forecasting
fit_arima<-auto.arima(time_series_data)
forecasted<-forecast(fit_arima,h=12)
plot(forecasted)

##Machine Learning
names(bac_stock_complete)
model_bac<-randomForest(close~open+high+low+volume,data=na.omit(bac_stock_complete))
predict<-predict(model_bac,na.omit(bac_stock_complete))
predict_df<-data.frame(predicted_close=predict)
View(predict_df)

predicted_df_mutate<-predict_df %>% 
  mutate(day=if_else(row_number()<=max(which(!is.na(predicted_close))),
                     row_number(),
                     as.numeric(NA))) %>%
  relocate(day,.before=1)
View(predicted_df_mutate)

ggplot(predicted_df_mutate,aes(x=day,y=predicted_close))+
  geom_line()