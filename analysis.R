#R file
library(dplyr)
library(ggplot2)
library(coinmarketcapr)
library(purrr)
library(ggthemes)

# Set API key for CoinMarketCap
apikey <- "ed1842ab-58c3-4cc1-a11c-8b3fa6e10b80"
coinmarketcapr::setup(apikey)

# Get top 20 cryptocurrencies by market cap
crypto_list <- get_crypto_listings(limit = 20)

# Calculate market dominance percentages for the top 3 cryptocurrencies
top3_dominance <- crypto_list %>% 
  slice(1:3) %>% 
  mutate(market_cap_percentage = USD_market_cap_dominance / sum(USD_market_cap_dominance) * 100) %>% 
  select(symbol, market_cap_percentage)

# Calculate market dominance percentages for all other cryptocurrencies
others_dominance <- crypto_list %>% 
  slice(4:n()) %>% 
  summarize(market_cap_percentage = sum(USD_market_cap_dominance) / sum(crypto_list$USD_market_cap_dominance) * 100) %>% 
  mutate(symbol = "Others")

# Combine the top 3 and "Others" data
market_dominance <- bind_rows(top3_dominance, others_dominance)

# Create a pie chart of the market dominance data
ggplot(market_dominance, aes(x = "", y = market_cap_percentage, fill = symbol)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("#F7931A", "darkblue", "darkred", "darkgreen")) +
  labs(title = "Market Dominance of Cryptocurrencies",
       fill = "Cryptocurrency",
       x = NULL,
       y = NULL) +
  geom_text(aes(label = paste(round(market_cap_percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3, fontface = "italic")

# Load crypto2 library
library(crypto2)

# Get historical data for BTC, ETH, DOGE and XRP
coins <- c("BTC", "ETH", "DOGE", "XRP")
coin_hist <- crypto_history(coins, limit=4, start_date="20220101", end_date="20230331", finalWait=FALSE) #15months

coin_hist$date <- as.Date(coin_hist$timestamp)

# View data
head(coin_hist)

# Load crypto2 library

BTC_hist <- filter(coin_hist, id == 1)
ETH_hist <- filter(coin_hist, id == 1027)
XRP_hist <- filter(coin_hist, id == 52)
DOGE_hist <- filter(coin_hist, id == 74)

ggplot(coin_hist, aes(x = date, y = close, color = name)) +
  geom_line(size = 0.5) +  # set line thickness to 1.5
  scale_y_log10() +  # set y-axis to log scale
  ggtitle("Historical Prices of BTC, ETH, and XRP (LOG Scale)") +
  labs(x = "Date", y = "Closing Price (USD) [log]") +
  theme_minimal() +  # apply a minimalist theme
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# Create line graph of closing prices over time for Bitcoin
ggplot(BTC_hist, aes(x = date, y = close)) +
  geom_line(size = 0.5, color = "orange") +
  ggtitle("Historical Prices of Bitcoin") +
  xlab("Date") +
  ylab("Closing Price (USD)") +
  theme_minimal()

# Create line graph of closing prices over time for Ethereum
ggplot(ETH_hist, aes(x = date, y = close)) +
  geom_line(size = 0.5, color = "blue") +
  ggtitle("Historical Prices of Ethereum") +
  xlab("Date") +
  ylab("Closing Price (USD)") +
  theme_minimal()

# Create line graph of closing prices over time for Ripple
ggplot(XRP_hist, aes(x = date, y = close)) +
  geom_line(size = 0.5, color = "darkgreen") +
  ggtitle("Historical Prices of Ripple") +
  xlab("Date") +
  ylab("Closing Price (USD)") +
  theme```r
_minimal()

# Create line graph of closing prices over time for Dogecoin
ggplot(DOGE_hist, aes(x = date, y = close)) +
  geom_line(size = 0.5, color = "red") +
  ggtitle("Historical Prices of Dogecoin") +
  xlab("Date") +
  ylab("Closing Price (USD)") +
  theme_minimal()

# Combine the four boxplots into one
library(tidyr)
# Calculate daily percentage change in closing prices for each coin
BTC_pct_change <- c(NA, diff(BTC_hist$close)/BTC_hist$close[-nrow(BTC_hist)])
ETH_pct_change <- c(NA, diff(ETH_hist$close)/ETH_hist$close[-nrow(ETH_hist)])
XRP_pct_change <- c(NA, diff(XRP_hist$close)/XRP_hist$close[-nrow(XRP_hist)])
DOGE_pct_change <- c(NA, diff(DOGE_hist$close)/DOGE_hist$close[-nrow(DOGE_hist)])

# Combine percentage change data into a single data frame
pct_change <- data.frame(date = BTC_hist$date,
                         BTC = BTC_pct_change,
                         ETH = ETH_pct_change,
                         XRP = XRP_pct_change,
                         DOGE = DOGE_pct_change)

# Reshape the data frame from wide to long format
pct_change_long <- pivot_longer(pct_change, cols = -date, names_to = "coin", values_to = "pct_change")

# Create a single boxplot of daily percentage change in closing prices for each coin
ggplot(pct_change_long, aes(x = coin, y = pct_change, fill = coin)) +
  geom_boxplot() +
  ggtitle("Volatility of Cryptocurrencies(Daily Closing prices)") +
  xlab("Coin") +
  ylab("Daily Percentage Change") +
  scale_fill_manual(values = c("orange", "blue", "green", "red")) +
  theme_minimal() + coord_flip()

# Calculate daily volatility using high and low prices for each coin
BTC_volatility <- (BTC_hist$high - BTC_hist$low) / BTC_hist$low
ETH_volatility <- (ETH_hist$high - ETH_hist$low) / ETH_hist$low
XRP_volatility <- (XRP_hist$high - XRP_hist$low) / XRP_hist$low
DOGE_volatility <- (DOGE_hist$high - DOGE_hist$low) / DOGE_hist$low

# Combine volatility data into a single data frame
volatility <- data.frame(date = BTC_hist$date,
                         BTC = BTC_volatility,
                         ETH = ETH_volatility,
                         XRP = XRP_volatility,
                         DOGE = DOGE_volatility)

# Reshape data for plotting
volatility_long <- pivot_longer(volatility, cols = -date, names_to = "coin", values_to = "volatility")

# Create a single plot with separate panels for each coin
ggplot(volatility_long, aes(x = date, y = volatility, color = coin)) +
  geom_line(size = 0.5) +
  facet_wrap(~coin, ncol = 2) +
  xlab("Date") +
  ylab("Daily Volatility") +
  ggtitle("Daily Volatility of Cryptocurrencies (Using daily Highs and Lows)") +
  theme_minimal() +
  scale_color_manual(values = c("orange", "blue", "green", "red"))

# Join the data by timestamp
coin_join <- full_join(BTC_hist, ETH_hist, by = "timestamp", suffix = c("_BTC", "_ETH")) %>%
  full_join(XRP_hist, by = "timestamp") %>%
  full_join(DOGE_hist, by = "timestamp", suffix = c("_XRP", "_DOGE"))

# Select only the closing prices and rename the columns
coin_close <- coin_join %>%
  select(timestamp, close_BTC, close_ETH, close_XRP, close_DOGE) %>%
  rename(BTC = close_BTC, ETH = close_ETH, XRP = close_XRP, DOGE = close_DOGE)

# Calculate the correlation matrix
coin_corr <- cor(coin_close[-1])

# Print the correlation matrix
print(coin_corr)

library(reshape2)
# Melt the correlation matrix for ggplot
coin_corr_melt <- melt(coin_corr)

# Plot the heatmap
ggplot(coin_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "darkred", midpoint = 0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 panel.grid = element_blank()) +
  labs(title = "Cryptocurrency Correlations (Daily Closing Price)",
       x = "Cryptocurrency",
       y = "Cryptocurrency",
       fill = "Correlation") +
  coord_fixed()

library(prophet)
# Filter the training data from 20220101 to 20221231
BTC_train <- BTC_hist %>% filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-12-31"))

# Filter the test data from 20230101 to 20230331
BTC_test <- BTC_hist %>% filter(date >= as.Date("2023-01-01") & date <= as.Date("2023-03-31"))

# Prepare the dataset for Prophet (rename columns to 'ds' and 'y')
BTC_prophet <- BTC_train %>% select(date, close) %>% rename(ds = date, y = close)

# Create a Prophet model
model <- prophet(BTC_prophet)
# Make predictions for the next 90 days (3 months)
future_dates <- make_future_dataframe(model, periods = 90)
forecast <- predict(model, future_dates)

# Combine the test data with the predictions
BTC_test_pred <- BTC_test %>% select(date, close) %>% left_join(forecast %>% select(ds, yhat), by = c("date" = "ds"))

# Calculate the Mean Absolute Percentage Error (MAPE)
MAPE <- mean(abs((BTC_test_pred$close - BTC_test_pred$yhat) / BTC_test_pred$close)) * 100
print(paste("MAPE:", MAPE))

# Convert the date column to POSIXct format
BTC_test_pred$date <- as.POSIXct(BTC_test_pred$date)

# Plot the actual data and forecast
plot(model, forecast) +
  geom_point(aes(x = date, y = close), data = BTC_test_pred, color = "red") +
  theme_minimal() +
  labs(title = "Bitcoin Price Forecast with Prophet 2022-2023",
       subtitle = paste("MAPE =", round(MAPE, 2), "%"))

# Plot the forecast components
prophet_plot_components(model, forecast)

# Continue with more coin data handling and model predictions
coins2 <- crypto_list(only_active=TRUE) |> filter(symbol %in% c("BTC"))
BTC_histPro <- crypto_history(coins2, limit=1, start_date="20140101", end_date="20230331", finalWait=FALSE) #15months
BTC_histPro$date <- as.Date(BTC_histPro$timestamp)

# Filter the training data from 2014-01-01 to 2020-12-31
BTC_train2 <- BTC_histPro %>% filter(date >= as.Date("2014-01-01") & date <= as.Date("2020-12-31"))

# Filter the test data from 2021-01-01 to 2023-03-31
BTC_test2 <- BTC_histPro %>% filter(date >= as.Date("2021-01-01") & date <= as.Date("2023-03-31"))

# Prepare the dataset for Prophet (rename columns to 'ds' and 'y')
BTC_prophet2 <- BTC_train2 %>% select(date, close) %>% rename(ds = date, y = close)

# Create a Prophet model
model2 <- prophet(BTC_prophet2)
# Make predictions for the next 820 days
future_dates2 <- make_future_dataframe(model2, periods = 820)
forecast2 <- predict(model2, future_dates2)

# Combine the test data with the predictions
BTC_test_pred2 <- BTC_test2 %>% select(date, close) %>% left_join(forecast2 %>% select(ds, yhat), by = c("date" = "ds"))

# Calculate the Mean Absolute Percentage Error (MAPE)
MAPE2 <- mean(abs((BTC_test_pred2$close - BTC_test_pred2$yhat) / BTC_test_pred2$close)) * 100
print(paste("MAPE:", MAPE2))

# Convert the date column to POSIXct format
BTC_test_pred2$date <- as.POSIXct(BTC_test_pred2$date)

# Plot the actual data and forecast
plot(model2, forecast2) +
  geom_point(aes(x = date, y = close), data = BTC_test_pred2, color = "red") +
  theme_minimal() +
  labs(title = "Bitcoin Price Forecast with Prophet 2014-2023",
       subtitle = paste("MAPE =", round(MAPE2, 2), "%"))

# Plot the forecast components
prophet_plot_components(model2, forecast2)
