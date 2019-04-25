# Import the package
library(tidyverse)
# install.packages("highcharter")
library(highcharter)
library(quantmod)
library(tibble)

import_stock_xts <- function(stock, src = 'yahoo') {
  df <- getSymbols(stock, src = src, env = NULL)
  names(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  return(df)
}

import_stock_df <- function(stock, src = 'yahoo') {
  df <- getSymbols(stock, src = src, env = NULL)
  names(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  df <- as.data.frame(df)
  df <- cbind(Date = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  df <- as.tibble(df)
  return(df)
}
aapl_xts <- import_stock_xts("AAPL")
aapl <- import_stock_df("AAPL")

# Use the base function and set the correct chart type
highchart(type = 'stock') %>%
  # Add the price data
  hc_add_series(aapl_xts)

# Create a line chart of the 'close' prices
hchart(aapl,
       # Specify the mappings
       hcaes(x = Date, y = Close),
       type = "line")

# Create a line chart of the open prices
hchart(aapl,
       # Specify the mappings and set the correct type
       hcaes(x = Date, y = Open),
       type = "line")


highchart(type = "stock") %>%
  hc_title(text = "Trends of Three Stocks") %>%
  hc_subtitle(text = "Daily Prices") %>%
  # Add JPM as a blue line called JP Morgan
  hc_add_series(stock_prices_xts$JPM, color = "blue", name = "JP Morgan") %>%
  # Add KO as a red line called Coke
  hc_add_series(stock_prices_xts$KO, color = "red", name = "Coke") %>%
  # Add DIS as a purple line named Disney
  hc_add_series(stock_prices_xts$DIS, color = "purple", name = "Disney") %>%
  hc_yAxis(title = list(text = "Price (USD)"),
           labels = list(format = "${value}"),
           opposite = FALSE) %>%
  hc_tooltip(pointFormat = 
               "{point.series.name}:
                $ {point.y: .2f}") %>%
  hc_legend(enabled = TRUE)

highchart(type = "stock") %>%
  hc_title(text = "Google vs. Amazon") %>%
  hc_subtitle(text = "Daily Prices") %>%
  # Add JPM as a blue line called JP Morgan
  hc_add_series(stock_prices_xts$GOOG, color = "darkblue", name = "Google") %>%
  # Add KO as a red line called Coke
  hc_add_series(stock_prices_xts$AMZN, color = "darkorange", name = "AMZN") %>%
  hc_yAxis(title = list(text = "Price (USD)"),
         labels = list(format = "${value}"),
         opposite = FALSE) %>%s
  hc_tooltip(pointFormat = 
               "{point.seriess.name}:
             $ {point.y: .2f}") %>%
  hc_legend(enabled = TRUE)

  
  # Import the data
  stock_wide_tibble_prices <- read_csv("stock_prices_xts.csv")
  stock_tidy_tibble_prices <- read_csv("stock_prices.csv")
  stock_tidy_tibble_returns <- read_csv("stock_returns.csv")
  
  # Visualize Multiple Stocks as line charts
  # Converting from xts data object to tibble object
  stock_wide_tibble_prices <- as.data.frame(stock_prices_xts)
  stock_wide_tibble_prices <- cbind(Date = rownames(stock_wide_tibble_prices), stock_wide_tibble_prices)
  rownames(stock_wide_tibble_prices) <- 1:nrow(stock_wide_tibble_prices)
  stock_wide_tibble_prices$Date <- as.Date(stock_wide_tibble_prices$Date, "%Y-%m-%d")
  stock_wide_tibble_prices <- as.tibble(stock_wide_tibble_prices)
  
  hchart(stock_wide_tibble_prices, hcaes(x = Date, y = DIS), 
         type = "line", 
         name = "DIS",
         color = "orange",
         showInLegend = TRUE) %>%
    hc_add_series(stock_wide_tibble_prices,
                  hcaes(x = Date, y = KO),
                  type = 'line',
                  color = "green",
                  name = "KO",
                  showInLegend = TRUE) %>%
    hc_add_series(stock_wide_tibble_prices,
                  hcaes(x = Date, y = JPM),
                  type = 'line',
                  color = "black",
                  name = "JPM",
                  showInLegend = TRUE) %>%
    hc_tooltip(shared = TRUE, 
               pointFormat = "{point.series.name}: ${point.y: .2f}<br>") %>%
    hc_yAxis(title = list(text = "prices (USD)"),
             labels = list(format = "${value}")) %>%
    hc_legend(enabled = TRUE)



chart_five_years <- function(stock, src = 'yahoo') {
    library(lubridate)
    df <- getSymbols(stock, src = src, env = NULL)
    names(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    df <- as.data.frame(df)
    df <- cbind(Date = rownames(df), df)
    rownames(df) <- 1:nrow(df)
    df$Date <- as.Date(df$Date, "%Y-%m-%d")
    df <- as.tibble(df)
    df <- df %>% filter(Date > Sys.Date() - years(5))
    hchart(df, hcaes(x = Date, y = Close), 
           type = "line", 
           name = stock,
           color = "orange") %>%
      hc_tooltip(pointFormat = "{point.series.name}: ${point.y: .2f}<br>") %>%
      hc_yAxis(title = list(text = "prices (USD)"),
               labels = list(format = "${value}"))
}

hchart(stock_wide_tibble_returns,
       hcaes(x = KO, y = AMZN),
       type =  "scatter",
       # Color the points pink
       color = "pink",
       name = "GOOG v. AMZN") %>%
  # Add a custom tooltip
  hc_tooltip(pointFormat = "{point.date} <br>
	                          AMZN: {point.y: .2f}% <br>
	                          KO: {point.x: .2f}%")


# Create a scatter plot
hchart(stock_wide_tibble_returns,    
       hcaes(x = KO, y = GOOG),
       type =  "scatter") %>%
  # Add the slope variable
  hc_add_series(stock_wide_tibble_returns,
                hcaes(x = KO, y = (KO * 1.15)),
                type =  "line") %>% 
  # Customize the tooltip to show the date, x-, and y-values 
  hc_tooltip(pointFormat = "{point.date} <br> GOOG {point.y: .2f}% <br> KO: {point.x: .2f}%")


hchart(stock_wide_tibble_returns,    
       hcaes(x = AMZN, y = DIS),
       type =  "scatter") %>%
  hc_add_series(stock_wide_tibble_returns,
                hcaes(x = AMZN, y = (AMZN * .492)),
                type =  "line",
                # Add the tooltip argument
                tooltip = list(
                  # Change the header of the line tooltip
                  headerFormat = "DIS/AMZN linear relationship<br>",
                  # Customize the y value display
                  pointFormat = "{point.y: .2f}%")) %>%
  # Customize the scatter tooltip
  hc_tooltip(pointFormat = "{point.date} <br> DIS: {point.y}% <br> AMZN: {point.x}%")


stock_tidy_tibble_prices %>%
  filter(symbol == "KO") %>%
  hchart(.,
         hcaes(x = date, y = price),
         type = 'line',
         color = 'darkred')

stock_tidy_tibble_prices %>%
  filter(symbol %in% c("GOOG", "AMZN")) %>%
  # Pass in the data
  hchart(.,
         # Define the aesthetic mappings
         hcaes(x = date, y = price, group = symbol),
         type = "line") %>%
  # Title the chart
  hc_title(text = "Daily Prices from Tidy Tibble") %>% 
  # Customize the y-axis and move the labels to the left
  hc_yAxis(title = list(text = "Prices (USD)"),
           labels = list(format = "${value}"),
           opposite = FALSE)


stock_tidy_tibble_returns <- stock_wide_tibble_returns %>% gather("symbol", "returns", -date)
stock_tidy_tibble_returns <- stock_tidy_tibble_returns[!is.na(stock_tidy_tibble_returns$returns),]

stock_tidy_tibble_returns %>%
  group_by(symbol) %>%
  # Calculate the standard deviation and mean of returns
  summarize(std_dev = sd(returns), 
            mean = mean(returns)) %>%
  hchart(., 
         hcaes(x = symbol, 
               # Make the height of each point the standard deviation
               y = std_dev, 
               # Color each point by symbol
               color = symbol,
               # Make the size of each point the mean return
               size = mean,
               group = symbol),
         type = "scatter") %>% 
  hc_title(text = "Standard Dev and Mean Return")


stock_tidy_tibble_returns %>%
  group_by(symbol) %>%
  summarize(avg_returns = mean(returns),
            risk = sd(returns),
            risk_return = risk/avg_returns) %>%
  # Pass the summary statistics to hchart
  hchart(., 
         # Map the stocks and the risk/return ratio
         hcaes(x = symbol, 
               y = risk_return, 
               group = symbol,
               # Color by symbol
               color = symbol),
         # Create a column plot
         type = "column") %>% 
  hc_title(text = "Risk/Return") %>% 
  hc_subtitle(text = "lower bars are better")

stock_tidy_tibble_prices %>%
  mutate(sector = case_when(symbol == "AMZN" ~ "tech", 
                            symbol == "GOOG" ~ "tech",
                            symbol == "DIS" ~ "fun",
                            symbol == "JPM" ~ "bank",
                            symbol == "KO" ~ "food")) %>%
  # Pass the daily price data to hchart
  hchart(.,
         # Map date, price and symbol aesthetics
         hcaes(x = date, y = price, group = symbol),
         type = "line") %>%
  # Set the tooltip display with curly braces
  hc_tooltip(pointFormat = "{point.symbol}: ${point.price: .2f}<br> sector: {point.sector}")

# Calculate the mean, sd, max and min returns
stock_tidy_tibble_returns %>% 
  group_by(symbol) %>%
  summarize(mean = mean(returns),
            st_dev = sd(returns),
            max_return = max(returns),
            min_return = min(returns)) %>%
  # Pass the summarized to data to hchart()
  hchart(.,
         # Map the data to the axes and group
         hcaes(x = symbol, y = st_dev, group = symbol),
         type = "column") %>% 
  # Customize the tooltip to show mean, max and minimum daily returns
  hc_tooltip(pointFormat = "mean: {point.mean: .4f}% <br> 
             max: {point.max_return: .4f}% <br> 
             min: {point.min_return: .4f}%")