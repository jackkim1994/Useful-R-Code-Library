# Highcharter

Highcharter is an interactive data visualization package that is primarily useful for Financial Analysis.
A warning note that this package is not free for a **commercial/governemtnal** uses.

First, we'll import libraries
``` R
# Import the package
library(tidyverse)
# install.packages("highcharter")
library(highcharter)
library(quantmod)
library(tibble)
```

Next, we'll import that data. Note that the `.RData` File of the **stock_prices_xts**  can be automatically imported by clicking on the file.
``` R
# Import the data
  stock_wide_tibble_prices <- as.data.frame(stock_prices_xts)
  stock_tidy_tibble_prices <- read_csv("stock_prices.csv")
  stock_tidy_tibble_returns <- read_csv("stock_returns.csv")
```

I constructed my own function for teasting out the package. Note that these functions below only works for a single
stock.
``` R
# Data Type: XTS
import_stock_xts <- function(stock, src = 'yahoo') {
  df <- getSymbols(stock, src = src, env = NULL)
  names(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  return(df)
}

# Data Type: Data Frame
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
```

Next, I tested it out with **Apple (APPL)** Stock:
``` R
aapl_xts <- import_stock_xts("AAPL")
aapl <- import_stock_df("AAPL")

# Use the base function and set the correct chart type
highchart(type = 'stock') %>%
  # Add the price data
  hc_add_series(aapl_xts)
```
![Rplot](https://user-images.githubusercontent.com/42131127/56934908-f2aa3d00-6aa2-11e9-88b0-579fffbe8b5e.png)

```
# Create a line chart of the 'close' prices
hchart(aapl,
       # Specify the mappings
       hcaes(x = Date, y = Close),
       type = "line")
```
![Rplot01](https://user-images.githubusercontent.com/42131127/56934953-1ff6eb00-6aa3-11e9-8f46-f8422736fb91.png)

**Warning:** This package uses significant RAMs for each plot, so make sure your computer will be able to handle the plot without
crashing.


## More on Highcharter Utilization

We are going to tidy the data up.
``` R
  # Visualize Multiple Stocks as line charts
  # Converting from xts data object to tibble object
  stock_wide_tibble_prices <- as.data.frame(stock_prices_xts)
  stock_wide_tibble_prices <- cbind(Date = rownames(stock_wide_tibble_prices), stock_wide_tibble_prices)
  rownames(stock_wide_tibble_prices) <- 1:nrow(stock_wide_tibble_prices)
  stock_wide_tibble_prices$Date <- as.Date(stock_wide_tibble_prices$Date, "%Y-%m-%d")
  stock_wide_tibble_prices <- as.tibble(stock_wide_tibble_prices)
```

For instance, We will compare Google vs Amazon Stock
``` R
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
```
![Rplot02](https://user-images.githubusercontent.com/42131127/56936142-9ea35680-6aaa-11e9-8b90-ee1b09531df0.png)

`hc_title`, `hc_subtitle`, `hc_yAxis`, `hc_tooltip`, and `hc_legend` function are formatting functions for `highcart` and `hchart` Highcharter packages.

Another useful example is comparing the relationship between Amazon and Disney stocks.
``` R
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
```
![Rplot03](https://user-images.githubusercontent.com/42131127/56936271-67817500-6aab-11e9-97a3-e691ba375652.png)

## Comparing Standard Deviation of each stock
``` R
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
```
![Rplot04](https://user-images.githubusercontent.com/42131127/56936327-a1527b80-6aab-11e9-8a92-04df5d97cb92.png)

The size of the bubble/scatter chart is the average return of each stock.

``` R
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
```
![Rplot05](https://user-images.githubusercontent.com/42131127/56936343-cba43900-6aab-11e9-8825-214a90cd7231.png)

THe plot idea is the same with above except that it uses the bar chart.

``` R
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
```
![Rplot06](https://user-images.githubusercontent.com/42131127/56936379-f1c9d900-6aab-11e9-882e-72fea56412aa.png)

Using `hc_tooltip`, I included Mean, Maximum, and Minimum of each stock's Returns.
