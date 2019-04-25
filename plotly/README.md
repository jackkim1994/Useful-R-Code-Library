# Plotly
Plotly is an alternative method of interactive data visualization using R.

We will use the election data to utilize plotly package.

``` R
library(plotly)
library(readr)
library(dplyr)
library(forcats)
```

``` R
# United States Election at November 12, 2018
turnout <- read_csv("TurnoutRates.csv")

# Create a scatterplot of turnout2018 against turnout2014
p <- turnout %>%
  plot_ly(x = ~turnout2014, y = ~turnout2018) %>%
  add_markers() %>%
  layout(xaxis = list(title = "2014 voter turnout"),
         yaxis = list(title = "2018 voter turnout"))
         
# Add the line y = x to the scatterplot
p %>%
  add_lines(x = c(0.25, 0.6), y = c(0.25, 0.6)) %>%
  layout(showlegend = FALSE)
```

![Rplot](https://user-images.githubusercontent.com/42131127/56564075-24327e00-6562-11e9-8f53-4ce31c1b7a6b.png)

The plot shows that 2018 election had more voter turnout than 2014 election. We will dive deeper into data.

``` R
# Create a dotplot of voter turnout in 2018 by state ordered by turnout
turnout %>%
  top_n(15, wt = turnout2018) %>%
  plot_ly(x = ~turnout2018, y = ~fct_reorder(state, turnout2018)) %>%
  add_markers() %>%
  layout(xaxis = list(title = "Eligible voter turnout"), 
         yaxis = list(title = "State", type = "category"))
```
![Rplot01](https://user-images.githubusercontent.com/42131127/56564201-665bbf80-6562-11e9-99e3-33f164d6d9ce.png)

### Bonus: Comparing with ggplot visualization
``` R
library(ggplot)
turnout %>%
  top_n(15, wt = turnout2018) %>%
  ggplot(aes(x  = turnout2018, y = fct_reorder(state, turnout2018))) +
  geom_point(color = "blue") +
  labs(x = "Eligible voter turnout",
       y = "State") +
  theme_minimal()
```
![Rplot02](https://user-images.githubusercontent.com/42131127/56564238-77a4cc00-6562-11e9-9621-5f2b2faeb32c.png)

Both plotly and ggplot operate in a similar way. However, ggplot does not have an interactive option to directly manipulate the plot.

``` R
# Create a histogram of receipts for the senate races
fundraising <- read_csv("fec_candidate_summary_2018.csv")
fundraising %>%
  filter(office =="S") %>%
  plot_ly(x = ~receipts) %>%
  add_histogram() %>%
  layout(title = "Fundraising for 2018 Senate races",
         xaxis = list(title = "Total contributions received"))
```
![Rplot03](https://user-images.githubusercontent.com/42131127/56564377-b9357700-6562-11e9-9296-0bf587bd0cd2.png)

``` R
# Create a dotplot of the top 15 Senate campaigns
fundraising %>%
  filter(office == "S") %>%
  top_n(15, wt = receipts) %>%
  plot_ly(x = ~receipts, y = ~fct_reorder(state, receipts),
          color = ~fct_drop(party),
          hoverinfo = "text",
          text = ~paste("Candidate:", name, "<br>",
                        "Party:", party, "<br>",
                        "Receipts:", receipts, "<br>",
                        "Disbursements:", disbursement)) %>%
  add_markers(colors = c('blue', 'red')) 
```
![Rplot04](https://user-images.githubusercontent.com/42131127/56564388-bd619480-6562-11e9-8abc-636841db66bc.png)

## Choropleth Map
Part of plotly package that creates a map visualization.

``` R
# Create a choropleth map of the change in voter turnout from 2014 to 2018
turnout %>%
  mutate(change = turnout2018 - turnout2014) %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z = ~change, locations = ~state.abbr) %>%
  layout(geo = list(scope = 'usa'))
```
![Rplot05](https://user-images.githubusercontent.com/42131127/56564671-6f00c580-6563-11e9-9949-24d64aa9087a.png)

``` R
# Create a choropleth map displaying the Senate results
senate_winners <- read_csv("senate_winners.csv")
senate_winners$name <- as.factor(senate_winners$name)
senate_winners$id <- as.factor(senate_winners$id)
senate_winners$state <- as.factor(senate_winners$state)
senate_winners$party <- as.factor(senate_winners$party)
senate_winners$incumbent <- as.factor(senate_winners$incumbent)

senate_winners %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z = ~as.numeric(party), locations = ~state,
            colors = c('dodgerblue', 'mediumseagreen', 'tomato'),
            hoverinfo = "text",
            text = ~paste("Candidate:", name, "<br>",
                          "Party:", party, "<br>",
                          "% vote:", round(pct.vote, 1))
  ) %>%
  layout(geo = list(scope = 'usa')) %>% 
  hide_colorbar()
```
![Rplot06](https://user-images.githubusercontent.com/42131127/56564717-8fc91b00-6563-11e9-9aa1-be250271ea80.png)

It seems that Deomcrats performed better during the 2018 midterm election, albeit not by a large margin.

``` R
# Join the fl_boundaries and fl_results data frames
fl_boundaries <- read_csv('fl_boundaries.csv')
fl_results <- read_csv('fl_results.csv')

senate_vote <- left_join(fl_boundaries, fl_results, by = c("subregion" = "CountyName"))

# Create a county-level choropleth map of Pctvote
senate_vote %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, 
          color = ~Pctvote,
          split = ~subregion) %>%
  add_polygons(line = list(width = 0.4), showlegend = FALSE, colors = c("blue", "red"))

# Specify the axis settings to polish the map
map_axes <- list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)

# Create a polished county-level choropleth map of Pctvote
senate_vote %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, 
          color = ~Pctvote,
          split = ~subregion) %>%
  add_polygons(line = list(width = 0.4), showlegend = FALSE, colors = c("blue", "red")) %>%
  layout(xaxis = map_axes, yaxis = map_axes)
 ```
 ![Rplot07](https://user-images.githubusercontent.com/42131127/56564852-d585e380-6563-11e9-88fc-90477c9a9e63.png)

We can notice that there are a few subregions that had less than 40% of the vote toward Republicans. Overall, Republicans have performed well in Florida.
