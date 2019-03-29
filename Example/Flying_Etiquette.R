library(tidyverse)
library(scales)
setwd("~/Downloads")

# Import CSV File of Flying Etiquette Survey from 538
flying_etiquette <- read_csv("flying-etiquette.csv")

#--- Changing Characters to Factors ---#
flying_etiquette %>%
  # Change characters to factors
  mutate_if(is.character, as.factor) %>%
  # Filter out those who have never flown on a plane
  filter(`How often do you travel by plane?` != "Never")

#--- Tidying Data ---#
gathered_data <- flying_etiquette %>%
  mutate_if(is.character, as.factor) %>%
  filter(`How often do you travel by plane?` != "Never") %>%
  # Select columns containing "rude"
  select(contains("rude")) %>%
  # Change format from wide to long
  gather("response_var", "value")

#--- Regular Expression ---#
gathered_data %>%
  # Remove everything before and including "rude to "
  mutate(response_var = str_remove(response_var, ".*rude to ")) %>%
  # Remove "on a plane"
  mutate(response_var = str_remove(response_var, "on a plane"))

#--- Dichotomizing Variables ---#
dichotimized_data <- gathered_data %>%
  mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
  mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
  # Remove rows that are NA in the value column
  filter(!is.na(value)) %>%
  # Dichotomize the value variable to make a new variable, rude
  mutate(rude = if_else(value %in% c('No, not rude at all', 'No, not at all rude'), 0, 1))

#--- Summarize the Data by Rude Behaviors ---#
rude_behaviors <- gathered_data %>%
  mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
  mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
  # Remove rows that are NA in the value column
  filter(!is.na(value)) %>%
  mutate(rude = if_else(value %in% c("No, not rude at all", "No, not at all rude"), 0, 1)) %>%
  # Create perc_rude, the percent considering each behavior rude
  group_by(response_var) %>%
  summarize(perc_rude = mean(rude))

rude_behaviors

#--- Plot ---#
initial_plot <- rude_behaviors %>%
  # reorder response_var by perc_rude
  mutate(response_var = fct_reorder(response_var, perc_rude)) %>%
  # make a bar plot of perc_rude by response_var
  ggplot(aes(x = response_var, y = perc_rude)) + 
  geom_col()

# View your plot
initial_plot

#--- Fixing the Plot ---#
titled_plot <- initial_plot + 
  # Add the title, subtitle, and caption
  labs(title = "Hell Is Other People In A Pressurized Metal Tube",
       subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
       caption = "Source: SurveyMonkey Audience", 
       # Remove the x- and y-axis labels
       x = "", 
       y = "") 

titled_plot

flipped_plot <- titled_plot + 
  # Flip the axes
  coord_flip() + 
  # Remove the x-axis ticks and labels
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())

flipped_plot + 
  # Add labels above the bar with the perc value
  geom_text(aes(label = percent(perc_rude), 
                y = perc_rude + .03), 
            position = position_dodge(0.9),
            vjust = 1)

