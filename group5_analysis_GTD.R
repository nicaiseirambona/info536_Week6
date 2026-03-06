# Group 5: Terrorist Activity by Country

#Team Members:
#Nicaise Irambona
#Sivarajan Evya

# Global Terrorism Database Collaborative Analysis

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
gtd_data <- read.csv("globalterrorismdb_0718dist.csv", 
                     stringsAsFactors = FALSE)

# Basic data exploration
glimpse(gtd_data)

# Role 1: Group and prepare data by country

# We select the column containing country names (country_txt)
# and remove any rows that may have missing country values

country_data <- gtd_data %>%
  select(country_txt) %>%
  filter(!is.na(country_txt))

head(country_data)

# Role 2: Count the number of attacks per country

# group_by() groups the data by country
# summarise() counts how many rows (attacks) occurred in each country
# arrange() sorts the results from highest to lowest attack count

attacks_by_country <- country_data %>%
group_by(country_txt) %>%
  summarise(total_attacks = n()) %>%
  arrange(desc(total_attacks))
#View results
head(attacks_by_country)

# Role 3: Visualize the data using a bar chart of attacks by country

# This chart shows the top 10 countries with the most attacks
# reorder() sorts the bars by number of attacks
# coord_flip() rotates the chart to make country names easier to read

ggplot(attacks_by_country[1:10, ],
       aes(x = reorder(country_txt, total_attacks),
           y = total_attacks)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Attacks by Country",
    x = "Country",
    y = "Number of Attacks"
  ) +
  theme_minimal()
