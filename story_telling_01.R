# set working directory
setwd('~/Story-Telling-and-Success-Story')

# load the data and packages for use
library(tidyverse)
index_data <- readr::read_csv('./index_data.csv')
# check at the data
glimpse(index_data)

# clean the data
#function that converts character variables into factors
char_to_fact_function <- function(df) {
  list_df <- list()
  for (var in seq_along(df)) {
    if (is.character(df[[var]])) {
      list_df[[var]] <- as.factor(df[[var]])
    } else{
      list_df[[var]] <- df[[var]]
    }
  }
  new_df <- list_df %>% as_tibble(.name_repair = 'unique')
  colnames(new_df) <- colnames(df)
  return(new_df)
}
clean_index_data <- index_data %>%
  # remove duplicate columns
  select(c(
    'Country',
    'Indicator',
    'Measure',
    'Inequality',
    'Unit',
    'PowerCode',
    'Value'
  )) %>%
  # remove missing values
  remove_missing() %>%
  # change from character to factors
  char_to_fact_function()

# check the data structure
str(clean_index_data)

# a barplot for inequalities in different countries
set.seed(1234)
countries <- sample(clean_index_data$Country,
                    size = 10,
                    replace = F)
clean_index_data %>%
  filter(Country %in% countries,
         Value <= 10000,
         Inequality %in% c('Men', 'Women', 'Total')) %>%
  ggplot(aes(x = Country, y = Value, fill = Inequality)) +
  geom_col(position = 'dodge') +
  labs(title = 'A bar plot showing how inequality is distributed in 10 different countries',
       x = '10 Different countries', y = 'Value')
# a treemap chart for different indicators between 5 countries
countries <- sample(clean_index_data$Country,
                    size = 5,
                    replace = F)
indicators <-
  c('Air pollution',
    'Employment rate',
    'Homicide rate',
    'Life expectancy',
    'Water quality')
clean_index_data %>%
  filter(Country %in% countries, Indicator %in% indicators) %>%
  ggplot(aes(
    area = Value,
    fill = Indicator,
    label = interaction(Indicator, Country),
    subgroup = interaction(Country, Indicator)
  )) +
  treemapify::geom_treemap() +
  treemapify::geom_treemap_text(
    fontface = "bold",
    color = "white",
    place = "centre",
    reflow = TRUE
  ) +
  theme(legend.position = 'bottom')
