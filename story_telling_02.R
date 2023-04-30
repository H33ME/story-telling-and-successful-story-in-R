# packages required
library(tidyverse)
library(janitor)
#load the data
index_dataframe <- read_csv('./index_data.csv')
# check the data
head(index_dataframe)

# clean the data
# refactoring variables function
factorize_function <- function(dataframe) {
  lst <- list()
  for (i in seq_along(dataframe)) {
    if (is.character(dataframe[[i]])) {
      lst[[i]] <- as.factor(dataframe[[i]])
    } else{
      lst[[i]] <- dataframe[[i]]
    }
  }
  new_dataframe <- lst %>% as_tibble(.name_repair = 'unique')
  colnames(new_dataframe) <- colnames(dataframe)
  return(new_dataframe)
}
clean_index_dataframe <- index_dataframe %>%
  # select data needed
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
  # clean the column names
  clean_names() %>%
  # refactoring variables
  factorize_function()

# a dotplot for the different countries
clean_index_dataframe %>% 
  group_by(country) %>% 
  filter(value<=2500, inequality %in% c('Men', 'Women', 'Total')) %>% 
  slice_max(value , n = 10) %>%
  ungroup() %>% 
  arrange(country, -value) %>% 
  sample_n(size = 20, replace = F) %>% 
  ggplot(aes(x = country, y = value, fill = inequality)) +
  geom_dotplot(
    binaxis = "y",
    stackdir = "center"
  ) +
  labs(title = "A dot plot for different countries and their values", x = "Different values", y = "Value")

# boxplot plot for different countries unit
out<- boxplot.stats(clean_index_dataframe$value)$out
countries<- c('Belgium', 'Canada', 'Germany', 'Japan', 'Korea')
clean_index_dataframe %>% 
  filter(country %in% countries, value<=1000, !value %in% out) %>% 
  ggplot(aes(y = value,x = country, fill = country))+
  geom_boxplot()+
  facet_wrap(~unit)+
  labs(title = 'box plot for different countries', x = 'different countries')
