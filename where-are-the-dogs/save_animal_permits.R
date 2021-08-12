
# download data from 
# https://www.data.brisbane.qld.gov.au/data/dataset/current-animal-related-permits/resource/c2823090-8a44-4c8b-9ed4-e988491b25f4
# extract the csv

library(dplyr)
library(tidyr)
library(ggplot2)

raw_data <- read.csv("where-are-the-dogs/animal_permits_july_2021.csv")
names(raw_data) <- snakecase::to_snake_case(names(raw_data))

clean_data <- raw_data %>% 
    rename(id = ï_index) %>% 
    mutate(suburb = stringr::str_to_title(suburb)) %>% 
    filter(
        permit_name %in% c('Dog Registration', 'Dog Permit'),
        breed != "", suburb != ""
    )

saveRDS(clean_data, 'where-are-the-dogs/animal_permits.rds')
