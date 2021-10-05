library(dplyr)
source("C:/Users/Crowi/OneDrive/Documents/R/AFL_scripts/load_afltables.R")

running_margin_data <- afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp_score) %>%
    distinct() %>% 
    # filter(season > 2020) %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    group_by(playing_for) %>% arrange(date) %>% 
    mutate(
        running_margin = cumsum(margin)
    )

library(ggplot2)

p <- ggplot(running_margin_data, aes(x = date, y = running_margin, colour = playing_for)) +
    geom_step() #+
    # geom_text(aes(x = date + 0.5, label = playing_for), hjust = 0)

library(plotly)
ggplotly(p)
