library(dplyr)
library(tidyr)
library(broom)

library(ggplot2)
library(ggpubr)
library(patchwork)
# source("C:/Users/Crowi/OneDrive/Documents/R/AFL_scripts/load_fryzigg.R")

clean_weather_data <- fryzigg_data %>% 
    select(
        date, season, round, weather = weather_type, 
        home_team, away_team, playing_for, id, first_name, surname,
        home_score = home_team_score, away_score = away_team_score,
        kicks, handballs, disposals, tackles, one_percenters,
        goals, behinds, metres_gained, turnovers,
        clearances, contested_marks, marks,
        cont_poss = contested_possessions, uncont_poss = uncontested_possessions,
        disp_eff_pcnt = disposal_efficiency_percentage,
        fantasy_points, rating_points
    ) %>% 
    mutate(
        kick_ratio = kicks/disposals*100,
        # total_score = home_score + away_score,
        playing_for_score = if_else(playing_for == home_team, home_score, away_score),
        opp_score = if_else(playing_for == home_team, away_score, home_score),
        weather_binary = if_else(weather %in% c('RAIN', 'WINDY_RAIN', 'THUNDERSTORMS'), 'Wet','Dry'),
        weather_binary = factor(weather_binary, levels = c('Dry','Wet'))
    ) %>% 
    filter(season >= 2010) %>% 
    mutate(full_name = paste(first_name, surname, id))

stat_name_vector <- c('kicks','handballs','disposals','goals','behinds','clearances','contested_marks','marks',
                      'disp_eff_pcnt','fantasy_points','kick_ratio','rating_points','tackles','one_percenters',
                      'cont_poss','uncont_poss','metres_gained', 'turnovers')

selected_stat <- 'kick_ratio'

clean_data_long <- clean_weather_data %>% 
    pivot_longer(
        cols = all_of(stat_name_vector),
        names_to = 'stat_name',
        values_to = 'value'
    ) #%>% 
# filter(stat_name == selected_stat)



saveRDS(object = clean_data_long, file = 'wet_weather_footy/weather_data.rds')

clean_data_long %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    mutate(full_name = paste(first_name, surname, id)) %>% 
    group_by(full_name) %>% 
    count() %>% View()


ggplot(clean_data_long, aes(value, fill = weather_binary)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~playing_for)

#### team average and totals ####

clean_weather_data_game_average <- clean_data_long %>% 
    group_by(season, round, date, playing_for, weather_binary) %>% 
    summarise(
        mean_stat = mean(value),
        sum_stat =  sum(value),
        .groups = 'drop'
    )

# weather_averages <- clean_weather_data_game_average %>% 
#     group_by(playing_for, weather_binary) %>% 
#     summarise(
#         avg = mean(mean_stat), .groups = 'drop'
#     ) %>% 
#     pivot_wider(
#         names_from = weather_binary,
#         values_from = avg
#     ) %>% 
#     mutate(dry_diff = Dry - Wet)
# 
# weather_pvalue <- clean_weather_data_game_average %>% 
#     group_by(playing_for) %>% 
#     do(w = wilcox.test(mean_stat~weather_binary, data=.)) %>% 
#     summarise(
#         playing_for,
#         wilcox_pval = w$p.value, .groups = 'drop'
#     )
# 
# inner_join(weather_averages, weather_pvalue, by = 'playing_for') %>% View()

# ggplot(clean_weather_data_game_average, aes(x = mean_stat, y = weather_binary, fill = weather_binary)) +
#     geom_violin(trim = F, draw_quantiles = c(0.5)) +
#     stat_compare_means(inherit.aes = F, aes(x = mean_stat, y = weather_binary, group = weather_binary), method = 'wilcox.test') +
#     # geom_jitter(height = 0.1, alpha = 0.4) +
#     # facet_wrap(~playing_for) + 
#     theme(
#         legend.position = 'top',
#         legend.title = element_blank()
#     ) +
#     labs(x = 'Average Player Rating',
#          y = '',
#          title = 'Average AFL Player Rating per Game by Team and Weather Type',
#          subtitle = 'Line represents 50% Quantile',
#          caption = '@crow_data_sci, data: @fryzigg and #fitzRoy')

# ggviolin(clean_weather_data_game_average, y = 'mean_stat', x = 'weather_binary', fill = 'weather_binary',
#          add = 'jitter', facet.by = 'playing_for', add.params = list(alpha = 0.5), trim = F) +
#     stat_compare_means(label = "p",label.x.npc = 0) +
#     labs(x = 'Average Player Rating',
#          y = '',
#          title = 'Average AFL Player Rating per Game by Team and Weather Type',
#          subtitle = 'Line represents 50% Quantile',
#          caption = '@crow_data_sci, data: @fryzigg and #fitzRoy') +
#     theme(
#         legend.position = 'top'
#     )

p_box <- ggboxplot(clean_weather_data_game_average, y = 'mean_stat', x = 'weather_binary', fill = 'weather_binary',
                   add = 'jitter', add.params = list(alpha = 0.4)) +
    stat_compare_means(aes(label = paste0(..p.format..,..p.signif..)),
                       label.x = 1.3, label.y.npc = 0.95, hide.ns = T) +
    labs(y = 'Average Disposals',
         x = '',
         title = 'Average Kick Ratio per Game by Weather Type',
         subtitle = "Each point represents the average kick ratio of a single game, 2010-2021",
         caption = '@crow_data_sci, data: @fryzigg and #fitzRoy') +
    theme(
        legend.position = 'top',
        legend.title = element_blank()
    )

p_box_facet <- facet(p_box, facet.by = 'playing_for', nrow = 2)

ggsave(p_box_facet, filename = 'wet_weather_footy/KR_wet_weather_disp.png', dpi = 300, type = 'cairo',
       width = 32, height = 18, units = 'cm')

#### player averages and totals ####

clean_weather_data_player <- clean_data_long %>% 
    filter(
        id == 11909
    ) %>% 
    drop_na()

p_box_player <- ggboxplot(clean_weather_data_player, y = 'value', x = 'weather_binary', fill = 'weather_binary',
                          add = 'jitter', add.params = list(alpha = 0.4)) +
    stat_compare_means(aes(label = paste0(..p.format..,..p.signif..)),
                       label.x = 1.3, label.y.npc = 0.95, hide.ns = T) +
    labs(y = '',
         x = '',
         title = 'Andrew Gaff Stat per Game by Weather Type',
         subtitle = "Each point represents the stats of a single game, 2010-2021",
         caption = '@crow_data_sci, data: @fryzigg and #fitzRoy') +
    theme(
        legend.position = 'top',
        legend.title = element_blank()
    )

p_box_player_facet <- facet(p_box_player, facet.by = 'stat_name', nrow = 2, scales = 'free_y')

p_box_player_facet

ggsave(p_box_player_facet, filename = 'wet_weather_footy/AG_wet_weather_disp.png', dpi = 300, type = 'cairo',
       width = 32, height = 18, units = 'cm')

#### team ####

clean_weather_data_team <- clean_data_long %>% 
    filter(
        playing_for == 'Geelong'
    ) %>% 
    group_by(season, round, weather_binary, stat_name) %>% 
    summarise(
        avg_value = mean(value),
        tot_value = sum(value),
        .groups = 'drop'
    ) %>% 
    drop_na()

p_box_team <- ggboxplot(clean_weather_data_team, y = 'avg_value', x = 'weather_binary', fill = 'weather_binary',
                        add = 'jitter', add.params = list(alpha = 0.4)) +
    stat_compare_means(aes(label = paste0(..p.format..,..p.signif..)),
                       label.x = 1.3, label.y.npc = 0.95, hide.ns = T) +
    labs(y = '',
         x = '',
         title = '<> Stat per Game by Weather Type',
         subtitle = "Each point represents the average stats of a single game, 2010-2021",
         caption = '@crow_data_sci, data: @fryzigg and #fitzRoy') +
    theme(
        legend.position = 'top',
        legend.title = element_blank()
    )

p_box_team_facet <- facet(p_box_team, facet.by = 'stat_name', nrow = 2, scales = 'free_y')

p_box_team_facet
