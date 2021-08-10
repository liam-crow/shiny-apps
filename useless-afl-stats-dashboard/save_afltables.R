library(dplyr)

afltables <- fitzRoy::fetch_player_stats_afltables(season = 2000:2021) %>% 
  rename_with(snakecase::to_snake_case) %>% 
  mutate(
    disposals = kicks + handballs,
    
    playing_for_score = if_else(playing_for == home_team, home_score, away_score),
    
    opp = if_else(playing_for == home_team, away_team, home_team),
    opp_score = if_else(playing_for == home_team, away_score, home_score),
    
    h_a = if_else(playing_for == home_team, 'H', 'A'),
    
    w_l = case_when(
      playing_for == home_team & home_score > away_score ~ 'W',
      playing_for == home_team & home_score < away_score ~ 'L',
      playing_for == away_team & away_score > home_score ~ 'W',
      playing_for == away_team & away_score < home_score ~ 'L',
      TRUE ~ 'D'
    ),
    pq_1_g = if_else(playing_for == home_team, hq_1_g, aq_1_g),
    pq_1_b = if_else(playing_for == home_team, hq_1_b, aq_1_b),
    pq_2_g = if_else(playing_for == home_team, hq_2_g, aq_2_g),
    pq_2_b = if_else(playing_for == home_team, hq_2_b, aq_2_b),
    pq_3_g = if_else(playing_for == home_team, hq_3_g, aq_3_g),
    pq_3_b = if_else(playing_for == home_team, hq_3_b, aq_3_b),
    pq_4_g = if_else(playing_for == home_team, hq_4_g, aq_4_g),
    pq_4_b = if_else(playing_for == home_team, hq_4_b, aq_4_b),
    
    oq_1_g = if_else(playing_for == home_team, aq_1_g, hq_1_g),
    oq_1_b = if_else(playing_for == home_team, aq_1_b, hq_1_b),
    oq_2_g = if_else(playing_for == home_team, aq_2_g, hq_2_g),
    oq_2_b = if_else(playing_for == home_team, aq_2_b, hq_2_b),
    oq_3_g = if_else(playing_for == home_team, aq_3_g, hq_3_g),
    oq_3_b = if_else(playing_for == home_team, aq_3_b, hq_3_b),
    oq_4_g = if_else(playing_for == home_team, aq_4_g, hq_4_g),
    oq_4_b = if_else(playing_for == home_team, aq_4_b, hq_4_b)
  ) %>% 
  select(season, round, date, local_start_time, venue, 
         attendance, home_team, hq_1_g, hq_1_b, hq_2_g, 
         hq_2_b, hq_3_g, hq_3_b, hq_4_g, hq_4_b, home_score, 
         away_team, aq_1_g, aq_1_b, aq_2_g, aq_2_b, aq_3_g, 
         aq_3_b, aq_4_g, aq_4_b, away_score, first_name, 
         surname, id, jumper_no, playing_for, playing_for_score, 
         opp, opp_score, w_l, h_a, kicks, marks, handballs, 
         goals, behinds, hit_outs, tackles, rebounds, 
         inside_50_s, clearances, clangers, frees_for, 
         frees_against, brownlow_votes, contested_possessions, 
         uncontested_possessions, contested_marks, 
         marks_inside_50, one_percenters, bounces, 
         goal_assists, time_on_ground, pq_1_g, 
         pq_1_b, pq_2_g, pq_2_b, pq_3_g, pq_3_b, 
         pq_4_g, pq_4_b, oq_1_g, oq_1_b, oq_2_g, 
         oq_2_b, oq_3_g, oq_3_b, oq_4_g, oq_4_b)

saveRDS(afltables, 'useless-afl-stats-dashboard/afltables.rds')
