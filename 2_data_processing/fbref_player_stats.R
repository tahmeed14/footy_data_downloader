# Author: Tahmeed Tureen <tureen@umich.edu>
# Description: Code script that leverages the worldfootballR library/package to pull player statistics from FBref.com

library(worldfootballR) # Credit to Jason Zivkovic: https://jaseziv.github.io/worldfootballR/index.html
library(dplyr)
library(ggplot2)

seasons <- 2017:2023

pl_seasons_url <- fb_league_urls(country = "ENG", gender = "M", season_end_year = seasons, tier = "1st")
buli_seasons_url <- fb_league_urls(country = "GER", gender = "M", season_end_year = seasons, tier = "1st")
laliga_seasons_url <- fb_league_urls(country = "ESP", gender = "M", season_end_year = seasons, tier = "1st")
seriea_seasons_url <- fb_league_urls(country = "ITA", gender = "M", season_end_year = seasons, tier = "1st")

# player_data <- fb_team_player_stats(team_urls= pl_team_urls[1], stat_type= 'standard')

stat_types <- c("standard", "shooting", "passing", "passing_types",
                "gca", "defense", "possession", "playing_time",
                "misc", "keeper", "keeper_adv")


pl_team_urls <- fb_teams_urls(pl_seasons_url[2])
league_stats <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                                     stat_type= "possession",
                                                     time_pause = 5)

pl_team_urls3 <- fb_teams_urls(pl_seasons_url[3])
league_stats3 <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls3, 
                                                     stat_type= "possession",
                                                     time_pause = 5)

pl_team_urls4 <- fb_teams_urls(pl_seasons_url[4])
league_stats4 <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls4, 
                                                      stat_type= "possession",
                                                      time_pause = 5)

pl_team_urls5 <- fb_teams_urls(pl_seasons_url[5])
league_stats5 <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls5, 
                                                      stat_type= "possession",
                                                      time_pause = 5)

pl_team_urls6 <- fb_teams_urls(pl_seasons_url[6])
league_stats6 <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls6, 
                                                      stat_type= "possession",
                                                      time_pause = 5)

possession_data <- testdata %>% 
  bind_rows(league_stats) %>%
  bind_rows(league_stats3) %>%
  bind_rows(league_stats4) %>%
  bind_rows(league_stats5) %>%
  bind_rows(league_stats6)
  

saveRDS(object = possession_data, file = "3_data/player_possession_stats.RDS")


pl_team_urls <- fb_teams_urls(pl_seasons_url[1])

# standard
league_stats_std <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                                         stat_type= "standard",
                                                         time_pause = 5)

standard_data <- league_stats_std
saveRDS(object = standard_data, file = "3_data/player_standard_stats.RDS")


stat_types <- c("standard", "shooting", "passing", "passing_types",
                "gca", "defense", "possession", "playing_time",
                "misc", "keeper", "keeper_adv")

# shooting
stat_j <- "shooting"
shooting_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                                     stat_type= stat_j,
                                                     time_pause = 3)

saveRDS(object = shooting_data, file = "3_data/player_shooting_stats.RDS")

# passing
stat_j <- "passing"
passing_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                              stat_type= stat_j,
                                              time_pause = 3)

saveRDS(object = passing_data, file = "3_data/player_passing_stats.RDS")

# passing_types
stat_j <- "passing_types"
passing_types_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                              stat_type= stat_j,
                                              time_pause = 3)

saveRDS(object = passing_types_data, file = "3_data/player_passing_types_stats.RDS")

# gca
stat_j <- "gca"
gca_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                              stat_type= stat_j,
                                              time_pause = 3)

saveRDS(object = gca_data, file = "3_data/player_gca_stats.RDS")

# defense
stat_j <- "defense"
defense_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                              stat_type= stat_j,
                                              time_pause = 3)

saveRDS(object = defense_data, file = "3_data/player_defense_stats.RDS")

# possession
stat_j <- "possession"
possession_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                              stat_type= stat_j,
                                              time_pause = 3)

saveRDS(object = possession_data, file = "3_data/player_possession_stats.RDS")

# misc
stat_j <- "misc"
misc_data <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                                        stat_type= stat_j,
                                                        time_pause = 3)

saveRDS(object = misc_data, file = "3_data/player_misc_stats.RDS")



