# Author: Tahmeed Tureen <tureen@umich.edu>
# Description: Code script that leverages the worldfootballR library/package to pull player statistics from FBref.com

library(worldfootballR) # Credit to Jason Zivkovic: https://jaseziv.github.io/worldfootballR/index.html
library(dplyr)
library(ggplot2)


pl_seasons_url <- fb_league_urls(country = "ENG", gender = "M", season_end_year = 2017:2023, tier = "1st")
buli_seasons_url <- fb_league_urls(country = "GER", gender = "M", season_end_year = 2017:2023, tier = "1st")
laliga_seasons_url <- fb_league_urls(country = "ESP", gender = "M", season_end_year = 2017:2023, tier = "1st")
seriea_seasons_url <- fb_league_urls(country = "ITA", gender = "M", season_end_year = 2017:2023, tier = "1st")

# player_data <- fb_team_player_stats(team_urls= pl_team_urls[1], stat_type= 'standard')

pl_team_urls <- fb_teams_urls(pl_seasons_url[1])

stat_types <- c("standard", "shooting", "passing", "passing_types",
                "gca", "defense", "possession", "playing_time",
                "misc", "keeper", "keeper_adv")



league_stats <- worldfootballR::fb_team_player_stats(team_urls= pl_team_urls, 
                                                     stat_type= "possession",
                                                     time_pause = 5)

saveRDS(object = league_stats, file = "3_data/player_possession_stats.RDS")


