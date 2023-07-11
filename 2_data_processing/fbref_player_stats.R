# Author: Tahmeed Tureen <tureen@umich.edu>

library(worldfootballR) # Credit to Jason Zivkovic: https://jaseziv.github.io/worldfootballR/index.html
library(dplyr)

# Process the Standard Stats from FBref.com

standard_data <- readRDS(file = "3_data/player_standard_stats.RDS")

main_data <- standard_data %>% 
  filter(!is.na(Min_Playing_Time)) %>% # 739 players to 569 players with non-zero playing time
  mutate(MP = if_else(is.na(MP),
                      true = 0,
                      false = MP),
         MP_Playing_Time = if_else(is.na(MP_Playing_Time),
                                   true = 0,
                                   false = MP_Playing_Time)) %>% # convert the NAs to 0's
  mutate(MP_Playing_Time = MP + MP_Playing_Time) %>% # finally, combine the two repetitive variables into one
  select(-c(MP, PlayerURL)) %>% 
  mutate(Age = as.numeric(Age))
  
std_corr <- cor(main_data %>% 
                  select(-c(Season, Squad, Comp, Player, Nation, Pos)), 
                use="complete.obs")

corrplot::corrplot(std_corr,
                   method = "square",
                   order = "hclust",
                   tl.col = 'black',
                   tl.cex = 0.75)


corrplot(cor(dat, use="complete.obs"),
         method = "square",
         order = "hclust", # order by hierarchical cluster
         tl.col='black',
         tl.cex=.75) 

# Process the Shooting Data
shooting_data <- readRDS(file = "3_data/player_shooting_stats.RDS")

main_data <- shooting_data %>% 
  select(-c(Nation, Pos, Age, Mins_Per_90)) %>% 
  right_join(main_data, by = c("Season", "Player", "Squad", "Comp"))
