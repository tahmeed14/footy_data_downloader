# Author: Tahmeed Tureen <tureen@umich.edu>
# Helper functions that leverage worldfootballR to download and process data from FBref.com

library(worldfootballR) # Credit to Jason Zivkovic: https://jaseziv.github.io/worldfootballR/index.html
library(dplyr)
library(corrplot)

# Function to visualize the correlation plots in hierarchical cluster order for LVA
vizCorrelation <- function(data_in) {
  require(dplyr)
  require(corrplot)
  
  cat("No. of Players:", nrow(data_in), "\n")
  # Drop non-numeric variables
  function_df <- data_in %>% 
    dplyr::select_if(is.numeric)
  
  # Create correlation matrix & visualize
  corrplot::corrplot(corr = cor(function_df),
                     method = "square",
                     order = "hclust",
                     tl.col = 'black',
                     tl.cex = 0.75)
}

# Function to visualize distributions of all continuous variables in data_in
vizContinuous <- function(data_in) {
  
  vars_plot <- colnames(data_in %>% select_if(is.numeric))
  
  par(mfrow = c(3,3))
  for (var in vars_plot) {
    hist(data_in[,var], xlab = var, main = " ")
    abline(v = mean(data_in[,var]), col = "red", lwd = 3)
  }
}

# processStandard: Function to process and subset standard data for LVA
processStandard <- function(data_in, season_in) {
  require(dplyr)
  n_prev <- nrow(data_in)
  
  output_data <- data_in %>% 
    filter(Season == season_in) %>% 
    filter(!is.na(Min_Playing_Time)) %>% 
    filter(Min_Playing_Time >= 5) %>% 
    mutate(MP = if_else(is.na(MP),
                        true = 0,
                        false = MP),
           MP_Playing_Time = if_else(is.na(MP_Playing_Time),
                                     true = 0,
                                     false = MP_Playing_Time)) %>% # convert the NAs to 0's
    mutate(MP_Playing_Time = MP + MP_Playing_Time) %>% # finally, combine the two repetitive variables into one
    select(-c(MP, 
              PlayerURL,
              CrdR,
              CrdY,
              xG_Expected,
              npxG_Expected,
              xAG_Expected,
              `npxG+xAG_Expected`,
              `xG_Per_Minutes`,
              npxG_Per_Minutes,
              `npxG+xAG_Per_Minutes`,
              xAG_Expected,
              xAG_Per_Minutes,
              `xG+xAG_Per_Minutes`)) %>% 
    mutate(Age = as.numeric(Age))
  
  cat("League(s):", unique(output_data$Comp), "\n")
  cat("Season(s):", unique(output_data$Season), "\n")
  cat("No. of players dropped due to zero playing minutes:", 
      n_prev - nrow(output_data) , "from", n_prev, "players \n")
  
  return(output_data)
}

# processPossession: Function to process and subset standard data for LVA
processPossession <- function(main_data_in, data_in, season_in){
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Season == season_in) %>%
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Age,
              Nation,
              Pos,
              Mins_Per_90,
              PlayerURL)) %>% 
    mutate(Succ_percent_Take_Ons = if_else(is.na(Succ_percent_Take_Ons),
                                           true = 0,
                                           false = Succ_percent_Take_Ons),
           Tkld_percent_Take_Ons = if_else(is.na(Tkld_percent_Take_Ons),
                                           true = 0,
                                           false = Tkld_percent_Take_Ons))
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
}

# processPass:
processPass <- function(main_data_in, data_in, season_in) {
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Nation,
              Pos,
              Age,
              Mins_Per_90,
              PlayerURL)) %>% 
    mutate(Cmp_percent_Total = if_else(is.na(Cmp_percent_Total),
                                       true = 0,
                                       false = Cmp_percent_Total),
           Cmp_percent_Short = if_else(is.na(Cmp_percent_Short),
                                       true = 0,
                                       false = Cmp_percent_Short),
           Cmp_percent_Medium = if_else(is.na(Cmp_percent_Medium),
                                        true = 0,
                                        false = Cmp_percent_Medium),
           Cmp_percent_Long = if_else(is.na(Cmp_percent_Long),
                                      true = 0,
                                      false = Cmp_percent_Long))
  
  # Need to impute NAs with Zeros for Percent Variables
  
  numeric_vars <- colnames(append_data %>% select_if(is.numeric))
  
  append_data <- append_data %>% 
    rename_with(.cols = numeric_vars, 
                .fn = function(x) paste0("Pass_", x)) # add a prefix "Pass" to variable names
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
}

# processPass:
processPassTypes <- function(main_data_in, data_in, season_in) {
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Nation,
              Pos,
              Age,
              Mins_Per_90,
              PlayerURL)) %>% 
    rename(Pass_Attempts = Att)
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
}

# SGCA
processSGCA <- function(main_data_in, data_in, season_in) {
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Nation,
              Pos,
              Age,
              Mins_Per_90,
              PlayerURL))
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
}

# DEF
processDef <- function(main_data_in, data_in, season_in) {
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Nation,
              Pos,
              Age,
              Mins_Per_90,
              PlayerURL)) %>% 
    mutate(Tkl_percent_Challenges = if_else(is.na(Tkl_percent_Challenges),
                                            true = 0,
                                            false = Tkl_percent_Challenges))
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
}

# Misc
processMisc <- function(main_data_in, data_in, season_in) {
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Nation,
              Pos,
              Age,
              Mins_Per_90,
              PlayerURL,
              CrdY,
              CrdR,
              `2CrdY`)) %>% 
    mutate(Won_percent_Aerial_Duels = if_else(condition = is.na(Won_percent_Aerial_Duels),
                                              true = 0,
                                              false = Won_percent_Aerial_Duels))
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
  
}

# Shooting
processShots <- function(main_data_in, data_in, season_in) {
  require(dplyr)
  
  append_data <- data_in %>% 
    filter(Mins_Per_90 != 0) %>% 
    select(-c(Nation,
              Pos,
              Age,
              Mins_Per_90,
              PlayerURL)) %>% 
    mutate(G_per_Sh_Standard = if_else(condition = is.na(G_per_Sh_Standard),
                                       true = 0,
                                       false = G_per_Sh_Standard),
           G_per_SoT_Standard = if_else(condition = is.na(G_per_SoT_Standard),
                                        true = 0,
                                        false = G_per_SoT_Standard),
           Dist_Standard = if_else(condition = is.na(Dist_Standard),
                                   true = 0,
                                   false = Dist_Standard),
           `np:G_minus_xG_Expected` = if_else(condition = is.na(`np:G_minus_xG_Expected`),
                                              true = 0,
                                              false = `np:G_minus_xG_Expected`),
           SoT_percent_Standard = if_else(condition = is.na(SoT_percent_Standard),
                                   true = 0,
                                   false = SoT_percent_Standard),
           npxG_per_Sh_Expected = if_else(condition = is.na(npxG_per_Sh_Expected),
                                   true = 0,
                                   false = npxG_per_Sh_Expected))
  
  vizCorrelation(data_in = append_data)
  vizContinuous(data_in = append_data)
  
  output_data <- main_data_in %>% 
    left_join(append_data, by = c("Season", "Player", "Squad", "Comp"))
  
  return(output_data)
  
}
