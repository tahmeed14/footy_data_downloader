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

