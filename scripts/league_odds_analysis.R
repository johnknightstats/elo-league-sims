
##########################################################################
### Analyze the odds for teams in the closing games of each title race ###
##########################################################################

# ---- Import libraries ----

library(tidyverse)
library(here)
library(data.table)

# ---- Load CSVs ----

odds_dir <- here("data", "odds")
csv_files <- list.files(odds_dir, pattern = "^title_odds_\\d{4}-\\d{4}_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)

# Function to read a file and add the season column
read_with_season <- function(file) {
  dt <- fread(file)
  season <- sub("^title_odds_(\\d{4}-\\d{4})_\\d{4}-\\d{2}-\\d{2}\\.csv$", "\\1", basename(file))
  dt[, season := season]
  return(dt)
}

# Read and bind all CSVs
all_odds <- rbindlist(lapply(csv_files, read_with_season), use.names = TRUE, fill = TRUE)


# Load all results
all_results <- read.csv(here("data/england_matches_elo_with_predictions.csv"))

# ---- Add number of games played per season ----

final_pld_per_season <- all_results %>%
  group_by(season) %>%
  summarise(
    num_teams = n_distinct(home_team),
    final_pld = (num_teams - 1) * 2,
    .groups = "drop" 
  ) %>%
  select(season, final_pld)

all_odds <- merge(all_odds, final_pld_per_season, by="season", all.x=TRUE, all.y=FALSE)
all_odds$games_left <- all_odds$final_pld - all_odds$pld

# ---- Find highest probability losers ----

# Highest odds for teams that did not win
losers <- subset(all_odds, actual_champion == 0 & games_left <= 5)
highest_losers <- as.data.table(losers)[order(-title_odds), .SD[1], by=season]

# Lowest odds for champions going into final game
last_game_winner_odds <- subset(all_odds, actual_champion==1 & games_left == 1)
last_game_winner_odds_low <- as.data.table(last_game_winner_odds)[order(title_odds), .SD[1], by=season]

# Highest odds losers with no 5 game limit
losers_all <- subset(all_odds, actual_champion == 0)
highest_losers_all <- as.data.table(losers_all)[order(-title_odds), .SD[1], by=season]
