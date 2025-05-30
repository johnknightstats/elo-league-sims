
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
  file_base <- basename(file)
  season <- sub("^title_odds_(\\d{4}-\\d{4})_\\d{4}-\\d{2}-\\d{2}\\.csv$", "\\1", file_base)
  date <- sub("^title_odds_\\d{4}-\\d{4}_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", file_base)
  dt[, season := season]
  dt[, date := as.Date(date)]
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
losers <- subset(all_odds, actual_champion == 0)
highest_losers <- as.data.table(losers)[order(-title_odds), .SD[1], by=season]

# ---- Plot Man U 2011-12 Odds Time Series ----

mu_12 <- subset(all_odds, team %in% c("Manchester United", "Manchester City")
                & season == "2011-2012")

mu_12_odds <- ggplot(mu_12, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(size=1.2) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Manchester United" = "red", "Manchester City" = "skyblue")) +
  labs(x = NULL, y = "Title Odds", color = NULL)

mu_12_odds

bu_12 <- subset(all_odds, team %in% c("Burnley", "Ipswich Town")
                & season == "1961-1962")

bu_12_odds <- ggplot(bu_12, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(size=1.2) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Burnley" = "maroon","Ipswich Town" = "blue3")) +
  labs(x = NULL, y = "Title Odds", color = NULL)

bu_12_odds

li_89 <- subset(all_odds, team %in% c("Liverpool", "Arsenal")
                & season == "1988-1989")

li_89_odds <- ggplot(li_89, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(size=1.2) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Liverpool" = "red", "Arsenal" = "yellow")) +
  labs(x = NULL, y = "Title Odds", color = NULL)

li_89_odds

mu_92 <- subset(all_odds, team %in% c("Manchester United", "Leeds United")
                & season == "1991-1992")

mu_92_odds <- ggplot(mu_92, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(size=1.2) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Manchester United" = "red", "Leeds United" = "white")) +
  labs(x = NULL, y = "Title Odds", color = NULL)

mu_92_odds

li_14 <- subset(all_odds, team %in% c("Liverpool", "Manchester City")
                & season == "2013-2014")

li_14_odds <- ggplot(li_14, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(size=1.2) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Liverpool" = "red", "Manchester City" = "skyblue")) +
  labs(x = NULL, y = "Title Odds", color = NULL)

li_14_odds
