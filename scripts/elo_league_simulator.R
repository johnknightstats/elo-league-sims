
#############################################################################
### Get standings at end of any date of any season and simulate remainder ###
#############################################################################

# ---- Import libraries ----

library(tidyverse)
library(here)
library(rlang)
library(future.apply)
library(progressr)

# ---- Load utility functions ----

source(here("utils", "elo_helper_functions.R"))
source(here("utils", "compute_elo_columns.R"))

# ---- Load data ----

matches <- read_csv(here("data", "england_matches_with_elo.csv")) %>%
  mutate(match_date = as.Date(match_date))

# Should have columns:

# match_date
# kickoff_local_time
# season
# home_team
# away_team
# home_goals
# away_goals
# home_elo
# away_elo

# Calculate the Elo columns 
matches <- compute_elo_columns(matches)

# ---- Load model and parameters ----

elo_model <- readRDS(here("data", "elo_model.rds"))
a <- elo_model$a
b <- elo_model$b
c <- elo_model$c
d <- elo_model$d
e <- elo_model$e
f <- elo_model$f
g_fit <- elo_model$g
h_fit <- elo_model$h
i_fit <- elo_model$i
get_odds <- elo_model$get_odds


# ---- Function to get standings on date ----

get_standings <- function(df, season, date) {
  year <- as.integer(substr(season, 6, 9))
  points_per_win <- ifelse(year <= 1981, 2, 3)
  tiebreaker <- ifelse(year <= 1976, "GA", "GD")
  
  results <- df[df$season == season & df$match_date <= date,]
  
  results$home_win <- as.integer(results$home_goals > results$away_goals)
  results$draw <- as.integer(results$home_goals == results$away_goals)
  results$away_win <- as.integer(results$home_goals < results$away_goals)
  
  results$home_pts <- ifelse(results$home_goals > results$away_goals, points_per_win,
                             ifelse(results$away_goals > results$home_goals, 0, 1))
  results$away_pts <- ifelse(results$home_goals < results$away_goals, points_per_win,
                             ifelse(results$away_goals < results$home_goals, 0, 1))
  
  home_stats <- results %>%
    transmute(
      team = home_team,
      won = home_win,
      drawn = draw,
      lost = away_win,
      goals_for = home_goals,
      goals_against = away_goals,
      pts = home_pts,
      games = 1
    )
  
  away_stats <- results %>%
    transmute(
      team = away_team,
      won = away_win,
      drawn = draw,
      lost = home_win,
      goals_for = away_goals,
      goals_against = home_goals,
      pts = away_pts,
      games = 1
    )
  
  standings <- bind_rows(home_stats, away_stats) %>%
    group_by(team) %>%
    summarise(
      pld = sum(games),
      won = sum(won),
      drawn = sum(drawn),
      lost = sum(lost),
      goals_for = sum(goals_for),
      goals_against = sum(goals_against),
      pts = sum(pts),
      .groups = "drop"
    ) %>%
    mutate(
      GD = goals_for - goals_against,
      GA = ifelse(goals_against == 0, 1, goals_for / goals_against)
    ) %>%
    arrange(desc(pts), desc(!!sym(tiebreaker)))
  
  # Get each team’s Elo from their first match *after* the given date
  next_match_elos <- df %>%
    filter(season == !!season, match_date > !!date) %>%
    select(match_date, home_team, away_team, home_elo, away_elo) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = "venue",
      values_to = "team"
    ) %>%
    mutate(
      elo = ifelse(venue == "home_team", home_elo, away_elo)
    ) %>%
    select(team, match_date, elo) %>%
    arrange(team, match_date) %>%
    distinct(team, .keep_all = TRUE)  # first match after the date
  
  # Backup Elo if no match after date
  fallback_elos <- df %>%
    filter(season == !!season, match_date <= !!date) %>%
    select(match_date, home_team, away_team, home_elo, away_elo) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = "venue",
      values_to = "team"
    ) %>%
    mutate(
      elo = ifelse(venue == "home_team", home_elo, away_elo)
    ) %>%
    select(team, match_date, elo) %>%
    arrange(team, desc(match_date)) %>%
    distinct(team, .keep_all = TRUE)
  
  # Combine: prefer next_match_elos, fallback if missing
  team_elos <- bind_rows(next_match_elos, fallback_elos) %>%
    arrange(team, match_date) %>%
    distinct(team, .keep_all = TRUE)
  
  standings <- standings %>%
    left_join(team_elos, by = "team") %>%
    rename(latest_elo = elo)

  return(standings)
}

get_final_standings <- function(season) {
  final_day <- max(matches$match_date[matches$season == season])
  final_standings <- get_standings(season, final_day)
  return(final_standings)
}

get_champions <- function(season) {
  final_standings <- get_final_standings(season)
  champions <- final_standings$team[1]
  return(champions)
}

simulate_season <- function(df, season, my_date) {
  
  my_date <- as.Date(my_date)

  results <- df[df$season == season & df$match_date <= my_date,]
  fixtures <- df[df$season == season & df$match_date > my_date,]
  
  if (nrow(fixtures) == 0) {
    cat("No fixtures remaining. Returning standings as of my_date.\n")
    return(get_standings(results, my_date))
  }

  fixtures <- fixtures %>%
    mutate(
      home_net_diff = home_elo - away_elo + home_advantage,
      fav_net_diff = abs(home_net_diff),
      sim = pmap(
        list(fav_net_diff, home_net_diff, home_team, away_team),
        function(fav_diff, home_diff, home_team, away_team) {
          score_probs <- get_odds(fav_diff, g_fit, h_fit, i_fit)$scores
          score_probs_vec <- as.vector(score_probs)
          
          sampled_index <- sample(length(score_probs_vec), 1, prob = score_probs_vec)
          n_scores <- nrow(score_probs)
          
          fav_goals <- (sampled_index - 1) %% n_scores
          dog_goals <- (sampled_index - 1) %/% n_scores
          
          if (home_diff >= 0) {
            c(home_goals = fav_goals, away_goals = dog_goals)
          } else {
            c(home_goals = dog_goals, away_goals = fav_goals)
          }
        }
      )
    ) %>%
    mutate(
      home_goals = map_dbl(sim, 1),
      away_goals = map_dbl(sim, 2)
    ) %>%
    select(-sim, -home_net_diff, -fav_net_diff)

  # Combine simulated fixtures and actual results
  all_matches <- bind_rows(results, fixtures)

  final_date <- all_matches$match_date[nrow(all_matches)]
  
  standings <- get_standings(all_matches, season, final_date)
  champions <- standings[1,"team"]
  
  return(list(standings=standings, champions=champions))
}

# ---- Function to get title odds on a date by running many sims ----

get_title_odds <- function(df, season, date, n) {

  date <- as.Date(date)
  
  # Get current standings as of the input date
  initial_standings <- get_standings(df, season, date)
  
  # Set up parallel processing and progress bar
  available_cores <- parallel::detectCores(logical = TRUE)
  safe_cores <- max(1, available_cores - 1)
  plan(multisession, workers = safe_cores)
  handlers(global = TRUE)
  handlers("txtprogressbar")
  
  # Run simulations with progress
  champ_list <- vector("character", n)
  
  with_progress({
    p <- progressor(steps = n)
    
    champ_list <- future_lapply(1:n, function(i) {
      result <- simulate_season(df, season, date)
      p()
      as.character(result$champions)
    }, future.seed = TRUE)
  })
  
  # Count champions
  champ_table <- table(unlist(champ_list))
  
  # Convert to named vector of title odds
  title_odds <- rep(0, nrow(initial_standings))
  names(title_odds) <- initial_standings$team
  title_odds[names(champ_table)] <- champ_table / n
  
  # Add to standings
  standings_with_odds <- initial_standings %>%
    mutate(title_odds = round(title_odds[team], 4))
  
  # Save CSV
  date_string <- format(date, "%Y-%m-%d")
  filename <- paste0("title_odds_", season, "_", date_string, ".csv")
  output_path <- here("data/odds", filename)
  write_csv(standings_with_odds, output_path)
  
  cat("Saved title odds to:", output_path, "\n")
  
  plan(sequential) # Reset parallel processing
  
  return(standings_with_odds)
}


title_odds_april <- get_title_odds(matches, "1974-1975", "1975-04-01", n = 10000)

