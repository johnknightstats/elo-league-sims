
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

# ---- Use parallel processing ----

available_cores <- parallel::detectCores(logical = TRUE)
safe_cores <- max(1, available_cores - 1)
plan(multisession, workers = safe_cores)

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
    )
  
  ### Points deductions ###
  if (season %in% c("1990-1991", "1996-1997", "2009-2010", "2023-2024")) {
    standings <- standings %>%
      mutate(
        pts = case_when(
          season == "1990-1991" & team == "Arsenal"            ~ pts - 2,
          season == "1990-1991" & team == "Manchester United"  ~ pts - 1,
          season == "1996-1997" & team == "Middlesbrough"      ~ pts - 3,
          season == "2009-2010" & team == "Portsmouth"         ~ pts - 9,
          season == "2023-2024" & team == "Everton"            ~ pts - 6,
          season == "2023-2024" & team == "Nottingham Forest"  ~ pts - 4,
          TRUE                                                 ~ pts
        )
      )
  }
  
  
  
  # Sort by point then tiebreakers
  standings <- standings %>%
    arrange(
      desc(pts),
      desc(!!sym(tiebreaker)),
      desc(goals_for)
    )

  standings <- standings %>%
    mutate(rank = dense_rank(desc(pts) * 1e6 + desc(!!sym(tiebreaker)) * 1e3 + desc(goals_for)))
  
  # Check if any ties exist after tiebreakers
  if (n_distinct(standings$rank) < nrow(standings)) {
    # If so, draw lots
    standings <- standings %>%
      group_by(rank) %>%
      mutate(
        tied = n() > 1
      ) %>%
      ungroup()
    
    standings <- standings %>%
      group_by(rank) %>%
      mutate(
        rank = if (tied[1]) {
          rank + (rank(runif(n())) - 1)
        } else {
          rank
        }
      ) %>%
      ungroup() %>%
      select(-tied, -rank)
  }
  
  
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

get_final_standings <- function(df, season) {
  final_day <- max(df$match_date[df$season == season])
  final_standings <- get_standings(df, season, final_day)
  return(final_standings)
}

get_champions <- function(df, season) {
  final_standings <- get_final_standings(df, season)
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
  
  # Progress bar
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
  
  # Add actual champion column
  actual_champion <- get_champions(df, season)
  standings_with_odds <- standings_with_odds %>%
    mutate(actual_champion = as.integer(team == actual_champion))
  
  # Save CSV
  date_string <- format(date, "%Y-%m-%d")
  filename <- paste0("title_odds_", season, "_", date_string, ".csv")
  output_path <- here("data/odds", filename)
  write_csv(standings_with_odds, output_path)
  
  cat("Saved title odds to:", output_path, "\n")
  
  return(standings_with_odds)
}

# ---- Summarize number of games left for each date ----

get_games_left_summary <- function(df) {
  
  # Create match_dates data frame with season + date pairs
  match_dates <- df %>%
    distinct(season, match_date) %>%
    arrange(season, match_date)
  
  # Add pld_min and pld_max by computing standings for each (season, match_date)
  match_dates <- match_dates %>%
    rowwise() %>%
    mutate(
      pld_values = list(get_standings(matches, season, match_date)$pld),
      pld_min = min(pld_values),
      pld_max = max(pld_values)
    ) %>%
    ungroup() %>%
    select(-pld_values)
  
  # For each season, compute the number of matches = (number of teams - 1) * 2
  final_pld_per_season <- df %>%
    group_by(season) %>%
    summarise(
      num_teams = n_distinct(home_team),
      final_pld = (num_teams - 1) * 2,
      .groups = "drop"
    )
  
  # Join final pld into match_dates
  match_dates <- match_dates %>%
    left_join(final_pld_per_season, by = "season") %>%
    mutate(
      min_games_left = final_pld - pld_max,
      max_games_left = final_pld - pld_min
    )
  
  return(match_dates)
}

# ---- Sim all dates and skip where eventual champion has >99% prob ----

sim_all_seasons <- function(df, games_left_df, n = 1000, start_date = '1900-01-01') {

  start_date <- as.Date(start_date)
  processed_seasons <- character()
  
  # Filter season end scenarios only
  games_left_df <- games_left_df %>%
    filter(min_games_left <= 5 & match_date >= start_date) %>%
    arrange(season, match_date)
  
  total_rows <- nrow(games_left_df)
  cat("Running simulations for", total_rows, "dates...\n\n")
  
  for (i in seq_len(total_rows)) {
    row <- games_left_df[i, , drop = FALSE]
    season <- row$season
    date <- row$match_date
    
    # Skip season if already resolved
    if (season %in% processed_seasons) next
    
    cat("[", i, "/", total_rows, "] Simulating", season, "on", date, "...\n")
    
    standings <- get_title_odds(df, season, date, n)
    
    # Check if any team has > 99% chance of winning
    top_team <- standings %>%
      filter(title_odds > 0.99) %>%
      slice(1)
    
    if (nrow(top_team) == 1 && top_team$actual_champion == 1) {
      cat("✅ Champion correctly predicted with >99% certainty. Skipping remaining for", season, "\n\n")
      processed_seasons <- c(processed_seasons, season)
    } 
  }
  
  cat("✅ All simulations complete.\n")
}



games_left <- get_games_left_summary(matches)
sim_all_seasons(matches, games_left, n = 10000, start_date = '1900-01-01')

plan(sequential) # Reset parallel processing
