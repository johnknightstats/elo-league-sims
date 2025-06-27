
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
source(here("utils", "league_table_funcs.R"))

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
alphas <- elo_model$alphas
sizes <- elo_model$sizes
a <- elo_model$a
b <- elo_model$b
c <- elo_model$c
d <- elo_model$d
e <- elo_model$e
f <- elo_model$f
g <- elo_model$g
h <- elo_model$h
i <- elo_model$i
j <- elo_model$j
k <- elo_model$k
l <- elo_model$l
m <- elo_model$m
n <- elo_model$n
o <- elo_model$o
get_odds_matrix <- elo_model$get_odds_matrix
get_expected_goals <- elo_model$get_expected_goals


# ---- Function to simulate one iteration of a season from a given date ----

simulate_season <- function(df, season, my_date) {
  
  my_date <- as.Date(my_date)
  
  results <- df[df$season == season & df$match_date <= my_date,]
  fixtures <- df[df$season == season & df$match_date > my_date,]
  
  if (nrow(fixtures) == 0) {
    cat("No fixtures remaining. Returning standings as of my_date.\n")
    return(get_standings(results, my_date))
  }
  
  # Preallocate home_goals and away_goals vectors
  n <- nrow(fixtures)
  home_goals <- integer(n)
  away_goals <- integer(n)
  
  for (i in seq_len(n)) {
    mat <- fixtures$score_matrix[[i]]
    is_home_fav <- fixtures$is_home_fav[i]
    
    probs <- as.vector(mat)
    sampled_index <- sample.int(length(probs), 1, prob = probs)
    n_scores <- nrow(mat)
    
    fav_goals <- (sampled_index - 1) %% n_scores
    dog_goals <- (sampled_index - 1) %/% n_scores
    
    if (is_home_fav) {
      home_goals[i] <- fav_goals
      away_goals[i] <- dog_goals
    } else {
      home_goals[i] <- dog_goals
      away_goals[i] <- fav_goals
    }
  }
  
  fixtures$home_goals <- home_goals
  fixtures$away_goals <- away_goals
  
  all_matches <- bind_rows(results, fixtures)
  
  final_date <- all_matches$match_date[nrow(all_matches)]
  standings <- get_standings(all_matches, season, final_date)
  champions <- standings[1, "team"]
  
  return(list(standings = standings, champions = champions))
}



# ---- Function to get title odds on a date by running many sims ----

get_title_odds <- function(df, season, date, n) {

  start_time <- Sys.time()
  
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
  
  end_time <- Sys.time()
  elapsed <- round(difftime(end_time, start_time, units = "secs"), 2)
  cat("Time elapsed:", elapsed, "seconds\n")
  
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

sim_all_seasons <- function(df, games_left_df, n = 10000, start_date = '1900-01-01') {

  start_date <- as.Date(start_date)
  processed_seasons <- character()
  
  # Filter season end scenarios only
  games_left_df <- games_left_df %>%
    filter(min_games_left <= 5 & match_date >= start_date & max_games_left > 0) %>%
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

# Add score matrices
matches <- matches %>%
  mutate(
    home_net_diff = home_elo - away_elo + home_advantage,
    fav_net_diff = abs(home_net_diff),
    is_home_fav = home_net_diff >= 0,
    score_matrix = map(fav_net_diff, ~ get_odds_matrix(.x, alphas, sizes, max_goals = 10))
  )

sim_all_seasons(matches, games_left, n = 10000, start_date = '2017-04-15')

plan(sequential) # Reset parallel processing
