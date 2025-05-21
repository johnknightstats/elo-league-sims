
####################################################
### Add new columns for Elo and calculate values ###
####################################################

compute_elo_columns <- function(matches) {
  
  # matches should have columns:
  
  # match_date
  # kickoff_local_time
  # season
  # home_team
  # away_team
  # home_goals
  # away_goals
  # home_elo
  # away_elo
  
  # ---- Add elo_diff, result, margin of victory ----
  
  matches$elo_diff <- matches$home_elo - matches$away_elo
  matches$result <- ifelse(matches$home_goals > matches$away_goals, 1,
                           ifelse(matches$home_goals < matches$away_goals, 0, 0.5))
  matches$margin <- matches$home_goals - matches$away_goals
  
  # ---- Calculate predictions and daily home advantage factor ----
  
  matches <- matches %>% arrange(match_date) # Ensure correct order
  
  # Create require columns for Elo data
  n <- nrow(matches)
  matches$home_advantage <- numeric(n)
  matches$prediction <- numeric(n)
  matches$elo_change <- numeric(n)
  
  # Initialize variables for loop
  current_date <- matches$match_date[1]
  home_adv <- 0
  daily_indices <- c()
  
  for (i in 1:n) {
    
    daily_indices <- c(daily_indices, i)
    matches$home_advantage[i] <- home_adv
    matches$prediction[i] <- get_prediction(matches$elo_diff[i], home_adv)
    
    matches$elo_change[i] <- get_elo_change(
      matches$prediction[i],
      matches$result[i],
      abs(matches$margin[i])
    )
    
    if (i == n | matches$match_date[i+1] != current_date) {
      
      daily_elo_sum <- sum(matches$elo_change[daily_indices])
      home_adv <- home_adv + 0.075 * daily_elo_sum
      
      daily_indices <- c()
      if (i < n) current_date <- matches$match_date[i+1]
      
    }
  }
  return(matches)
}