
###################################
### ClubElo Ratings Calibration ###
###################################

# See how the Elo ratings correlate with actual results

# ---- Import libraries ----

library(tidyverse)
library(here)

# ---- Load data ----

matches <- read_csv(here("data", "england_matches_with_elo.csv"))

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

# ---- Add elo_diff, result, margin of victory ----

matches$elo_diff <- matches$home_elo - matches$away_elo
matches$result <- ifelse(matches$home_goals > matches$away_goals, 1,
                         ifelse(matches$home_goals < matches$away_goals, 0, 0.5))
matches$margin <- matches$home_goals - matches$away_goals

#############################
### How Elo is calculated ###
#############################

# Source: http://clubelo.com/System

# Note that 1 = win, 0.5 = draw, 0 = loss

# E = 1 / (10 ^ (-d / 400) + 1) where E = expected result, d = Elo diff

# Elo_change = (R - E) * k where R = actual result, k = constant (set to 20)

# Margin of victory adjustment not clear from above source, so am 
# using https://www.eloratings.net/about. That is,

# m = 1 --> k = 20
# m = 2 --> k *= 1.5
# m = 3 --> k *= 1.75
# m >= 4 --> k *= (1.75 + (m - 3) / 8)


# Home advantage is calculated daily for each league as the sum of Elo_change
# for all matches multiplied by 0.075

# ---- Calculate predictions and daily home advantage factor ----

get_k <- function(m) {
  k = case_when(
    m <= 1 ~ 20,
    m == 2 ~ 30,
    m == 3 ~ 35,
    m >= 4 ~ 20 * (1.75 + (m - 3) / 8)
  )
  return(k)
}

get_prediction <- function(elo_diff, home_adv) {
  d <- elo_diff + home_adv
  prediction <- 1 / (10 ^ (-d / 400) + 1)
  return(prediction)
}

get_elo_change <- function(prediction, result, margin) {
  k <- get_k(margin)
  elo_change <- (result - prediction) * k
  return(elo_change)
}

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

# ---- See how predictions compare to results ----

# Bin into deciles based on predicted probability
matches <- matches[matches$season != "1946-1947",] # Burn-in first season for Elo
deciles <- matches %>%
  mutate(pred_decile = ntile(prediction, 10)) %>%
  group_by(pred_decile) %>%
  summarise(
    avg_prediction = mean(prediction),
    avg_result = mean(result),
    count = n(),
    se = sd(result) / sqrt(count),
    lower = avg_result - 1.96 * se,
    upper = avg_result + 1.96 * se
  )

ggplot(deciles, aes(x = avg_prediction, y = avg_result)) +
  geom_point(size = 3, color = "dodgerblue3") +
  geom_line(color = "dodgerblue3") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, color = "goldenrod2") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Elo Model Accuracy by Home Prediction Decile",
    x = "Mean Predicted Probability",
    y = "Mean Actual Result"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

write.csv(matches, file=here("data", "england_matches_elo_with_predictions.csv"))
