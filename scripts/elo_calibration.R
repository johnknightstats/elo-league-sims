
###################################
### ClubElo Ratings Calibration ###
###################################

# See how the Elo ratings correlate with actual results

# ---- Import libraries ----

library(tidyverse)
library(here)

# ---- Load utility functions ----

source(here("utils", "elo_helper_functions.R"))
source(here("utils", "compute_elo_columns.R"))

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

# Calculate the Elo columns
matches <- compute_elo_columns(matches)


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
