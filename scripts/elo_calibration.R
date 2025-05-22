
###################################
### ClubElo Ratings Calibration ###
###################################

# See how the Elo ratings correlate with actual results

# ---- Import libraries ----

library(tidyverse)
library(here)
library(patchwork)

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

# ---- Inspect goal distribution vs. Elo difference ----

# First create long DF with home and away combined

homes <- data.frame(goals_for = matches$home_goals,
                    goals_against = matches$away_goals,
                    net_elo = matches$elo_diff + matches$home_advantage)
aways <- data.frame(goals_for = matches$away_goals,
                    goals_against = matches$home_goals,
                    net_elo = -matches$elo_diff - matches$home_advantage)

matches_long <- rbind(homes, aways)

# Generalized additive model
ggplot(matches_long, aes(x = net_elo, y = goals_for)) +
  geom_jitter(width = 10, height = 0.1, alpha = 0.2, color = "dodgerblue3") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "goldenrod2", se = FALSE) +
  labs(
    title = "Goals Scored vs Net Elo Difference (GAM Fit)",
    x = "Net Elo Difference",
    y = "Goals Scored"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# ---- Compare observed goals vs. Poisson distribution ----

# Calculate observed frequencies
goal_counts <- matches_long %>%
  count(goals_for) %>%
  rename(observed = n)

# Add Poisson probabilities
lambda_all <- mean(matches_long$goals_for)
total_n <- sum(goal_counts$observed)

goal_counts <- goal_counts %>%
  mutate(
    poisson_prob = dpois(goals_for, lambda_all),
    poisson = poisson_prob,
    observed = observed / total_n  # convert to relative frequency
  )

# Convert to long format for plotting
goal_plot_data <- goal_counts %>%
  pivot_longer(cols = c("observed", "poisson"), names_to = "type", values_to = "rel_freq")

ggplot(goal_plot_data, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = c("observed" = "dodgerblue3", "poisson" = "goldenrod2")) +
  labs(
    title = "Observed vs Poisson Distribution of Goals Scored",
    x = "Goals Scored",
    y = "Relative Frequency"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# ---- Compare observed vs Poisson, conditional on opponent goals ----

plot_conditional <- function(opp_goals) {
  subset_df <- matches_long %>% filter(goals_against == opp_goals)
  lambda <- mean(subset_df$goals_for)
  total_n <- nrow(subset_df)
  
  observed <- subset_df %>%
    count(goals_for) %>%
    rename(observed = n)
  
  observed <- observed %>%
    mutate(
      poisson_prob = dpois(goals_for, lambda),
      poisson = poisson_prob,
      observed = observed / total_n  # convert to relative frequency
    ) %>%
    pivot_longer(cols = c("observed", "poisson"), names_to = "type", values_to = "rel_freq")
  
  ggplot(observed, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
    geom_col(position = position_dodge(width=0.6), width=0.5) +
    scale_fill_manual(values = c("observed" = "dodgerblue3", "poisson" = "goldenrod2")) +
    labs(
      title = paste("Goals Scored | Opponent Scored", opp_goals),
      x = "Goals Scored",
      y = "Relative Frequency",
      fill = element_blank()
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key.size=unit(0.2, 'cm'))
}

plot_conditional(0) + plot_conditional(1) + plot_conditional(2) + plot_conditional(3)

# We see:

# - zero inflation
# - draw inflation