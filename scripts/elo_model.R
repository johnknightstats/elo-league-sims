
########################
### Elo Scores Model ###
########################

# Use a bivariate nested conditional Poisson model to generate home
# and away goals based on difference in Elo ratings.

# From observation, it seems that favorites' goals when generated independently
# have a greater chance of 0 and maybe also a slightly greater chance of 1 than 
# a Poisson distribution.

# It also seems that underdogs are slightly more likely to achieve the same score
# as the favorite, conditional on the favorite's score, and slightly less likely
# to score one goal more than the favorite.

# Therefore, I will employ the following algorithm to calculate the probability
# distribution for the home and away score for each game:

# 1. Calculate the expected goals for both teams from net elo diff
# using degree 2 polynomials.

# 2. Get Poisson distribution for fav from expected goals.

# 3. Give fav=0 and fav=1 a boost based on some parameters.

# 4. Proportionally readjust all fav probs so that they sum to 1.

# 5. Get Poisson distribution for underdog from expected goals.

# 6. Get some proportion of the probability underdog scores fav + 1 goals,
# and move it to equal with fav goals (i.e. draw).

# ---- Import libraries ----

library(tidyverse)
library(here)
library(rlang)

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

# Calculate the Elo columns 
matches <- compute_elo_columns(matches)

# Calculate net elo ratings and frame as fav v dog
matches_model <- matches %>%
  mutate(
    home_net = home_elo + home_advantage,
    away_net = away_elo,
    net_elo_diff = home_net - away_net,
    fav_goals = if_else(net_elo_diff >= 0, home_goals, away_goals),
    dog_goals = if_else(net_elo_diff >= 0, away_goals, home_goals),
    fav_net_diff = abs(net_elo_diff),
    fav_win = as.numeric(fav_goals > dog_goals),
    dog_win = as.numeric(dog_goals > fav_goals),
    draw = 1 - fav_win - dog_win
  )

# ---- Fit the Elo model ----

# Parameters:

# a, b, c, d, e, f = get expected goals for each team from net 
# elo diff using polynomial ax + bx^2 + c and dx + ex^2 + f

# g = proportion to inflate probability of fav scoring 0

# h = proportion to inflate probability of fav scoring 1

# i = if underdog scores 1 more than fav, probability that this gets converted
# to same score as fav. e.g. if d = 0.1, then if fav scored 1 and underdog has
# 0.2 probability of scoring 2, this becomes 0.18 and the 0.02 gets added to p(1)

# Fit polynomial expected goals models
fit_fav <- lm(fav_goals ~ fav_net_diff + I(fav_net_diff^2), data = matches_model)
fit_dog <- lm(dog_goals ~ fav_net_diff + I(fav_net_diff^2), data = matches_model)

a <- coef(fit_fav)["fav_net_diff"]
b <- coef(fit_fav)["I(fav_net_diff^2)"]
c <- coef(fit_fav)["(Intercept)"]

d <- coef(fit_dog)["fav_net_diff"]
e <- coef(fit_dog)["I(fav_net_diff^2)"]
f <- coef(fit_dog)["(Intercept)"]


get_odds <- function(fav_net_diff, g, h, i) {
  
  fav_exp_goals <- a * fav_net_diff + b * fav_net_diff^2 + c
  dog_exp_goals <- d * fav_net_diff + e * fav_net_diff^2 + f
  
  # Inflation of 0 and 1
  fav_probs <- dpois(0:9, fav_exp_goals)
  fav_probs[10] <- fav_probs[10] + (1 - ppois(9, fav_exp_goals))  # 10+ goals
  fav_probs[1] <- fav_probs[1] * (1 + g)
  fav_probs[2] <- fav_probs[2] * (1 + h)
  fav_probs <- fav_probs / sum(fav_probs)
  
  # Dog goal distribution
  dog_probs <- dpois(0:9, dog_exp_goals)
  dog_probs[10] <- dog_probs[10] + (1 - ppois(9, dog_exp_goals))  # 10+ goals
  
  joint_matrix <- outer(fav_probs, dog_probs)
  
  # Compute win probability for favorite (x > y)
  f_win <- sum(joint_matrix[lower.tri(joint_matrix)])
  
  # Apply shift from dog=x+1 to draw
  for (x in 0:8) {
    p <- joint_matrix[x+1, x+2]
    p_adj <- p * i
    joint_matrix[x+1, x+2] <- joint_matrix[x+1, x+2] - p_adj
    joint_matrix[x+1, x+1] <- joint_matrix[x+1, x+1] + p_adj
    if (x == 0) { # also take some from 2
      q <- joint_matrix[x+1, x+3]
      q_adj <- q * i * 0.33
      joint_matrix[x+1, x+3] <- joint_matrix[x+1, x+3] - q_adj
      joint_matrix[x+1, x+1] <- joint_matrix[x+1, x+1] + q_adj
    }
  }
  
  d_win <- sum(joint_matrix[upper.tri(joint_matrix)])
  draw <- sum(diag(joint_matrix))
  
  # Expected goals
  exp_fav_goals <- sum(0:9 * rowSums(joint_matrix))
  exp_dog_goals <- sum(0:9 * colSums(joint_matrix))
  
  return(list(
    fav_p = f_win,
    dog_p = d_win,
    draw_p = draw,
    fav_xg = exp_fav_goals,
    dog_xg = exp_dog_goals,
    scores = joint_matrix
  ))
}



# Define loss function


loss_function <- function(params, data) {
  g <- params[1]
  h <- params[2]
  i <- params[3]
  
  if (any(c(g, h, i) < 0) || g > 1 || h > 1 || i > 1) return(Inf)
  
  losses <- mapply(function(net_diff, y_fav, y_dog) {
    probs <- get_odds(net_diff, g, h, i)
    
    eps <- 1e-15
    score_prob <- probs$scores[y_fav + 1, y_dog + 1]  # actual score
    -log(max(score_prob, eps))
  },
  data$fav_net_diff,
  data$fav_goals,
  data$dog_goals
  )
  
  loss <- mean(losses)
  cat(sprintf("g=%.4f h=%.4f i=%.4f loss=%.6f\n", g, h, i, loss))
  flush.console()
  return(loss)
}


# Starting values
start_params <- c(g = 0.15, h = 0.05, i = 0.1)

fit <- optim(
  par = start_params,
  fn = loss_function,
  data = matches_model,
  method = "L-BFGS-B",
  lower = c(0, 0, 0),
  upper = c(1, 1, 1)
)

# Extract fitted values
g_fit <- fit$par[1]
h_fit <- fit$par[2]
i_fit <- fit$par[3]


# ---- Add fitted probabilities and scorelines to matches_model ----

matches_model <- matches_model %>%
  mutate(
    fav_exp_goals = a * fav_net_diff + b * fav_net_diff^2 + c,
    dog_exp_goals = d * fav_net_diff + e * fav_net_diff^2 + f
  )

scoreline_labels <- as.vector(outer(0:3, 0:3, paste, sep = "-"))

matches_model <- matches_model %>%
  mutate(
    score_data = map(fav_net_diff, ~ get_odds(.x, g_fit, h_fit, i_fit)),
    fav_p = map_dbl(score_data, "fav_p"),
    dog_p = map_dbl(score_data, "dog_p"),
    draw_p = map_dbl(score_data, "draw_p"),
    score_mat = map(score_data, ~ .x$scores[1:4, 1:4])
  )


# Add scoreline probabilities 0-0 to 3-3
for (x in 0:3) {
  for (y in 0:3) {
    label <- paste0(x, "-", y)
    matches_model[[label]] <- map_dbl(matches_model$score_mat, ~ .x[x + 1, y + 1])
  }
}

# Drop intermediate score_mat column
matches_model <- matches_model %>% select(-score_mat, -score_data)

# ---- Save parameters and function for later use ----

elo_model <- list(
  a = a, b = b, c = c,
  d = d, e = e, f = f,
  g = g_fit, h = h_fit, i = i_fit,
  get_odds = get_odds
)

saveRDS(elo_model, file = here("data", "elo_model.rds"))


# ---- Calibrate Elo model ---- 

# Compare deciles of predicted win versus observed

deciles <- matches_model %>%
  mutate(
    win_decile = ntile(fav_p, 10)
  ) %>%
  group_by(win_decile) %>%
  summarise(
    mean_pred_win = mean(fav_p),
    actual_win_rate = mean(fav_win),
    n = n()
  )

ggplot(deciles, aes(x = mean_pred_win, y = actual_win_rate)) +
  geom_point(color = "dodgerblue3", size = 3) +
  geom_errorbar(aes(ymin = actual_win_rate - 1.96 * sqrt(actual_win_rate*(1-actual_win_rate)/n),
                    ymax = actual_win_rate + 1.96 * sqrt(actual_win_rate*(1-actual_win_rate)/n)),
                width = 0.01, color = "gray60") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration: Predicted vs Actual Win Rate",
    x = "Mean Predicted Win Probability",
    y = "Actual Win Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# ---- Compare predicted W-D-L versus actual results ----

plot_data <- matches_model %>%
  summarise(
    actual_win = mean(fav_win),
    actual_draw = mean(draw),
    actual_loss = mean(dog_win),
    pred_win = mean(fav_p),
    pred_draw = mean(draw_p),
    pred_loss = mean(dog_p)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("type", "outcome"),
    names_sep = "_",
    values_to = "value"
  )

ggplot(plot_data, aes(x = outcome, y = value, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = c("actual" = "dodgerblue3", "pred" = "goldenrod2")) +
  labs(
    title = "Predicted vs Actual Match Outcomes (Favorite Perspective)",
    x = "Outcome",
    y = "Proportion"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# ---- Compare predicted scorelines with actual scorelines ----

matches_model <- matches_model %>%
  mutate(score_label = paste(fav_goals, dog_goals, sep = "-"))

scoreline_cols <- colnames(matches_model) %>% 
  intersect(as.vector(outer(0:3, 0:3, paste, sep = "-")))

pred_long <- matches_model %>%
  summarise(across(all_of(scoreline_cols), mean)) %>%
  pivot_longer(everything(), names_to = "scoreline", values_to = "value") %>%
  mutate(type = "pred")

actual_long <- matches_model %>%
  count(score_label) %>%
  mutate(
    value = n / nrow(matches_model),  # normalize by full dataset
    scoreline = score_label,
    type = "actual"
  ) %>%
  filter(scoreline %in% scoreline_cols) %>%
  select(scoreline, value, type)



plot_data <- bind_rows(pred_long, actual_long)

ggplot(plot_data, aes(x = scoreline, y = value, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width = 0.5) +
  scale_fill_manual(values = c("actual" = "dodgerblue3", "pred" = "goldenrod2")) +
  labs(
    title = "Predicted vs Actual Scorelines (0–0 to 3–3)",
    x = "Scoreline (Fav Goals – Dog Goals)",
    y = "Proportion"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


