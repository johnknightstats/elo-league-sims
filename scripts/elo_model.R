
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


# Fit a, b, c, d, e, f

# Fav
fit_poly <- lm(fav_goals ~ fav_net_diff + I(fav_net_diff^2), data = matches_model)
a <- coef(fit_poly)["fav_net_diff"]
b <- coef(fit_poly)["I(fav_net_diff^2)"]
c <- coef(fit_poly)["(Intercept)"]

# Dog
fit_poly <- lm(dog_goals ~ fav_net_diff + I(fav_net_diff^2), data = matches_model)
d <- coef(fit_poly)["fav_net_diff"]
e <- coef(fit_poly)["I(fav_net_diff^2)"]
f <- coef(fit_poly)["(Intercept)"]

# Add expected goals to data frame
get_fav_exp_goals <- function(net_diff) {
  xg <- a * net_diff + b * net_diff ^ 2 + c
  return(xg)
}
get_dog_exp_goals <- function(net_diff) {
  xg <- d * net_diff + e * net_diff ^ 2 + f
  return(xg)
}

matches_model$fav_exp_goals <- sapply(matches_model$fav_net_diff, get_fav_exp_goals)
matches_model$dog_exp_goals <- sapply(matches_model$fav_net_diff, get_dog_exp_goals)


# Fit g, h, i

get_odds <- function(fav_exp_goals, dog_exp_goals, g, h, i) {

  fav_probs <- dpois(0:9, fav_exp_goals)
  fav_probs[1] <- fav_probs[1] * (1 + g)
  fav_probs[2] <- fav_probs[2] * (1 + h)
  sum_prob <- sum(fav_probs)
  fav_probs <- fav_probs / sum_prob
  
  dog_probs <- dpois(0:9, dog_exp_goals)
  
  # Create 10x10 matrix of joint probabilities
  joint_matrix <- outer(fav_probs, dog_probs)
  
  # Favorite wins: x > y (upper triangle)
  f_win <- sum(joint_matrix[lower.tri(joint_matrix)])
  
  # Underdog wins: y > x
  d_win <- 0
  for (x in 0:8) {
    for (y in (x+1):9) {
      p <- joint_matrix[x+1, y+1]
      if (y == x + 1) {
        d_win <- d_win + p * (1 - i)  # Reduce by (1 - i)
      } else {
        d_win <- d_win + p
      }
    }
  }
  
  draw <- 1 - f_win - d_win
  
  return(list(fav_p = f_win, dog_p = d_win, draw_p = draw))
}

# Define loss function

loss_function <- function(params, data) {
  g <- params[1]
  h <- params[2]
  i <- params[3]
  
  # Constrain parameters to avoid invalid probabilities
  if (g < 0 || h < 0 || i < 0 || g > 1 || h > 1 || i > 1) {
    return(Inf)
  }
  
  losses <- mapply(function(fav_exp, dog_exp, y_fav, y_dog, y_draw) {
    probs <- get_odds(fav_exp, dog_exp, g, h, i)
    
    # Clip to avoid log(0)
    eps <- 1e-15
    fav_p <- max(min(probs$fav_p, 1 - eps), eps)
    dog_p <- max(min(probs$dog_p, 1 - eps), eps)
    draw_p <- max(min(probs$draw_p, 1 - eps), eps)
    
    - (y_fav * log(fav_p) + y_dog * log(dog_p) + y_draw * log(draw_p))
  },
  data$fav_exp_goals,
  data$dog_exp_goals,
  data$fav_win,
  data$dog_win,
  data$draw
  )
  
  loss <- mean(losses)
  
  cat(sprintf("g=%.4f  h=%.4f  i=%.4f  loss=%.6f\n", g, h, i, loss))
  flush.console()
  
  return(loss)
}

# Starting values
start_params <- c(g = 0.15, h = 0.05, i = 0.1)

# Run optimization
fit <- optim(
  par = start_params,
  fn = loss_function,
  data = matches_model,
  method = "L-BFGS-B",
  lower = c(0.0001, 0.0001, 0),
  upper = c(10, 10, 1)
)

# Extract best-fit parameters
fit$par

# Add fitted probabilities to data frame
g_fit <- fit$par[1]
h_fit <- fit$par[2]
i_fit <- fit$par[3]

matches_model <- matches_model %>%
  mutate(
    pred = pmap(
      list(fav_exp_goals, dog_exp_goals),
      ~ get_odds(..1, ..2, g_fit, h_fit, i_fit)
    ),
    fav_p = map_dbl(pred, "fav_p"),
    dog_p = map_dbl(pred, "dog_p"),
    draw_p = map_dbl(pred, "draw_p")
  ) %>%
  select(-pred)

