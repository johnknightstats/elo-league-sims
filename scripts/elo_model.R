
########################
### Elo Scores Model ###
########################

# Use a bivariate nested conditional Negative Binomial model to generate home
# and away goals based on difference in Elo ratings.

# From data inspection (see elo_calibration.R), it seems that favorites' goals 
# when generated independently follow a negative binomial distribution with
# dispersion parameter ~ 35.

# However, underdog scores when viewed conditional on the favourite's score
# exhibit a higher dispersion with inflation of the 'draw' scores.

# Therefore, I will employ the following algorithm to calculate the probability
# distribution for the home and away score for each game:

# 1. Calculate the expected goals for the favourite from net elo diff using
# a simple quadratic model from Net Elo Diff (Elo Diff plus Home Advantage).

# 2. Get distribution for fav from expected goals using NB distribution.

# 3. Calculate expected goals for the underdog using a model that accounts for
# fav goals as well as Net Elo Diff.

# 4. Get distribution for the underdog using NB with an additional adjustment
# for the 'draw' scoreline and a counteradjustment to ensure expected goals remains the same.


# ---- Import libraries ----

library(tidyverse)
library(here)
library(rlang)

# Set a custom color palette
my_palette <- c("#233D4D", "#FF9F1C", "#41EAD4", "#FDFFFC", "#F71735")

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

# Fit expected goals models
fav_model <- lm(fav_goals ~ fav_net_diff + I(fav_net_diff^2), data = matches_model)

matches_model <- matches_model %>%
  mutate(fav_goals_cat = factor(
    ifelse(fav_goals >= 3, "3+", as.character(fav_goals)),
    levels = c("0", "1", "2", "3+")
  ))

dog_model <- lm(dog_goals ~ fav_goals_cat * (fav_net_diff + I(fav_net_diff^2)), data = matches_model)

# Extract coefficients
a <- coef(fav_model)["fav_net_diff"]
b <- coef(fav_model)["I(fav_net_diff^2)"]
c <- coef(fav_model)["(Intercept)"]

d <- coef(dog_model)["fav_goals_cat1"]
e <- coef(dog_model)["fav_goals_cat2"]
f <- coef(dog_model)["fav_goals_cat3+"]

g <- coef(dog_model)["fav_net_diff"]
h <- coef(dog_model)["I(fav_net_diff^2)"]
i <- coef(dog_model)["(Intercept)"]

j <- coef(dog_model)["fav_goals_cat1:fav_net_diff"]
k <- coef(dog_model)["fav_goals_cat2:fav_net_diff"]
l <- coef(dog_model)["fav_goals_cat3+:fav_net_diff"]

m <- coef(dog_model)["fav_goals_cat1:I(fav_net_diff^2)"]
n <- coef(dog_model)["fav_goals_cat2:I(fav_net_diff^2)"]
o <- coef(dog_model)["fav_goals_cat3+:I(fav_net_diff^2)"]

# Function to calculate expected goals for favorite and dog
get_expected_goals <- function(fav_net_diff, fav_goals) {
  fav_xg <- a * fav_net_diff + b * fav_net_diff^2 + c
  
  cat_label <- factor(
    ifelse(fav_goals >= 3, "3+", as.character(fav_goals)),
    levels = c("0", "1", "2", "3+")
  )
  
  # Base category effect (intercept shift)
  cat_effect <- case_when(
    cat_label == "0" ~ 0,
    cat_label == "1" ~ d,
    cat_label == "2" ~ e,
    cat_label == "3+" ~ f
  )
  
  # Linear interaction
  lin_int <- case_when(
    cat_label == "0" ~ 0,
    cat_label == "1" ~ j * fav_net_diff,
    cat_label == "2" ~ k * fav_net_diff,
    cat_label == "3+" ~ l * fav_net_diff
  )
  
  # Quadratic interaction
  quad_int <- case_when(
    cat_label == "0" ~ 0,
    cat_label == "1" ~ m * fav_net_diff^2,
    cat_label == "2" ~ n * fav_net_diff^2,
    cat_label == "3+" ~ o * fav_net_diff^2
  )
  
  dog_xg <- i + g * fav_net_diff + h * fav_net_diff^2 + cat_effect + lin_int + quad_int
  
  return(list(fav = fav_xg, dog = dog_xg))
}

           

# Define loss function robust to errors
loss_function <- function(params, data) {
  alpha_0 <- params[1]
  alpha_1 <- params[2]
  alpha_2 <- params[3]
  alpha_3 <- params[4]
  size_0 <- params[5]
  size_1 <- params[6]
  size_2 <- params[7]
  size_3 <- params[8]
  
  alphas <- c(alpha_0, alpha_1, alpha_2, alpha_3)
  sizes <- c(size_0, size_1, size_2, size_3)
  
  # Check parameter bounds and validity
  if (any(!is.finite(alphas)) || any(!is.finite(sizes)) ||
      any(alphas < 0) || any(alphas > 0.2) || any(sizes <= 0)) {
    return(Inf)
  }
  
  losses <- mapply(function(fav_net_diff, fav_goals, dog_goals) {
    # Predict favorite expected goals
    fav_xg <- a * fav_net_diff + b * fav_net_diff^2 + c
    
    # Predict underdog expected goals, conditional on fav_goals
    goal_cat <- ifelse(fav_goals >= 3, "3+", as.character(fav_goals))
    dog_xg <- switch(goal_cat,
                     "0" = i + g * fav_net_diff + h * fav_net_diff^2,
                     "1" = i + d + g * fav_net_diff + h * fav_net_diff^2,
                     "2" = i + e + g * fav_net_diff + h * fav_net_diff^2,
                     "3+" = i + f + g * fav_net_diff + h * fav_net_diff^2)
    
    # Interaction terms
    dog_xg <- dog_xg + switch(goal_cat,
                              "0" = j * fav_net_diff + m * fav_net_diff^2,
                              "1" = k * fav_net_diff + n * fav_net_diff^2,
                              "2" = l * fav_net_diff + o * fav_net_diff^2,
                              "3+" = 0)
    
    # Select pointmass and dispersion parameter based on fav_goals
    idx <- ifelse(fav_goals >= 3, 4, fav_goals + 1)
    alpha <- alphas[idx]
    size <- sizes[idx]
    
    # Compute underdog goal probabilities with pointmass at dog_goals == fav_goals
    k <- fav_goals
    denom <- (1 - alpha)
    if (!is.finite(dog_xg) || denom <= 0) return(Inf)
    
    mu_0 <- (dog_xg - alpha * k) / denom
    if (!is.finite(mu_0) || mu_0 <= 0) return(Inf)
    
    probs <- (1 - alpha) * dnbinom(0:9, size = size, mu = mu_0)
    probs <- c(probs, max(0, 1 - sum(probs)))  # 10+ bucket
    probs[k + 1] <- probs[k + 1] + alpha
    probs[-(k + 1)] <- (1 - alpha) * probs[-(k + 1)]
    
    if (any(!is.finite(probs)) || any(probs < 0) || any(is.na(probs))) return(Inf)
    
    eps <- 1e-15
    -log(max(probs[dog_goals + 1], eps))
    
  }, data$fav_net_diff, data$fav_goals, data$dog_goals)
  
  loss <- mean(losses)
  if (!is.finite(loss)) return(Inf)
  cat(sprintf("loss = %.6f\n", loss))
  flush.console()
  return(loss)
}




start_params <- c(rep(0.01, 4), rep(10, 4)) # alpha (for pointmass) and dispersion parameter

fit <- optim(
  par = start_params,
  fn = loss_function,
  data = matches_model,
  method = "L-BFGS-B",
  lower = c(rep(0, 4), rep(0.1, 4)),
  upper = c(rep(0.15, 4), rep(100000, 4))
)

fitted_alphas <- setNames(fit$par[1:4], c("0", "1", "2", "3+"))
fitted_sizes <- setNames(fit$par[5:8], c("0", "1", "2", "3+"))


# ---- Add fitted probabilities and scorelines to matches_model ----

# Function to calculate expected goals for favorite and dog
get_expected_goals <- function(fav_net_diff, fav_goals) {
  fav_xg <- a * fav_net_diff + b * fav_net_diff^2 + c
  
  cat_label <- factor(
    ifelse(fav_goals >= 3, "3+", as.character(fav_goals)),
    levels = c("0", "1", "2", "3+")
  )
  
  # Base category effect (intercept shift)
  cat_effect <- case_when(
    cat_label == "0" ~ 0,
    cat_label == "1" ~ d,
    cat_label == "2" ~ e,
    cat_label == "3+" ~ f
  )
  
  # Linear interaction
  lin_int <- case_when(
    cat_label == "0" ~ 0,
    cat_label == "1" ~ j * fav_net_diff,
    cat_label == "2" ~ k * fav_net_diff,
    cat_label == "3+" ~ l * fav_net_diff
  )
  
  # Quadratic interaction
  quad_int <- case_when(
    cat_label == "0" ~ 0,
    cat_label == "1" ~ m * fav_net_diff^2,
    cat_label == "2" ~ n * fav_net_diff^2,
    cat_label == "3+" ~ o * fav_net_diff^2
  )
  
  dog_xg <- i + g * fav_net_diff + h * fav_net_diff^2 + cat_effect + lin_int + quad_int
  
  return(list(fav = fav_xg, dog = dog_xg))
}

get_odds_matrix <- function(fav_net_diff, alphas, sizes, max_goals = 10) {
  
  fav_xg <- get_expected_goals(fav_net_diff, fav_goals = 0)$fav # fav_goals is irrelevant here but needs a value
  
  fav_probs <- dnbinom(0:(max_goals - 1), mu = fav_xg, size = 35)
  fav_probs <- c(fav_probs, 1 - sum(fav_probs))  # 10+ lumped

  joint_matrix <- matrix(0, nrow = max_goals + 1, ncol = max_goals + 1)
  
  # Loop over all possible fav_goals (k)
  for (k in 0:max_goals) {
    # Get expected goals for dog given this fav_goals
    xg <- get_expected_goals(fav_net_diff, k)
    dog_xg <- xg$dog
    
    # Determine adjustment category
    k_cat <- ifelse(k >= 3, "3+", as.character(k))
    alpha <- alphas[[k_cat]]
    size <- sizes[[k_cat]]
    
    # Unadjusted dog_probs
    dog_probs <- dnbinom(0:(max_goals - 1), mu = dog_xg, size = size)
    dog_probs <- c(dog_probs, 1 - sum(dog_probs))
    
    # Apply pointmass adjustment at dog_goals == fav_goals (i.e., k)
    dog_probs[k + 1] <- alpha + (1 - alpha) * dog_probs[k + 1]
    dog_probs[-(k + 1)] <- (1 - alpha) * dog_probs[-(k + 1)]
    
    # Multiply row by fav_probs[k+1]
    joint_matrix[k + 1, ] <- fav_probs[k + 1] * dog_probs
  }
  
  return(joint_matrix)
}

matches_model <- matches_model %>%
  mutate(
    score_mat = map(fav_net_diff, ~ get_odds_matrix(.x, alphas = fitted_alphas, sizes = fitted_sizes,
                                                    max_goals = 10)),
    fav_p = map_dbl(score_mat, ~ sum(.x[lower.tri(.x)])),
    draw_p = map_dbl(score_mat, ~ sum(diag(.x))),
    dog_p = map_dbl(score_mat, ~ sum(.x[upper.tri(.x)])),
    total_p = fav_p + draw_p + dog_p)



# Add scoreline probabilities 0-0 to 3-3
for (x in 0:3) {
  for (y in 0:3) {
    label <- paste0(x, "-", y)
    matches_model[[label]] <- map_dbl(matches_model$score_mat, ~ .x[x + 1, y + 1])
  }
}


# ---- Save parameters and function for later use ----

elo_model <- list(
  a = a, b = b, c = c,
  d = d, e = e, f = f,
  g = g, h = h, i = i,
  j = j, k = k, l = l,
  m = m, n = n, o = o,
  alphas = fitted_alphas,
  sizes = fitted_sizes,
  get_odds_matrix = get_odds_matrix,
  get_expected_goals = get_expected_goals
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

deciles_new <- ggplot(deciles, aes(x = mean_pred_win, y = actual_win_rate)) +
  geom_point(color = my_palette[1], size = 3) +
  geom_errorbar(aes(ymin = actual_win_rate - 1.96 * sqrt(actual_win_rate*(1-actual_win_rate)/n),
                    ymax = actual_win_rate + 1.96 * sqrt(actual_win_rate*(1-actual_win_rate)/n)),
                width = 0.01, color = my_palette[2]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration: Predicted vs Actual Win Rate",
    x = "Mean Predicted Win Probability",
    y = "Actual Win Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

deciles_new
ggsave(filename = here("docs/viz","deciles_new.png"), deciles_new, height=4, width=6, dpi=600)


# ---- Compare predicted W-D-L versus actual results ----

plot_data <- matches_model %>%
  summarise(
    Actual_Win = mean(fav_win),
    Actual_Draw = mean(draw),
    Actual_Loss = mean(dog_win),
    Pred_Win = mean(fav_p),
    Pred_Draw = mean(draw_p),
    Pred_Loss = mean(dog_p)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Type", "Outcome"),
    names_sep = "_",
    values_to = "value"
  )

wdl <- ggplot(plot_data, aes(x = Outcome, y = value, fill = Type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "Predicted vs Actual Match Outcomes (Favourite Perspective)",
    x = "Outcome",
    y = "Proportion"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

wdl
ggsave(filename = here("docs/viz","wdl.png"), deciles_new, height=4, width=6, dpi=600)

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
  dplyr::select(scoreline, value, type)



plot_data <- bind_rows(pred_long, actual_long)

model_v_scores <- ggplot(plot_data, aes(x = scoreline, y = value, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width = 0.5) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "Predicted vs Actual Scorelines (0–0 to 3–3)",
    x = "Scoreline (Favourite First)",
    y = "Proportion"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

model_v_scores
ggsave(filename = here("docs/viz","model_v_scores.png"), model_v_scores, height=4, width=6, dpi=600)
