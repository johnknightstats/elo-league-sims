
###################################
### ClubElo Ratings Calibration ###
###################################

# See how the Elo ratings correlate with actual results

# ---- Import libraries ----

library(tidyverse)
library(here)
library(patchwork)
library(MASS)

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

# Set a custom color palette
my_palette <- c("#233D4D", "#FF9F1C", "#41EAD4", "#FDFFFC", "#F71735")

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

elo_deciles <- ggplot(deciles, aes(x = avg_prediction, y = avg_result)) +
  geom_point(size = 3, color = my_palette[1]) +
  geom_line(color = my_palette[1]) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, color = my_palette[2]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = my_palette[1]) +
  labs(
    title = "Elo Model Accuracy by Home Prediction Decile",
    x = "Mean Predicted Probability",
    y = "Mean Actual Result"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

elo_deciles
ggsave(filename = here("docs/viz","elo_deciles.png"), elo_deciles, 
       bg = "white", height=4, width=6, dpi=600)

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
elo_gam <- ggplot(matches_long, aes(x = net_elo, y = goals_for)) +
  geom_jitter(width = 10, height = 0.1, alpha = 0.2, color = my_palette[1]) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = my_palette[2], se = FALSE) +
  labs(
    title = "Goals Scored vs Net Elo Difference (GAM)",
    x = "Net Elo Difference",
    y = "Goals Scored"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

elo_gam
ggsave(filename = here("docs/viz","elo_gam.png"), elo_gam, 
       bg = "white", height=4, width=6, dpi=600)

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
    poisson = dpois(goals_for, lambda_all),
    observed = observed / total_n  # convert to relative frequency
  )

# Convert to long format for plotting
goal_plot_data <- goal_counts %>%
  pivot_longer(cols = c("observed", "poisson"), names_to = "type", values_to = "rel_freq")

# Plot
goal_plot_data$type <- factor(goal_plot_data$type, levels = c("observed", "poisson"),
                              labels = c("Observed", "Poisson"))

poisson_v_obs <- ggplot(goal_plot_data, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "Poisson Distribution vs Observed Goals Scored",
    x = "Goals Scored",
    y = "Relative Frequency",
    fill = NULL
  ) +
  scale_x_discrete(limits = as.character(0:6)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

poisson_v_obs
ggsave(filename = here("docs/viz","poisson_v_obs.png"), poisson_v_obs, 
       bg = "white", height=4, width=6, dpi=600)

lambda_all
var(matches_long$goals_for) # Variance is higher than mean


# ---- Now add Negative Binomial distribution for comparison ----

# Set dispersion parameter (lower = more dispersion)
size_nb <- 10

# Join and fill in missing observed values as 0
goal_counts <- goal_counts %>%
        mutate(negbin = dnbinom(goals_for, size = size_nb, mu = lambda_all))

# Convert to long format
goal_plot_data <- goal_counts %>%
  pivot_longer(cols = c("observed", "poisson", "negbin"), names_to = "type", values_to = "rel_freq")

# Plot
goal_plot_data$type <- factor(goal_plot_data$type, levels = c("observed", "poisson", "negbin"),
                              labels = c("Observed", "Poisson", "Neg. Binomial"))

poisson_v_nb_v_obs <- ggplot(goal_plot_data, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = expression("Observed vs Poisson vs Negative Binomial (" *theta* " = 10)"),
    x = "Goals Scored",
    y = "Relative Frequency",
    fill = NULL
  ) +
  scale_x_discrete(limits = as.character(0:6)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

poisson_v_nb_v_obs
ggsave(filename = here("docs/viz","poisson_v_nb_v_obs.png"), poisson_v_nb_v_obs,
       bg = "white", height=4, width=6, dpi=600)


# ---- Now set dispersion to 10,000 for demonstration ----

# Set dispersion parameter (lower = more dispersion)
size_nb <- 10000

# Join and fill in missing observed values as 0
goal_counts <- goal_counts %>%
  mutate(negbin = dnbinom(goals_for, size = size_nb, mu = lambda_all))

# Convert to long format
goal_plot_data <- goal_counts %>%
  pivot_longer(cols = c("observed", "poisson", "negbin"), names_to = "type", values_to = "rel_freq")

# Plot
goal_plot_data$type <- factor(goal_plot_data$type, levels = c("observed", "poisson", "negbin"),
                              labels = c("Observed", "Poisson", "Neg. Binomial"))

poisson_v_nb_v_obs <- ggplot(goal_plot_data, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = expression("Observed vs Poisson vs Negative Binomial (" *theta* " = 10,000)"),
    x = "Goals Scored",
    y = "Relative Frequency",
    fill = NULL
  ) +
  scale_x_discrete(limits = as.character(0:6)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

poisson_v_nb_v_obs
ggsave(filename = here("docs/viz","poisson_v_nb_v_obs_10k.png"), poisson_v_nb_v_obs,
       bg = "white", height=4, width=6, dpi=600)


# ---- Compare observed vs Poisson & NB, conditional on opponent goals ----

plot_conditional <- function(df, opp_goals, size_nb = 10) {
  subset_df <- df %>% filter(goals_against == opp_goals)
  lambda <- mean(subset_df$goals_for)
  total_n <- nrow(subset_df)
  
  observed <- subset_df %>%
    count(goals_for) %>%
    rename(observed = n)
  
  observed <- observed %>%
    mutate(
      poisson = dpois(goals_for, lambda),
      negbin = dnbinom(goals_for, size = size_nb, mu = lambda),
      observed = observed / total_n  # convert to relative frequency
    ) %>%
    pivot_longer(cols = c("observed", "poisson", "negbin"), names_to = "type", values_to = "rel_freq")
  
  observed$type <- factor(observed$type, levels = c("observed", "poisson", "negbin"),
                                labels = c("Observed", "Poisson", "Neg. Binomial"))
  
  ggplot(observed, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
    geom_col(position = position_dodge(width=0.6), width=0.5) +
    scale_fill_manual(values = my_palette) +
    scale_x_discrete(limits = as.character(0:4)) +
    scale_y_continuous(limits = c(0, 0.4)) +
    labs(
      title = paste("Goals Scored | Opponent Scored", opp_goals),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    )
}

cond_plots <- plot_conditional(matches_long, 0) + 
  plot_conditional(matches_long, 1) + 
  plot_conditional(matches_long, 2) + 
  plot_conditional(matches_long, 3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

cond_plots
ggsave(filename = here("docs/viz","cond_plots.png"), cond_plots, 
       bg="white", height=4, width=6, dpi=600)

# ---- Histogram of Elo diff distribution ----

ggplot(matches_long, aes(x=net_elo)) +
  geom_histogram(color="grey", fill=my_palette[1], boundary=0) +
  labs(title = "Histogram of Net Elo Difference",
       x = "Elo Difference") +
  theme_minimal()

# ---- Conditional comparisons for close games (not big favs)

close_elo <- subset(matches_long, net_elo >= -100 & net_elo <= 100)

cond_plots <- plot_conditional(close_elo, 0) + 
  plot_conditional(close_elo, 1) + 
  plot_conditional(close_elo, 2) + 
  plot_conditional(close_elo, 3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

cond_plots

# We see:

# - NB is better than Poisson with the overall distribution
# - It is not so clear in the conditional distribution
# - NB still under-predicts draws
# - In close games, NB is better when opponent scores 0 but otherwise Poisson is good
# - Therefore, the dispersion parameter may vary based on Elo difference


# ---- Conditional distribution on fitted Elo

# Need to fit the model predicting each team's goals based on Elo, then
# compare that in the fav distribution and underdog conditional distribution.

# Not going to account for varying goal level across teams/seasons. That's something a 
# more advanced model would require.

favs <- subset(matches_long, net_elo > 0)

# Fit a quadratic model
fav_model <- lm(goals_for ~ net_elo + I(net_elo^2), data = favs)

favs <- favs %>%
  mutate(goals_for_cat = factor(
    ifelse(goals_for >= 3, "3+", as.character(goals_for)),
    levels = c("0", "1", "2", "3+")
  ))

dog_model <- lm(goals_against ~ goals_for_cat * (net_elo + I(net_elo^2)), data = favs)

a <- coef(fav_model)["net_elo"]
b <- coef(fav_model)["I(net_elo^2)"]
c <- coef(fav_model)["(Intercept)"]

# Use the models to add expected goals for fav and dog
favs <- favs %>%
  mutate(
    fav_exp_goals = a * net_elo + b * net_elo^2 + c,
    dog_exp_goals = predict(dog_model, newdata = .)
  )


# Plot observed, Poisson, and NB for fav goals
size_nb <- 35

max_goals <- max(favs$goals_for, 9)
goal_range <- 0:max_goals

observed_freq <- favs %>%
  count(goals_for) %>%
  complete(goals_for = goal_range, fill = list(n = 0)) %>%
  mutate(observed = n / sum(n)) %>%
  dplyr::select(goals_for, observed)

# Calculate average Poisson and NB probabilities for each goal count
poisson_probs <- sapply(goal_range, function(k) {
  mean(dpois(k, favs$fav_exp_goals))
})

negbin_probs <- sapply(goal_range, function(k) {
  mean(dnbinom(k, size = size_nb, mu = favs$fav_exp_goals))
})

goal_plot_data <- tibble(
  goals_for = goal_range,
  observed = observed_freq$observed,
  poisson = poisson_probs,
  negbin = negbin_probs
) %>%
  pivot_longer(cols = c("observed", "poisson", "negbin"), names_to = "type", values_to = "rel_freq")

# Create plot
goal_plot_data$type <- factor(goal_plot_data$type, levels = c("observed", "poisson", "negbin"),
                              labels = c("Observed", "Poisson", "Neg. Binomial"))

fav_goals_fitted <- ggplot(goal_plot_data, aes(x = as.factor(goals_for), y = rel_freq, fill = type)) +
  geom_col(position = position_dodge(width=0.7), width=0.5) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "Favourite Goal Distributions from Fitted Elo Model",
    x = "Goals Scored",
    y = "Relative Frequency",
    fill = NULL
  ) +
  # Plot
  scale_x_discrete(limits = as.character(0:5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

fav_goals_fitted

ggsave(filename = here("docs/viz", "fav_goals_fitted.png"), fav_goals_fitted,
       bg="white", height = 4, width = 6, dpi = 600)

# ---- Now plot the underdog goals conditional on fav goals ----

plot_conditional_underdog <- function(df, fav_goals, size_nb = 5) {
  subset_df <- df %>% filter(goals_for == fav_goals)
  total_n <- nrow(subset_df)
  
  # Observed frequency
  observed <- subset_df %>%
    count(goals_against) %>%
    complete(goals_against = 0:max(goals_against), fill = list(n = 0)) %>%
    mutate(observed = n / total_n)
  
  # Create predicted distributions using row-level dog_exp_goals
  support <- 0:max(subset_df$goals_against)  # range of possible goals
  probs_poisson <- rep(0, length(support))
  probs_negbin  <- rep(0, length(support))
  
  for (i in 1:nrow(subset_df)) {
    mu <- subset_df$dog_exp_goals[i]
    probs_poisson <- probs_poisson + dpois(support, lambda = mu)
    probs_negbin  <- probs_negbin  + dnbinom(support, size = size_nb, mu = mu)
  }
  
  # Normalize to get average predicted probability
  probs_poisson <- probs_poisson / total_n
  probs_negbin  <- probs_negbin  / total_n
  
  # Assemble results
  pred_df <- tibble(
    goals_against = support,
    poisson = probs_poisson,
    negbin = probs_negbin
  )
  
  full_df <- observed %>%
    left_join(pred_df, by = "goals_against") %>%
    pivot_longer(cols = c("observed", "poisson", "negbin"),
                 names_to = "type", values_to = "rel_freq") %>%
    mutate(type = factor(type, levels = c("observed", "poisson", "negbin"),
                         labels = c("Observed", "Poisson", "Neg. Binomial")))
  
  ggplot(full_df, aes(x = as.factor(goals_against), y = rel_freq, fill = type)) +
    geom_col(position = position_dodge(width = 0.6), width = 0.5) +
    scale_fill_manual(values = my_palette) +
    scale_x_discrete(limits = as.character(0:4)) +
    scale_y_continuous(limits = c(0, 0.4)) +
    labs(
      title = paste("Underdog Goals | Favorite Scored", fav_goals),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    )
}


underdog_cond_plots <- plot_conditional_underdog(favs, 0, size_nb = 5) +
  plot_conditional_underdog(favs, 1, size_nb = 5) +
  plot_conditional_underdog(favs, 2, size_nb = 5) +
  plot_conditional_underdog(favs, 3, size_nb = 5) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

underdog_cond_plots

ggsave(filename = here("docs/viz", "underdog_cond_plots_disperson_5.png"),
       bg="white", underdog_cond_plots, height = 4, width = 6, dpi = 600)

# When fav scores 0, NB dispersion parameter is ~ 5. When fav scores 1 or more,
# dispersion parameter is much higher. So we need different parameters for each score.

# Define negative log-likelihood function
negloglik_nb <- function(size, y, mu) {
  -sum(dnbinom(y, size = size, mu = mu, log = TRUE))
}

# Fit dispersion for each fav_goals group
dispersion_parameters <- favs %>%
  filter(goals_for %in% 0:3) %>%
  group_by(fav_goals = goals_for) %>%
  summarise(
    size = {
      y <- goals_against
      mu <- dog_exp_goals
      result <- optim(par = 1,
                      fn = function(size) negloglik_nb(size, y = y, mu = mu),
                      method = "Brent",
                      lower = 0.01,
                      upper = 10000)
      result$par
    },
    .groups = "drop"
  )

dispersion_parameters

# Now make conditional plots using the dispersion parameters


underdog_cond_plots_fitted <- plot_conditional_underdog(favs, 0, dispersion_parameters$size[1]) +
  plot_conditional_underdog(favs, 1, dispersion_parameters$size[2]) +
  plot_conditional_underdog(favs, 2, dispersion_parameters$size[3]) +
  plot_conditional_underdog(favs, 3, dispersion_parameters$size[4]) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

underdog_cond_plots_fitted

ggsave(filename = here("docs/viz", "underdog_cond_plots_fitted_dispersion.png"),
       bg="white", underdog_cond_plots_fitted, height = 4, width = 6, dpi = 600)

# It still doesn't quite get the distribution right. So we need another parameter to
# inflate the draw scores, and also adjust mu to counterbalance this.

negloglik_nb_with_pointmass <- function(params, y, mu, k) {
  size <- params[1]
  alpha <- params[2]
  
  if (!is.finite(size) || !is.finite(alpha) || alpha < 0 || alpha >= 1 || size <= 0)
    return(Inf)
  
  mu_0 <- (mu - alpha * k) / (1 - alpha)
  
  # Defensive check: mu_0 should be > 0
  if (any(mu_0 <= 0 | is.na(mu_0))) return(Inf)
  
  probs <- ifelse(
    y == k,
    alpha + (1 - alpha) * dnbinom(k, size = size, mu = mu_0),
    (1 - alpha) * dnbinom(y, size = size, mu = mu_0)
  )
  
  if (any(!is.finite(probs)) || any(probs <= 0)) return(Inf)
  
  -sum(log(probs))
}



dispersion_with_pointmass <- favs %>%
  filter(goals_for %in% 0:3) %>%
  group_by(fav_goals = goals_for) %>%
  summarise(
    result = list({
      y <- goals_against
      mu <- dog_exp_goals
      k <- unique(goals_for)
      
      # Optimize [size, alpha] jointly
      optim(par = c(1, 0.01),
            fn = function(p) negloglik_nb_with_pointmass(p, y = y, mu = mu, k = k),
            method = "L-BFGS-B",
            lower = c(0.01, 0.0001),
            upper = c(10000, 0.1))
    }),
    .groups = "drop"
  ) %>%
  mutate(
    size = map_dbl(result, ~ .x$par[1]),
    alpha = map_dbl(result, ~ .x$par[2])
  ) %>%
  dplyr::select(fav_goals, size, alpha)



dispersion_with_pointmass

# Now make conditional plots using dispersion_with_pointmass

plot_conditional_underdog <- function(df, fav_goals, size_nb, alpha) {
  subset_df <- df %>% filter(goals_for == fav_goals)
  mu_target <- mean(subset_df$dog_exp_goals)  # expected underdog goals
  k <- fav_goals
  mu_0 <- (mu_target - alpha * k) / (1 - alpha)
  
  total_n <- nrow(subset_df)
  
  observed <- subset_df %>%
    count(goals_against) %>%
    complete(goals_against = 0:max(goals_against), fill = list(n = 0)) %>%
    mutate(observed = n / total_n)
  
  observed <- observed %>%
    mutate(
      poisson_pm = ifelse(
        goals_against == k,
        alpha + (1 - alpha) * dpois(goals_against, lambda = mu_0),
        (1 - alpha) * dpois(goals_against, lambda = mu_0)
      ),
      negbin_pm = ifelse(
        goals_against == k,
        alpha + (1 - alpha) * dnbinom(goals_against, size = size_nb, mu = mu_0),
        (1 - alpha) * dnbinom(goals_against, size = size_nb, mu = mu_0)
      )
    ) %>%
    pivot_longer(cols = c("observed", "poisson_pm", "negbin_pm"),
                 names_to = "type", values_to = "rel_freq")
  
  observed$type <- factor(observed$type,
                          levels = c("observed", "poisson_pm", "negbin_pm"),
                          labels = c("Observed", "Poisson + Inflation", "NegBin + Inflation"))
  
  ggplot(observed, aes(x = as.factor(goals_against), y = rel_freq, fill = type)) +
    geom_col(position = position_dodge(width = 0.6), width = 0.5) +
    scale_fill_manual(values = my_palette) +
    scale_x_discrete(limits = as.character(0:4)) +
    scale_y_continuous(limits = c(0, 0.4)) +
    labs(
      title = paste("Underdog Goals | Favorite Scored", fav_goals),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    )
}



underdog_cond_pointmass <- plot_conditional_underdog(favs, 0, dispersion_with_pointmass$size[1],
                                                     dispersion_with_pointmass$alpha[1]) +
  plot_conditional_underdog(favs, 1, dispersion_with_pointmass$size[2], dispersion_with_pointmass$alpha[2]) +
  plot_conditional_underdog(favs, 2, dispersion_with_pointmass$size[3], dispersion_with_pointmass$alpha[3]) +
  plot_conditional_underdog(favs, 3, dispersion_with_pointmass$size[4], dispersion_with_pointmass$alpha[4]) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

underdog_cond_pointmass

ggsave(filename = here("docs/viz", "underdog_cond_pointmass.png"),
       bg="white", underdog_cond_pointmass, height = 4, width = 6, dpi = 600)

# Pretty good overall. 1-0 to the fav still seems slightly more common than expected.


