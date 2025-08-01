
#####################################################
### English Top Flight 1946-2024 with Elo Ratings ###
############# Descriptive Analysis ##################
#####################################################

# ---- Import required packages ----

library(tidyverse)
library(here)
library(forecast) # For time series

# ---- Load the data ----

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

# Set a custom color palette
my_palette <- c("#233D4D", "#FF9F1C", "#41EAD4", "#FDFFFC", "#F71735")

# ---- Check missing values ----

na_sum <- colSums(is.na(matches))
na_df <- data.frame(Column = names(na_sum), NAs = na_sum)

print(na_df)

# ---- Get means and distributions of goals ----

# Overall mean of home and away goals
h_mean <- mean(matches$home_goals)
a_mean <- mean(matches$away_goals)
cat("Mean goals:\n\n")
cat("\nHome:", round(h_mean, 2), "\n")
cat("\nAway:", round(a_mean, 2), "\n")

# Home v away distribution
matches_long <- matches %>% 
  pivot_longer(
    cols = c(home_goals, away_goals),
    names_to = "side",
    values_to = "goals"
  ) %>%
  mutate(side = ifelse(side == "home_goals", "home", "away"))

matches_long$side <- factor(matches_long$side, levels=c("home", "away"))

ggplot(matches_long, aes(x=factor(goals), fill=side)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = my_palette) +
  scale_x_discrete(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  labs(
    title = "Distribution of Home and Away Goals",
    x = "Goals",
    y = "Frequency",
    fill = "Side"
  ) + theme_minimal()

# Total goals time series
matches$total_goals <- matches$home_goals + matches$away_goals

matches$season_year <- as.numeric(str_sub(matches$season, -4))

goals_by_season <- matches %>%
  group_by(season_year) %>%
  summarise(mean_goals = mean(total_goals)) %>%
  arrange(season_year)


ts_goals <- ts(goals_by_season$mean_goals, start = min(goals_by_season$season_year), frequency = 1)

goals_pg_seasons <- ggplot(goals_by_season, aes(x = season_year, y = mean_goals)) +
  geom_line(color = my_palette[1], size = 1.2) +
  geom_point(color = my_palette[2], size = 2) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_y_continuous(limits=c(2,4)) +
  theme_minimal() +
  labs(
    title = "Average Goals per Match by Season",
    x = "Season",
    y = "Average Goals per Match") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
  

goals_pg_seasons
ggsave(filename = here("docs/viz","goals_pg_seasons.png"), goals_pg_seasons, 
       bg = "white", height=4, width=6, dpi=600)

# Home supremacy time series
matches$home_supremacy <- matches$home_goals - matches$away_goals
matches$season_year <- as.numeric(stringr::str_sub(matches$season, -4))

supremacy_by_season <- matches %>%
  group_by(season_year) %>%
  summarise(mean_supremacy = mean(home_supremacy)) %>%
  arrange(season_year)

home_suprem_seasons <- ggplot(supremacy_by_season, aes(x = season_year, y = mean_supremacy)) +
  geom_line(color = my_palette[1], size = 1.2) +
  geom_point(color = my_palette[2], size = 2) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(
    title = "Mean Home Supremacy by Season",
    x = "Season",
    y = "Mean Home Supremacy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )

home_suprem_seasons
ggsave(filename = here("docs/viz","home_suprem_seasons.png"), home_suprem_seasons, 
       bg = "white", height=4, width=6, dpi=600)


# ---- Look at mean and SD of Elo ratings by season ----

elo_summary <- matches %>%
  group_by(season_year) %>%
  summarise(
    mean_elo = mean(home_elo, na.rm = TRUE),
    sd_elo = sd(home_elo, na.rm = TRUE)
  ) %>%
  arrange(season_year)

# Add upper and lower bounds
elo_summary <- elo_summary %>%
  mutate(
    upper = mean_elo + sd_elo,
    lower = mean_elo - sd_elo
  )

elo_mean_seasons <- ggplot(elo_summary, aes(x = season_year, y = mean_elo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = my_palette[3], alpha = 0.3) +
  geom_line(color = my_palette[1], size = 1.2) +
  geom_point(color = my_palette[2], size = 2) +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  labs(
    title = "Mean Elo Rating by Season",
    x = "Season",
    y = "Elo Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )

elo_mean_seasons
ggsave(filename = here("docs/viz","elo_mean_seasons.png"), elo_mean_seasons, 
       bg = "white", height=4, width=6, dpi=600)
