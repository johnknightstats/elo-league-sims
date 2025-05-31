
##########################################################################
### Analyze the odds for teams in the closing games of each title race ###
##########################################################################

# ---- Import libraries & functions ----

library(tidyverse)
library(here)
library(data.table)
library(ggrepel)
library(gt)
library(glue)

source(here("utils", "league_table_funcs.R"))

# ---- Load CSVs ----

odds_dir <- here("data", "odds")
csv_files <- list.files(odds_dir, pattern = "^title_odds_\\d{4}-\\d{4}_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)

# Function to read a file and add the season column
read_with_season <- function(file) {
  dt <- fread(file)
  file_base <- basename(file)
  season <- sub("^title_odds_(\\d{4}-\\d{4})_\\d{4}-\\d{2}-\\d{2}\\.csv$", "\\1", file_base)
  date <- sub("^title_odds_\\d{4}-\\d{4}_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", file_base)
  dt[, season := season]
  dt[, date := as.Date(date)]
  return(dt)
}

# Read and bind all CSVs
all_odds <- rbindlist(lapply(csv_files, read_with_season), use.names = TRUE, fill = TRUE)


# Load all results
all_results <- read.csv(here("data/england_matches_elo_with_predictions.csv"))

# ---- Add number of games played per season ----

final_pld_per_season <- all_results %>%
  group_by(season) %>%
  summarise(
    num_teams = n_distinct(home_team),
    final_pld = (num_teams - 1) * 2,
    .groups = "drop" 
  ) %>%
  select(season, final_pld)

all_odds <- merge(all_odds, final_pld_per_season, by="season", all.x=TRUE, all.y=FALSE)
all_odds$games_left <- all_odds$final_pld - all_odds$pld

# ---- Find highest probability losers ----

# Highest odds for teams that did not win
losers <- subset(all_odds, actual_champion == 0)
highest_losers <- as.data.table(losers)[order(-title_odds), .SD[1], by=season]

############################
### Plot the time series ###
############################

# ---- Plot Man U 2011-12 Odds Time Series ----

mu_12 <- subset(all_odds, team %in% c("Manchester United", "Manchester City")
                & season == "2011-2012") %>%
  select(team, title_odds, date)

# Manually add final odds to make time series look complete
final_odds <- data.frame(team = c("Manchester United", "Manchester City"),
                         title_odds = c(0, 1),
                         date = as.Date(c("2012-05-13", "2012-05-13")))
mu_12 <- rbind(mu_12, final_odds)

fill_missing_odds <- function(df) {
  
  df_filled <- df %>%
    group_by(team) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    arrange(team, date) %>%
    fill(title_odds, .direction = "down") %>% 
    ungroup()
  
  return(df_filled)
}

mu_12 <- fill_missing_odds(mu_12)

# Add annotations for key results
mu_12$annotation <- NA
mu_12$annotation[mu_12$team == "Manchester United" & mu_12$date == as.Date("2012-04-11")] <- "Wigan 1\nMan U 0"
mu_12$annotation[mu_12$team == "Manchester United" & mu_12$date == as.Date("2012-04-22")] <- "Man U 4\nEverton 4"
mu_12$annotation[mu_12$team == "Manchester City" & mu_12$date == as.Date("2012-04-30")] <- "Man C 1\nMan U 0"
mu_12$annotation[mu_12$team == "Manchester City" & mu_12$date == as.Date("2012-05-06")] <- "Newcastle 0\nMan C 2"
mu_12$annotation[mu_12$team == "Manchester City" & mu_12$date == as.Date("2012-05-13")] <- "Man C 3\nQPR 2"

mu_12_labels <- mu_12 %>%
  filter(!is.na(annotation)) %>%
  mutate(
    nudge_x = ifelse(annotation == "Man C 1\nMan U 0", 3,
                     ifelse(annotation == "Newcastle 0\nMan C 2", -3, 0)),
    nudge_y = ifelse(annotation == "Man C 1\nMan U 0", -0.1,
                     ifelse(annotation == "Newcastle 0\nMan C 2", 0.05, 0))
  )

mu_12_odds <- ggplot(mu_12, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(linewidth=1.2) +
  geom_hline(yintercept = c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Manchester United" = "red", "Manchester City" = "skyblue")) +
  labs(title = "Probability of Winning Title 2011-2012", x = NULL, y = "Title Probability", 
       color = NULL) +
  geom_point(data = filter(mu_12, !is.na(annotation)),
             aes(x = date, y = title_odds), color = "black", size = 2) +
  geom_label_repel(data = filter(mu_12, !is.na(annotation)), 
            aes(x = date, y = title_odds, label = annotation),
            size = 3.5, color = "black", box.padding = 0.5, max.overlaps = 10, force = 15,
            nudge_x = mu_12_labels$nudge_x,
            nudge_y = mu_12_labels$nudge_y)
  

mu_12_odds
ggsave(filename = here("data/viz","mu_12_odds.png"), mu_12_odds, height=4, width=8, dpi=600)

# Standings
mydate <- "2012-04-10"
formatted_table <- get_top_n_formatted(all_odds, "2011-2012", mydate, n = 6)
html_code <- as_raw_html(formatted_table)
filename <- paste0("mu_12_table_fragment_", mydate, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_table

myseason <- "2011-2012"
formatted_final <- final_table_formatted(all_results, myseason, n = 6)
html_code <- as_raw_html(formatted_final)
filename <- paste0("final_table_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_final

end_results <- print_results_formatted(all_results, c("Manchester City", "Manchester United"), myseason, mydate)
html_code <- as_raw_html(end_results)
filename <- paste0("end_results_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
end_results

# ---- Plot Burnley 1961-1962 Odds Time Series ----

bu_62 <- subset(all_odds, team %in% c("Burnley", "Ipswich Town")
                & season == "1961-1962") %>%
  select(team, title_odds, date)

# Manually add final odds to make time series look complete
final_odds <- data.frame(team = c("Burnley", "Ipswich Town"),
                         title_odds = c(0, 1),
                         date = as.Date(c("1962-04-30", "1962-04-30")))
bu_62 <- rbind(bu_62, final_odds)

bu_62 <- fill_missing_odds(bu_62)

# Add annotations for key results
bu_62$annotation <- NA
bu_62$annotation[bu_62$team == "Burnley" & bu_62$date == as.Date("1962-04-14")] <- "Burnley 1\nMan U 3"
bu_62$annotation[bu_62$team == "Burnley" & bu_62$date == as.Date("1962-04-17")] <- "Burnley 0\nBlackburn 1"
bu_62$annotation[bu_62$team == "Ipswich Town" & bu_62$date == as.Date("1962-04-23")] <- "Arsenal 0\nIpswich 3"
bu_62$annotation[bu_62$team == "Burnley" & bu_62$date == as.Date("1962-04-23")] <- "Blackpool 1\nBurnley 1"
bu_62$annotation[bu_62$team == "Ipswich Town" & bu_62$date == as.Date("1962-04-28")] <- "Ipswich 2\nA Villa 0"
bu_62$annotation[bu_62$team == "Burnley" & bu_62$date == as.Date("1962-04-28")] <- "Burnley 1\nChelsea 1"

bu_62_odds <- ggplot(bu_62, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(linewidth=1.2) +
  geom_hline(yintercept = c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Burnley" = "maroon","Ipswich Town" = "blue3")) +
  labs(title = "Probability of Winning Title 1961-1962", x = NULL, y = "Title Probability", 
       color = NULL) +
  geom_point(data = filter(bu_62, !is.na(annotation)),
             aes(x = date, y = title_odds), color = "black", size = 2) +
  geom_label_repel(data = filter(bu_62, !is.na(annotation)), 
                   aes(x = date, y = title_odds, label = annotation),
                   size = 3.5, color = "black", box.padding = 0.5, force = 15)

bu_62_odds
ggsave(filename = here("data/viz","bu_62_odds.png"), bu_62_odds, height=4, width=8, dpi=600)

# Standings
mydate <- "1962-04-09"
formatted_table <- get_top_n_formatted(all_odds, "1961-1962", mydate, n = 6)
html_code <- as_raw_html(formatted_table)
filename <- paste0("bu_62_table_fragment_", mydate, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_table

myseason <- "1961-1962"
formatted_final <- final_table_formatted(all_results, myseason, n = 6)
html_code <- as_raw_html(formatted_final)
filename <- paste0("final_table_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_final

end_results <- print_results_formatted(all_results, c("Burnley", "Ipswich Town"), myseason, mydate)
html_code <- as_raw_html(end_results)
filename <- paste0("end_results_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
end_results

# ---- Plot Liverpool 1988-1989 Odds Time Series ----

li_89 <- subset(all_odds, team %in% c("Liverpool", "Arsenal")
                & season == "1988-1989") %>%
  select(team, title_odds, date)

# Manually add final odds to make time series look complete
final_odds <- data.frame(team = c("Liverpool", "Arsenal"),
                         title_odds = c(0, 1),
                         date = as.Date(c("1989-05-26", "1989-05-26")))
li_89 <- rbind(li_89, final_odds)

li_89 <- fill_missing_odds(li_89)

li_89 <- subset(li_89, date >= "1989-04-28")

# Add annotations for key results
li_89$annotation <- NA
li_89$annotation[li_89$team == "Arsenal" & li_89$date == as.Date("1989-05-01")] <- "Arsenal 5\nNorwich 0"
li_89$annotation[li_89$team == "Arsenal" & li_89$date == as.Date("1989-05-06")] <- "Middlesbrough 0\nArsenal 1"
li_89$annotation[li_89$team == "Arsenal" & li_89$date == as.Date("1989-05-13")] <- "Arsenal 1\nDerby 2"
li_89$annotation[li_89$team == "Liverpool" & li_89$date == as.Date("1989-05-13")] <- "Wimbledon 1\nLiverpool 2"
li_89$annotation[li_89$team == "Liverpool" & li_89$date == as.Date("1989-05-23")] <- "Liverpool 5\nWest Ham 1"
li_89$annotation[li_89$team == "Liverpool" & li_89$date == as.Date("1989-05-26")] <- "Liverpool 0\nArsenal 2"

li_89_labels <- li_89 %>%
  filter(!is.na(annotation)) %>%
  mutate(
    nudge_x = ifelse(annotation == "Liverpool 5\nWest Ham 1", -3,0),
    nudge_y = ifelse(annotation == "Liverpool 5\nWest Ham 1", 0,0)
  )

li_89_odds <- ggplot(li_89, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(linewidth=1.2) +
  geom_hline(yintercept = c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Liverpool" = "red", "Arsenal" = "yellow")) +
  labs(title = "Probability of Winning Title 1988-1989", x = NULL, y = "Title Probability", 
       color = NULL) +
  geom_point(data = filter(li_89, !is.na(annotation)),
             aes(x = date, y = title_odds), color = "black", size = 2) +
  geom_label_repel(data = filter(li_89, !is.na(annotation)), 
                   aes(x = date, y = title_odds, label = annotation),
                   size = 3.5, color = "black", box.padding = 0.5, max.overlaps = 10, force = 15,
                   nudge_x = li_89_labels$nudge_x,
                   nudge_y = li_89_labels$nudge_y)

li_89_odds
ggsave(filename = here("data/viz","li_89_odds.png"), li_89_odds, height=4, width=8, dpi=600)

# Standings
mydate <- "1989-05-06"
formatted_table <- get_top_n_formatted(all_odds, "1988-1989", mydate, n = 6)
html_code <- as_raw_html(formatted_table)
filename <- paste0("li_89_table_fragment_", mydate, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_table

mydate <- "1989-05-23"
formatted_table <- get_top_n_formatted(all_odds, "1988-1989", mydate, n = 6)
html_code <- as_raw_html(formatted_table)
filename <- paste0("li_89_table_fragment_", mydate, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_table

myseason <- "1988-1989"
formatted_final <- final_table_formatted(all_results, myseason, n = 6)
html_code <- as_raw_html(formatted_final)
filename <- paste0("final_table_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_final

end_results <- print_results_formatted(all_results, c("Liverpool", "Arsenal"), myseason, mydate)
html_code <- as_raw_html(end_results)
filename <- paste0("end_results_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
end_results

# ---- Plot Man Utd 1991-1992 Odds Time Series ----

mu_92 <- subset(all_odds, team %in% c("Manchester United", "Leeds United")
                & season == "1991-1992") %>%
  select(team, title_odds, date)

# Manually add final odds to make time series look complete
final_odds <- data.frame(team = c("Manchester United", "Leeds United"),
                         title_odds = c(0, 1),
                         date = as.Date(c("1992-05-02", "1992-05-02")))
mu_92 <- rbind(mu_92, final_odds)

mu_92 <- fill_missing_odds(mu_92)

# Add annotations for key results
mu_92$annotation <- NA
mu_92$annotation[mu_92$team == "Manchester United" & mu_92$date == as.Date("1992-04-20")] <- "Man U 1\nN Forest 2"
mu_92$annotation[mu_92$team == "Manchester United" & mu_92$date == as.Date("1992-04-22")] <- "West Ham 1\nMan U 0"
mu_92$annotation[mu_92$team == "Manchester United" & mu_92$date == as.Date("1992-04-26")] <- "Liverpool 2\nMan U 0"

mu_92_odds <- ggplot(mu_92, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(linewidth=1.2) +
  geom_hline(yintercept = c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Manchester United" = "red", "Leeds United" = "yellow")) +
  labs(title = "Probability of Winning Title 1991-1992", x = NULL, y = "Title Probability", 
       color = NULL) +
  geom_point(data = filter(mu_92, !is.na(annotation)),
             aes(x = date, y = title_odds), color = "black", size = 2) +
  geom_label_repel(data = filter(mu_92, !is.na(annotation)), 
                   aes(x = date, y = title_odds, label = annotation),
                   size = 3.5, color = "black", box.padding = 0.5, max.overlaps = 10, force = 15)

mu_92_odds
ggsave(filename = here("data/viz","mu_92_odds.png"), mu_92_odds, height=4, width=8, dpi=600)

mydate <- "1992-04-04"
formatted_table <- get_top_n_formatted(all_odds, "1991-1992", mydate, n = 6)
html_code <- as_raw_html(formatted_table)
filename <- paste0("mu_92_table_fragment_", mydate, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_table

myseason <- "1991-1992"
formatted_final <- final_table_formatted(all_results, myseason, n = 6)
html_code <- as_raw_html(formatted_final)
filename <- paste0("final_table_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_final

end_results <- print_results_formatted(all_results, c("Manchester United", "Leeds United"), myseason, mydate)
html_code <- as_raw_html(end_results)
filename <- paste0("end_results_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
end_results

# ---- Plot Liverpool 2013-2014 Odds Time Series ----

li_14 <- subset(all_odds, team %in% c("Liverpool", "Manchester City", "Chelsea")
                & season == "2013-2014") %>%
  select(team, title_odds, date)

# Manually add final odds to make time series look complete
final_odds <- data.frame(team = c("Liverpool", "Manchester City", "Chelsea"),
                         title_odds = c(0, 1, 0),
                         date = as.Date(c("2014-05-11", "2014-05-11", "2014-05-11")))
li_14 <- rbind(li_14, final_odds)

li_14 <- fill_missing_odds(li_14)

# Add annotations for key results
li_14$annotation <- NA
li_14$annotation[li_14$team == "Liverpool" & li_14$date == as.Date("2014-04-13")] <- "Liverpool 3\nMan C 2"
li_14$annotation[li_14$team == "Liverpool" & li_14$date == as.Date("2014-04-20")] <- "Norwich 2\nLiverpool 3"
li_14$annotation[li_14$team == "Liverpool" & li_14$date == as.Date("2014-04-27")] <- "Liverpool 0\nChelsea 2"
li_14$annotation[li_14$team == "Manchester City" & li_14$date == as.Date("2014-05-03")] <- "Everton 2\nMan C 3"
li_14$annotation[li_14$team == "Liverpool" & li_14$date == as.Date("2014-05-05")] <- "C Palace 3\nLiverpool 3"
li_14$annotation[li_14$team == "Manchester City" & li_14$date == as.Date("2014-05-11")] <- "Man C 2\nWest Ham 0"

li_14_labels <- li_14 %>%
  filter(!is.na(annotation)) %>%
  mutate(
    nudge_x = ifelse(annotation == "Everton 2\nMan C 3", 2,0),
    nudge_y = ifelse(annotation == "Everton 2\nMan C 3", -0.1,0)
  )

li_14_odds <- ggplot(li_14, aes(x = date, y = title_odds, color = team, group = team)) +
  geom_line(linewidth=1.2) +
  geom_hline(yintercept = c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_color_manual(values = c("Liverpool" = "red", "Manchester City" = "skyblue",
                                "Chelsea" = "blue")) +
  labs(title = "Probability of Winning Title 2013-2014", x = NULL, y = "Probability",
       color = NULL) +
  geom_point(data = filter(li_14, !is.na(annotation)),
             aes(x = date, y = title_odds), color = "black", size = 2) +
  geom_label_repel(data = filter(li_14, !is.na(annotation)), 
                   aes(x = date, y = title_odds, label = annotation),
                   size = 3.5, color = "black", box.padding = 0.5, max.overlaps = 10, force = 15,
                   nudge_x = li_14_labels$nudge_x,
                   nudge_y = li_14_labels$nudge_y)


li_14_odds
ggsave(filename = here("data/viz","li_14_odds.png"), li_14_odds, height=4, width=8, dpi=600)

mydate <- "2014-04-20"
formatted_table <- get_top_n_formatted(all_odds, "2013-2014", mydate, n = 6)
html_code <- as_raw_html(formatted_table)
filename <- paste0("li_14_table_fragment_", mydate, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_table

myseason <- "2013-2014"
formatted_final <- final_table_formatted(all_results, myseason, n = 6)
html_code <- as_raw_html(formatted_final)
filename <- paste0("final_table_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
formatted_final

end_results <- print_results_formatted(all_results, c("Liverpool", "Chelsea", "Manchester City"), myseason, mydate)
html_code <- as_raw_html(end_results)
filename <- paste0("end_results_", myseason, ".html")
writeLines(html_code, here("data/viz",filename))
end_results
