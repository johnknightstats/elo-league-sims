
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
library(webshot2)

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
losers <- subset(all_odds, actual_champion == 0 & games_left <= 6)
highest_losers <- as.data.table(losers)[order(-title_odds), .SD[1], by=.(season, team)]


################################
### Function to generate viz ###
################################


generate_season_viz <- function(file_abbrev, myseason, teams_df, annotations_df,
                                date_breaks, table_display_dates, team_show, 
                                start_date="1900-01-01") {
  # teams_df = team, title_bin, colours
  # annotations_df = team, match_date, caption, nudge_x, nudge_y
  
  # get season end date
  all_results$match_date <- as.Date(all_results$match_date)
  end_date <- max(all_results$match_date[all_results$season == myseason], na.rm = TRUE)
  
  my_odds <- subset(all_odds, team %in% teams_df$team
                  & season == myseason & date >= start_date) %>%
    select(team, title_odds, date)
  
  fill_missing_odds <- function(df) {
    
    df_filled <- df %>%
      group_by(team) %>%
      complete(date = seq(min(date), max(date), by = "day")) %>%
      arrange(team, date) %>%
      fill(title_odds, .direction = "down") %>% 
      ungroup()
    
    return(df_filled)
  }
  
  # Manually add final odds to make time series look complete
  final_odds <- data.frame(team = teams_df$team,
                           title_odds = teams_df$title_bin,
                           date = rep(end_date, length(teams_df$team)))
  my_odds <- rbind(my_odds, final_odds)
  
  my_odds <- fill_missing_odds(my_odds)
  
  
  annotations_df <- annotations_df %>%
    mutate(match_date = as.Date(match_date)) %>%
    left_join(my_odds, by = c("team", "match_date" = "date"))
  
  my_ts <- ggplot(my_odds, aes(x = date, y = title_odds, color = team, group = team)) +
    geom_line(linewidth=1.2) +
    geom_hline(yintercept = c(0,1)) +
    scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1), labels = scales::percent_format()) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%b\n%d") +
    scale_color_manual(values = setNames(teams_df$colours, teams_df$team)) +
    labs(title = NULL, x = NULL, y = "Win Probability",
         color = NULL) +
    geom_point(data = annotations_df,
               aes(x = match_date, y = title_odds), color = "black", size = 2) +
    geom_label_repel(data = annotations_df, 
                     aes(x = match_date, y = title_odds, label = caption),
                     size = 3.5, color = "black", box.padding = 0.5, max.overlaps = 10, force = 15,
                     nudge_x = annotations_df$nudge_x,
                     nudge_y = annotations_df$nudge_y) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 12, hjust = 0.5))

    filename <- paste0(file_abbrev, "_odds.png")
    ggsave(filename = here("docs/viz", filename), plot=my_ts, height=4, width=6, dpi=600)
    
    # Output standings as html and png
    for (mydate in table_display_dates) {
      formatted_table <- get_top_n_formatted(all_odds, myseason, mydate, n = 6)
      html_code <- as_raw_html(formatted_table)
      filename <- paste0(file_abbrev, "_table_", mydate, ".html")
      writeLines(html_code, here("docs/viz",filename))
      formatted_table
      filename <- paste0(file_abbrev, "_table_", mydate, ".png")
      gtsave(formatted_table, here("docs/viz",filename))
    }
    
    # Now same for final table
    formatted_final <- final_table_formatted(all_results, myseason, n = 6)
    html_code <- as_raw_html(formatted_final)
    filename <- paste0("final_table_", myseason, ".html")
    writeLines(html_code, here("docs/viz",filename))
    formatted_final
    filename <- paste0("final_table_", myseason, ".png")
    gtsave(formatted_final, here("docs/viz",filename))
    
    team_results <- single_team_results_formatted(all_results, team_show, myseason, 
                                                  my_odds$date[1])
    html_code <- as_raw_html(team_results)
    filename <- paste0(file_abbrev, "_results_", myseason, ".html")
    writeLines(html_code, here("docs/viz",filename))
    team_results
    filename <- paste0(file_abbrev, "_results_", myseason, ".png")
    gtsave(team_results, here("docs/viz",filename))
    
}

###########################################
### Generate the viz for top 10 seasons ###
###########################################


# ---- Manchester United 2011-2012 ----

file_abbrev <- "mu_12"
myseason <- "2011-2012"
teams_df <- data.frame(team=c("Manchester United", "Manchester City"),
                       title_bin = c(0,1),
                       colours = c("red", "skyblue"))
annotations_df <- data.frame(team=c("Manchester United", "Manchester United",
                              "Manchester City", "Manchester City", "Manchester City"),
                             match_date=c("2012-04-11", "2012-04-22", "2012-04-30",
                                          "2012-05-06", "2012-05-13"),
                             caption=c("Wigan 1\nMan U 0", "Man U 4\nEverton 4",
                                       "Man C 1\nMan U 0", "Newcastle 0\nMan C 2",
                                       "Man C 3\nQPR 2"),
                             nudge_x=c(0,0,0,-3,0),
                             nudge_y=c(0,0,0,0.05,0))
date_breaks <- "3 days"
table_display_dates = "2012-04-09"
team_show = "Manchester United"


generate_season_viz(file_abbrev, myseason, teams_df, annotations_df,
                    date_breaks, table_display_dates, team_show)

# ---- Liverpool 1988-1989 ----



file_abbrev <- "li_89"
myseason <- "1988-1989"
teams_df <- data.frame(team=c("Liverpool", "Arsenal"),
                       title_bin = c(0,1),
                       colours = c("red", "yellow"))
annotations_df <- data.frame(team=c("Arsenal", "Arsenal", "Arsenal",
                                    "Liverpool", "Arsenal", "Liverpool", "Liverpool"),
                             match_date=c("1989-05-01", "1989-05-06", "1989-05-13",
                                          "1989-05-13", "1989-05-17",
                                          "1989-05-23", "1989-05-26"),
                             caption=c("Arsenal 5\nNorwich 0", "Middlesbrough 0\nArsenal 1",
                                       "Arsenal 1\nDerby 2", "Wimbledon 1\nLiverpool 2",
                                       "Arsenal 2\nWimbledon 2",
                                       "Liverpool 5\nWest Ham 1", "Liverpool 0\nArsenal 2"),
                             nudge_x=c(0,0,0,0,0,-3,-1),
                             nudge_y=c(0.15,0,0,0,0,0,0.2))
date_breaks <- "3 days"
table_display_dates <- c("1989-05-06", "1989-05-23")
team_show <- "Liverpool"

generate_season_viz(file_abbrev, myseason, teams_df, annotations_df,
                    date_breaks, table_display_dates, team_show, start_date="1989-04-24")

# ---- Liverpool 2013-2014 ----

file_abbrev <- "li_14"
myseason <- "2013-2014"
teams_df <- data.frame(team=c("Liverpool", "Manchester City", "Chelsea"),
                       title_bin = c(0,1,0),
                       colours = c("red", "skyblue", "blue"))
annotations_df <- data.frame(team=c("Liverpool", "Chelsea", "Liverpool", "Liverpool", 
                                    "Manchester City", "Liverpool", "Manchester City"),
                             match_date=c("2014-04-13", "2014-04-19",
                                          "2014-04-20", "2014-04-27",
                                          "2014-05-03", "2014-05-05", "2014-05-11"),
                             caption=c("Liverpool 3\nMan C 2", "Chelsea 1\nSunderland 2",
                                       "Norwich 2\nLiverpool 3",
                                       "Liverpool 0\nChelsea 2", "Everton 2\nMan C 3",
                                       "C Palace 3\nLiverpool 3", "Man C 2\nWest Ham 0"),
                             nudge_x=c(0,0,0,0,2,0,0),
                             nudge_y=c(0,0,0,0,-0.1,0,0))
date_breaks <- "3 days"
table_display_dates = "2014-04-20"
team_show = "Liverpool"



generate_season_viz(file_abbrev, myseason, teams_df, annotations_df,
                    date_breaks, table_display_dates, team_show, start_date="2014-04-08")

# ---- Manchester United 1991-1992 ----

file_abbrev <- "mu_92"
myseason <- "1991-1992"
teams_df <- data.frame(team=c("Manchester United", "Leeds United"),
                       title_bin = c(0,1),
                       colours = c("red", "white"))
annotations_df <- data.frame(team=c("Manchester United", "Manchester United",
                                    "Leeds United", "Manchester United"),
                             match_date=c("1992-04-20", "1992-04-22", "1992-04-26",
                                          "1992-04-26"),
                             caption=c("Man U 1\nN Forest 2", "West Ham 1\nMan U 0",
                                       "Sheff U 2\nLeeds 3", "Liverpool 2\nMan U 0"),
                             nudge_x=c(0,0,0,0),
                             nudge_y=c(0,0,0,0))
date_breaks <- "3 days"
table_display_dates = "1992-04-16"
team_show = "Manchester United"


generate_season_viz(file_abbrev, myseason, teams_df, annotations_df,
                    date_breaks, table_display_dates, team_show, start_date="1992-04-14")

# ---- Burnley 1961-1962 ----

file_abbrev <- "bu_62"
myseason <- "1961-1962"
teams_df <- data.frame(team=c("Burnley", "Ipswich Town"),
                       title_bin = c(0,1),
                       colours = c("maroon", "blue3"))
annotations_df <- data.frame(team=c("Burnley", "Ipswich Town", "Burnley",
                                    "Ipswich Town", "Burnley"),
                             match_date=c("1962-04-17", "1962-04-23",
                                          "1962-04-23", "1962-04-28", "1962-04-28"),
                             caption=c("Burnley 0\nBlackburn 1",
                                       "Arsenal 0\nIpswich 3", "Blackpool 1\nBurnley 1",
                                       "Ipswich 2\nA Villa 0", "Burnley 1\nChelsea 1"),
                             nudge_x=c(0,0,0,0,0),
                             nudge_y=c(0,0,0,0,0))
date_breaks <- "3 days"
table_display_dates = "1962-04-14"
team_show = "Burnley"

generate_season_viz(file_abbrev, myseason, teams_df, annotations_df,
                    date_breaks, table_display_dates, team_show, "1962-04-12")




