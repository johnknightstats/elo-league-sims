
######################################################
### Helper functions for calculating league tables ###
######################################################


get_standings <- function(df, season, date) {
  year <- as.integer(substr(season, 6, 9))
  points_per_win <- ifelse(year <= 1981, 2, 3)
  tiebreaker <- ifelse(year <= 1976, "GA", "GD")
  
  results <- df[df$season == season & df$match_date <= date,]
  
  results$home_win <- as.integer(results$home_goals > results$away_goals)
  results$draw <- as.integer(results$home_goals == results$away_goals)
  results$away_win <- as.integer(results$home_goals < results$away_goals)
  
  results$home_pts <- ifelse(results$home_goals > results$away_goals, points_per_win,
                             ifelse(results$away_goals > results$home_goals, 0, 1))
  results$away_pts <- ifelse(results$home_goals < results$away_goals, points_per_win,
                             ifelse(results$away_goals < results$home_goals, 0, 1))
  
  home_stats <- results %>%
    transmute(
      team = home_team,
      won = home_win,
      drawn = draw,
      lost = away_win,
      goals_for = home_goals,
      goals_against = away_goals,
      pts = home_pts,
      games = 1
    )
  
  away_stats <- results %>%
    transmute(
      team = away_team,
      won = away_win,
      drawn = draw,
      lost = home_win,
      goals_for = away_goals,
      goals_against = home_goals,
      pts = away_pts,
      games = 1
    )
  
  standings <- bind_rows(home_stats, away_stats) %>%
    group_by(team) %>%
    summarise(
      pld = sum(games),
      won = sum(won),
      drawn = sum(drawn),
      lost = sum(lost),
      goals_for = sum(goals_for),
      goals_against = sum(goals_against),
      pts = sum(pts),
      .groups = "drop"
    ) %>%
    mutate(
      GD = goals_for - goals_against,
      GA = ifelse(goals_against == 0, 1, goals_for / goals_against)
    )
  
  # Drop unneeded tiebreaker
  if (tiebreaker == "GD") {
    standings <- standings %>% select(-GA)
  } else {standings %>% select(-GD)}
  
  ### Points deductions ###
  if (season %in% c("1990-1991", "1996-1997", "2009-2010", "2023-2024")) {
    standings <- standings %>%
      mutate(
        pts = case_when(
          season == "1990-1991" & team == "Arsenal"            ~ pts - 2,
          season == "1990-1991" & team == "Manchester United"  ~ pts - 1,
          season == "1996-1997" & team == "Middlesbrough"      ~ pts - 3,
          season == "2009-2010" & team == "Portsmouth"         ~ pts - 9,
          season == "2023-2024" & team == "Everton"            ~ pts - 6,
          season == "2023-2024" & team == "Nottingham Forest"  ~ pts - 4,
          TRUE                                                 ~ pts
        )
      )
  }
  
  
  
  # Sort by point then tiebreakers
  standings <- standings %>%
    arrange(
      desc(pts),
      desc(!!sym(tiebreaker)),
      desc(goals_for)
    )
  
  standings <- standings %>%
    mutate(rank = dense_rank(desc(pts) * 1e6 + desc(!!sym(tiebreaker)) * 1e3 + desc(goals_for)))
  
  # Check if any ties exist after tiebreakers
  if (n_distinct(standings$rank) < nrow(standings)) {
    # If so, draw lots
    standings <- standings %>%
      group_by(rank) %>%
      mutate(
        tied = n() > 1
      ) %>%
      ungroup()
    
    standings <- standings %>%
      group_by(rank) %>%
      mutate(
        rank = if (tied[1]) {
          rank + (rank(runif(n())) - 1)
        } else {
          rank
        }
      ) %>%
      ungroup() %>%
      select(-tied, -rank)
  }
  
  
  # Get each team’s Elo from their first match *after* the given date
  next_match_elos <- df %>%
    filter(season == !!season, match_date > !!date) %>%
    select(match_date, home_team, away_team, home_elo, away_elo) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = "venue",
      values_to = "team"
    ) %>%
    mutate(
      elo = ifelse(venue == "home_team", home_elo, away_elo)
    ) %>%
    select(team, match_date, elo) %>%
    arrange(team, match_date) %>%
    distinct(team, .keep_all = TRUE)  # first match after the date
  
  # Backup Elo if no match after date
  fallback_elos <- df %>%
    filter(season == !!season, match_date <= !!date) %>%
    select(match_date, home_team, away_team, home_elo, away_elo) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = "venue",
      values_to = "team"
    ) %>%
    mutate(
      elo = ifelse(venue == "home_team", home_elo, away_elo)
    ) %>%
    select(team, match_date, elo) %>%
    arrange(team, desc(match_date)) %>%
    distinct(team, .keep_all = TRUE)
  
  # Combine: prefer next_match_elos, fallback if missing
  team_elos <- bind_rows(next_match_elos, fallback_elos) %>%
    arrange(team, match_date) %>%
    distinct(team, .keep_all = TRUE)
  
  standings <- standings %>%
    left_join(team_elos, by = "team") %>%
    rename(latest_elo = elo)
  
  return(standings)
}

get_final_standings <- function(df, season) {
  final_day <- max(df$match_date[df$season == season])
  final_standings <- get_standings(df, season, final_day)
  return(final_standings)
}

get_champions <- function(df, season) {
  final_standings <- get_final_standings(df, season)
  champions <- final_standings$team[1]
  return(champions)
}

get_top_n_formatted <- function(odds_df, season, date, n = 8) {
  
  date <- as.Date(date)
  
  odds_dates <- unique(odds_df$date[odds_df$date <= date])
  latest_date <- max(odds_dates)
  
  standings <- subset(odds_df, date == latest_date)
  
  year <- as.numeric(substr(season, 1, 4))
  tiebreaker <- if (year >= 1981) "GD" else "GA"
  
  cols_to_use <- c("rank", "team", "pld", "won", "drawn", "lost", "goals_for",
                   "goals_against", tiebreaker, "pts", "latest_elo", "title_odds")
  
  topn <- standings[1:min(n, nrow(standings)), ..cols_to_use]

  names(topn) <- c("#", "Team", "Pld", "W", "D", "L", "F", "A", tiebreaker, "Pts", "Elo", "Win %")
  
  topn$Elo <- round(topn$Elo)
  topn$`Win %` <- round(topn$`Win %` * 100, 1)
  
  formatted_date <- format(date, "%d %b, %Y") 
  
  # Use all Elo vals with buffer for color scale
  elo_vals <- standings$latest_elo
  elo_range <- range(elo_vals, na.rm = TRUE)
  buffer <- 10
  elo_domain <- c(elo_range[1] - buffer, elo_range[2] + buffer)
  
  formatted_table <- topn %>%
    gt() %>%
    tab_header(
      title = md(glue::glue("**League Table after matches on {formatted_date}**"))
    ) %>%
    cols_label(
      `#` = "#", Team = "Team", Pld = "Pld", W = "W", D = "D", L = "L",
      F = "F", A = "A", !!tiebreaker := tiebreaker, Pts = "Pts", Elo = "Elo"
    ) %>%
    
    # Bold all column headers
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    
    # Add vertical line after Team
    tab_style(
      style = cell_borders(sides = "right", color = "grey80", weight = px(1)),
      locations = cells_body(columns = Team)
    ) %>%
    
    # Add vertical line after Pts
    tab_style(
      style = cell_borders(sides = "right", color = "grey80", weight = px(1)),
      locations = cells_body(columns = Pts)
    ) %>%
    
    # Increase weight of Team and Pts text
    tab_style(
      style = cell_text(weight = 600),
      locations = cells_body(columns = c(Team, Pts))
    ) %>%

    data_color(
      columns = `Win %`,
      fn = scales::col_numeric(
        palette = c("white", "springgreen4"),
        domain = c(0,100)
      )
    ) %>%
    
    # Other style settings
    tab_options(
      table.background.color = "white",
      column_labels.background.color = "lightgrey", 
      heading.align = "center",
      column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "darkgrey",
      table.border.top.width = px(1),
      table.border.bottom.width = px(1),
      table.border.top.color = "darkgrey",
      table.border.bottom.color = "darkgrey",
      data_row.padding = px(5),
      table.font.size = px(14)
    )
  
  return(formatted_table)
}

final_table_formatted <- function(results_df, season, n=6) {
  
  standings <- get_final_standings(results_df, season)
    year <- as.numeric(substr(season, 1, 4))
  tiebreaker <- if (year >= 1981) "GD" else "GA"
  
  cols_to_use <- c("rank", "team", "pld", "won", "drawn", "lost", "goals_for",
                   "goals_against", tiebreaker, "pts")
  
  topn <- standings[1:min(n, nrow(standings)), cols_to_use]
  
  names(topn) <- c("#", "Team", "Pld", "W", "D", "L", "F", "A", tiebreaker, "Pts")
  
  formatted_table <- topn %>%
    gt() %>%
    tab_header(
      title = md(glue::glue("**Final Table {season}**"))
    ) %>%
    cols_label(
      `#` = "#", Team = "Team", Pld = "Pld", W = "W", D = "D", L = "L",
      F = "F", A = "A", !!tiebreaker := tiebreaker, Pts = "Pts"
    ) %>%
    
    # Bold all column headers
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    
    # Add vertical line after Team
    tab_style(
      style = cell_borders(sides = "right", color = "grey80", weight = px(1)),
      locations = cells_body(columns = Team)
    ) %>%
    
    # Increase weight of Team and Pts text
    tab_style(
      style = cell_text(weight = 600),
      locations = cells_body(columns = c(Team, Pts))
    ) %>%
    
    # Other style settings
    tab_options(
      table.background.color = "white",
      column_labels.background.color = "lightgrey", 
      heading.align = "center",
      column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "darkgrey",
      table.border.top.width = px(1),
      table.border.bottom.width = px(1),
      table.border.top.color = "darkgrey",
      table.border.bottom.color = "darkgrey",
      data_row.padding = px(5),
      table.font.size = px(14)
    )
  
  return(formatted_table)
}

print_results_formatted <- function(results_df, teams, my_season, start_date) {
  
  start_date <- as.Date(start_date)
  
  results_df %>%
    mutate(match_date = as.Date(match_date)) %>%
    filter(season == my_season,
           match_date >= start_date,
           home_team %in% teams | away_team %in% teams) %>%
    mutate(score = paste0(home_goals, " - ", away_goals)) %>%
    select(match_date, home_team, score, away_team) %>%
    arrange(match_date) %>%
    gt() %>%
    cols_label(
      match_date = "Date",
      home_team = "Home Team",
      score = "Score",
      away_team = "Away Team"
    ) %>%
    fmt_date(
      columns = match_date,
      date_style = "day_month_year"
    ) %>%
    tab_style(
      style = cell_text(align = "right"),
      locations = cells_body(columns = home_team)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "white", weight = px(10)),
      locations = cells_body(columns = match_date)
    ) %>%
    tab_options(
      table.background.color = "white",
      column_labels.hidden = TRUE, 
      heading.align = "center"
    )
}
