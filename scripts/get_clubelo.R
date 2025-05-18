
###################################################
### Download daily Elo ratings from clubelo.com ###
###################################################

# ----- Load packages -----
library(lubridate)
library(here)

# ---- Look for existing Elo files in data folder ----
data_dir <- here("data")
if (!dir.exists(data_dir)) dir.create(data_dir)
existing_files <- list.files(data_dir, pattern = "^eloratings_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = FALSE)

start_date <- NA # Set to "YYYY-MM-DD" to override 

# ---- Set start date from last existing file ----
if (is.na(start_date)) {
  if (length(existing_files) > 0) {
    dates <- as.Date(sub("eloratings_(\\d{4}-\\d{2}-\\d{2})\\.csv", "\\1", existing_files))
    start_date <- max(dates) + 1  # Continue from the next day
  } else {
    start_date <- as.Date("1940-01-01")  # Default start date
  }
}

# ---- Set end date as today's date ----
end_date <- Sys.Date()

# ---- Generate sequence of dates to download ----
if (start_date > end_date) {
  stop("Start date is after end date — nothing to download.")
}
download_dates <- as.character(seq(start_date, end_date, by = "day"))

# ---- Download daily Elo ratings ----
for (mydate in download_dates) {
  message("Downloading Elo ratings for ", mydate)
  api_url <- paste0("http://api.clubelo.com/", mydate)
  filename <- paste0("eloratings_", mydate, ".csv")
  filepath <- file.path(data_dir, filename)
  
  # Only download if the file doesn’t already exist
  if (!file.exists(filepath)) {
    tryCatch({
      download.file(api_url, destfile = filepath, quiet = TRUE)
    }, error = function(e) {
      warning("Failed to download data for ", mydate, ": ", e$message)
    })
  } else {
    message("File already exists for ", mydate, ", skipping.")
  }
}
