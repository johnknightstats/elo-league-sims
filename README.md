# elo-league-sims

This project simulates historical English football league seasons using Elo ratings, to explore which teams suffered the biggest late-season collapses in title races. The results were featured in the following articles:

- 📘 [Title Collapses in Football History](https://johnknightstats.com/posts/title_collapses/)
- 🔧 [How the Elo Simulation Model Works](https://johnknightstats.com/posts/elo_sim_model/)

## Overview

The Elo-based model simulates each remaining match in a historical season from any given matchday onward, using estimated goal distributions based on pre-match Elo ratings. It then projects the probability of each team winning the league.

This repo includes:
- Elo model calibration scripts
- Simulation engine for full seasons
- Helper functions for Elo updates and league tables
- Visualization outputs from selected seasons

## Folder Structure

```
├── scripts/ # Main R scripts for analysis and simulation
├── utils/ # Helper functions for Elo and league tables
├── data/ # (empty placeholder — see below)
├── docs/viz/ # Figures used in the published articles
├── LICENSE
├── README.md
└── .gitignore
```


## Getting Started

To run the simulations, you’ll need:
- Elo ratings (e.g., from [clubelo.com](https://clubelo.com/))
- Historical match data with scores, dates, and teams

⚠️ **Note:** I do not include scraped match data in this repo for licensing reasons. However, there are many public repositories and scraping guides available for football data. The scripts assume a standard format — see `scripts/descriptive_analysis.R` for expected column names.

## Scripts Summary

| Script                        | Purpose |
|------------------------------|---------|
| `elo_model.R`                | Trains the Elo model and sets parameters |
| `elo_league_simulator.R`     | Runs season simulations from any point |
| `descriptive_analysis.R`     | Generates exploratory plots |
| `elo_calibration.R`          | Evaluates Elo accuracy and goal modeling |
| `get_clubelo.R`              | Downloads Elo data from clubelo.com |
| `league_odds_analysis.R`     | Creates visualizations for the article |

Helper functions are in `utils/`.

## Requirements

You'll need R and the following packages:

- `tidyverse`
- `here`
- `forecast`
- `patchwork`
- `MASS`
- `rlang`
- `future.apply`
- `progressr`
- `data.table`
- `ggrepel`
- `gt`
- `glue`
- `webshot2`

You can optionally add a `requirements.R` file or use [`renv`](https://rstudio.github.io/renv/) to snapshot your dependencies.

## License

This project is licensed under the MIT License. See [LICENSE](./LICENSE) for details.

## Author

John Knight  
[https://johnknightstats.com](https://johnknightstats.com)
