# College basketball scouting reports

An R Shiny app that builds a scouting report for any qualified men's college basketball player from real 2025-26 play-by-play data.

- **Shot chart:** efficiency map (hex size = how often, colour = FG% versus the league in the same spot) or every shot.
- **Zones:** shot mix and FG% by zone against the league.
- **Shot quality and shot-making:** expected points per shot from shot locations, and actual minus expected per 100 shots (shrunk toward zero for small samples).
- **Percentiles** against about 2,000 qualified players, **archetypes** (k-means on shot mix and production) and **similar players**.
- **Leaderboard** with filters and a scatter plot.

Data: ESPN play-by-play and box scores via [sportsdataverse](https://sportsdataverse.org). The parsed shots match 98.8% of box-score field-goal attempts. See the app's "How it works" tab for methods and limits.

## Run it

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "plotly", "scales"))
shiny::runApp()
```

## Rebuild the data

```
Rscript scripts/build_data.R
```

Downloads the season's play-by-play and box scores and writes `data/players.rds`, `data/shots.rds` and `data/league.rds`. Change `SEASON` at the top of the script to build a different season.
