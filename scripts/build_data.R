# Build the data the scouting app reads.
#
# Source: ESPN play-by-play and box scores for the 2025-26 men's college
# basketball season, published by sportsdataverse (the same files hoopR loads).
# Run from the repo root:   Rscript scripts/build_data.R
#
# Output (data/):
#   players.rds  one row per qualified player: shot profile, box-score rates,
#                percentiles, archetype, standardized features for comps
#   shots.rds    every field-goal attempt for those players, in half-court feet
#   league.rds   league-wide zone and hex baselines + metadata
suppressMessages({ library(dplyr); library(tidyr) })

SEASON    <- 2026            # ESPN labels the 2025-26 season as 2026
MIN_FGA   <- 150
MIN_MIN   <- 300
BASE      <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/download"
get_rds   <- function(url) { f <- tempfile(fileext = ".rds"); download.file(url, f, mode = "wb", quiet = TRUE); readRDS(f) }

pbp <- get_rds(sprintf("%s/espn_mens_college_basketball_pbp/play_by_play_%d.rds", BASE, SEASON))
box <- get_rds(sprintf("%s/espn_mens_college_basketball_player_boxscores/player_box_%d.rds", BASE, SEASON))

# ---- Shots -------------------------------------------------------------
# ESPN logs shots on a full-court grid in feet, centred at mid-court, with the
# baskets near x = +/-41.75. Fold every shot onto one half court with the hoop at (0, 0).
HOOP_X <- 41.75
shots <- pbp |>
  filter(shooting_play, points_attempted %in% c(2, 3), !is.na(athlete_id_1),
         abs(coordinate_x) <= 47, abs(coordinate_y) <= 25) |>
  transmute(
    game_id, pid = athlete_id_1, made = as.integer(scoring_play), three = as.integer(points_attempted == 3),
    assisted = as.integer(scoring_play & !is.na(athlete_id_2)),
    kind = case_when(type_text == "DunkShot" ~ "dunk", type_text %in% c("LayUpShot", "TipShot") ~ "layup",
                     TRUE ~ "jumper"),
    ly = coordinate_x, lx = coordinate_y
  ) |>
  mutate(
    sgn = ifelse(ly >= 0, 1, -1),
    hy  = HOOP_X - abs(ly),              # feet out from the hoop, toward mid-court (hoop = 0)
    hx  = lx * sgn,                      # lateral, rotated so every shot faces the same basket
    d   = sqrt(hx^2 + hy^2)
  ) |>
  select(-ly, -lx, -sgn)

n_all <- nrow(shots)
# Drop obvious logging errors: threes taken well inside the arc, twos from behind it
shots <- shots |> filter(!(three == 1 & d < 20), !(three == 0 & d > 23.5))
cat("shots kept:", nrow(shots), "of", n_all, "\n")

zone_of <- function(three, d, hx, hy) {
  case_when(
    three == 1 & hy <= 8.75 & abs(hx) >= 19 ~ "Corner 3",
    three == 1                              ~ "Above-break 3",
    d <= 4                                  ~ "Restricted area",
    abs(hx) <= 8 & hy <= 13.75              ~ "Paint (non-RA)",
    TRUE                                    ~ "Mid-range"
  )
}
ZONES <- c("Restricted area", "Paint (non-RA)", "Mid-range", "Corner 3", "Above-break 3")
shots <- shots |> mutate(zone = factor(zone_of(three, d, hx, hy), levels = ZONES))

# ---- Box score: totals per player ---------------------------------------
box <- box |> filter(!is.na(athlete_id), !isTRUE(did_not_play)) 
tot <- box |>
  group_by(pid = athlete_id) |>
  summarise(
    name = first(athlete_display_name),
    team = names(sort(table(team_location), decreasing = TRUE))[1],
    pos  = names(sort(table(athlete_position_abbreviation), decreasing = TRUE))[1],
    games = n(), minutes = sum(minutes, na.rm = TRUE),
    pts = sum(points, na.rm = TRUE), ftm = sum(free_throws_made, na.rm = TRUE), fta = sum(free_throws_attempted, na.rm = TRUE),
    fga_box = sum(field_goals_attempted, na.rm = TRUE),
    reb = sum(rebounds, na.rm = TRUE), ast = sum(assists, na.rm = TRUE),
    stl = sum(steals, na.rm = TRUE), blk = sum(blocks, na.rm = TRUE), tov = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Per-player shot profile -------------------------------------------
prof <- shots |>
  group_by(pid) |>
  summarise(
    fga = n(), fgm = sum(made), tpa = sum(three), tpm = sum(made[three == 1]),
    assisted_share = ifelse(sum(made) > 0, sum(assisted) / sum(made), NA_real_),
    dunk_att = sum(kind == "dunk"),
    .groups = "drop"
  )
zone_ct <- shots |> count(pid, zone, name = "a") |> pivot_wider(names_from = zone, values_from = a, values_fill = 0)
zone_mk <- shots |> filter(made == 1) |> count(pid, zone, name = "m") |> pivot_wider(names_from = zone, values_from = m, values_fill = 0)
for (z in ZONES) { if (!z %in% names(zone_ct)) zone_ct[[z]] <- 0L; if (!z %in% names(zone_mk)) zone_mk[[z]] <- 0L }
names(zone_ct)[match(ZONES, names(zone_ct))] <- paste0("a_", 1:5)
names(zone_mk)[match(ZONES, names(zone_mk))] <- paste0("m_", 1:5)

players <- tot |> inner_join(prof, by = "pid") |> inner_join(zone_ct, by = "pid") |> left_join(zone_mk, by = "pid") |>
  mutate(across(starts_with("m_"), ~ coalesce(.x, 0L)))

# Sanity check: shots we parsed vs. attempts in the box score
chk <- players |> filter(fga_box > 0)
cat(sprintf("Parsed FGA / box FGA across all players: %.3f\n", sum(chk$fga) / sum(chk$fga_box)))

# ---- League baselines (all shots, all players) --------------------------
league_zone <- shots |> group_by(zone) |>
  summarise(fga = n(), fg = mean(made), pps = mean(made * (2 + three)), .groups = "drop") |>
  mutate(share = fga / sum(fga))
print(league_zone)
lz_pps <- setNames(league_zone$pps, league_zone$zone)
lz_fg  <- setNames(league_zone$fg,  league_zone$zone)

players <- players |> filter(fga >= MIN_FGA, minutes >= MIN_MIN) |>
  mutate(
    fg_pct = fgm / fga, three_rate = tpa / fga, three_pct = ifelse(tpa > 0, tpm / tpa, NA_real_),
    rim_rate = a_1 / fga, paint_rate = a_2 / fga, mid_rate = a_3 / fga,
    corner_share = a_4 / pmax(tpa, 1), dunk_rate = dunk_att / fga,
    pts_est = fgm * 2 + tpm + ftm,
    ts_pct = pts / (2 * (fga_box + 0.44 * fta)),
    ft_rate = fta / pmax(fga_box, 1), ft_pct = ifelse(fta > 0, ftm / fta, NA_real_),
    fga40 = fga / minutes * 40,
    pts40 = pts / minutes * 40, reb40 = reb / minutes * 40, ast40 = ast / minutes * 40,
    stl40 = stl / minutes * 40, blk40 = blk / minutes * 40, tov40 = tov / minutes * 40,
    mpg = minutes / games,
    self_created = 1 - assisted_share
  )

# Expected points per shot given shot mix (league-average zone efficiency), and
# shot-making = actual minus expected, shrunk toward 0 for small samples.
A <- as.matrix(players[, paste0("a_", 1:5)]); M <- as.matrix(players[, paste0("m_", 1:5)])
pts_per_made <- c(2, 2, 2, 3, 3)
actual_pts   <- as.vector(M %*% pts_per_made)
expected_pts <- as.vector(A %*% lz_pps[ZONES])
K <- 150
players$shot_quality <- expected_pts / players$fga
players$pps          <- actual_pts / players$fga
players$shot_making  <- (actual_pts - expected_pts) / (players$fga + K)   # per shot, shrunk
for (i in 1:5) {
  zfg_raw <- ifelse(A[, i] > 0, M[, i] / A[, i], NA_real_)
  players[[paste0("fg_", i)]] <- (M[, i] + lz_fg[i] * 20) / (A[, i] + 20)  # light shrinkage, 20-shot prior
  players[[paste0("raw_", i)]] <- zfg_raw
}

# ---- Percentiles across qualified players -------------------------------
pct <- function(x) { r <- rank(x, ties.method = "average", na.last = "keep"); 100 * (r - 1) / (sum(!is.na(x)) - 1) }
PCT_VARS <- c("fga40", "three_rate", "rim_rate", "mid_rate", "corner_share", "self_created", "dunk_rate",
              "shot_quality", "shot_making", "pps", "ts_pct", "ft_rate", "three_pct",
              "pts40", "ast40", "reb40", "stl40", "blk40", "tov40")
for (v in PCT_VARS) players[[paste0("p_", v)]] <- pct(players[[v]])
players$p_tov40 <- 100 - players$p_tov40          # higher percentile = fewer turnovers

# ---- Archetypes ---------------------------------------------------------
FEATS <- c("three_rate", "rim_rate", "mid_rate", "corner_share", "self_created", "dunk_rate",
           "fga40", "ast40", "reb40", "blk40", "stl40", "ft_rate")
X <- players[, FEATS] |> mutate(across(everything(), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))
Z <- scale(as.matrix(X)); Z[is.na(Z)] <- 0
set.seed(2026)
km <- kmeans(Z, centers = 6, nstart = 50, iter.max = 100)
players$cluster <- km$cluster
cen <- as.data.frame(km$centers); cen$cluster <- 1:6; cen$n <- as.vector(table(km$cluster))
print(round(cen, 2))
players <- bind_cols(players, setNames(as.data.frame(Z), paste0("z_", FEATS)))

# Name each cluster from its centroid (assigned in order, each name used once)
nm <- rep(NA_character_, 6); left <- 1:6
take <- function(score, label) { i <- left[which.max(score[left])]; nm[i] <<- label; left <<- setdiff(left, i) }
take(cen$blk40 + cen$dunk_rate, "Rim-running big")
take(cen$ast40 + cen$self_created, "Primary creator")
take(cen$three_rate - cen$rim_rate, "Perimeter shooter")
take(cen$mid_rate + cen$fga40, "Volume scorer")
take(cen$corner_share, "Corner-and-rim role player")
take(rep(0, 6), "Interior forward")
players$archetype <- nm[players$cluster]
print(cbind(cen[, c("cluster", "n")], archetype = nm))

dir.create("data", showWarnings = FALSE)

# ---- NBA comparison pool (2025-26 regular season) -------------------------
# Same features, computed for NBA players, standardized *within the NBA* so a
# college player is matched to the NBA player who plays the most similar role
# relative to their own league. This is a style match, not a talent projection.
NBA_MIN_FGA <- 300; NBA_MIN_MIN <- 500
nba_pbp <- get_rds(sprintf("%s/espn_nba_pbp/play_by_play_%d.rds", BASE, SEASON))
nba_box <- get_rds(sprintf("%s/espn_nba_player_boxscores/player_box_%d.rds", BASE, SEASON))

nba_shots <- nba_pbp |>
  filter(shooting_play, season_type == 2, points_attempted %in% c(2, 3), !is.na(athlete_id_1),
         abs(coordinate_x) <= 47, abs(coordinate_y) <= 25) |>
  transmute(
    pid = athlete_id_1, made = as.integer(scoring_play), three = as.integer(points_attempted == 3),
    assisted = as.integer(scoring_play & !is.na(athlete_id_2)),
    kind = case_when(grepl("Dunk", type_text) ~ "dunk", grepl("Layup|Tip|Finger Roll|Hook|Alley", type_text) ~ "layup", TRUE ~ "jumper"),
    ly = coordinate_x, lx = coordinate_y) |>
  mutate(sgn = ifelse(ly >= 0, 1, -1), hy = HOOP_X - abs(ly), hx = lx * sgn, d = sqrt(hx^2 + hy^2)) |>
  filter(!(three == 1 & d < 20), !(three == 0 & d > 25)) |>
  mutate(zone = factor(zone_of(three, d, hx, hy), levels = ZONES))
cat("NBA shots:", nrow(nba_shots), "\n")

nba_box <- nba_box |> filter(season_type == 2, !is.na(athlete_id), !isTRUE(did_not_play))
nba_tot <- nba_box |>
  group_by(pid = athlete_id) |>
  summarise(name = first(athlete_display_name),
            team = names(sort(table(team_location), decreasing = TRUE))[1],
            pos = names(sort(table(athlete_position_abbreviation), decreasing = TRUE))[1],
            games = n(), minutes = sum(minutes, na.rm = TRUE), pts = sum(points, na.rm = TRUE),
            fga_box = sum(field_goals_attempted, na.rm = TRUE), fta = sum(free_throws_attempted, na.rm = TRUE),
            reb = sum(rebounds, na.rm = TRUE), ast = sum(assists, na.rm = TRUE), stl = sum(steals, na.rm = TRUE),
            blk = sum(blocks, na.rm = TRUE), .groups = "drop")
nba_prof <- nba_shots |> group_by(pid) |>
  summarise(fga = n(), tpa = sum(three), a_rim = sum(zone == "Restricted area"), a_mid = sum(zone == "Mid-range"),
            a_c3 = sum(zone == "Corner 3"), dunk_att = sum(kind == "dunk"),
            made = sum(made), assisted = sum(assisted), .groups = "drop")
nba <- nba_tot |> inner_join(nba_prof, by = "pid") |> filter(fga >= NBA_MIN_FGA, minutes >= NBA_MIN_MIN) |>
  mutate(three_rate = tpa / fga, rim_rate = a_rim / fga, mid_rate = a_mid / fga,
         corner_share = a_c3 / pmax(tpa, 1), self_created = 1 - assisted / pmax(made, 1),
         dunk_rate = dunk_att / fga, fga40 = fga / minutes * 40, ast40 = ast / minutes * 40,
         reb40 = reb / minutes * 40, blk40 = blk / minutes * 40, stl40 = stl / minutes * 40,
         ft_rate = fta / pmax(fga_box, 1), pts40 = pts / minutes * 40, mpg = minutes / games)
cat("NBA players:", nrow(nba), " parsed/box FGA:", round(sum(nba$fga) / sum(nba$fga_box), 3), "\n")
NZ <- scale(as.matrix(nba[, FEATS])); NZ[is.na(NZ)] <- 0
nba <- bind_cols(nba, setNames(as.data.frame(NZ), paste0("z_", FEATS)))
saveRDS(nba, "data/nba_players.rds", compress = "xz")

# ---- Save shots for qualified players, league hex baseline ---------------
q <- shots |> filter(pid %in% players$pid)
league <- list(zone = league_zone, season = "2025-26", n_shots = nrow(shots), n_players = nrow(players),
               min_fga = MIN_FGA, min_min = MIN_MIN, built = as.character(Sys.Date()))
saveRDS(q |> transmute(pid = as.integer(pid), hx = round(hx, 1), hy = round(hy, 1), made, three, kind = factor(kind), zone, assisted),
        "data/shots.rds", compress = "xz")
saveRDS(league, "data/league.rds")
saveRDS(players, "data/players.rds", compress = "xz")
cat("shots saved:", nrow(q), "\n")
