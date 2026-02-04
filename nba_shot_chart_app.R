# Scout Report Generator (Shiny)
# Readymade, single-file app
# - Conference -> Team -> Position -> Player filter order
# - Synthetic shots (replaceable later), but adds scout priors so draft ranges are believable
# - Tabs: (1) Scout Snapshot + Scout Notes (2) NBA Comparison (3) Position Rank + Draft Fit (4) Shot Deep Dive
# - Role Archetype line (shot diet + efficiency thresholds)
# - Strengths/Concerns always populate (absolute + relative logic)

required_packages <- c(
  "shiny", "ggplot2", "dplyr", "plotly", "viridis", "hexbin",
  "scales", "bslib", "bsicons", "stringr"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------
# Players with scout priors
# -----------------------------
get_top_college_players <- function() {
  players <- data.frame(
    player_id = 1:100,
    player_name = c(
      "Cooper Flagg", "Ace Bailey", "Dylan Harper", "Kon Knueppel", "Khaman Maluach",
      "VJ Edgecombe", "Labaron Philon", "Tre Johnson", "Liam McNeeley", "Derik Queen",
      "Egor Demin", "Kanon Catchings", "Ian Jackson", "Kasparas Jakucionis", "Will Riley",
      "Malik Dia", "Coleman Hawkins", "Ryan Kalkbrenner", "Hunter Sallis", "Mark Sears",
      "RJ Davis", "Johni Broome", "Wade Taylor IV", "Norchad Omier", "Great Osobor",
      "Maxime Raynaud", "Graham Ike", "Tyon Grant-Foster", "Braden Smith", "Tucker DeVries",
      "Jalon Moore", "Oumar Ballo", "Kam Jones", "Jaxon Kohler", "PJ Hall",
      "Zach Edey", "Armando Bacot", "Caleb Love", "Ryan Nembhard", "Drew Timme",
      "Hunter Dickinson", "Tristan da Silva", "Jaime Jaquez Jr", "Oscar Tshiebwe", "Armaan Franklin",
      "Gradey Dick", "Keyonte George", "Nick Smith Jr", "Cason Wallace", "Anthony Black",
      "Marcus Sasser", "Adam Flagler", "Drew Peterson", "Boogie Ellis", "Tyger Campbell",
      "Terquavion Smith", "Isaiah Wong", "Trayce Jackson-Davis", "Seth Lundy", "Jalen Wilson",
      "Azuolas Tubelis", "Baylor Scheierman", "Santiago Vescovi", "Kyle Filipowski", "Jeremy Roach",
      "Caleb Foster", "Tyrese Proctor", "Ryan Young", "Mark Mitchell", "Dariq Whitehead",
      "Dereck Lively II", "Harrison Ingram", "Brandon Miller", "Noah Clowney", "Rylan Griffen",
      "Jahvon Quinerly", "Darius Miles", "Nick Honor", "Kendric Davis", "David Jones",
      "Souley Boum", "Marcus Carr", "Kerr Kriisa", "Timmy Allen", "Fardaws Aimaq",
      "Matthew Mayer", "LJ Cryer", "Keyshawn Hall", "Adou Thiero", "Tre Mitchell",
      "Nijel Pack", "Joseph Girard III", "Judah Mintz", "Jesse Edwards", "Chris Bell",
      "Justin Taylor", "Benny Williams", "Quadir Copeland", "Maliq Brown", "JJ Starling"
    ),
    team = c(
      "Duke", "Rutgers", "Rutgers", "Duke", "Duke",
      "Baylor", "Alabama", "Texas", "UConn", "Maryland",
      "BYU", "BYU", "North Carolina", "Illinois", "Illinois",
      "Duke", "Kansas State", "Creighton", "Wake Forest", "Alabama",
      "North Carolina", "Auburn", "Texas A&M", "Miami", "Washington",
      "Stanford", "Gonzaga", "Grand Canyon", "Purdue", "Drake",
      "Oklahoma", "Indiana", "Marquette", "Michigan State", "Clemson",
      "Purdue", "North Carolina", "Arizona", "Gonzaga", "Gonzaga",
      "Kansas", "Marquette", "UCLA", "Kentucky", "Virginia",
      "Kansas", "Baylor", "Arkansas", "Kentucky", "Arkansas",
      "Houston", "Baylor", "USC", "USC", "UCLA",
      "NC State", "Miami", "Indiana", "Penn State", "Kansas",
      "Arizona", "Creighton", "Tennessee", "Duke", "Duke",
      "Duke", "Duke", "Duke", "Duke", "Duke",
      "Duke", "Stanford", "Alabama", "Alabama", "Alabama",
      "Alabama", "Alabama", "Clemson", "Memphis", "Memphis",
      "Xavier", "Texas", "Arizona", "Texas Tech", "California",
      "Baylor", "Baylor", "George Mason", "Arkansas", "Dayton",
      "Miami", "Syracuse", "Syracuse", "Syracuse", "Syracuse",
      "Syracuse", "Syracuse", "Syracuse", "Syracuse", "Syracuse"
    ),
    position = c(
      "F", "G", "G", "G", "C",
      "G", "G", "G", "F", "C",
      "G", "F", "G", "G", "G",
      "F", "F", "C", "G", "G",
      "G", "C", "G", "F", "F",
      "C", "C", "G", "G", "G",
      "F", "C", "G", "C", "F",
      "C", "C", "G", "G", "F",
      "C", "F", "F", "C", "G",
      "G", "G", "G", "G", "G",
      "G", "G", "G", "G", "G",
      "G", "G", "F", "G", "F",
      "F", "G", "G", "F", "G",
      "G", "G", "C", "F", "F",
      "C", "F", "F", "F", "G",
      "G", "G", "G", "G", "F",
      "G", "G", "G", "G", "C",
      "F", "F", "G", "F", "F",
      "G", "G", "G", "C", "G",
      "G", "F", "G", "F", "G"
    ),
    conference = c(
      "ACC", "Big Ten", "Big Ten", "ACC", "ACC",
      "Big 12", "SEC", "Big 12", "Big East", "Big Ten",
      "Big 12", "Big 12", "ACC", "Big Ten", "Big Ten",
      "ACC", "Big 12", "Big East", "ACC", "SEC",
      "ACC", "SEC", "SEC", "ACC", "Big Ten",
      "ACC", "WCC", "WAC", "Big Ten", "MVC",
      "Big 12", "Big Ten", "Big East", "Big Ten", "ACC",
      "Big Ten", "ACC", "Pac-12", "WCC", "WCC",
      "Big 12", "Big East", "Pac-12", "SEC", "ACC",
      "Big 12", "Big 12", "SEC", "SEC", "SEC",
      "Big 12", "Big 12", "Pac-12", "Pac-12", "Pac-12",
      "ACC", "ACC", "Big Ten", "Big Ten", "Big 12",
      "Pac-12", "Big East", "SEC", "ACC", "ACC",
      "ACC", "ACC", "ACC", "ACC", "ACC",
      "ACC", "ACC", "SEC", "SEC", "SEC",
      "SEC", "SEC", "ACC", "AAC", "AAC",
      "Big East", "Big 12", "Pac-12", "Big 12", "Pac-12",
      "Big 12", "Big 12", "A-10", "SEC", "A-10",
      "ACC", "ACC", "ACC", "ACC", "ACC",
      "ACC", "ACC", "ACC", "ACC", "ACC"
    ),
    stringsAsFactors = FALSE
  )
  
  # Scout priors: projection tier + grade + role value
  # You can tweak these without touching the rest of the app.
  manual <- data.frame(
    player_name = c("Cooper Flagg", "Ace Bailey", "Dylan Harper", "Kon Knueppel", "Khaman Maluach",
                    "VJ Edgecombe", "Tre Johnson", "Liam McNeeley", "Derik Queen"),
    proj_tier = c("T0: Franchise (1-3)", "T1: High Lottery (4-8)", "T1: High Lottery (4-8)",
                  "T2: Lottery (9-14)", "T2: Lottery (9-14)",
                  "T2: Lottery (9-14)", "T2: Lottery (9-14)", "T3: Mid 1st (15-22)", "T3: Mid 1st (15-22)"),
    scout_grade = c(98, 92, 91, 86, 85, 85, 84, 80, 80),
    role_value = c(9.5, 8.5, 8.5, 7.5, 8.0, 7.5, 7.5, 7.0, 7.5),
    stringsAsFactors = FALSE
  )
  
  players <- players %>%
    left_join(manual, by = "player_name")
  
  # Fill remaining priors with reasonable defaults (still synthetic, but coherent)
  default_grade <- function(name, pos) {
    base <- if (pos == "G") 74 else if (pos == "F") 75 else 73
    wiggle <- (sum(utf8ToInt(name)) %% 13) - 6
    pmin(86, pmax(65, base + wiggle))
  }
  default_role <- function(name, pos) {
    base <- if (pos == "G") 6.5 else if (pos == "F") 7.0 else 6.8
    wiggle <- ((sum(utf8ToInt(paste0(name, "_role"))) %% 11) - 5) / 10
    pmin(8.5, pmax(5.5, base + wiggle))
  }
  
  players <- players %>%
    mutate(
      scout_grade = ifelse(is.na(scout_grade), mapply(default_grade, player_name, position), scout_grade),
      role_value  = ifelse(is.na(role_value),  mapply(default_role,  player_name, position), role_value),
      proj_tier   = ifelse(is.na(proj_tier),
                           dplyr::case_when(
                             scout_grade >= 90 ~ "T1: High Lottery (4-8)",
                             scout_grade >= 85 ~ "T2: Lottery (9-14)",
                             scout_grade >= 80 ~ "T3: Mid 1st (15-22)",
                             scout_grade >= 75 ~ "T4: Late 1st (23-30)",
                             scout_grade >= 70 ~ "T5: 2nd (31-45)",
                             TRUE ~ "T6: Late 2nd/UDFA (46+)"
                           ),
                           proj_tier),
      declares_pred = TRUE  # for now, assume all are "declaring" pool
    )
  
  players
}

tier_to_range <- function(tier) {
  # returns c(lo, hi)
  if (is.na(tier) || !nzchar(tier)) return(c(46, 58))
  if (grepl("^T0", tier)) return(c(1, 3))
  if (grepl("^T1", tier)) return(c(4, 8))
  if (grepl("^T2", tier)) return(c(9, 14))
  if (grepl("^T3", tier)) return(c(15, 22))
  if (grepl("^T4", tier)) return(c(23, 30))
  if (grepl("^T5", tier)) return(c(31, 45))
  c(46, 58)
}

# -----------------------------
# Synthetic shot generator
# -----------------------------
generate_college_shots <- function(player_name, team, position, num_shots = 750) {
  set.seed(sum(utf8ToInt(player_name)))
  
  if (position == "G") {
    shot_zones <- sample(c("paint", "mid", "three"), num_shots, replace = TRUE, prob = c(0.22, 0.23, 0.55))
    make_rates <- c(paint = 0.56, mid = 0.38, three = 0.36)
  } else if (position == "F") {
    shot_zones <- sample(c("paint", "mid", "three"), num_shots, replace = TRUE, prob = c(0.44, 0.30, 0.26))
    make_rates <- c(paint = 0.59, mid = 0.40, three = 0.35)
  } else {
    shot_zones <- sample(c("paint", "mid", "three"), num_shots, replace = TRUE, prob = c(0.75, 0.20, 0.05))
    make_rates <- c(paint = 0.62, mid = 0.37, three = 0.28)
  }
  
  shots <- data.frame(
    PLAYER_NAME = rep(player_name, num_shots),
    TEAM = rep(team, num_shots),
    POSITION = rep(position, num_shots),
    LOC_X = numeric(num_shots),
    LOC_Y = numeric(num_shots),
    SHOT_MADE_FLAG = numeric(num_shots),
    SHOT_DISTANCE = numeric(num_shots),
    SHOT_TYPE = character(num_shots),
    ZONE = character(num_shots),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:num_shots) {
    zone <- shot_zones[i]
    shots$ZONE[i] <- zone
    
    if (zone == "paint") {
      angle <- runif(1, -pi/2.2, pi/2.2)
      distance <- runif(1, 5, 80)
      shots$LOC_X[i] <- distance * cos(angle) + rnorm(1, 0, 8)
      shots$LOC_Y[i] <- abs(distance * sin(angle)) + rnorm(1, 0, 8)
      shots$SHOT_DISTANCE[i] <- sqrt(shots$LOC_X[i]^2 + shots$LOC_Y[i]^2) / 10
      shots$SHOT_MADE_FLAG[i] <- rbinom(1, 1, make_rates["paint"])
      shots$SHOT_TYPE[i] <- sample(c("Layup", "Dunk", "Hook Shot", "Floater"), 1, prob = c(0.5, 0.25, 0.15, 0.1))
    } else if (zone == "mid") {
      angle <- runif(1, -pi/2, pi/2)
      distance <- runif(1, 80, 225)
      shots$LOC_X[i] <- distance * cos(angle) + rnorm(1, 0, 12)
      shots$LOC_Y[i] <- abs(distance * sin(angle)) + rnorm(1, 0, 12)
      shots$SHOT_DISTANCE[i] <- sqrt(shots$LOC_X[i]^2 + shots$LOC_Y[i]^2) / 10
      shots$SHOT_MADE_FLAG[i] <- rbinom(1, 1, make_rates["mid"])
      shots$SHOT_TYPE[i] <- sample(c("Jump Shot", "Fadeaway", "Pull-up", "Turnaround"), 1)
    } else {
      angle <- runif(1, -1.0, 1.0)
      distance <- runif(1, 228, 240)
      shots$LOC_X[i] <- distance * cos(angle) + rnorm(1, 0, 5)
      shots$LOC_Y[i] <- abs(distance * sin(angle)) + rnorm(1, 0, 5)
      shots$SHOT_DISTANCE[i] <- sqrt(shots$LOC_X[i]^2 + shots$LOC_Y[i]^2) / 10
      shots$SHOT_MADE_FLAG[i] <- rbinom(1, 1, make_rates["three"])
      shots$SHOT_TYPE[i] <- sample(c("3PT Jump Shot", "3PT Pull-up", "Corner 3"), 1, prob = c(0.50, 0.30, 0.20))
    }
  }
  
  shots
}

# -----------------------------
# Court
# -----------------------------
draw_court <- function() {
  ggplot() +
    xlim(-260, 260) +
    ylim(-55, 430) +
    geom_rect(aes(xmin = -60, xmax = 60, ymin = -52.5, ymax = 137.5), fill = NA, color = "#8B4513", linewidth = 1.1) +
    geom_segment(aes(x = -250, xend = 250, y = 417.5, yend = 417.5), color = "#8B4513", linewidth = 1.1) +
    geom_segment(aes(x = -30, xend = 30, y = -7.5, yend = -7.5), color = "#8B4513", linewidth = 2) +
    geom_path(
      data = data.frame(
        x = 228 * cos(seq(-0.95, 0.95, length.out = 150)),
        y = 228 * sin(seq(-0.95, 0.95, length.out = 150))
      ),
      aes(x = x, y = y),
      color = "#003399", linewidth = 1.2
    ) +
    geom_segment(aes(x = -216, xend = -216, y = -52.5, yend = 95), color = "#003399", linewidth = 1.2) +
    geom_segment(aes(x = 216, xend = 216, y = -52.5, yend = 95), color = "#003399", linewidth = 1.2) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f5e6d3", color = NA)
    ) +
    coord_fixed()
}

# -----------------------------
# Scout helpers
# -----------------------------
wilson_ci <- function(x, n, conf = 0.95) {
  if (n == 0) return(c(NA, NA))
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- x / n
  denom <- 1 + (z^2 / n)
  center <- (phat + (z^2 / (2 * n))) / denom
  half <- (z * sqrt((phat * (1 - phat) / n) + (z^2 / (4 * n^2)))) / denom
  c(max(0, center - half), min(1, center + half))
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("NA")
  paste0(round(100 * x, digits), "%")
}

fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("NA")
  as.character(round(x, digits))
}

add_flags <- function(df) {
  df %>%
    mutate(
      is_three = SHOT_DISTANCE >= 22.75,
      is_rim = SHOT_DISTANCE < 8,
      is_mid = SHOT_DISTANCE >= 8 & SHOT_DISTANCE < 22.75,
      is_corner3 = (SHOT_DISTANCE >= 22.75) & (abs(LOC_X) >= 200) & (LOC_Y <= 95)
    )
}

summarize_profile <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list(n = 0))
  df <- add_flags(df)
  
  n <- nrow(df)
  fgm <- sum(df$SHOT_MADE_FLAG == 1)
  fga <- n
  
  three_att <- sum(df$is_three)
  three_made <- sum(df$is_three & df$SHOT_MADE_FLAG == 1)
  
  two_att <- sum(!df$is_three)
  two_made <- sum(!df$is_three & df$SHOT_MADE_FLAG == 1)
  
  rim_att <- sum(df$is_rim)
  rim_made <- sum(df$is_rim & df$SHOT_MADE_FLAG == 1)
  
  mid_att <- sum(df$is_mid)
  mid_made <- sum(df$is_mid & df$SHOT_MADE_FLAG == 1)
  
  c3_att <- sum(df$is_corner3)
  c3_made <- sum(df$is_corner3 & df$SHOT_MADE_FLAG == 1)
  
  fg <- fgm / fga
  fg_ci <- wilson_ci(fgm, fga)
  
  tp <- if (three_att > 0) three_made / three_att else NA
  tp_ci <- if (three_att > 0) wilson_ci(three_made, three_att) else c(NA, NA)
  
  twp <- if (two_att > 0) two_made / two_att else NA
  
  rim <- if (rim_att > 0) rim_made / rim_att else NA
  rim_ci <- if (rim_att > 0) wilson_ci(rim_made, rim_att) else c(NA, NA)
  
  mid <- if (mid_att > 0) mid_made / mid_att else NA
  c3 <- if (c3_att > 0) c3_made / c3_att else NA
  
  efg <- (fgm + 0.5 * three_made) / fga
  
  three_rate <- three_att / fga
  rim_rate <- rim_att / fga
  mid_rate <- mid_att / fga
  avg_dist <- mean(df$SHOT_DISTANCE)
  
  points <- ifelse(df$is_three, 3, 2)
  epps <- mean(points * df$SHOT_MADE_FLAG)
  
  pts_outcome <- points * df$SHOT_MADE_FLAG
  se <- stats::sd(pts_outcome) / sqrt(fga)
  z <- qnorm(0.975)
  epps_ci <- c(max(0, epps - z * se), epps + z * se)
  
  list(
    n = n,
    fg = fg, fg_ci = fg_ci,
    twp = twp,
    tp = tp, tp_ci = tp_ci,
    rim = rim, rim_ci = rim_ci,
    mid = mid,
    c3 = c3,
    efg = efg,
    three_rate = three_rate,
    rim_rate = rim_rate,
    mid_rate = mid_rate,
    avg_dist = avg_dist,
    epps = epps, epps_ci = epps_ci,
    three_att = three_att,
    rim_att = rim_att,
    c3_att = c3_att
  )
}

build_baseline <- function(players_df, position, exclude_name, num_players = 20, shots_per_player = 420) {
  pool <- players_df %>% filter(position == position, player_name != exclude_name)
  if (nrow(pool) == 0) return(NULL)
  
  set.seed(2026)
  sample_n <- min(num_players, nrow(pool))
  sampled <- pool %>% slice_sample(n = sample_n)
  
  all <- NULL
  for (i in seq_len(nrow(sampled))) {
    s <- generate_college_shots(sampled$player_name[i], sampled$team[i], sampled$position[i], num_shots = shots_per_player)
    all <- if (is.null(all)) s else rbind(all, s)
  }
  summarize_profile(all)
}

role_archetype <- function(pos, p, b = NULL) {
  if (is.null(p$n) || p$n == 0) return("Unknown (no data loaded)")
  
  high_3_vol <- !is.na(p$three_rate) && p$three_rate >= 0.45
  low_3_vol  <- !is.na(p$three_rate) && p$three_rate <= 0.25
  high_rim   <- !is.na(p$rim_rate)   && p$rim_rate >= 0.55
  good_3     <- !is.na(p$tp)         && p$tp >= 0.37
  elite_3    <- !is.na(p$tp)         && p$tp >= 0.40
  good_rim   <- !is.na(p$rim)        && p$rim >= 0.62
  mid_heavy  <- !is.na(p$mid_rate)   && p$mid_rate >= 0.35
  low_mid    <- !is.na(p$mid_rate)   && p$mid_rate <= 0.22
  high_efg   <- !is.na(p$efg)        && p$efg >= 0.54
  
  tp_plus  <- (!is.null(b) && !is.na(p$tp)  && !is.na(b$tp)  && (p$tp  - b$tp)  >= 0.03)
  rim_plus <- (!is.null(b) && !is.na(p$rim) && !is.na(b$rim) && (p$rim - b$rim) >= 0.04)
  efg_plus <- (!is.null(b) && !is.na(p$efg) && !is.na(b$efg) && (p$efg - b$efg) >= 0.02)
  
  if (pos == "G") {
    if (high_3_vol && (elite_3 || tp_plus) && low_mid) return("Movement/spacing guard (high-volume shooter)")
    if (high_3_vol && (good_3 || tp_plus) && (high_efg || efg_plus)) return("Shot-creator guard (efficient perimeter scorer)")
    if (high_rim && (good_rim || rim_plus) && low_3_vol) return("Rim-pressure guard (downhill finisher)")
    if (mid_heavy && !high_3_vol) return("Midrange creator guard")
    if (high_3_vol && (good_3 || tp_plus)) return("Off-ball shooter guard")
    return("Combo guard (mixed scoring profile)")
  }
  
  if (pos == "F") {
    if (high_3_vol && (good_3 || tp_plus) && (high_efg || efg_plus)) return("3-and-D wing archetype (spacing-driven)")
    if (high_rim && (good_rim || rim_plus) && low_3_vol) return("Slashing/finishing wing (rim-first)")
    if (mid_heavy && !high_3_vol) return("Shot-making wing (midrange lean)")
    if (high_3_vol && (elite_3 || tp_plus)) return("Spacing wing (high-volume shooter)")
    return("Versatile forward (balanced shot diet)")
  }
  
  if (pos == "C") {
    if (high_rim && (good_rim || rim_plus) && low_3_vol) return("Rim-running big (paint finisher)")
    if (high_rim && !good_rim) return("Interior big (needs finishing polish)")
    if (high_3_vol && (good_3 || tp_plus)) return("Stretch big (spacing center)")
    return("Traditional big (inside-out mix)")
  }
  
  "Unknown"
}

# Strengths/concerns that never go empty:
# - absolute thresholds (scout style)
# - plus/minus vs baseline when available
strengths_concerns <- function(pos, p, b) {
  pros <- c()
  cons <- c()
  
  # Absolute reads
  if (!is.na(p$tp) && p$tp >= 0.39 && !is.na(p$three_rate) && p$three_rate >= 0.35) pros <- c(pros, "Real shooting threat (good percentage on meaningful volume).")
  if (!is.na(p$rim) && p$rim >= 0.64 && !is.na(p$rim_rate) && p$rim_rate >= 0.35) pros <- c(pros, "Finishes at the rim efficiently and gets there often.")
  if (!is.na(p$efg) && p$efg >= 0.54) pros <- c(pros, "Efficient overall shot profile (strong eFG%).")
  if (!is.na(p$mid_rate) && p$mid_rate <= 0.22 && !is.na(p$three_rate) && p$three_rate >= 0.40) pros <- c(pros, "Modern diet: avoids midrange and leans into rim and threes.")
  
  if (!is.na(p$tp) && p$tp <= 0.33 && !is.na(p$three_rate) && p$three_rate >= 0.35) cons <- c(cons, "Volume is there from three, but efficiency is a swing factor.")
  if (!is.na(p$rim) && p$rim <= 0.58 && !is.na(p$rim_rate) && p$rim_rate >= 0.35) cons <- c(cons, "Gets to the rim, but conversion needs work.")
  if (!is.na(p$mid_rate) && p$mid_rate >= 0.35) cons <- c(cons, "Shot diet is midrange-heavy, which can cap efficiency at the next level.")
  if (!is.na(p$efg) && p$efg <= 0.50) cons <- c(cons, "Overall efficiency profile is below where you want it.")
  
  # Relative reads vs baseline (adds scout flavor)
  if (!is.null(b) && !is.na(p$tp) && !is.na(b$tp)) {
    d <- p$tp - b$tp
    if (d >= 0.03) pros <- c(pros, paste0("3P% trends above same-position baseline (", fmt_pct(p$tp), " vs ", fmt_pct(b$tp), ")."))
    if (d <= -0.03) cons <- c(cons, paste0("3P% trails same-position baseline (", fmt_pct(p$tp), " vs ", fmt_pct(b$tp), ")."))
  }
  if (!is.null(b) && !is.na(p$rim) && !is.na(b$rim)) {
    d <- p$rim - b$rim
    if (d >= 0.04) pros <- c(pros, paste0("Rim finishing above baseline (", fmt_pct(p$rim), " vs ", fmt_pct(b$rim), ")."))
    if (d <= -0.04) cons <- c(cons, paste0("Rim finishing below baseline (", fmt_pct(p$rim), " vs ", fmt_pct(b$rim), ")."))
  }
  if (!is.null(b) && !is.na(p$efg) && !is.na(b$efg)) {
    d <- p$efg - b$efg
    if (d >= 0.02) pros <- c(pros, paste0("Efficiency edge vs peers (eFG% ", fmt_pct(p$efg), " vs ", fmt_pct(b$efg), ")."))
    if (d <= -0.02) cons <- c(cons, paste0("Efficiency lags peers (eFG% ", fmt_pct(p$efg), " vs ", fmt_pct(b$efg), ")."))
  }
  
  # Guaranteed fallback, but still specific
  if (length(pros) == 0) {
    pros <- c(paste0("Neutral shot profile under current filters (eFG% ", fmt_pct(p$efg), ", 3P% ", fmt_pct(p$tp), ")."))
  }
  if (length(cons) == 0) {
    cons <- c("No major red flags in the current shot sample. Main question is how the skill scales with role and difficulty.")
  }
  
  # Keep it tidy
  list(pros = unique(pros)[1:min(4, length(unique(pros)))],
       cons = unique(cons)[1:min(4, length(unique(cons)))])
}

make_summary_paragraph <- function(info, p, b, archetype) {
  if (is.null(p$n) || p$n == 0) return(paste0("<b>", info$player_name, "</b>: Load shot data to generate the report."))
  
  pos_word <- if (info$position == "G") "guard" else if (info$position == "F") "forward" else "center"
  diet <- if (!is.na(p$three_rate) && p$three_rate >= 0.48) "perimeter-heavy" else if (!is.na(p$rim_rate) && p$rim_rate >= 0.55) "rim-heavy" else "balanced"
  
  baseline_line <- ""
  if (!is.null(b) && !is.na(p$epps) && !is.na(b$epps)) {
    d <- p$epps - b$epps
    baseline_line <- paste0(" Against same-position peers, expected points per shot is ",
                            ifelse(d >= 0, "above", "below"), " baseline by ",
                            round(abs(d), 2), " (", round(p$epps, 2), " vs ", round(b$epps, 2), ").")
  }
  
  stability <- "Uncertainty is moderate."
  if (!is.na(p$fg_ci[1]) && !is.na(p$fg_ci[2])) {
    width <- p$fg_ci[2] - p$fg_ci[1]
    if (width <= 0.06) stability <- "The shooting read looks fairly stable for this sample size."
    if (width > 0.10) stability <- "The shooting read is noisy, so treat the exact percentages cautiously."
  }
  
  paste0(
    "<b>", info$player_name, "</b> (", info$team, ", ", info$conference, ") profiles as a ", pos_word,
    " with a <b>", diet, "</b> shot diet and an archetype of <b>", archetype, "</b>. ",
    "On the filtered sample: <b>", fmt_pct(p$fg), "</b> FG and <b>", fmt_pct(p$efg), "</b> eFG%. ",
    "From three: <b>", fmt_pct(p$tp), "</b> on ", p$three_att, " attempts. ",
    "At the rim: <b>", fmt_pct(p$rim), "</b> on ", p$rim_att, " attempts. ",
    baseline_line, " ",
    stability
  )
}

# -----------------------------
# NBA comp pool (expanded)
# -----------------------------
nba_comp_pool <- function() {
  # archetype tags are deliberately broad and interpretable
  data.frame(
    nba_player = c(
      # Guards: creators and shooters
      "Jalen Brunson","Tyrese Maxey","De'Aaron Fox","Shai Gilgeous-Alexander","Jamal Murray","Trae Young",
      "Donovan Mitchell","Zach LaVine","Devin Booker","CJ McCollum","Darius Garland","Fred VanVleet",
      "Anfernee Simons","Jordan Poole","Terry Rozier","Malcolm Brogdon","Immanuel Quickley","Tyus Jones",
      "Buddy Hield","Luke Kennard","Desmond Bane","Duncan Robinson","Grayson Allen","Seth Curry",
      "Marcus Smart","Alex Caruso","Jrue Holiday","Derrick White","Gary Payton II","De'Anthony Melton",
      # Wings: 3-and-D and scorers
      "Mikal Bridges","OG Anunoby","Herb Jones","Kawhi Leonard","Paul George","Jaylen Brown",
      "Brandon Ingram","Franz Wagner","Michael Porter Jr.","Khris Middleton","Tobias Harris","Andrew Wiggins",
      "De'Andre Hunter","Keegan Murray","Aaron Nesmith","Cam Johnson","Max Strus","Josh Hart",
      "Jaden McDaniels","Scottie Barnes","Jimmy Butler","RJ Barrett","Austin Reaves","Cody Martin",
      "Kentavious Caldwell-Pope","Dorian Finney-Smith","Royce O'Neale","Trevor Ariza (prime)","Bruce Brown","Nicolas Batum",
      # Bigs: rim runners and stretch
      "Rudy Gobert","Clint Capela","Jarrett Allen","Mitchell Robinson","Daniel Gafford","Robert Williams III",
      "Nic Claxton","Ivica Zubac","Jakob Poeltl","Steven Adams","Myles Turner","Brook Lopez",
      "Kristaps Porzingis","Karl-Anthony Towns","Al Horford","Bobby Portis","Naz Reid","Isaiah Hartenstein",
      "Bam Adebayo","Draymond Green","Jaren Jackson Jr.","Evan Mobley","Chet Holmgren","Victor Wembanyama"
    ),
    bucket = c(
      rep("G", 30),
      rep("F", 30),
      rep("C", 24)
    ),
    archetype = c(
      # 30 guards
      "Shot-creator guard (efficient perimeter scorer)","Shot-creator guard (efficient perimeter scorer)","Rim-pressure guard (downhill finisher)",
      "Shot-creator guard (efficient perimeter scorer)","Shot-creator guard (efficient perimeter scorer)","Shot-creator guard (efficient perimeter scorer)",
      "Shot-creator guard (efficient perimeter scorer)","Combo guard (mixed scoring profile)","Shot-creator guard (efficient perimeter scorer)",
      "Combo guard (mixed scoring profile)","Shot-creator guard (efficient perimeter scorer)","Combo guard (mixed scoring profile)",
      "Off-ball shooter guard","Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)",
      "Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)",
      "Off-ball shooter guard","Off-ball shooter guard","Off-ball shooter guard","Off-ball shooter guard","Off-ball shooter guard","Off-ball shooter guard",
      "Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)",
      "Combo guard (mixed scoring profile)","Combo guard (mixed scoring profile)",
      # 30 wings
      "3-and-D wing archetype (spacing-driven)","3-and-D wing archetype (spacing-driven)","3-and-D wing archetype (spacing-driven)",
      "Shot-making wing (midrange lean)","Shot-making wing (midrange lean)","Versatile forward (balanced shot diet)",
      "Shot-making wing (midrange lean)","Versatile forward (balanced shot diet)","Spacing wing (high-volume shooter)","Versatile forward (balanced shot diet)",
      "Versatile forward (balanced shot diet)","Versatile forward (balanced shot diet)","3-and-D wing archetype (spacing-driven)","Versatile forward (balanced shot diet)",
      "3-and-D wing archetype (spacing-driven)","3-and-D wing archetype (spacing-driven)","3-and-D wing archetype (spacing-driven)","Versatile forward (balanced shot diet)",
      "3-and-D wing archetype (spacing-driven)","Versatile forward (balanced shot diet)","Shot-making wing (midrange lean)","Versatile forward (balanced shot diet)",
      "Versatile forward (balanced shot diet)","3-and-D wing archetype (spacing-driven)",
      "3-and-D wing archetype (spacing-driven)","3-and-D wing archetype (spacing-driven)","3-and-D wing archetype (spacing-driven)",
      "3-and-D wing archetype (spacing-driven)","Versatile forward (balanced shot diet)","Versatile forward (balanced shot diet)",
      # 24 bigs
      "Rim-running big (paint finisher)","Rim-running big (paint finisher)","Rim-running big (paint finisher)","Rim-running big (paint finisher)",
      "Rim-running big (paint finisher)","Rim-running big (paint finisher)","Rim-running big (paint finisher)","Traditional big (inside-out mix)",
      "Traditional big (inside-out mix)","Traditional big (inside-out mix)",
      "Stretch big (spacing center)","Stretch big (spacing center)","Stretch big (spacing center)","Stretch big (spacing center)",
      "Stretch big (spacing center)","Traditional big (inside-out mix)","Stretch big (spacing center)","Traditional big (inside-out mix)",
      "Traditional big (inside-out mix)","Traditional big (inside-out mix)","Traditional big (inside-out mix)","Traditional big (inside-out mix)",
      "Traditional big (inside-out mix)","Traditional big (inside-out mix)"
    ),
    stringsAsFactors = FALSE
  )
}

bucket_from_pos <- function(pos) {
  if (pos == "G") return("G")
  if (pos == "F") return("F")
  "C"
}

# distance metric using shot profile only (interpretable)
profile_distance <- function(p, target_archetype, nba_row) {
  # We cannot compute NBA stats here, so we use archetype match + a few lightweight heuristics.
  # Lower is better.
  base <- if (nba_row$archetype == target_archetype) 0 else 1.2
  
  # add a small regularization based on "diet" implied by archetype
  rim_lean <- if (!is.na(p$rim_rate)) p$rim_rate else 0.35
  three_lean <- if (!is.na(p$three_rate)) p$three_rate else 0.33
  mid_lean <- if (!is.na(p$mid_rate)) p$mid_rate else 0.28
  
  want_three <- grepl("shooter|spacing|Movement", nba_row$archetype, ignore.case = TRUE)
  want_rim <- grepl("Rim-running|Rim-pressure", nba_row$archetype, ignore.case = TRUE)
  want_mid <- grepl("Midrange|Shot-making", nba_row$archetype, ignore.case = TRUE)
  
  penalty <- 0
  if (want_three) penalty <- penalty + abs(three_lean - 0.48)
  if (want_rim)   penalty <- penalty + abs(rim_lean - 0.55)
  if (want_mid)   penalty <- penalty + abs(mid_lean - 0.34)
  
  base + penalty
}

nba_comps <- function(pos, archetype, p) {
  pool <- nba_comp_pool()
  pool <- pool %>% filter(bucket == bucket_from_pos(pos))
  
  # Rank by archetype match then diet fit penalty
  pool$dist <- vapply(seq_len(nrow(pool)), function(i) profile_distance(p, archetype, pool[i, ]), numeric(1))
  pool <- pool %>% arrange(dist)
  
  # Primary is closest, floor and ceiling are same archetype but framed differently
  primary <- pool$nba_player[1]
  
  same_arch <- pool %>% filter(archetype == archetype)
  if (nrow(same_arch) >= 3) {
    floor <- same_arch$nba_player[min(2, nrow(same_arch))]
    ceiling <- same_arch$nba_player[min(3, nrow(same_arch))]
  } else {
    floor <- pool$nba_player[min(2, nrow(pool))]
    ceiling <- pool$nba_player[min(3, nrow(pool))]
  }
  
  list(primary = primary, floor = floor, ceiling = ceiling)
}

nba_comp_blurbs <- function(info, archetype, p, comps) {
  diet_bits <- c()
  if (!is.na(p$three_rate)) diet_bits <- c(diet_bits, paste0("3PA rate ", fmt_pct(p$three_rate)))
  if (!is.na(p$rim_rate)) diet_bits <- c(diet_bits, paste0("rim rate ", fmt_pct(p$rim_rate)))
  if (!is.na(p$efg)) diet_bits <- c(diet_bits, paste0("eFG% ", fmt_pct(p$efg)))
  diet_line <- paste(diet_bits, collapse = ", ")
  
  primary_txt <- paste0("<b>Primary:</b> ", comps$primary, " because the archetype matches (<b>", archetype, "</b>) and the shot profile aligns (", diet_line, ").")
  floor_txt <- paste0("<b>Floor:</b> ", comps$floor, " if the efficiency stabilizes lower or the role is narrower early (bench utility version of the same archetype).")
  ceiling_txt <- paste0("<b>Ceiling:</b> ", comps$ceiling, " if the key skill scales to NBA difficulty and the role expands (starter-level version of the archetype).")
  
  paste0("<p>", primary_txt, "</p><p>", floor_txt, "</p><p>", ceiling_txt, "</p>")
}

# -----------------------------
# Team fit table + blurbs
# -----------------------------
team_fit_pool <- function() {
  data.frame(
    team = c(
      "Spurs","Wizards","Pistons","Hornets","Blazers","Raptors","Jazz","Magic","Rockets","Grizzlies",
      "Nets","Bulls","Hawks","Heat","Pelicans","Kings","Suns","Lakers","Warriors","Celtics"
    ),
    pick_lo = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,18,22,24,26,28),
    pick_hi = c(3,5,7,10,10,12,14,14,18,20,20,22,24,24,26,28,30,30,30,30),
    needs = c(
      "shooting, secondary creation, wing size",
      "primary creation, shooting, rim protection",
      "spacing, two-way wing, decision-making",
      "shot creation, POA defense, shooting",
      "two-way wing, shooting, secondary creation",
      "guard creation, spacing, rim pressure",
      "perimeter shooting, size, rim pressure",
      "shooting, half-court creation, spacing wing",
      "self-creation, shooting, wing defense",
      "half-court scoring, spacing, perimeter defense",
      "shooting, guard creation, wing defense",
      "shooting, creation, rim pressure",
      "POA defense, shooting, connective play",
      "shot creation, spacing, frontcourt scoring",
      "spacing, creation, defensive versatility",
      "shooting, secondary creation, perimeter defense",
      "bench scoring, shooting, size",
      "shooting, perimeter defense, secondary creation",
      "spacing, secondary creation, wing defense",
      "depth shooting, lineup versatility, two-way wing"
    ),
    identity = c(
      "development, size-driven lineups",
      "rebuild, high reps for creators",
      "young core, needs spacing",
      "pace, athleticism, needs defense",
      "build around guards, needs wings",
      "versatile lineups, positionless",
      "big lineups, needs guards/wings",
      "defense + size, needs scoring",
      "switchability, needs shotmaking",
      "physical, needs half-court juice",
      "spacing, needs initiators",
      "retool, needs modern shot diet",
      "up-tempo, needs defense",
      "culture, needs efficient scoring",
      "stars, needs role glue",
      "offense, needs defense",
      "win-now, needs depth",
      "win-now, needs role fits",
      "motion, needs shooters",
      "contender, needs cheap contributors"
    ),
    stringsAsFactors = FALSE
  )
}

fit_score <- function(archetype, p, team_row) {
  needs <- tolower(team_row$needs)
  score <- 0
  
  has_shooting <- (!is.na(p$tp) && p$tp >= 0.36) || (!is.na(p$three_rate) && p$three_rate >= 0.35)
  rim_pressure <- (!is.na(p$rim_rate) && p$rim_rate >= 0.40)
  spacing_wing <- grepl("wing|3-and-D|Spacing", archetype, ignore.case = TRUE)
  creator <- grepl("Shot-creator|Midrange", archetype, ignore.case = TRUE)
  rim_big <- grepl("Rim-running|Interior", archetype, ignore.case = TRUE)
  stretch_big <- grepl("Stretch big", archetype, ignore.case = TRUE)
  
  if (grepl("shoot", needs) && has_shooting) score <- score + 3
  if (grepl("secondary creation|creation|initiator", needs) && creator) score <- score + 3
  if (grepl("rim protection", needs) && (rim_big || stretch_big)) score <- score + 2
  if (grepl("wing", needs) && spacing_wing) score <- score + 2
  if (grepl("rim pressure", needs) && rim_pressure) score <- score + 2
  if (grepl("defense|two-way|POA", needs) && (spacing_wing || grepl("Combo guard", archetype))) score <- score + 1
  
  # Minor tiebreak using efficiency
  if (!is.na(p$efg)) score <- score + (p$efg - 0.50) * 10
  
  score
}

best_fit_team <- function(pick_range, archetype, p) {
  pool <- team_fit_pool() %>%
    filter(!(pick_hi < pick_range[1] | pick_lo > pick_range[2]))
  
  if (nrow(pool) == 0) pool <- team_fit_pool()
  
  pool$score <- vapply(seq_len(nrow(pool)), function(i) fit_score(archetype, p, pool[i, ]), numeric(1))
  pool <- pool %>% arrange(desc(score))
  pool[1, ]
}

fit_blurb <- function(info, archetype, p, fit_row) {
  why <- c()
  
  if (grepl("shoot", tolower(fit_row$needs)) && !is.na(p$tp)) {
    why <- c(why, paste0("adds spacing value (3P% ", fmt_pct(p$tp), ", 3PA rate ", fmt_pct(p$three_rate), ")"))
  }
  if (grepl("creation|initiator", tolower(fit_row$needs))) {
    why <- c(why, paste0("fits a ", archetype, " role that can scale with reps"))
  }
  if (length(why) < 2 && !is.na(p$rim_rate)) {
    why <- c(why, paste0("shot diet suggests pressure points (rim rate ", fmt_pct(p$rim_rate), ")"))
  }
  why <- why[1:min(2, length(why))]
  
  paste0(
    "<b>Best fit:</b> ", fit_row$team, " (projected pick range ", fit_row$pick_lo, " to ", fit_row$pick_hi, "). ",
    "This fit makes sense because it ", paste(why, collapse = " and "), ". ",
    "Team identity: ", fit_row$identity, "."
  )
}

# -----------------------------
# Draft score (blends priors + shot profile)
# -----------------------------
shot_score_0_100 <- function(p) {
  if (is.null(p$n) || p$n == 0) return(50)
  # A simple, interpretable mapping:
  # eFG and EPPS do most of the work, with 3P% as a swing skill
  efg <- ifelse(is.na(p$efg), 0.50, p$efg)
  epps <- ifelse(is.na(p$epps), 0.95, p$epps)  # typical range ~0.8-1.2 in this synthetic world
  tp <- ifelse(is.na(p$tp), 0.34, p$tp)
  val <- 100 * (0.50 * efg + 0.35 * (pmin(1.3, pmax(0.6, epps)) / 2) + 0.15 * tp)
  pmin(95, pmax(35, val))
}

final_draft_score <- function(info, p) {
  ss <- shot_score_0_100(p)
  # Blend: priors matter most, shot profile meaningfully informs
  0.55 * info$scout_grade + 0.35 * ss + 0.10 * (info$role_value * 10)
}

draft_pick_from_score <- function(score, tier) {
  # Anchor to tier range, then nudge within range by score
  rng <- tier_to_range(tier)
  lo <- rng[1]; hi <- rng[2]
  # Map score (60-98) into [0,1]
  t <- (pmin(98, pmax(60, score)) - 60) / (98 - 60)
  # Higher score pushes toward earlier pick in range
  pick <- round(hi - t * (hi - lo))
  pick
}

# -----------------------------
# UI
# -----------------------------
ui <- bslib::page_sidebar(
  title = "🏀 Scout Report Generator",
  sidebar = bslib::sidebar(
    width = 370,
    bslib::card(
      bslib::card_header("Filters"),
      selectInput("conf_filter", "Conference", choices = NULL),
      selectInput("team_filter", "Team", choices = NULL),
      selectInput("pos_filter", "Position", choices = c("All" = "all", "G", "F", "C"), selected = "all"),
      selectInput("player_filter", "Player", choices = NULL),
      hr(),
      sliderInput("distance_filter", "Distance (ft)", min = 0, max = 35, value = c(0, 35)),
      checkboxGroupInput("result_filter", "Results", choices = c("Makes" = "1", "Misses" = "0"), selected = c("1", "0")),
      actionButton("load_data", "Generate Report", class = "btn-primary"),
      hr(),
      tags$small(
        style = "color:#666;",
        "This app uses synthetic shots for the visual layer, then blends shot profile with scout priors (tier, grade, role value) so draft ranges and comps behave coherently."
      )
    )
  ),
  bslib::layout_column_wrap(
    width = 1,
    bslib::navset_card_tab(
      height = "fit-content",
      bslib::nav_panel(
        "Scout Snapshot + Notes",
        br(),
        bslib::card(
          bslib::card_header("Scout Snapshot"),
          uiOutput("snapshot_cards")
        ),
        br(),
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            bslib::card_header("Scouting Notes"),
            uiOutput("notes_ui")
          ),
          bslib::card(
            bslib::card_header("Draft Projection"),
            uiOutput("draft_ui")
          )
        )
      ),
      bslib::nav_panel(
        "NBA Comparison",
        br(),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("NBA Comps (Primary, Floor, Ceiling)"),
            uiOutput("nba_comp_ui")
          ),
          bslib::card(
            bslib::card_header("Floor / Ceiling Ranges (Shooting Uncertainty)"),
            tableOutput("floor_ceiling")
          )
        )
      ),
      bslib::nav_panel(
        "Position Rank + Draft Fit",
        br(),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Position Rank (among predicted declarers)"),
            tableOutput("pos_rank_table")
          ),
          bslib::card(
            bslib::card_header("Best Fit Team"),
            uiOutput("fit_team_ui")
          )
        )
      ),
      bslib::nav_panel(
        "Shot Deep Dive",
        br(),
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            bslib::card_header("Shot Map"),
            plotlyOutput("shot_chart", height = "500px")
          ),
          bslib::card(
            bslib::card_header("Zone Profile"),
            plotOutput("zone_profile", height = "500px")
          )
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  players_df <- get_top_college_players()
  shot_data <- reactiveVal(NULL)
  
  # Conference first
  observe({
    confs <- sort(unique(players_df$conference))
    updateSelectInput(session, "conf_filter", choices = c("All" = "all", confs), selected = "all")
  })
  
  # Conference -> Team
  observeEvent(input$conf_filter, {
    df <- players_df
    if (!is.null(input$conf_filter) && input$conf_filter != "all") df <- df %>% filter(conference == input$conf_filter)
    teams <- sort(unique(df$team))
    updateSelectInput(session, "team_filter", choices = c("All" = "all", teams), selected = "all")
  }, ignoreInit = TRUE)
  
  # Conference + Team + Position -> Player
  observeEvent(list(input$conf_filter, input$team_filter, input$pos_filter), {
    df <- players_df
    if (!is.null(input$conf_filter) && input$conf_filter != "all") df <- df %>% filter(conference == input$conf_filter)
    if (!is.null(input$team_filter) && input$team_filter != "all") df <- df %>% filter(team == input$team_filter)
    if (!is.null(input$pos_filter)  && input$pos_filter  != "all") df <- df %>% filter(position == input$pos_filter)
    
    if (nrow(df) == 0) {
      updateSelectInput(session, "player_filter", choices = c("No players found for these filters" = ""))
      return()
    }
    
    choices <- stats::setNames(as.character(df$player_id), paste0(df$player_name, " (", df$team, ")"))
    updateSelectInput(session, "player_filter", choices = choices, selected = choices[1])
  }, ignoreInit = TRUE)
  
  # Generate report data
  observeEvent(input$load_data, {
    req(nzchar(input$player_filter))
    pid <- suppressWarnings(as.integer(input$player_filter))
    req(!is.na(pid))
    
    info <- players_df %>% filter(player_id == pid)
    req(nrow(info) == 1)
    
    withProgress(message = "Generating player shot sample...", value = 0, {
      incProgress(0.4)
      shots <- generate_college_shots(info$player_name, info$team, info$position, num_shots = 750)
      incProgress(1)
      shots$CONFERENCE <- info$conference
      shot_data(shots)
    })
  })
  
  filtered_shots <- reactive({
    req(shot_data())
    df <- shot_data()
    df %>%
      filter(
        as.character(SHOT_MADE_FLAG) %in% input$result_filter,
        SHOT_DISTANCE >= input$distance_filter[1],
        SHOT_DISTANCE <= input$distance_filter[2]
      )
  })
  
  report <- reactive({
    req(filtered_shots())
    req(nzchar(input$player_filter))
    pid <- suppressWarnings(as.integer(input$player_filter))
    req(!is.na(pid))
    
    info <- players_df %>% filter(player_id == pid)
    req(nrow(info) == 1)
    
    p <- summarize_profile(filtered_shots())
    b <- build_baseline(players_df, info$position, exclude_name = info$player_name)
    
    archetype <- role_archetype(info$position, p, b)
    notes <- strengths_concerns(info$position, p, b)
    
    # Draft scoring
    sscore <- shot_score_0_100(p)
    fscore <- final_draft_score(info, p)
    pick_est <- draft_pick_from_score(fscore, info$proj_tier)
    pick_rng <- tier_to_range(info$proj_tier)
    
    # NBA comps
    comps <- nba_comps(info$position, archetype, p)
    
    # Best fit team based on projected tier range
    fit_row <- best_fit_team(pick_rng, archetype, p)
    
    list(
      info = info, p = p, b = b,
      archetype = archetype, notes = notes,
      shot_score = sscore, final_score = fscore,
      pick_est = pick_est, pick_rng = pick_rng,
      comps = comps, fit_row = fit_row
    )
  })
  
  # Snapshot cards
  output$snapshot_cards <- renderUI({
    req(report())
    r <- report()
    p <- r$p
    info <- r$info
    
    pos_label <- ifelse(info$position == "G", "Guard", ifelse(info$position == "F", "Forward", "Center"))
    
    bslib::layout_columns(
      bslib::value_box(
        title = "Player",
        value = info$player_name,
        showcase = bsicons::bs_icon("person-badge"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Team",
        value = paste0(info$team, " (", info$conference, ")"),
        showcase = bsicons::bs_icon("building"),
        theme = "info"
      ),
      bslib::value_box(
        title = "Position",
        value = pos_label,
        showcase = bsicons::bs_icon("arrows-move"),
        theme = "secondary"
      ),
      bslib::value_box(
        title = "Role Archetype",
        value = r$archetype,
        showcase = bsicons::bs_icon("tags"),
        theme = "success"
      ),
      bslib::value_box(
        title = "Shot Sample (filtered)",
        value = p$n,
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "warning"
      ),
      bslib::value_box(
        title = "FG% / eFG%",
        value = paste0(fmt_pct(p$fg), " / ", fmt_pct(p$efg)),
        showcase = bsicons::bs_icon("bullseye"),
        theme = "success"
      ),
      bslib::value_box(
        title = "3P% (3PA rate)",
        value = paste0(fmt_pct(p$tp), " (", fmt_pct(p$three_rate), ")"),
        showcase = bsicons::bs_icon("activity"),
        theme = "success"
      ),
      bslib::value_box(
        title = "Rim FG% (Rim rate)",
        value = paste0(fmt_pct(p$rim), " (", fmt_pct(p$rim_rate), ")"),
        showcase = bsicons::bs_icon("circle"),
        theme = "info"
      )
    )
  })
  
  # Notes + paragraph
  output$notes_ui <- renderUI({
    req(report())
    r <- report()
    info <- r$info
    p <- r$p
    paragraph <- make_summary_paragraph(info, p, r$b, r$archetype)
    
    pros_html <- paste0("<ul>", paste0("<li>", r$notes$pros, "</li>", collapse = ""), "</ul>")
    cons_html <- paste0("<ul>", paste0("<li>", r$notes$cons, "</li>", collapse = ""), "</ul>")
    
    tagList(
      tags$h4("Strengths"),
      HTML(pros_html),
      tags$h4("Concerns"),
      HTML(cons_html),
      tags$hr(),
      tags$h4("Summary"),
      HTML(paste0("<p>", paragraph, "</p>"))
    )
  })
  
  # Draft UI (tier + score + pick range explanation)
  output$draft_ui <- renderUI({
    req(report())
    r <- report()
    info <- r$info
    
    rng <- r$pick_rng
    tagList(
      tags$p(HTML(paste0("<b>Projection tier:</b> ", info$proj_tier))),
      tags$p(HTML(paste0("<b>Scout grade:</b> ", round(info$scout_grade, 0), " / 100"))),
      tags$p(HTML(paste0("<b>Role value:</b> ", round(info$role_value, 1), " / 10"))),
      tags$hr(),
      tags$p(HTML(paste0("<b>Shot score:</b> ", round(r$shot_score, 1), " / 100 (from eFG, EPPS, 3P%)"))),
      tags$p(HTML(paste0("<b>Final draft score:</b> ", round(r$final_score, 1), " / 100 (blended with priors)"))),
      tags$p(HTML(paste0("<b>Projected pick:</b> ~", r$pick_est, " (tier range ", rng[1], " to ", rng[2], ")"))),
      tags$small(style="color:#666;",
                 "Note: This is a synthetic scouting model. Priors prevent top prospects from being misranked by shot-only noise.")
    )
  })
  
  # NBA comparison UI
  output$nba_comp_ui <- renderUI({
    req(report())
    r <- report()
    info <- r$info
    p <- r$p
    comps <- r$comps
    
    blurb <- nba_comp_blurbs(info, r$archetype, p, comps)
    
    tagList(
      tags$p(HTML(paste0("<b>Archetype filter:</b> ", r$archetype))),
      HTML(blurb),
      tags$small(style="color:#666;",
                 "Comps are archetype-first. Floor and ceiling stay within the archetype so they read like scout outcomes.")
    )
  })
  
  # Floor / ceiling table (shooting uncertainty)
  output$floor_ceiling <- renderTable({
    req(report())
    p <- report()$p
    
    data.frame(
      Metric = c("FG%", "3P%", "Rim FG%", "EPPS"),
      Floor = c(fmt_pct(p$fg_ci[1]), fmt_pct(p$tp_ci[1]), fmt_pct(p$rim_ci[1]), fmt_num(p$epps_ci[1])),
      Current = c(fmt_pct(p$fg), fmt_pct(p$tp), fmt_pct(p$rim), fmt_num(p$epps)),
      Ceiling = c(fmt_pct(p$fg_ci[2]), fmt_pct(p$tp_ci[2]), fmt_pct(p$rim_ci[2]), fmt_num(p$epps_ci[2])),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Position rank table (among predicted declarers) using final draft score
  output$pos_rank_table <- renderTable({
    req(report())
    r <- report()
    info <- r$info
    
    # Build quick shot profiles for peers (synthetic, small samples) so ranking is not purely priors
    # This keeps it coherent but still "uses data".
    pool <- players_df %>% filter(position == info$position, declares_pred)
    
    # compute "final score" for each peer with a lightweight shot sample (cached by seed)
    compute_peer_score <- function(row) {
      s <- generate_college_shots(row$player_name, row$team, row$position, num_shots = 420)
      p <- summarize_profile(s)
      final_draft_score(row, p)
    }
    
    pool$peer_final_score <- vapply(seq_len(nrow(pool)), function(i) compute_peer_score(pool[i, ]), numeric(1))
    pool <- pool %>% arrange(desc(peer_final_score))
    pool$pos_rank <- seq_len(nrow(pool))
    
    out <- pool %>%
      select(pos_rank, player_name, team, conference, scout_grade, role_value, proj_tier, peer_final_score) %>%
      mutate(peer_final_score = round(peer_final_score, 1)) %>%
      head(15)
    
    names(out) <- c("Pos Rank", "Player", "Team", "Conf", "Scout Grade", "Role Value", "Tier", "Final Score")
    out
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Fit team UI with short blurb
  output$fit_team_ui <- renderUI({
    req(report())
    r <- report()
    info <- r$info
    p <- r$p
    fit_row <- r$fit_row
    
    blurb <- fit_blurb(info, r$archetype, p, fit_row)
    
    tagList(
      tags$p(HTML(blurb)),
      tags$hr(),
      tags$p(HTML(paste0("<b>Team needs tags:</b> ", fit_row$needs))),
      tags$small(style="color:#666;",
                 "Fit is a simple rule-based match between archetype and team needs within the projected pick range.")
    )
  })
  
  # Shot map
  output$shot_chart <- renderPlotly({
    req(filtered_shots())
    df <- filtered_shots()
    court <- draw_court()
    
    p <- court +
      geom_point(
        data = df,
        aes(
          x = LOC_X, y = LOC_Y,
          color = factor(SHOT_MADE_FLAG),
          text = paste0(
            "Result: ", ifelse(SHOT_MADE_FLAG == 1, "Made", "Miss"),
            "<br>Distance: ", round(SHOT_DISTANCE, 1), " ft",
            "<br>Type: ", SHOT_TYPE
          )
        ),
        alpha = 0.60, size = 2.2
      ) +
      scale_color_manual(values = c("0" = "#d62728", "1" = "#2ca02c"), labels = c("Miss", "Make"), name = "Result") +
      labs(title = "Shot Map (filtered)") +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "pan")
  })
  
  # Zone profile
  output$zone_profile <- renderPlot({
    req(filtered_shots())
    df <- add_flags(filtered_shots())
    
    zone_df <- df %>%
      mutate(zone = case_when(
        is_rim ~ "Rim (0-8)",
        is_mid ~ "Mid (8-22.75)",
        is_three & is_corner3 ~ "Corner 3",
        is_three ~ "Above-break 3",
        TRUE ~ "Other"
      )) %>%
      group_by(zone) %>%
      summarise(att = n(), fg = mean(SHOT_MADE_FLAG), .groups = "drop") %>%
      mutate(att_share = att / sum(att))
    
    ggplot(zone_df, aes(x = reorder(zone, -att_share), y = att_share)) +
      geom_col(alpha = 0.85) +
      geom_text(
        aes(label = paste0("Share ", scales::percent(att_share, accuracy = 1),
                           "\nFG ", scales::percent(fg, accuracy = 0.1))),
        vjust = -0.2, size = 3.6
      ) +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(zone_df$att_share) * 1.25)) +
      labs(x = "", y = "Attempt Share", title = "Shot Diet + FG% by Zone (filtered)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)
