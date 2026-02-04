#
# NBA Scouting Tool (Shiny)
#
# Run: Click "Run App" in RStudio
#

library(shiny)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(readr)
  library(purrr)
})

# ============================
# NBA SCOUTING TOOL (R)
# ============================

# ----------------------------
# Utility helpers
# ----------------------------
clamp <- function(x, lo = 0, hi = 1) pmin(pmax(x, lo), hi)

safe_z <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# Convert ranks (1 best) to a normalized [0,1] scale where higher is better
rank_to_score <- function(rank, max_rank = 363) {
  clamp(1 - (rank - 1) / (max_rank - 1), 0, 1)
}

minmax01 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# ----------------------------
# Feature engineering: stats + context
# ----------------------------
make_features <- function(prospects, max_rank = 363) {
  p <- prospects %>%
    mutate(
      # Basic rates
      fg_pct  = ifelse(fga > 0, fgm / fga, NA_real_),
      fg3_pct = ifelse(fg3a > 0, fg3m / fg3a, NA_real_),
      ft_pct  = ifelse(fta > 0, ftm / fta, NA_real_),
      
      # Shooting profile
      three_rate = ifelse(fga > 0, fg3a / fga, NA_real_),
      ft_rate    = ifelse(fga > 0, fta / fga, NA_real_),
      
      # Playmaking / decision-making
      ast_rate = ifelse(mp > 0, ast / (mp/40), NA_real_),
      tov_rate = ifelse(mp > 0, tov / (mp/40), NA_real_),
      ast_to   = ifelse(tov > 0, ast / tov, ast),
      
      # Defense proxies
      stl40 = ifelse(mp > 0, stl / (mp/40), NA_real_),
      blk40 = ifelse(mp > 0, blk / (mp/40), NA_real_),
      dreb40 = ifelse(mp > 0, drb / (mp/40), NA_real_),
      
      # Rebounding
      trb40 = ifelse(mp > 0, trb / (mp/40), NA_real_),
      orb40 = ifelse(mp > 0, orb / (mp/40), NA_real_),
      
      # Scoring load
      pts40 = ifelse(mp > 0, pts / (mp/40), NA_real_),
      
      # Experience & role
      starter_share = ifelse(gp > 0, starts / gp, NA_real_),
      is_upperclass = ifelse(class_year %in% c("JR","SR"), 1, 0),
      
      # Schedule context
      opp_off_score = rank_to_score(opp_off_rank, max_rank = max_rank),
      opp_def_score = rank_to_score(opp_def_rank, max_rank = max_rank),
      sos_scaled = minmax01(sos),
      context_quality = 0.55 * opp_def_score + 0.45 * opp_off_score,
      
      # Size for position targets
      height_target = case_when(
        position_group == "G" ~ 75,
        position_group == "W" ~ 79,
        position_group == "B" ~ 82,
        TRUE ~ 78
      ),
      wings_target = case_when(
        position_group == "G" ~ 78,
        position_group == "W" ~ 82,
        position_group == "B" ~ 85,
        TRUE ~ 81
      ),
      size_plus = (height_in - height_target) * 0.6 + (wingspan_in - wings_target) * 0.4
    )
  
  p %>%
    group_by(position_group) %>%
    mutate(
      z_ts     = safe_z(ts),
      z_efg    = safe_z(efg),
      z_ft     = safe_z(ft_pct),
      z_3p     = safe_z(fg3_pct),
      z_3rate  = safe_z(three_rate),
      
      z_pts40  = safe_z(pts40),
      z_usg    = safe_z(usg),
      
      z_ast40  = safe_z(ast_rate),
      z_astto  = safe_z(ast_to),
      
      z_stl40  = safe_z(stl40),
      z_blk40  = safe_z(blk40),
      z_dreb40 = safe_z(dreb40),
      
      z_trb40  = safe_z(trb40),
      z_orb40  = safe_z(orb40),
      
      z_size   = safe_z(size_plus),
      
      z_context = safe_z(context_quality),
      z_sos     = safe_z(sos_scaled),
      
      z_startshare = safe_z(starter_share)
    ) %>%
    ungroup()
}

# ----------------------------
# Injury tool
# ----------------------------
injury_model <- function(prospects, injuries) {
  if (is.null(injuries) || nrow(injuries) == 0) {
    return(
      prospects %>%
        mutate(
          games_missed = NA_real_,
          injury_events = 0,
          severe_events = 0,
          surgery_events = 0,
          recurrence_events = 0,
          days_missed_total = 0,
          availability_score = 85,
          injury_prone_score = 25,
          injury_flags = "No injury log provided (neutral defaults)."
        )
    )
  }
  
  inj <- injuries %>%
    mutate(
      severe = ifelse(severity >= 4, 1, 0),
      surgery = ifelse(surgery %in% c(1, TRUE), 1, 0),
      recurrence = ifelse(recurrence %in% c(1, TRUE), 1, 0),
      days_missed = ifelse(is.na(days_missed), 0, days_missed)
    ) %>%
    group_by(player, season) %>%
    summarise(
      injury_events = n(),
      severe_events = sum(severe, na.rm = TRUE),
      surgery_events = sum(surgery, na.rm = TRUE),
      recurrence_events = sum(recurrence, na.rm = TRUE),
      days_missed_total = sum(days_missed, na.rm = TRUE),
      common_body = names(sort(table(body_part), decreasing = TRUE))[1] %||% NA_character_,
      .groups = "drop"
    )
  
  prospects %>%
    left_join(inj, by = c("player","season")) %>%
    mutate(
      injury_events = replace_na(injury_events, 0),
      severe_events = replace_na(severe_events, 0),
      surgery_events = replace_na(surgery_events, 0),
      recurrence_events = replace_na(recurrence_events, 0),
      days_missed_total = replace_na(days_missed_total, 0),
      
      games_missed = pmax(0, 32 - gp),
      
      load_proxy = clamp(minmax01(mp) * 0.6 + minmax01(usg) * 0.4, 0, 1),
      
      injury_risk = clamp(
        0.35 * minmax01(days_missed_total) +
          0.20 * minmax01(injury_events) +
          0.20 * minmax01(severe_events) +
          0.15 * minmax01(surgery_events) +
          0.10 * (recurrence_events > 0) +
          0.10 * load_proxy,
        0, 1
      ),
      
      availability_score = round(100 * clamp(0.85 * (1 - injury_risk) + 0.15 * minmax01(gp), 0, 1)),
      injury_prone_score = round(100 * injury_risk),
      
      injury_flags = case_when(
        surgery_events >= 1 ~ "Surgery history: diligence on medicals.",
        recurrence_events >= 1 ~ "Recurrence history: monitor chronic issue.",
        severe_events >= 1 ~ "At least one severe injury: check re-injury risk.",
        days_missed_total >= 30 ~ "Significant time missed: investigate conditioning/return.",
        injury_events >= 2 ~ "Multiple injury events: trend review recommended.",
        TRUE ~ "No major injury flags from provided log."
      )
    )
}

# ----------------------------
# Scoring model (weights you can tune)
# ----------------------------
score_prospect <- function(df) {
  z_to_0100 <- function(z) 100 * plogis(z)
  
  df %>%
    mutate(
      comp_scoring = z_to_0100(
        0.50 * z_ts +
          0.20 * z_efg +
          0.25 * z_pts40 +
          0.10 * z_context +
          0.05 * z_sos -
          0.10 * z_usg
      ),
      comp_shooting = z_to_0100(
        0.45 * z_ft +
          0.25 * z_3p +
          0.20 * z_3rate +
          0.10 * z_context
      ),
      comp_playmaking = z_to_0100(
        0.50 * z_ast40 +
          0.35 * z_astto +
          0.10 * z_context -
          0.05 * safe_z(tov_rate)
      ),
      comp_defense = z_to_0100(
        0.40 * z_stl40 +
          0.30 * z_blk40 +
          0.20 * z_dreb40 +
          0.10 * z_size +
          0.10 * z_context
      ),
      comp_rebounding = z_to_0100(
        0.60 * z_trb40 +
          0.20 * z_orb40 +
          0.20 * z_size
      ),
      comp_readiness = z_to_0100(
        0.35 * z_startshare +
          0.25 * z_context +
          0.20 * z_sos +
          0.20 * safe_z(is_upperclass)
      ),
      
      exp_bonus = 0 +
        3 * one_and_done +
        4 * four_year_starter +
        2 * (starter_share >= 0.8),
      
      upside_age = clamp((21 - age) / 3, 0, 1),
      floor_age  = clamp((age - 19) / 4, 0, 1),
      
      availability_penalty = clamp((85 - availability_score) / 2, 0, 20),
      
      scout_score_raw =
        0.20 * comp_scoring +
        0.20 * comp_shooting +
        0.16 * comp_playmaking +
        0.18 * comp_defense +
        0.10 * comp_rebounding +
        0.08 * comp_readiness +
        0.08 * (100 * upside_age) +
        0.02 * (100 * floor_age) +
        exp_bonus -
        availability_penalty,
      
      scout_score = round(clamp(scout_score_raw / 100, 0, 1) * 100),
      
      tier = case_when(
        scout_score >= 85 ~ "Top-10 caliber",
        scout_score >= 75 ~ "1st round",
        scout_score >= 65 ~ "2nd round",
        scout_score >= 55 ~ "Two-way / camp",
        TRUE ~ "Monitor / development"
      ),
      projection_notes = case_when(
        comp_shooting >= 70 & comp_defense >= 70 ~ "Two-way spacer archetype.",
        comp_playmaking >= 75 & comp_scoring >= 70 ~ "Primary initiator upside.",
        comp_defense >= 75 & comp_rebounding >= 70 ~ "Defensive anchor / rim-impact profile.",
        comp_scoring >= 75 & comp_shooting < 55 ~ "Scorer with shooting swing skill.",
        TRUE ~ "Balanced profile: evaluate role fit + film context."
      )
    )
}

# ----------------------------
# Comps engine
# ----------------------------
get_comps <- function(scored_df, historical_scored, k = 5) {
  if (is.null(historical_scored) || nrow(historical_scored) == 0) return(tibble())
  
  comp_features <- c(
    "comp_scoring","comp_shooting","comp_playmaking",
    "comp_defense","comp_rebounding","comp_readiness",
    "availability_score","age","height_in","wingspan_in","weight_lb"
  )
  
  comp_features <- intersect(comp_features, intersect(names(scored_df), names(historical_scored)))
  
  hist_mat <- historical_scored %>%
    select(all_of(comp_features)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  hist_mat[is.na(hist_mat)] <- 0
  
  map_dfr(seq_len(nrow(scored_df)), function(i) {
    target <- scored_df[i, ] %>%
      select(all_of(comp_features)) %>%
      mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    target[is.na(target)] <- 0
    
    dists <- sqrt(rowSums((t(t(hist_mat) - as.vector(target)))^2))
    idx <- order(dists)[1:min(k, length(dists))]
    
    tibble(
      player = scored_df$player[i],
      comp_rank = seq_along(idx),
      comp_player = historical_scored$player[idx],
      comp_season = historical_scored$season[idx],
      distance = round(dists[idx], 3),
      comp_scout_score = historical_scored$scout_score[idx]
    )
  })
}

# ----------------------------
# Reporting helpers
# ----------------------------
scout_card_df <- function(scored_row) {
  scored_row %>%
    transmute(
      player, season, position_group, age,
      height_in, wingspan_in, weight_lb,
      scout_score, tier,
      scoring = round(comp_scoring),
      shooting = round(comp_shooting),
      playmaking = round(comp_playmaking),
      defense = round(comp_defense),
      rebounding = round(comp_rebounding),
      readiness = round(comp_readiness),
      availability_score, injury_prone_score,
      injury_flags,
      projection_notes
    )
}

components_long <- function(card) {
  card %>%
    select(player, scoring, shooting, playmaking, defense, rebounding, readiness) %>%
    pivot_longer(-player, names_to = "component", values_to = "value")
}

# ----------------------------
# Example data (so the app runs immediately)
# ----------------------------
set.seed(7)

example_prospects <- tibble(
  player = c("Prospect A","Prospect B","Prospect C"),
  season = c("2025-26","2025-26","2025-26"),
  position_group = c("W","G","B"),
  age = c(19.2, 22.1, 20.5),
  height_in = c(80, 74, 83),
  wingspan_in = c(83, 77, 87),
  weight_lb = c(215, 185, 245),
  gp = c(31, 29, 30),
  starts = c(29, 29, 30),
  mp = c(980, 920, 860),
  pts = c(520, 470, 410),
  trb = c(210, 120, 320),
  orb = c(55, 20, 110),
  drb = c(155, 100, 210),
  ast = c(105, 165, 70),
  tov = c(65, 70, 55),
  stl = c(45, 55, 30),
  blk = c(35, 6, 65),
  pf = c(70, 55, 85),
  fga = c(390, 360, 280),
  fgm = c(190, 160, 150),
  fg3a = c(140, 190, 20),
  fg3m = c(50, 72, 4),
  fta = c(140, 90, 160),
  ftm = c(110, 78, 105),
  usg = c(24, 27, 22),
  ts = c(0.585, 0.575, 0.555),
  efg = c(0.535, 0.540, 0.560),
  sos = c(9.5, 7.2, 8.8),
  opp_off_rank = c(55, 40, 70),
  opp_def_rank = c(30, 75, 25),
  class_year = c("FR","SR","SO"),
  one_and_done = c(1, 0, 0),
  four_year_starter = c(0, 1, 0)
)

example_injuries <- tibble(
  player = c("Prospect A","Prospect B","Prospect B","Prospect C"),
  season = c("2025-26","2025-26","2025-26","2025-26"),
  injury_type = c("Ankle sprain","Hamstring strain","Ankle sprain","Shoulder subluxation"),
  body_part = c("Ankle","Hamstring","Ankle","Shoulder"),
  severity = c(2,3,2,4),
  days_missed = c(8, 12, 6, 28),
  surgery = c(0,0,0,0),
  recurrence = c(0,1,0,0)
)

example_historical <- example_prospects %>%
  mutate(
    player = paste0(player, " (Hist)"),
    season = "2021-22",
    age = age + runif(n(), -0.4, 0.4),
    ts = clamp(ts + rnorm(n(), 0, 0.02), 0.45, 0.70),
    efg = clamp(efg + rnorm(n(), 0, 0.02), 0.40, 0.70),
    usg = clamp(usg + rnorm(n(), 0, 2), 10, 35)
  )

# ============================
# SHINY APP
# ============================

ui <- fluidPage(
  titlePanel("NBA Prospect Scouting Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Load data (optional)"),
      fileInput("prospects_file", "Upload prospects.csv", accept = c(".csv")),
      fileInput("injuries_file", "Upload injuries.csv", accept = c(".csv")),
      fileInput("historical_file", "Upload historical.csv (for comps)", accept = c(".csv")),
      
      tags$hr(),
      
      h4("Scoring settings"),
      sliderInput("max_rank", "Max rank for opponent ranks (NCAA teams)", min = 250, max = 400, value = 363, step = 1),
      
      sliderInput("k_comps", "Number of comps", min = 1, max = 10, value = 5, step = 1),
      
      tags$hr(),
      
      h4("Select player"),
      uiOutput("player_picker"),
      
      helpText("If you do not upload files, the app uses built-in example data.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Big Board",
                 tableOutput("board_tbl"),
                 plotOutput("board_plot", height = "300px")
        ),
        tabPanel("Scout Card",
                 tableOutput("card_tbl"),
                 plotOutput("components_plot", height = "300px"),
                 tableOutput("comps_tbl")
        ),
        tabPanel("Data Preview",
                 h4("Prospects (first 10 rows)"),
                 tableOutput("prospects_preview"),
                 h4("Injuries (first 10 rows)"),
                 tableOutput("injuries_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  prospects_data <- reactive({
    if (is.null(input$prospects_file)) {
      return(example_prospects)
    }
    read_csv(input$prospects_file$datapath, show_col_types = FALSE)
  })
  
  injuries_data <- reactive({
    if (is.null(input$injuries_file)) {
      return(example_injuries)
    }
    read_csv(input$injuries_file$datapath, show_col_types = FALSE)
  })
  
  historical_data <- reactive({
    if (is.null(input$historical_file)) {
      return(example_historical)
    }
    read_csv(input$historical_file$datapath, show_col_types = FALSE)
  })
  
  # Previews
  output$prospects_preview <- renderTable({
    head(prospects_data(), 10)
  })
  
  output$injuries_preview <- renderTable({
    head(injuries_data(), 10)
  })
  
  # Pipeline
  scored_board <- reactive({
    p <- prospects_data()
    
    # Minimal validation: required columns
    required <- c(
      "player","season","position_group","age","height_in","wingspan_in","weight_lb",
      "gp","starts","mp","pts","trb","orb","drb","ast","tov","stl","blk","pf",
      "fga","fgm","fg3a","fg3m","fta","ftm","usg","ts","efg",
      "sos","opp_off_rank","opp_def_rank","class_year","one_and_done","four_year_starter"
    )
    missing <- setdiff(required, names(p))
    validate(need(length(missing) == 0, paste("prospects is missing columns:", paste(missing, collapse = ", "))))
    
    feat <- make_features(p, max_rank = input$max_rank)
    feat_inj <- injury_model(feat, injuries_data())
    score_prospect(feat_inj) %>%
      arrange(desc(scout_score))
  })
  
  scored_historical <- reactive({
    h <- historical_data()
    if (is.null(h) || nrow(h) == 0) return(tibble())
    
    # historical should be same schema as prospects for smooth feature engineering
    feat <- make_features(h, max_rank = input$max_rank)
    feat_inj <- injury_model(feat, tibble())
    score_prospect(feat_inj)
  })
  
  # Player picker
  output$player_picker <- renderUI({
    players <- scored_board()$player
    selectInput("player", "Prospect", choices = players, selected = players[1])
  })
  
  # Big board table
  output$board_tbl <- renderTable({
    scored_board() %>%
      select(player, season, position_group, age, scout_score, tier, availability_score, injury_prone_score) %>%
      head(50)
  })
  
  # Big board plot
  output$board_plot <- renderPlot({
    df <- scored_board() %>%
      mutate(player = factor(player, levels = rev(player))) %>%
      head(25)
    
    ggplot(df, aes(x = player, y = scout_score)) +
      geom_col() +
      coord_flip() +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "Top Prospects by Scout Score", x = NULL, y = "Scout Score (0-100)") +
      theme_minimal()
  })
  
  # Scout card for selected player
  selected_row <- reactive({
    scored_board() %>% filter(player == input$player) %>% slice(1)
  })
  
  output$card_tbl <- renderTable({
    scout_card_df(selected_row())
  })
  
  output$components_plot <- renderPlot({
    card <- scout_card_df(selected_row())
    plot_df <- components_long(card)
    
    ggplot(plot_df, aes(x = component, y = value)) +
      geom_col() +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = paste0(card$player[1], " Components"), x = NULL, y = "Score (0-100)") +
      theme_minimal()
  })
  
  output$comps_tbl <- renderTable({
    hist_scored <- scored_historical()
    if (nrow(hist_scored) == 0) return(tibble(note = "Upload a historical.csv to enable comps."))
    
    comps <- get_comps(selected_row(), hist_scored, k = input$k_comps) %>%
      arrange(comp_rank)
    
    if (nrow(comps) == 0) return(tibble(note = "No comps available."))
    
    comps %>% select(comp_rank, comp_player, comp_season, distance, comp_scout_score)
  })
}

shinyApp(ui = ui, server = server)

