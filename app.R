# College basketball scouting reports
# Real 2025-26 men's college shot data (ESPN play-by-play via sportsdataverse),
# with style matches to 2025-26 NBA players.
# Data is built by scripts/build_data.R and read from data/.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
players <- readRDS("data/players.rds")
nba     <- readRDS("data/nba_players.rds")
shots   <- as.data.frame(readRDS("data/shots.rds"))
league  <- readRDS("data/league.rds")

ZONES    <- levels(shots$zone)
lg_zone  <- league$zone
shots_by <- split(shots, shots$pid)

players <- players |> arrange(desc(fga))
players$label <- sprintf("%s  |  %s  |  %s", players$name, players$team, players$pos)
choices <- setNames(as.character(players$pid), players$label)
default_pid <- {
  i <- which(players$name == "AJ Dybantsa"); if (length(i)) players$pid[i[1]] else players$pid[1]
}

zcols   <- grep("^z_", names(players), value = TRUE)
feat_z  <- as.matrix(players[, zcols]); rownames(feat_z) <- players$pid
nba_z   <- as.matrix(nba[, zcols])

POS_NAME <- c(G = "Guard", PG = "Guard", F = "Forward", SF = "Forward", PF = "Forward", C = "Center")
pos_name <- function(x) ifelse(x %in% names(POS_NAME), POS_NAME[x], x)

# ---------------------------------------------------------------------------
# Hex binning (pointy-top hexes, hoop at the origin, feet)
# ---------------------------------------------------------------------------
HEX_W    <- 3.2
HEX_SIZE <- HEX_W / sqrt(3)

hex_assign <- function(x, y, size = HEX_SIZE) {
  q <- (sqrt(3) / 3 * x - y / 3) / size
  r <- (2 / 3 * y) / size
  s <- -q - r
  rq <- round(q); rr <- round(r); rs <- round(s)
  dq <- abs(rq - q); dr <- abs(rr - r); ds <- abs(rs - s)
  fix_q <- dq > dr & dq > ds
  fix_r <- !fix_q & dr > ds
  rq <- ifelse(fix_q, -rr - rs, rq)
  rr <- ifelse(fix_r, -rq - rs, rr)
  data.frame(key = paste(rq, rr, sep = "_"),
             cx = size * sqrt(3) * (rq + rr / 2),
             cy = size * 1.5 * rr)
}

hex_polygon <- function(cx, cy, size) {
  ang <- (30 + 60 * 0:5) * pi / 180
  data.frame(px = rep(cx, each = 6) + size * cos(ang), py = rep(cy, each = 6) + size * sin(ang),
             id = rep(seq_along(cx), each = 6))
}

lg_hex <- local({
  s <- shots[shots$hy > -6 & shots$hy < 36 & abs(shots$hx) < 25.5, ]
  h <- hex_assign(s$hx, s$hy)
  data.frame(key = h$key, made = s$made) |>
    group_by(key) |> summarise(n = n(), lg_fg = mean(made), .groups = "drop")
})

# ---------------------------------------------------------------------------
# Court
# ---------------------------------------------------------------------------
arc <- function(r, a0, a1, cx = 0, cy = 0, n = 80) {
  t <- seq(a0, a1, length.out = n)
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}
court_lines <- local({
  corner_x <- 21.67; r3 <- 22.15
  a3 <- atan2(sqrt(r3^2 - corner_x^2), corner_x)
  L <- list(
    outer = data.frame(x = c(-25, -25, 25, 25, -25), y = c(-5.25, 36, 36, -5.25, -5.25)),
    paint = data.frame(x = c(-8, -8, 8, 8), y = c(-5.25, 13.75, 13.75, -5.25)),
    ft    = arc(6, 0, pi, 0, 13.75),
    ra    = arc(4, 0, pi, 0, 0),
    hoop  = arc(0.75, 0, 2 * pi, 0, 0),
    board = data.frame(x = c(-3, 3), y = c(-1.25, -1.25)),
    c3_l  = data.frame(x = c(-corner_x, -corner_x), y = c(-5.25, r3 * sin(a3))),
    c3_r  = data.frame(x = c(corner_x, corner_x), y = c(-5.25, r3 * sin(a3))),
    arc3  = arc(r3, a3, pi - a3, 0, 0, 120)
  )
  bind_rows(lapply(names(L), function(n) cbind(L[[n]], part = n)))
})
court_lines_layer <- function(alpha = 1) {
  geom_path(data = court_lines, aes(x, y, group = part), colour = "#5f5877", linewidth = 0.5, alpha = alpha)
}
court_layer <- function() {
  list(
    coord_fixed(xlim = c(-25.5, 25.5), ylim = c(-5.75, 36.5), expand = FALSE),
    theme_void(base_size = 12),
    theme(plot.background = element_rect(fill = "#faf8fc", colour = NA),
          panel.background = element_rect(fill = "#faf8fc", colour = NA),
          legend.position = "none",
          plot.margin = margin(2, 2, 2, 2))
  )
}

# ---------------------------------------------------------------------------
# Helpers and metric definitions
# ---------------------------------------------------------------------------
INK <- "#1f1933"; NAVY <- "#3b2a6b"; ACCENT <- "#c77d14"; SLATE <- "#9a93b3"; MUTED <- "#6b6580"

pct_txt <- function(x, d = 0) ifelse(is.na(x), "-", paste0(formatC(100 * x, format = "f", digits = d), "%"))
ord <- function(n) {
  n <- round(n); s <- ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  paste0(n, s)
}

# key, short label, group, value formatter, high-text, low-text
M <- list(
  list("three_rate",   "Three-point rate",  "Shot profile", function(v) pct_txt(v),
       "Three-point heavy: %s of shots", "Rarely shoots threes: %s of shots"),
  list("rim_rate",     "Shots at the rim",  "Shot profile", function(v) pct_txt(v),
       "Lives at the rim: %s of shots", "Few shots at the rim: %s of shots"),
  list("mid_rate",     "Mid-range rate",    "Shot profile", function(v) pct_txt(v),
       "Mid-range scorer: %s of shots", "Almost no mid-range: %s of shots"),
  list("self_created", "Unassisted makes",  "Shot profile", function(v) pct_txt(v),
       "Creates his own shot: %s of makes unassisted", "Mostly finishes off others: %s of makes unassisted"),
  list("fga40",        "Shots per 40",      "Shot profile", function(v) sprintf("%.1f", v),
       "High volume: %s shots per 40", "Low volume: %s shots per 40"),
  list("shot_quality", "Shot quality",      "Efficiency",   function(v) sprintf("%.2f", v),
       "Gets good looks: %s expected points per shot", "Tough shot diet: %s expected points per shot"),
  list("shot_making",  "Shot-making",       "Efficiency",   function(v) sprintf("%+.1f", 100 * v),
       "Beats expectation: %s points per 100 shots", "Below expectation: %s points per 100 shots"),
  list("pps",          "Points per shot",   "Efficiency",   function(v) sprintf("%.2f", v),
       "Efficient scorer: %s points per shot", "Inefficient scorer: %s points per shot"),
  list("ts_pct",       "True shooting %",   "Efficiency",   function(v) pct_txt(v, 1),
       "Strong true shooting: %s", "Weak true shooting: %s"),
  list("ft_rate",      "Free-throw rate",   "Efficiency",   function(v) sprintf("%.2f", v),
       "Gets to the line: %s free throws per shot", "Rarely gets to the line: %s free throws per shot"),
  list("pts40",        "Points",            "Per 40 minutes", function(v) sprintf("%.1f", v),
       "Scores: %s points per 40", "Low scoring: %s points per 40"),
  list("ast40",        "Assists",           "Per 40 minutes", function(v) sprintf("%.1f", v),
       "Passer: %s assists per 40", "Few assists: %s per 40"),
  list("reb40",        "Rebounds",          "Per 40 minutes", function(v) sprintf("%.1f", v),
       "Rebounder: %s per 40", "Few rebounds: %s per 40"),
  list("stl40",        "Steals",            "Per 40 minutes", function(v) sprintf("%.1f", v),
       "Active hands: %s steals per 40", "Few steals: %s per 40"),
  list("blk40",        "Blocks",            "Per 40 minutes", function(v) sprintf("%.1f", v),
       "Shot blocker: %s blocks per 40", "Few blocks: %s per 40"),
  list("tov40",        "Turnovers (fewer = better)", "Per 40 minutes", function(v) sprintf("%.1f", v),
       "Protects the ball: %s turnovers per 40", "Turnover-prone: %s per 40")
)

LB_CHOICES <- c(
  "Shot-making (pts per 100)" = "shot_making", "Shot quality" = "shot_quality",
  "Points per shot" = "pps", "True shooting %" = "ts_pct",
  "Three-point rate" = "three_rate", "Three-point %" = "three_pct", "Shots at the rim" = "rim_rate",
  "Mid-range rate" = "mid_rate", "Shots per 40" = "fga40", "Points per 40" = "pts40",
  "Assists per 40" = "ast40", "Rebounds per 40" = "reb40", "Blocks per 40" = "blk40",
  "Steals per 40" = "stl40", "Free-throw rate" = "ft_rate"
)
PCT_KEYS <- c("three_rate", "three_pct", "rim_rate", "mid_rate", "ts_pct")
fmt_metric <- function(v, key) {
  if (key %in% PCT_KEYS) pct_txt(v, 1)
  else if (key == "shot_making") sprintf("%+.1f", 100 * v)
  else if (key %in% c("shot_quality", "pps", "ft_rate")) sprintf("%.2f", v)
  else sprintf("%.1f", v)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#faf8fc", fg = INK, primary = NAVY, secondary = MUTED,
  base_font = font_collection("IBM Plex Sans", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("IBM Plex Serif", "Charter", "Georgia", "Times New Roman", "serif"),
  "font-size-base" = "0.94rem", "border-radius" = "2px", "border-radius-lg" = "2px",
  "navbar-bg" = "#faf8fc"
)

css <- "
  @font-face { font-family:'IBM Plex Sans'; font-weight:400; src:url('fonts/IBMPlexSans-Regular.woff2') format('woff2'); font-display:swap; }
  @font-face { font-family:'IBM Plex Sans'; font-weight:500; src:url('fonts/IBMPlexSans-Medium.woff2') format('woff2'); font-display:swap; }
  @font-face { font-family:'IBM Plex Sans'; font-weight:600; src:url('fonts/IBMPlexSans-SemiBold.woff2') format('woff2'); font-display:swap; }
  @font-face { font-family:'IBM Plex Serif'; font-weight:400; src:url('fonts/IBMPlexSerif-Regular.woff2') format('woff2'); font-display:swap; }
  @font-face { font-family:'IBM Plex Serif'; font-weight:500; src:url('fonts/IBMPlexSerif-Medium.woff2') format('woff2'); font-display:swap; }
  :root { --serif: 'IBM Plex Serif', Charter, Georgia, serif; }
  body { background:#faf8fc; font-variant-numeric: tabular-nums; }
  .navbar { border-bottom:1px solid #e2dcee; padding-top:.35rem; padding-bottom:.35rem; box-shadow:none; }
  .navbar-brand { font-family:inherit; font-weight:600; font-size:.95rem; letter-spacing:.01em; }
  .navbar .nav-link { font-size:.9rem; color:#6b6580; }
  .navbar .nav-link.active { color:#1f1933; font-weight:600; border-bottom:2px solid #1f1933; }
  .wrap { max-width:1080px; margin:0 auto; padding:26px 20px 70px; }
  .eyebrow { font-size:.7rem; letter-spacing:.09em; text-transform:uppercase; color:#6b6580; margin:0 0 6px; font-weight:600; }
  .sect { border-top:1px solid #1f1933; margin-top:34px; padding-top:10px; }
  .sect h3 { font-family:inherit; font-size:.72rem; letter-spacing:.09em; text-transform:uppercase; font-weight:700; margin:0 0 14px; color:#1f1933; }
  .picker .selectize-input { border-radius:2px; border-color:#cfc7e0; box-shadow:none; padding:9px 12px; font-size:1rem; }
  .picker .form-group { margin-bottom:4px; max-width:520px; }
  .picker .hint { color:#6b6580; font-size:.8rem; }
  .player h1 { font-family:var(--serif); font-size:2.3rem; font-weight:500; margin:22px 0 2px; letter-spacing:-.01em; }
  .player .meta { color:#6b6580; font-size:.9rem; }
  .player .arch { margin-top:8px; font-size:.9rem; }
  .player .arch b { font-weight:600; }
  .keys { display:flex; flex-wrap:wrap; margin:20px 0 0; border-top:1px solid #e2dcee; border-bottom:1px solid #e2dcee; }
  .keys .k { flex:1 1 110px; padding:10px 14px; border-left:1px solid #e2dcee; }
  .keys .k:first-child { border-left:0; padding-left:0; }
  .keys .v { font-family:var(--serif); font-size:1.5rem; line-height:1.15; }
  .keys .l { font-size:.68rem; letter-spacing:.07em; text-transform:uppercase; color:#6b6580; }
  .stand { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:10px 44px; }
  .stand h4 { font-family:var(--bs-body-font-family); font-size:.72rem; letter-spacing:.07em; text-transform:uppercase; color:#6b6580; margin:0 0 6px; font-weight:600; }
  .stand ul { list-style:none; padding:0; margin:0; }
  .stand li { padding:5px 0; border-bottom:1px solid #ebe6f3; display:flex; justify-content:space-between; gap:14px; font-size:.9rem; }
  .stand li span.p { color:#6b6580; white-space:nowrap; }
  .stand .up li::before { content:''; width:6px; height:6px; background:#c77d14; margin:7px 8px 0 0; flex:none; }
  .stand .dn li::before { content:''; width:6px; height:6px; background:#9a93b3; margin:7px 8px 0 0; flex:none; }
  .stand li div { flex:1; display:flex; }
  .two { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:10px 44px; }
  .viewtoggle .shiny-options-group { display:flex; gap:16px; }
  .viewtoggle .form-check { padding-left:0; margin:0; }
  .viewtoggle input { display:none; }
  .viewtoggle label { cursor:pointer; font-size:.82rem; color:#6b6580; padding-bottom:2px; border-bottom:2px solid transparent; }
  .viewtoggle input:checked + span { color:#1f1933; font-weight:600; border-bottom:2px solid #1f1933; }
  .viewtoggle .form-group { margin:0 0 6px; }
  table.shiny-table, .shiny-table { width:100%; border-collapse:collapse; font-size:.88rem; }
  .shiny-table th { font-size:.68rem; letter-spacing:.07em; text-transform:uppercase; color:#6b6580; font-weight:600; border-bottom:1px solid #1f1933 !important; padding:6px 8px; background:transparent; }
  .shiny-table td { border-bottom:1px solid #ebe6f3 !important; border-top:0 !important; padding:7px 8px; background:transparent; }
  .shiny-table tr:hover td { background:#f1edf8; }
  .pos { color:#c77d14; } .neg { color:#6d5fa8; }
  .comp { display:flex; justify-content:space-between; gap:12px; padding:8px 0; border-bottom:1px solid #ebe6f3; }
  .comp .nm { font-weight:600; } .comp a { color:#1f1933; text-decoration:underline; text-decoration-color:#cfc7e0; text-underline-offset:3px; }
  .comp a:hover { text-decoration-color:#1f1933; }
  .comp .meta { color:#6b6580; font-size:.82rem; }
  .comp .m { font-family:var(--serif); font-size:1.05rem; white-space:nowrap; }
  .legend { display:flex; align-items:center; gap:10px; justify-content:center; font-size:.78rem; color:#6b6580; margin-top:4px; }
  .legend .bar { width:180px; height:6px; border-radius:3px; background:linear-gradient(90deg,#6d5fa8,#ebe6f3,#d9902a); }
  .pgroup { margin-bottom:18px; }
  .pgroup h4 { font-family:var(--bs-body-font-family); font-size:.7rem; letter-spacing:.09em; text-transform:uppercase; color:#6b6580; font-weight:600; margin:0 0 4px; }
  .pr { display:grid; grid-template-columns:190px minmax(0,1fr) 64px 28px; align-items:center; gap:14px; padding:6px 0; border-bottom:1px solid #ebe6f3; }
  .pr .pl { font-size:.9rem; }
  .pr .pv { text-align:right; font-weight:500; }
  .pr .pp { text-align:right; color:#9a93b3; font-size:.78rem; }
  .track { position:relative; height:4px; background:#e6e0f0; border-radius:2px; }
  .fill { position:absolute; left:0; top:0; height:4px; border-radius:2px; }
  .dot { position:absolute; top:-4px; width:12px; height:12px; margin-left:-6px; border-radius:50%; }
  .fill.mid, .dot.mid { background:#3b2a6b; } .fill.hi, .dot.hi { background:#c77d14; } .fill.lo, .dot.lo { background:#9a93b3; }
  .note { color:#6b6580; font-size:.8rem; line-height:1.45; margin:8px 0 0; }
  .filters { display:flex; flex-wrap:wrap; gap:6px 22px; align-items:flex-end; }
  .filters .form-group { margin-bottom:6px; min-width:190px; }
  .filters label { font-size:.7rem; letter-spacing:.07em; text-transform:uppercase; color:#6b6580; font-weight:600; }
  .prose { max-width:700px; line-height:1.6; }
  .prose h3 { font-size:1.25rem; margin:28px 0 6px; }
  @media (max-width: 760px) {
    .wrap { padding:18px 16px 60px; }
    .stand, .two { grid-template-columns:minmax(0,1fr); }
    .player h1 { font-size:1.8rem; }
    .keys .k { flex:1 1 30%; border-left:0; padding-left:0; }
    #chart { height:360px !important; }
    .pr { grid-template-columns:118px minmax(0,1fr) 52px; gap:10px; } .pr .pp { display:none; }
    .shiny-table th, .shiny-table td { padding:6px 5px; font-size:.78rem; }
  }
"

ui <- page_navbar(
  title = "Scouting reports",
  id = "nav",
  theme = theme,
  fillable = FALSE,
  lang = "en",
  window_title = "Scouting reports | Rishabh Singh",
  header = tags$head(tags$style(HTML(css))),

  nav_panel("Scout report",
    div(class = "wrap",
      div(class = "picker",
          p(class = "eyebrow", "Player"),
          selectizeInput("player", NULL, choices = NULL, width = "100%",
                         options = list(placeholder = "Search a player or team", maxOptions = 50)),
          div(class = "hint", sprintf("%s men's college basketball. %s players with at least %d shots and %d minutes.",
                                      league$season, format(league$n_players, big.mark = ","), league$min_fga, league$min_min))),
      uiOutput("header"),
      div(class = "sect", h3("What stands out"), uiOutput("standouts")),
      div(class = "sect",
          div(style = "display:flex; justify-content:space-between; align-items:baseline; flex-wrap:wrap;",
              h3("Shot chart and zones"),
              div(class = "viewtoggle",
                  radioButtons("view", NULL, inline = TRUE,
                               choices = c("Efficiency map" = "hex", "Every shot" = "pts"), selected = "hex"))),
          div(class = "two",
              div(plotOutput("chart", height = "440px"), uiOutput("chart_legend"), p(class = "note", textOutput("chart_note", inline = TRUE))),
              div(div(style = "overflow-x:auto;", tableOutput("zones")),
                  p(class = "note", "League is every shot by every player in the data. Zone FG% is lightly pulled toward the league average for small samples.")))),
      div(class = "sect", h3("Percentiles against all qualified players"),
          uiOutput("pctplot"),
          p(class = "note", "Amber = top 20%, grey = bottom 20%. On wider screens the faint number at the far right is the percentile. Shot quality is expected points per shot from where the shots were taken. Shot-making is points per 100 shots above or below that expectation.")),
      div(class = "sect", h3("Similar players"),
          div(class = "two",
              div(p(class = "eyebrow", "Closest college profiles"), uiOutput("comps_college")),
              div(p(class = "eyebrow", "Closest 2025-26 NBA profiles"), uiOutput("comps_nba"))),
          p(class = "note", "Matched on shot mix, shot creation, free-throw rate and per-40 rebounding, passing and defense, each measured against the player's own league. It describes role and style, not talent level or an NBA projection."))
    )),

  nav_panel("Leaderboard",
    div(class = "wrap",
      div(class = "filters",
          selectInput("lb_metric", "Rank by", choices = LB_CHOICES, selected = "shot_making"),
          selectInput("lb_arch", "Archetype", choices = c("All", sort(unique(players$archetype)))),
          selectInput("lb_pos", "Position", choices = c("All", "Guard" = "G", "Forward" = "F", "Center" = "C")),
          sliderInput("lb_minfga", "Minimum shots", min = league$min_fga, max = 500, value = 250, step = 10)),
      div(class = "sect", h3("Top 25"), tableOutput("lb_table")),
      div(class = "sect", h3("Every filtered player"),
          div(class = "filters",
              selectInput("lb_x", "X-axis", choices = LB_CHOICES, selected = "shot_quality"),
              selectInput("lb_y", "Y-axis", choices = LB_CHOICES, selected = "shot_making")),
          plotlyOutput("lb_scatter", height = "520px"))
    )),

  nav_panel("How it works",
    div(class = "wrap prose",
      h3("What this is"),
      p("A scouting-report generator built from real play-by-play data for the 2025-26 men's college basketball season. ",
        "Pick a player to see where they shoot from, how efficient they are given where they shoot, how their production ranks, and which college and NBA players have the closest profile."),
      h3("Data"),
      p("ESPN play-by-play and box scores, published by ", tags$a("sportsdataverse", href = "https://sportsdataverse.org", target = "_blank"),
        ". ESPN logs each shot with court coordinates; every shot is folded onto one half court with the hoop at the origin. ",
        sprintf("%s college shots were charted league-wide. The reports use the %s shots from %s players with at least %d shots and %d minutes.",
                format(league$n_shots, big.mark = ","), format(nrow(shots), big.mark = ","), format(league$n_players, big.mark = ","), league$min_fga, league$min_min)),
      p("As a check, the shots parsed match 98.8% of the field-goal attempts in the college box scores (100.3% for the NBA set). Shots logged without coordinates and a few obvious logging errors (threes from inside 20 feet, twos from behind the arc) are dropped."),
      h3("Zones"),
      p("Restricted area (within 4 ft of the hoop), paint outside the restricted area, mid-range (other twos), corner three, and above-the-break three."),
      h3("Shot quality and shot-making"),
      p(tags$b("Shot quality"), " is the points per shot a league-average shooter would score from the same zones in the same proportions. It measures shot selection, not skill."),
      p(tags$b("Shot-making"), " is actual points minus that expectation, per 100 shots. It is shrunk toward zero with a 150-shot prior so small samples don't produce extreme values."),
      h3("Archetypes"),
      p("Players are grouped with k-means (6 clusters) on twelve standardized features: three-point rate, rim rate, mid-range rate, share of threes from the corner, unassisted share of makes, dunk rate, shots per 40, assists, rebounds, blocks and steals per 40, and free-throw rate. Each cluster is named from its average profile."),
      h3("Similar players"),
      p("College matches are the nearest neighbors in that feature space. NBA matches use the same twelve features computed for the ",
        sprintf("%d NBA players with at least 300 shots and 500 minutes in the 2025-26 regular season", nrow(nba)),
        ", each standardized within its own league. That makes the match a comparison of role, such as a high-volume creator or a rim-running big, and not of how good either player is."),
      h3("Limits"),
      tags$ul(
        tags$li("Coordinates are logged by hand and are noisy, especially near the rim and on the three-point line."),
        tags$li("No defensive tracking, shot clock, or defender distance. Shot quality uses location only."),
        tags$li("One season. Small samples are flagged, but percentiles are still affected by them."),
        tags$li("Descriptive, not predictive. This is not a draft board or a projection of NBA success.")
      ),
      p(class = "note", "Code and data pipeline: ", tags$a("github.com/rishabhsxngh/nba-prospect-evaluation-tool",
        href = "https://github.com/rishabhsxngh/nba-prospect-evaluation-tool", target = "_blank"))
    ))
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  updateSelectizeInput(session, "player", choices = choices, selected = as.character(default_pid), server = TRUE)

  observeEvent(input$goto, {
    updateSelectizeInput(session, "player", choices = choices, selected = as.character(input$goto), server = TRUE)
    nav_select("nav", "Scout report")
  })

  cur <- reactive({
    req(input$player)
    players[players$pid == as.integer(input$player), ][1, ]
  })

  cur_shots <- reactive({
    sh <- shots_by[[as.character(cur()$pid)]]
    req(!is.null(sh))
    sh
  })

  # ---- Header -------------------------------------------------------------
  output$header <- renderUI({
    p <- cur()
    keys <- list(
      list(sprintf("%.1f", p$pts40), "Points / 40"), list(sprintf("%.1f", p$ast40), "Assists / 40"),
      list(sprintf("%.1f", p$reb40), "Rebounds / 40"), list(pct_txt(p$fg_pct, 1), "FG%"),
      list(pct_txt(p$three_pct, 1), "3P%"), list(pct_txt(p$ts_pct, 1), "True shooting"),
      list(format(p$fga, big.mark = ","), "Shots charted")
    )
    tagList(div(class = "player",
      h1(p$name),
      div(class = "meta", sprintf("%s  ·  %s  ·  %d games  ·  %.1f minutes per game", p$team, pos_name(p$pos), p$games, p$mpg)),
      div(class = "arch", "Archetype: ", tags$b(p$archetype)),
      div(class = "keys", lapply(keys, function(k) div(class = "k", div(class = "v", k[[1]]), div(class = "l", k[[2]]))))
    ))
  })

  # ---- What stands out ------------------------------------------------------
  output$standouts <- renderUI({
    p <- cur()
    rows <- lapply(M, function(m) {
      key <- m[[1]]
      list(pct = p[[paste0("p_", key)]], up = sprintf(m[[5]], m[[4]](p[[key]])), dn = sprintf(m[[6]], m[[4]](p[[key]])))
    })
    pct <- vapply(rows, function(r) r$pct, numeric(1))
    hi <- order(-pct); hi <- hi[pct[hi] >= 85][seq_len(min(5, sum(pct >= 85, na.rm = TRUE)))]
    lo <- order(pct);  lo <- lo[pct[lo] <= 15][seq_len(min(4, sum(pct <= 15, na.rm = TRUE)))]
    li <- function(idx, txt) if (length(idx)) lapply(idx, function(i) tags$li(div(rows[[i]][[txt]]), span(class = "p", paste0(ord(rows[[i]]$pct), " pct")))) else tags$li(div("None"))
    caveat <- if (p$fga < 250) p(class = "note", sprintf("Small sample (%d shots): treat the efficiency numbers with care.", p$fga)) else NULL
    tagList(
      div(class = "stand",
          div(h4("Strengths and tendencies"), tags$ul(class = "up", li(hi, "up"))),
          div(h4("Weak spots"), tags$ul(class = "dn", li(lo, "dn")))),
      caveat)
  })

  # ---- Shot chart ---------------------------------------------------------
  output$chart <- renderPlot({
    sh <- cur_shots()
    sh <- sh[sh$hy > -6 & sh$hy < 36 & abs(sh$hx) < 25.5, ]
    if (input$view == "pts") {
      sh$res <- factor(ifelse(sh$made == 1, "Made", "Missed"), levels = c("Missed", "Made"))
      ggplot() + court_layer() + court_lines_layer(0.9) +
        geom_point(data = sh[sh$res == "Missed", ], aes(hx, hy), shape = 4, size = 1.7, colour = "#bcb5cf", stroke = 0.6) +
        geom_point(data = sh[sh$res == "Made", ], aes(hx, hy), shape = 21, size = 2.2, fill = NAVY, colour = "#faf8fc", stroke = 0.4)
    } else {
      h <- hex_assign(sh$hx, sh$hy)
      d <- data.frame(key = h$key, cx = h$cx, cy = h$cy, made = sh$made) |>
        group_by(key, cx, cy) |> summarise(n = n(), fg = mean(made), .groups = "drop") |>
        left_join(lg_hex, by = "key", suffix = c("", "_lg")) |>
        mutate(lg_fg = coalesce(lg_fg, mean(sh$made)),
               diff = ((fg * n + lg_fg * 4) / (n + 4) - lg_fg),
               scale = pmin(1, 0.38 + 0.62 * sqrt(n / quantile(n, 0.95))))
      poly <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
        z <- hex_polygon(d$cx[i], d$cy[i], HEX_SIZE * d$scale[i] * 0.98)
        z$diff <- d$diff[i]; z$id <- i; z
      }))
      lim <- 0.14
      ggplot() + court_layer() +
        geom_polygon(data = poly, aes(px, py, group = id, fill = pmax(-lim, pmin(lim, diff))), colour = NA) +
        scale_fill_gradient2(low = "#6d5fa8", mid = "#ebe6f3", high = "#d9902a", midpoint = 0,
                             limits = c(-lim, lim), breaks = c(-lim, 0, lim),
                             guide = "none") +
        court_lines_layer(0.85)
    }
  }, res = 96, bg = "#faf8fc")

  output$chart_legend <- renderUI({
    if (input$view != "hex") return(NULL)
    div(class = "legend", span("Worse than league"), div(class = "bar"), span("Better"))
  })

  output$chart_note <- renderText({
    if (input$view == "hex")
      "Hex size is how often they shoot there. Colour is FG% against the league from the same spot, with small samples pulled toward the league."
    else "Filled is made, x is missed. The hoop is at the bottom and the half-court line at the top."
  })

  # ---- Zone table ---------------------------------------------------------
  output$zones <- renderTable({
    p <- cur()
    a <- as.numeric(p[paste0("a_", 1:5)]); m <- as.numeric(p[paste0("m_", 1:5)])
    lg <- lg_zone[match(ZONES, as.character(lg_zone$zone)), ]
    fg_shr <- (m + lg$fg * 20) / (a + 20)
    dif <- 100 * (fg_shr - lg$fg)
    data.frame(
      Zone = ZONES,
      Shots = ifelse(a == 0, "0", format(a, big.mark = ",")),
      Share = pct_txt(a / sum(a)),
      `FG%` = ifelse(a == 0, "-", pct_txt(fg_shr, 1)),
      League = pct_txt(lg$fg, 1),
      Diff = ifelse(a == 0, "-", sprintf("<span class='%s'>%+.1f</span>", ifelse(dif >= 0, "pos", "neg"), dif)),
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x, striped = FALSE, hover = FALSE, bordered = FALSE, align = "lrrrrr", width = "100%")

  # ---- Percentiles --------------------------------------------------------
  output$pctplot <- renderUI({
    p <- cur()
    groups <- unique(vapply(M, function(m) m[[3]], character(1)))
    div(class = "pct", lapply(groups, function(g) {
      div(class = "pgroup", h4(g),
          lapply(Filter(function(m) m[[3]] == g, M), function(m) {
            pc <- p[[paste0("p_", m[[1]])]]
            col <- if (pc >= 80) "hi" else if (pc <= 20) "lo" else "mid"
            div(class = "pr",
                span(class = "pl", m[[2]]),
                div(class = "track", div(class = paste("fill", col), style = sprintf("width:%.1f%%", pc)),
                    div(class = paste("dot", col), style = sprintf("left:%.1f%%", pc))),
                span(class = "pv", m[[4]](p[[m[[1]]]])),
                span(class = "pp", paste0(round(pc))))
          }))
    }))
  })

  # ---- Similar players ----------------------------------------------------
  comp_rows <- function(top, med, meta_fun, link) {
    lapply(seq_along(top), function(i) {
      match <- max(0, round(100 * (1 - top[[i]] / med)))
      nm <- names(top)[i]
      div(class = "comp",
          div(div(class = "nm", if (link) tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('goto', %s, {priority: 'event'}); return false;", nm), meta_fun(nm)$name) else meta_fun(nm)$name),
              div(class = "meta", meta_fun(nm)$meta)),
          div(class = "m", paste0(match, "%")))
    })
  }

  output$comps_college <- renderUI({
    p <- cur(); me <- feat_z[as.character(p$pid), ]
    d <- sqrt(rowSums((sweep(feat_z, 2, me))^2)); names(d) <- rownames(feat_z)
    d <- d[names(d) != as.character(p$pid)]
    top <- sort(d)[1:6]
    tagList(comp_rows(top, median(d), function(id) {
      q <- players[players$pid == as.integer(id), ][1, ]
      list(name = q$name, meta = sprintf("%s  ·  %s  ·  %s", q$team, pos_name(q$pos), q$archetype))
    }, link = TRUE))
  })

  output$comps_nba <- renderUI({
    p <- cur(); me <- as.numeric(p[, zcols])
    d <- sqrt(rowSums((sweep(nba_z, 2, me))^2)); names(d) <- as.character(nba$pid)
    top <- sort(d)[1:6]
    tagList(comp_rows(top, median(d), function(id) {
      q <- nba[nba$pid == as.integer(id), ][1, ]
      list(name = q$name, meta = sprintf("%s  ·  %s  ·  %.1f shots per 40", q$team, pos_name(q$pos), q$fga40))
    }, link = FALSE))
  })

  # ---- Leaderboard --------------------------------------------------------
  lb <- reactive({
    d <- players |> filter(fga >= input$lb_minfga)
    if (input$lb_arch != "All") d <- d |> filter(archetype == input$lb_arch)
    if (input$lb_pos == "G") d <- d |> filter(pos %in% c("G", "PG"))
    if (input$lb_pos == "F") d <- d |> filter(pos %in% c("F", "SF", "PF"))
    if (input$lb_pos == "C") d <- d |> filter(pos == "C")
    d
  })

  output$lb_table <- renderTable({
    d <- lb(); key <- input$lb_metric
    validate(need(nrow(d) > 0, "No players match these filters."))
    d <- d[order(-d[[key]]), ][seq_len(min(25, nrow(d))), ]
    out <- data.frame(
      `#` = seq_len(nrow(d)), Player = d$name, Team = d$team, Pos = pos_name(d$pos), Archetype = d$archetype,
      Shots = d$fga, check.names = FALSE, stringsAsFactors = FALSE)
    out[[names(LB_CHOICES)[LB_CHOICES == key]]] <- fmt_metric(d[[key]], key)
    out
  }, striped = FALSE, hover = FALSE, bordered = FALSE, width = "100%")

  output$lb_scatter <- renderPlotly({
    d <- lb(); validate(need(nrow(d) > 0, "No players match these filters."))
    xk <- input$lb_x; yk <- input$lb_y
    xl <- names(LB_CHOICES)[LB_CHOICES == xk]; yl <- names(LB_CHOICES)[LB_CHOICES == yk]
    d$tip <- sprintf("%s (%s)<br>%s: %s<br>%s: %s", d$name, d$team, xl, fmt_metric(d[[xk]], xk), yl, fmt_metric(d[[yk]], yk))
    sc <- function(v, k) if (k == "shot_making" || k %in% PCT_KEYS) 100 * v else v
    d$xv <- sc(d[[xk]], xk); d$yv <- sc(d[[yk]], yk)
    xl <- paste0(xl, if (xk %in% PCT_KEYS) " (%)" else ""); yl <- paste0(yl, if (yk %in% PCT_KEYS) " (%)" else "")
    plot_ly(d, x = ~xv, y = ~yv, color = ~archetype, text = ~tip, hoverinfo = "text",
            type = "scatter", mode = "markers", marker = list(size = 7, opacity = 0.8),
            colors = c(NAVY, ACCENT, "#6d5fa8", "#9aa0ad", "#b8912a", "#5f8f6b")) |>
      layout(xaxis = list(title = xl, zeroline = FALSE, gridcolor = "#ebe6f3"),
             yaxis = list(title = yl, zeroline = FALSE, gridcolor = "#ebe6f3"),
             paper_bgcolor = "#faf8fc", plot_bgcolor = "#faf8fc", legend = list(orientation = "h", y = -0.2),
             font = list(family = "IBM Plex Sans, -apple-system, Segoe UI, Arial, sans-serif", color = INK))
  })
}

shinyApp(ui, server)
