# College Basketball Scouting Reports
# Real 2025-26 men's college shot data (ESPN play-by-play via sportsdataverse).
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

feat_z <- as.matrix(players[, grep("^z_", names(players))])
rownames(feat_z) <- players$pid

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

# League baseline FG% per hex (all qualified players' shots)
lg_hex <- local({
  s <- shots[shots$hy > -6 & shots$hy < 36 & abs(shots$hx) < 25.5, ]
  h <- hex_assign(s$hx, s$hy)
  d <- data.frame(key = h$key, made = s$made) |>
    group_by(key) |> summarise(n = n(), lg_fg = mean(made), .groups = "drop")
  d
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
    outer   = data.frame(x = c(-25, -25, 25, 25, -25), y = c(-5.25, 36, 36, -5.25, -5.25)),
    paint   = data.frame(x = c(-8, -8, 8, 8), y = c(-5.25, 13.75, 13.75, -5.25)),
    ft      = arc(6, 0, pi, 0, 13.75),
    ra      = arc(4, 0, pi, 0, 0),
    hoop    = arc(0.75, 0, 2 * pi, 0, 0),
    board   = data.frame(x = c(-3, 3), y = c(-1.25, -1.25)),
    c3_l    = data.frame(x = c(-corner_x, -corner_x), y = c(-5.25, r3 * sin(a3))),
    c3_r    = data.frame(x = c(corner_x, corner_x), y = c(-5.25, r3 * sin(a3))),
    arc3    = arc(r3, a3, pi - a3, 0, 0, 120)
  )
  bind_rows(lapply(names(L), function(n) cbind(L[[n]], part = n)))
})

court_lines_layer <- function(alpha = 1) {
  geom_path(data = court_lines, aes(x, y, group = part), colour = "#6f7686", linewidth = 0.5, alpha = alpha)
}
court_layer <- function() {
  list(
    coord_fixed(xlim = c(-25.5, 25.5), ylim = c(-5.75, 36.5), expand = FALSE),
    theme_void(base_size = 12),
    theme(plot.background = element_rect(fill = "#fbfaf7", colour = NA),
          panel.background = element_rect(fill = "#fbfaf7", colour = NA),
          legend.position = "bottom", legend.title = element_text(size = 10),
          plot.margin = margin(4, 4, 4, 4))
  )
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
pct_txt <- function(x, d = 0) ifelse(is.na(x), "-", paste0(formatC(100 * x, format = "f", digits = d), "%"))
ord <- function(n) {
  n <- round(n); s <- ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  paste0(n, s)
}
pct_bucket <- function(p) cut(p, c(-1, 20, 40, 60, 80, 101), labels = c("low", "below average", "around average", "above average", "high"))

METRICS <- list(
  `Shot profile` = list(
    list("three_rate", "Three-point rate", function(v) pct_txt(v), FALSE),
    list("rim_rate",   "Shots at the rim", function(v) pct_txt(v), FALSE),
    list("mid_rate",   "Mid-range rate", function(v) pct_txt(v), FALSE),
    list("self_created", "Unassisted makes", function(v) pct_txt(v), FALSE),
    list("fga40",      "Shots per 40", function(v) sprintf("%.1f", v), FALSE)),
  Efficiency = list(
    list("shot_quality", "Shot quality", function(v) sprintf("%.2f", v), TRUE),
    list("shot_making",  "Shot-making", function(v) sprintf("%+.1f", 100 * v), TRUE),
    list("pps",          "Points per shot", function(v) sprintf("%.2f", v), TRUE),
    list("ts_pct",       "True shooting %", function(v) pct_txt(v, 1), TRUE),
    list("ft_rate",      "Free-throw rate", function(v) sprintf("%.2f", v), TRUE)),
  `Production per 40` = list(
    list("pts40", "Points", function(v) sprintf("%.1f", v), TRUE),
    list("ast40", "Assists", function(v) sprintf("%.1f", v), TRUE),
    list("reb40", "Rebounds", function(v) sprintf("%.1f", v), TRUE),
    list("stl40", "Steals", function(v) sprintf("%.1f", v), TRUE),
    list("blk40", "Blocks", function(v) sprintf("%.1f", v), TRUE),
    list("tov40", "Turnovers (fewer = better)", function(v) sprintf("%.1f", v), TRUE))
)

LB_CHOICES <- c(
  "Shot-making (pts/100 vs expected)" = "shot_making", "Shot quality" = "shot_quality",
  "Points per shot" = "pps", "True shooting %" = "ts_pct",
  "Three-point rate" = "three_rate", "Three-point %" = "three_pct", "Shots at the rim" = "rim_rate",
  "Mid-range rate" = "mid_rate", "Shots per 40" = "fga40", "Points per 40" = "pts40",
  "Assists per 40" = "ast40", "Rebounds per 40" = "reb40", "Blocks per 40" = "blk40",
  "Steals per 40" = "stl40", "Free-throw rate" = "ft_rate"
)
fmt_metric <- function(v, key) {
  if (key %in% c("three_rate", "three_pct", "rim_rate", "mid_rate")) pct_txt(v, 1)
  else if (key == "ts_pct") pct_txt(v, 1)
  else if (key == "shot_making") sprintf("%+.1f", 100 * v)
  else if (key %in% c("shot_quality", "pps", "ft_rate")) sprintf("%.2f", v)
  else sprintf("%.1f", v)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#fbfaf7", fg = "#171b24", primary = "#1c2a4a", secondary = "#6b7280",
  base_font = font_collection("Inter", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Iowan Old Style", "Georgia", "Times New Roman", "serif"),
  "font-size-base" = "0.95rem", "card-border-color" = "#e4e0d6", "card-cap-bg" = "#fbfaf7",
  "navbar-bg" = "#fbfaf7"
)

css <- "
  body { background:#fbfaf7; }
  .navbar { border-bottom:1px solid #e4e0d6; }
  .navbar-brand { font-family: Georgia, serif; font-weight:600; }
  .bslib-sidebar-layout > .sidebar { background:#f4f1ea; }
  .report-head h2 { margin:0 0 2px; font-size:1.9rem; }
  .report-head .sub { color:#6b7280; margin-bottom:10px; }
  .tag { display:inline-block; padding:2px 10px; border-radius:99px; background:#1c2a4a; color:#fff; font-size:.8rem; margin-right:6px; }
  .tag.soft { background:#ece7da; color:#3b3f4a; }
  .summary { max-width:70ch; line-height:1.55; margin:10px 0 0; color:#2b2f3a; }
  .stat-row { display:flex; flex-wrap:wrap; gap:10px; margin:14px 0 4px; }
  .stat { border:1px solid #e4e0d6; border-radius:8px; padding:8px 14px; background:#fff; min-width:110px; }
  .stat .v { font-family:Georgia, serif; font-size:1.35rem; line-height:1.2; }
  .stat .l { font-size:.74rem; color:#6b7280; text-transform:uppercase; letter-spacing:.04em; }
  .comp a { text-decoration:none; font-weight:600; color:#1c2a4a; }
  .comp .meta { color:#6b7280; font-size:.85rem; }
  .table { font-size:.88rem; }
  .note { color:#6b7280; font-size:.82rem; }
  .card-header { font-weight:600; }
  @media (max-width: 600px) {
    #chart { height: 360px !important; }
    #pctplot { height: 520px !important; }
    .stat { min-width: 96px; }
    .table th, .table td { padding: .3rem .4rem; font-size: .8rem; }
  }
"

ui <- page_navbar(
  title = "College scouting reports",
  id = "nav",
  theme = theme,
  header = tags$head(tags$style(HTML(css))),
  nav_panel(
    "Scout report",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("player", "Player", choices = NULL,
                       options = list(placeholder = "Search a player or team", maxOptions = 50)),
        radioButtons("view", "Shot chart",
                     choices = c("Efficiency map" = "hex", "Every shot" = "pts"), selected = "hex"),
        helpText(class = "note",
                 sprintf("%s men's college basketball. %s players with at least %d shots and %d minutes.",
                         league$season, format(league$n_players, big.mark = ","), league$min_fga, league$min_min)),
        helpText(class = "note", "Real play-by-play data. Percentiles compare each player with every other qualified player.")
      ),
      uiOutput("header"),
      layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        card(card_header("Shot chart"), plotOutput("chart", height = "470px"),
             tags$p(class = "note", style = "padding:0 14px 10px;", textOutput("chart_note", inline = TRUE))),
        card(card_header("Where the shots come from"), div(style = "overflow-x:auto; padding:0 6px;", tableOutput("zones")),
             tags$p(class = "note", style = "padding:0 14px 10px;",
                    "League = every shot by every player in the data. Zone FG% is lightly shrunk toward the league average for small samples."))
      ),
      layout_columns(
        col_widths = c(7, 5), fill = FALSE,
        card(card_header("Percentiles vs. qualified players"), plotOutput("pctplot", height = "560px"),
             tags$p(class = "note", style = "padding:0 14px 10px;",
                    "Bar = percentile among qualified players; number on the right = the player's value. Shot quality is expected points per shot given where the shots were taken; shot-making is points per 100 shots above or below that expectation.")),
        card(card_header("Most similar profiles"), uiOutput("comps"),
             tags$p(class = "note", style = "padding:0 14px 10px;",
                    "Matched on shot mix, shot creation, free-throw rate and per-40 production. Click a name to open that report."))
      )
    )
  ),
  nav_panel(
    "Leaderboard",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("lb_metric", "Rank by", choices = LB_CHOICES, selected = "shot_making"),
        selectInput("lb_arch", "Archetype", choices = c("All", sort(unique(players$archetype)))),
        selectInput("lb_pos", "Position", choices = c("All", "Guard (G, PG)" = "G", "Forward (F, SF)" = "F", "Center (C)" = "C")),
        sliderInput("lb_minfga", "Minimum shots", min = league$min_fga, max = 500, value = 250, step = 10),
        hr(),
        selectInput("lb_x", "Scatter: x-axis", choices = LB_CHOICES, selected = "shot_quality"),
        selectInput("lb_y", "Scatter: y-axis", choices = LB_CHOICES, selected = "shot_making")
      ),
      card(card_header("Top 25"), tableOutput("lb_table"), fill = FALSE),
      card(card_header("All filtered players"), plotlyOutput("lb_scatter", height = "520px"), fill = FALSE)
    )
  ),
  nav_panel(
    "How it works",
    div(style = "max-width:760px; margin:10px auto 40px; line-height:1.6;",
      h3("What this is"),
      p("A scouting-report generator built from real play-by-play data for the 2025-26 men's college basketball season. ",
        "Pick a player to see where they shoot from, how efficient they are compared with their shot mix, how their production ranks, and which players have the most similar profile."),
      h3("Data"),
      p("ESPN play-by-play and box scores, published by ", tags$a("sportsdataverse", href = "https://sportsdataverse.org", target = "_blank"),
        ". ESPN logs each shot with court coordinates; I fold every shot onto one half court with the hoop at the origin. ",
        sprintf("%s charted shots league-wide. The reports use the %s shots from %s players with at least %d shots and %d minutes.",
                format(league$n_shots, big.mark = ","), format(nrow(shots), big.mark = ","), format(league$n_players, big.mark = ","), league$min_fga, league$min_min)),
      p("As a check, the shots I parse match 98.8% of the field-goal attempts in the box scores. Shots logged without coordinates, and a few obvious logging errors (threes from inside 20 feet, twos from behind the arc), are dropped."),
      h3("Zones"),
      p("Restricted area (within 4 ft of the hoop), paint outside the restricted area, mid-range (other twos), corner three, and above-the-break three."),
      h3("Shot quality and shot-making"),
      p(tags$b("Shot quality"), " is the points per shot a league-average shooter would score from the same zones in the same proportions. It measures shot selection, not skill."),
      p(tags$b("Shot-making"), " is actual points minus that expectation, per 100 shots. It is shrunk toward zero with a 150-shot prior so small samples don't produce extreme values."),
      h3("Archetypes and similar players"),
      p("Players are grouped with k-means (6 clusters) on twelve standardized features: three-point rate, rim rate, mid-range rate, share of threes from the corner, unassisted share of makes, dunk rate, shots per 40, assists, rebounds, blocks and steals per 40, and free-throw rate. ",
        "Each cluster is named from its average profile. Similar players are the nearest neighbors in the same feature space."),
      h3("Limits"),
      tags$ul(
        tags$li("Coordinates are logged by hand and are noisy, especially near the rim and on the three-point line."),
        tags$li("No defensive tracking, shot clock, or defender distance. Shot quality uses location only."),
        tags$li("One season. Small samples are flagged, but percentiles are still affected by them."),
        tags$li("This is a descriptive tool, not a draft board or a projection of NBA success.")
      ),
      p(class = "note", "Code and data pipeline: ", tags$a("github.com/rishabhsxngh/nba-prospect-evaluation-tool",
        href = "https://github.com/rishabhsxngh/nba-prospect-evaluation-tool", target = "_blank"))
    )
  )
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
    tiles <- list(
      list(sprintf("%.1f", p$pts40), "Points / 40"),
      list(sprintf("%.1f", p$ast40), "Assists / 40"),
      list(sprintf("%.1f", p$reb40), "Rebounds / 40"),
      list(pct_txt(p$fg_pct, 1), "FG%"),
      list(pct_txt(p$three_pct, 1), "3P%"),
      list(pct_txt(p$ts_pct, 1), "True shooting"),
      list(format(p$fga, big.mark = ","), "Shots charted")
    )
    sm <- 100 * p$shot_making
    shot_diet <- sprintf("Takes %s of shots at the rim, %s from three and %s from mid-range.",
                         pct_txt(p$rim_rate), pct_txt(p$three_rate), pct_txt(p$mid_rate))
    quality <- sprintf("That shot mix is worth %.2f points per attempt (%s percentile for shot quality), and %s makes %s points per 100 shots %s what the mix predicts (%s percentile).",
                       p$shot_quality, ord(p$p_shot_quality), p$name,
                       sprintf("%.1f", abs(sm)), ifelse(sm >= 0, "more than", "fewer than"), ord(p$p_shot_making))
    caveat <- if (p$fga < 250) sprintf(" Sample is small (%d shots), so treat the efficiency numbers with care.", p$fga) else ""
    tagList(
      div(class = "report-head",
          h2(p$name),
          div(class = "sub", sprintf("%s  |  %s  |  %d games, %.1f minutes per game", p$team, p$pos, p$games, p$mpg)),
          span(class = "tag", p$archetype), span(class = "tag soft", "2025-26 season"),
          p(class = "summary", paste(shot_diet, quality, caveat))
      ),
      div(class = "stat-row", lapply(tiles, function(t) div(class = "stat", div(class = "v", t[[1]]), div(class = "l", t[[2]]))))
    )
  })

  # ---- Shot chart ---------------------------------------------------------
  output$chart <- renderPlot({
    sh <- cur_shots()
    sh <- sh[sh$hy > -6 & sh$hy < 36 & abs(sh$hx) < 25.5, ]
    if (input$view == "pts") {
      sh$res <- factor(ifelse(sh$made == 1, "Made", "Missed"), levels = c("Missed", "Made"))
      ggplot() + court_layer() + court_lines_layer(0.9) +
        geom_point(data = sh[sh$res == "Missed", ], aes(hx, hy), shape = 4, size = 1.7, colour = "#b9b4a6", stroke = 0.6) +
        geom_point(data = sh[sh$res == "Made", ], aes(hx, hy), shape = 21, size = 2.2, fill = "#1c2a4a", colour = "#fbfaf7", stroke = 0.4) +
        labs(caption = NULL)
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
        scale_fill_gradient2(low = "#2f5d8a", mid = "#e9e4d8", high = "#c8641a", midpoint = 0,
                             limits = c(-lim, lim), breaks = c(-lim, 0, lim),
                             labels = c("Worse", "League avg.", "Better"),
                             name = NULL, guide = guide_colourbar(barwidth = 14, barheight = 0.5, ticks = FALSE)) +
        court_lines_layer(0.85)
    }
  }, res = 96, bg = "#fbfaf7")

  output$chart_note <- renderText({
    if (input$view == "hex")
      "Hex size = how often they shoot there. Colour = FG% versus the league in the same spot (small samples pulled toward the league)."
    else "Filled = made, x = missed. Hoop is at the bottom; the half-court line is at the top."
  })

  # ---- Zone table ---------------------------------------------------------
  output$zones <- renderTable({
    p <- cur()
    a <- as.numeric(p[paste0("a_", 1:5)]); m <- as.numeric(p[paste0("m_", 1:5)])
    lg <- lg_zone[match(ZONES, as.character(lg_zone$zone)), ]
    fg_shr <- (m + lg$fg * 20) / (a + 20)
    data.frame(
      Zone = ZONES,
      Shots = ifelse(a == 0, "0", format(a, big.mark = ",")),
      Share = pct_txt(a / sum(a)),
      `FG%` = ifelse(a == 0, "-", pct_txt(fg_shr, 1)),
      `Lg FG%` = pct_txt(lg$fg, 1),
      Diff = ifelse(a == 0, "-", sprintf("%+.1f", 100 * (fg_shr - lg$fg))),
      check.names = FALSE
    )
  }, striped = FALSE, hover = TRUE, align = "lrrrrr", width = "100%", spacing = "s")

  # ---- Percentiles --------------------------------------------------------
  output$pctplot <- renderPlot({
    p <- cur()
    rows <- list()
    for (g in names(METRICS)) for (m in METRICS[[g]]) {
      key <- m[[1]]
      rows[[length(rows) + 1]] <- data.frame(group = g, label = m[[2]], pct = p[[paste0("p_", key)]],
                                             val = m[[3]](p[[key]]), directional = m[[4]])
    }
    d <- bind_rows(rows)
    d$group <- factor(d$group, levels = names(METRICS))
    d$label <- factor(d$label, levels = rev(d$label))
    pal <- colorRampPalette(c("#c8641a", "#e9e4d8", "#2f5d8a"))(101)
    d$fill <- ifelse(d$directional, pal[pmax(1, pmin(101, round(d$pct) + 1))], "#1c2a4a")
    d$tx <- 108
    w <- session$clientData$output_pctplot_width
    narrow <- !is.null(w) && w < 520
    ggplot(d, aes(y = label)) +
      geom_col(aes(x = 100), fill = "#efebe0", width = 0.62) +
      geom_col(aes(x = pct, fill = fill), width = 0.62) +
      geom_text(aes(x = 103, label = val), hjust = 0, size = 3.5, colour = "#171b24") +
      geom_text(aes(x = pmax(pct - 2, 6), label = round(pct)), hjust = 1, size = 3, colour = ifelse(d$directional & d$pct > 35 & d$pct < 65, "#171b24", "#ffffff")) +
      scale_fill_identity() +
      scale_x_continuous(breaks = c(0, 50, 100), expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, 100), clip = "off") +
      facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(), axis.text.x = element_text(colour = "#6b7280", size = 9),
            axis.text.y = element_text(colour = "#171b24", size = 10.5),
            strip.placement = "outside",
            strip.text.y.left = if (narrow) element_blank() else element_text(angle = 0, hjust = 0, face = "bold", colour = "#1c2a4a"),
            plot.background = element_rect(fill = "#fbfaf7", colour = NA), panel.spacing = unit(10, "pt"), plot.margin = margin(4, if (narrow) 40 else 52, 4, 4))
  }, res = 96, bg = "#fbfaf7")

  # ---- Similar profiles ---------------------------------------------------
  output$comps <- renderUI({
    p <- cur()
    me <- feat_z[as.character(p$pid), ]
    d <- sqrt(rowSums((sweep(feat_z, 2, me))^2))
    names(d) <- rownames(feat_z)
    d <- d[names(d) != as.character(p$pid)]
    med <- median(d)
    top <- sort(d)[1:6]
    tagList(lapply(names(top), function(id) {
      q <- players[players$pid == as.integer(id), ][1, ]
      match <- max(0, round(100 * (1 - top[[id]] / med)))
      div(class = "comp", style = "padding:8px 14px; border-top:1px solid #efebe0;",
          tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('goto', %s, {priority: 'event'}); return false;", id), q$name),
          span(style = "float:right; font-weight:600;", paste0(match, "% match")),
          div(class = "meta", sprintf("%s  |  %s  |  %s", q$team, q$pos, q$archetype)))
    }))
  })

  # ---- Leaderboard --------------------------------------------------------
  lb <- reactive({
    d <- players |> filter(fga >= input$lb_minfga)
    if (input$lb_arch != "All") d <- d |> filter(archetype == input$lb_arch)
    if (input$lb_pos == "G") d <- d |> filter(pos %in% c("G", "PG"))
    if (input$lb_pos == "F") d <- d |> filter(pos %in% c("F", "SF"))
    if (input$lb_pos == "C") d <- d |> filter(pos == "C")
    d
  })

  output$lb_table <- renderTable({
    d <- lb(); key <- input$lb_metric
    validate(need(nrow(d) > 0, "No players match these filters."))
    d <- d[order(-d[[key]]), ][seq_len(min(25, nrow(d))), ]
    out <- data.frame(
      `#` = seq_len(nrow(d)), Player = d$name, Team = d$team, Pos = d$pos, Archetype = d$archetype,
      Shots = d$fga, check.names = FALSE, stringsAsFactors = FALSE)
    out[[names(LB_CHOICES)[LB_CHOICES == key]]] <- fmt_metric(d[[key]], key)
    out
  }, striped = FALSE, hover = TRUE, width = "100%")

  output$lb_scatter <- renderPlotly({
    d <- lb(); validate(need(nrow(d) > 0, "No players match these filters."))
    xk <- input$lb_x; yk <- input$lb_y
    xl <- names(LB_CHOICES)[LB_CHOICES == xk]; yl <- names(LB_CHOICES)[LB_CHOICES == yk]
    d$tip <- sprintf("%s (%s)<br>%s: %s<br>%s: %s", d$name, d$team, xl, fmt_metric(d[[xk]], xk), yl, fmt_metric(d[[yk]], yk))
    sc <- function(v, k) if (k == "shot_making") 100 * v else if (k %in% c("three_rate", "three_pct", "rim_rate", "mid_rate", "ts_pct")) 100 * v else v
    d$xv <- sc(d[[xk]], xk); d$yv <- sc(d[[yk]], yk)
    xl <- paste0(xl, if (xk %in% c("three_rate", "three_pct", "rim_rate", "mid_rate", "ts_pct")) " (%)" else "")
    yl <- paste0(yl, if (yk %in% c("three_rate", "three_pct", "rim_rate", "mid_rate", "ts_pct")) " (%)" else "")
    plot_ly(d, x = ~xv, y = ~yv, color = ~archetype, text = ~tip, hoverinfo = "text",
            type = "scatter", mode = "markers", marker = list(size = 7, opacity = 0.75),
            colors = c("#1c2a4a", "#c8641a", "#2f5d8a", "#8a8f9c", "#b8912a", "#5f8f6b")) |>
      layout(xaxis = list(title = xl), yaxis = list(title = yl),
             paper_bgcolor = "#fbfaf7", plot_bgcolor = "#fbfaf7", legend = list(orientation = "h", y = -0.2))
  })
}

shinyApp(ui, server)
