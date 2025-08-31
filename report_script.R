library(httr) 
library(jsonlite)
library(dplyr)
library(tidyverse)
library(plotly)
library(reactable)
library(bslib)
library(shiny)
library(bsicons)
library(htmltools)
library(htmlwidgets)
library(scales)
library(lubridate)
library(stringr)
library(here)
library(cli)
library(glue)

### PARAMETERS ###
api_key = Sys.getenv("RIOT_API")
summoner_name = "LivingWeapon" 
region = "europe"
sub_region = "euw1"
tagline = "EUW"
game_type = "RANKED_TFT"

### LOAD DATA ###
source(here("get_data.R"))

### SUMMONER OVERVIEW ###
value_box_summoner <- value_box(
  title = "Summoner",
  value = paste0(summoner_data$gameName, "#", summoner_data$tagLine),
  showcase = tags$img(src = paste0("http://ddragon.leagueoflegends.com/cdn/",
                                   latest_version,
                                   "/img/profileicon/",
                                   summoner_profile$profileIconId,
                                   ".png"),
                      height = "50px"),
  theme = "gray",
  p(paste0("Region: ", region, " / ", sub_region))
)

### SUMMONER GENERAL PLAYER STATS ###
summoner_overview <- tft_overview_data %>% filter(queueType == game_type)

value_box_player_stats <- value_box(
  title = "Status",
  value = paste0(tft_overview_data$tier, " | ", tft_overview_data$rank),
  showcase = tags$img(src = sprintf(
    "https://raw.communitydragon.org/latest/plugins/rcp-fe-lol-static-assets/global/default/images/ranked-emblem/emblem-%s.png",
    tolower(tft_overview_data$tier)), 
    height = "100px"),
  theme = "gray",
  p(paste0("League Points: ", tft_overview_data$leaguePoints)), 
  p(paste0(tft_overview_data$wins, " Wins, ", tft_overview_data$losses, " Losses")),
  p(paste0("Average Placement: ", mean(match_details$placement)))
)

### MATCH OVERVIEW ###
color_placement <- function(value) {
  colors <- c("#00FF00", "#32CD32", "#7FFF00", "#FFD700", "#FFA500", "#FF4500", "#DC143C", "#8B0000")
  index <- min(max(value, 1), 8) 
  div(style = paste(
    "display: inline-block;",
    "padding: 5px 20px;",
    "border-radius: 12px;",
    "background-color:", colors[index], ";",
    "color: white;",
    "font-weight: bold;"),
    value)}

damage_color <- col_numeric(
  palette = c("#FFCCCC", "#FF6666", "#CC0000", "#8B0000"), 
  domain = range(match_details$total_damage_to_players, na.rm = TRUE))

overview_games <- match_details %>%
  mutate(game_datetime = as_datetime(game_datetime / 1000, tz = "UTC"),
         time_eliminated = paste0(as.character(round(time_eliminated/60)), " Min")) %>%
  select(game_datetime, time_eliminated, placement, players_eliminated, total_damage_to_players, last_round, level, gold_left) %>%
  arrange(desc(game_datetime))

overview_games_tbl <- overview_games %>%
  reactable(
    columns = list(
      game_datetime = colDef("Date"),
      time_eliminated = colDef("Elimination Time"),
      placement = colDef("Placement", 
                         cell = function(value) color_placement(value),
                         align = "center"),
      players_eliminated = colDef("Players Eliminated"),
      total_damage_to_players = colDef("Total Damage", 
                                       style = function(value) list(
                                         background = damage_color(value),
                                         color = "white")),
      last_round = colDef("Last Round"),
      level = colDef("Level reached"), 
      gold_left = colDef("Gold left")
    ), 
    compact = TRUE, 
    bordered = TRUE, 
    highlight = TRUE
  )

card_01 <- card(overview_games_tbl)

### TIME SPENT OVERVIEW ###
time_plot <- overview_games %>%
  mutate(game_date = as_date(game_datetime)) %>%
  group_by(game_date) %>%
  summarise(total_minutes = sum(as.numeric(str_extract(time_eliminated, "\\d+")))) %>% 
  mutate(weekday = wday(game_date, label = TRUE)) %>% 
  plot_ly(
    x = ~game_date,
    y = ~total_minutes,
    type = "bar",
    marker = list(
      color = ~total_minutes,
      colorscale = list(c(0,1), c("black", "lightgray")),  
      reversescale = TRUE,                                
      showscale = FALSE                                   
    ),
    text = NULL,                                           
    hoverinfo = "text",
    textposition = "none",
    hovertext = ~paste0(
      "Date: ", game_date, "<br>",
      "Weekday: ", weekday, "<br>",
      "Minutes Played: ", total_minutes
    )
  ) %>%
  layout(
    title = "Player Activity: Minutes Played per Day",
    xaxis = list(
      title = "Date",
      tickmode = "array",
      tickvals = ~game_date,
      tickformat = "%Y-%m-%d",
      tickangle = -45,
      tickfont = list(size = 10)
    ),
    yaxis = list(title = "Minutes Played"),
    margin = list(t = 50)
  )

card_02 <- card(time_plot)
### INLINE BAR PLOT FUNCTION ###
inline_game_bar <- function(total_games, top4_wins, max) {
  # Create a vector of x positions, one for each game
  x_positions <- seq_len(total_games)
  
  # Colors: green for wins, red for losses
  dot_colors <- c(rep("green", top4_wins), rep("red", total_games - top4_wins))
  
  plot_ly(
    x = x_positions,
    y = rep(1, total_games),   # all dots on the same row
    type = 'scatter',
    mode = 'markers',
    marker = list(
      color = dot_colors,
      size = 8
    ),
    hoverinfo = "text",
    hovertext = paste0(top4_wins, "/", total_games, 
                       " top 4 (", round(top4_wins/total_games*100, 1), "%)")
  ) %>%
    layout(
      xaxis = list(
        showgrid = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE,
        range = c(0, max + 1)  # use global max for consistent spacing
      ),
      yaxis = list(
        showgrid = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      height = 20
    )
}
### COMMON TRAITS ###
common_traits <- match_details %>% 
  select(gameId, placement, traits) %>%
  unnest(cols = traits) %>%
  mutate(top4 = placement <= 4) %>%
  group_by(name) %>%
  summarise(
    total_games = n_distinct(gameId),
    top4_wins = sum(top4),
    .groups = "drop"
  ) %>%
  arrange(desc(total_games)) %>%
  mutate(
    clean_name = sub(".*_(.*)", "\\1", name),
    trait_icon_link = paste0("https://cdn.metatft.com/file/metatft/traits/",
                             tolower(clean_name),
                             ".png"),
    trait_icon = paste0(
      '<div style="display:flex; align-items:center;">',
      '<div style="width:40px; height:40px; background-color:black; border-radius:50%; display:flex; justify-content:center; align-items:center;">',
      '<img src="', trait_icon_link, '" height="24">',
      '</div>',
      '<span style="margin-left:8px;">', clean_name, '</span>',
      '</div>'
    ),
    win_rate = round(top4_wins / total_games * 100, 1)
  ) %>% 
  select(trait_icon, win_rate, total_games, top4_wins)

# Display in reactable
trait_tbl <- reactable(
  common_traits,
  pagination = FALSE,   
  rowStyle = list(height = "45px"),
  columns = list(
    trait_icon = colDef(
      name = "Trait",
      html = TRUE,
      align = "left",                 
      headerStyle = list(textAlign = "left")  
    ),  
    win_rate = colDef(
      name = "Win Rate",
      align = "left",
      headerStyle = list(textAlign = "left"),
      cell = function(value) paste0(value, "%")
    ),
    total_games = colDef(
      name = "Game Count",
      align = "left",                  
      headerStyle = list(textAlign = "left"),
      cell = function(value, index) {
        row <- common_traits[index, ]
        plot <- inline_game_bar(
          row$total_games,
          row$top4_wins,
          max(common_traits$total_games, na.rm = TRUE)
        ) %>% plotly::config(displayModeBar = FALSE)
        
        htmltools::tags$div(
          style = "width: 150px; display: flex; align-items: center;", plot
        )
      }
    ),
    top4_wins = colDef(show = FALSE)
  )
)

### COMMON UNITS ###
common_units <- match_details %>% 
  select(gameId, placement, units) %>%
  unnest(cols = units) %>%
  mutate(
    top4 = placement <= 4
  ) %>%
  group_by(character_id) %>%
  summarise(
    total_games = n_distinct(gameId),
    top4_wins = sum(top4),
    .groups = "drop"
  ) %>%
  mutate(
    win_rate = round(top4_wins / total_games * 100, 1),
    character_icon_link = paste0("https://cdn.metatft.com/cdn-cgi/image/width=48,height=48,format=auto/https://cdn.metatft.com/file/metatft/champions/",
                                 tolower(character_id),
                                 ".png"),
    clean_name = sub(".*_(.*)", "\\1", character_id),
    character_icon = paste0(
      '<div style="display:flex; align-items:center;">',
      '<div style="width:40px; height:40px; display:flex; justify-content:center; align-items:center;">',
      '<img src="', character_icon_link, '" height="24">',
      '</div>',
      '<span style="margin-left:8px;">', clean_name, '</span>',
      '</div>'
    )
  ) %>% 
  select(character_icon, win_rate, total_games, top4_wins)


unit_tbl <- reactable(
  common_units,
  pagination = FALSE,
  rowStyle = list(height = "45px"),
  columns = list(
    character_icon = colDef(
      name = "Unit",
      html = TRUE,
      align = "left",
      headerStyle = list(textAlign = "left")
    ),
    win_rate = colDef(
      name = "Win Rate",
      align = "left",
      headerStyle = list(textAlign = "left"),
      cell = function(value) paste0(value, "%")
    ),
    total_games = colDef(
      name = "Games Played",
      align = "left",
      headerStyle = list(textAlign = "left"),
      cell = function(value, index) {
        row <- common_units[index, ]
        plot <- inline_game_bar(row$total_games, row$top4_wins, max(common_units$total_games, na.rm = TRUE)) %>%
          plotly::config(displayModeBar = FALSE)
        htmltools::tags$div(
          style = "width: 150px; display: flex; align-items: center;",
          plot
        )
      }
    ),
    top4_wins = colDef(show = FALSE)
  )
)
### COMMON ITEMS ###
common_items <- match_details %>% 
  select(gameId, placement, units) %>%
  unnest(cols = units) %>%
  mutate(itemNames = map_chr(itemNames, ~ ifelse(is.list(.), paste(unlist(.), collapse = ", "), .))) %>%
  unnest(cols = itemNames) %>% 
  filter(!is.na(itemNames)) %>%
  filter(!grepl("Core", itemNames, ignore.case = TRUE)) %>%
  mutate(top4 = placement <= 4) %>%
  group_by(itemNames, gameId) %>% 
  summarise(
    top4_game = any(top4),   # 1 wenn dieses Spiel mit dem Item ein Top4 war
    .groups = "drop"
  ) %>%
  group_by(itemNames) %>%
  summarise(
    total_games = n(),       # Anzahl distinct games
    top4_wins   = sum(top4_game),
    .groups = "drop"
  ) %>%
  arrange(desc(total_games)) %>%
  mutate(
    item_icon_link = paste0(
      "https://cdn.metatft.com/cdn-cgi/image/width=48,height=48,format=auto/https://cdn.metatft.com/file/metatft/items/",
      tolower(itemNames),
      ".png"
    ),
    clean_names = sub(".*_(.*)", "\\1", itemNames),
    item_icon = paste0(
      '<div style="display:flex; align-items:center;">',
      '<div style="width:40px; height:40px; display:flex; justify-content:center; align-items:center;">',
      '<img src="', item_icon_link, '" height="24">',
      '</div>',
      '<span style="margin-left:8px;">', clean_names, '</span>',
      '</div>'
    ),
    win_rate = round(top4_wins / total_games * 100, 1)
  ) %>% 
  select(item_icon, win_rate, total_games, top4_wins)

item_tbl <- reactable(
  common_items,
  pagination = FALSE,   
  rowStyle = list(height = "45px"),
  columns = list(
    item_icon = colDef(
      name = "Item",
      html = TRUE,
      align = "left",                 
      headerStyle = list(textAlign = "left")  
    ),  
    win_rate = colDef(
      name = "Win Rate",
      align = "left",
      headerStyle = list(textAlign = "left"),
      cell = function(value) paste0(value, "%")
    ),
    total_games = colDef(
      name = "Game Count",
      align = "left",                  
      headerStyle = list(textAlign = "left"),
      cell = function(value, index) {
        row <- common_items[index, ]
        plot <- inline_game_bar(
          row$total_games,
          row$top4_wins,
          max(common_items$total_games, na.rm = TRUE)
        ) %>% plotly::config(displayModeBar = FALSE)
        
        htmltools::tags$div(
          style = "width: 150px; display: flex; align-items: center;", 
          plot
        )
      }
    ),
    top4_wins = colDef(show = FALSE)
  )
)

########################################## LAYOUT

layout <- fluidPage(navset_tab(
  nav_panel(title = "Overview", p("Summoner Overview"),
            layout_column_wrap(
              width = "200px", height = 200,
              value_box_01, value_box_02, value_box_03),
            card_01),
  nav_panel(title = "Details", p("TFT performance details"),
            layout_column_wrap(
              width = NULL, height = 200, fill = FALSE,
              style = css(grid_template_columns = "1fr 4fr"),
              value_box_04, card_02),
            card_03, 
            card_04, 
            card_05)))


save_html(layout, file = "C:/Users/Lenovo/Desktop/tft/tft_stats_vic.html")
