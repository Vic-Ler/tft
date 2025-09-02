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
library(rcartocolor)

### PARAMETERS ###
api_key = Sys.getenv("RIOT_API")
summoner_name = "Bron_Nashor" 
region = "europe"
sub_region = "euw1"
tagline = "EUW"
game_type = "RANKED_TFT"

### LOAD DATA ###
source(here("get_data.R"))

#filtering desired game type
tft_overview_data <- tft_overview_data %>% 
  filter(queueType == game_type)

### DROPDOWN FUNCTION ###
dropdown_function <- function(tableId) {
  function(values, name) {
    tags$select(
      class = "transparent-select",
      onchange = sprintf(
        "Reactable.setFilter('%s', '%s', event.target.value || undefined)", 
        tableId, name
      ),
      tags$option(value = "", "All"),
      lapply(sort(unique(values)), tags$option),
      `aria-label` = sprintf("Filter %s", name)
    )}}

### SUMMONER OVERVIEW ###
value_box_summoner <- value_box( 
  title = "Summoner", 
  class = "transparent", 
  value = paste0(summoner_data$gameName, 
                 "#", 
                 summoner_data$tagLine), 
  showcase = tags$img(
    src = paste0("http://ddragon.leagueoflegends.com/cdn/", 
                 latest_version, 
                 "/img/profileicon/", 
                 summoner_profile$profileIconId, 
                 ".png"),
    height = "70px"), 
  theme = "gray", 
  p(paste0("Region: ", region, " / ", sub_region)))
  
### SUMMONER GENERAL PLAYER STATS ###
value_box_player_stats <- value_box(
  title = "Status",
  class = "transparent",
  value = paste0(tft_overview_data$tier, 
                 " | ", 
                 tft_overview_data$rank),
  showcase = tags$img(src = sprintf(
    "https://raw.communitydragon.org/latest/plugins/rcp-fe-lol-static-assets/global/default/images/ranked-emblem/emblem-%s.png",
    tolower(tft_overview_data$tier)), 
    height = "200px"),
  theme = "gray",
  p(paste0("League Points: ", 
           tft_overview_data$leaguePoints)), 
  p(paste0(tft_overview_data$wins, 
           " Wins, ", 
           tft_overview_data$losses, 
           " Losses")),
  p(paste0("Average Placement: ", 
           mean(match_details$placement)))
)

### MATCH OVERVIEW ###
color_placement <- function(value, n_categories = 8) {
  colors <- c("#00FFFF", 
              "#22DDFF", 
              "#44BBFF", 
              "#6699FF", 
              "#8844FF", 
              "#AA22FF", 
              "#CC00FF", 
              "#FF00FF")
  index <- min(max(value, 1), 8)
  div(
    class = "placement-badge",
    style = paste0("background-color: ", colors[index], ";"),
    value
  )}

damage_badge <- function(value) {
  damage_color <- col_numeric(
    palette = c("#FF93BF", 
                "#FF69A6", 
                "#FF4791", 
                "#FF0066"), 
    domain = range(match_details$total_damage_to_players, 
                   na.rm = TRUE))
  color <- damage_color(value)  
  div(
    class = "damage-badge",
    style = paste0("background-color: ", color, ";"),
    value
  )}

overview_games <- match_details %>%
  mutate(game_datetime = as_datetime(game_datetime / 1000, 
                                     tz = "UTC"),
         time_eliminated = paste0(as.character(round(time_eliminated/60)), 
                                  " Min")) %>%
  select(game_datetime, 
         time_eliminated, 
         placement, 
         players_eliminated, 
         total_damage_to_players, 
         last_round, 
         level, 
         gold_left) %>%
  arrange(desc(game_datetime))

overview_games_tbl <- overview_games %>%
  reactable(
    columns = list(
      game_datetime = colDef("Date",
                             cell = function(value) {
                               format(value, "%d-%m %H:%M")
                             }),
      time_eliminated = colDef("Elimination Time"),
      placement = colDef("Placement", 
                         filterInput = dropdown_function("match_history"),
                         cell = function(value) color_placement(value),
                         align = "center"),
      players_eliminated = colDef("Players Eliminated", 
                                  filterInput = dropdown_function("match_history")),
      total_damage_to_players = colDef("Total Damage",
                                       cell = function(value) damage_badge(value),
                                       align = "center"),
      last_round = colDef("Last Round"),
      level = colDef("Level reached",
                     filterInput = dropdown_function("match_history")),
      gold_left = colDef("Gold left")
    ), 
    rowStyle = list(height = "45px"),
    compact = TRUE, 
    bordered = FALSE, 
    highlight = TRUE,
    sortable = TRUE,
    filterable = TRUE,
    pagination = TRUE,
    elementId = "match_history",
    class = "transparent"
  )

### TIME SPENT OVERVIEW ###
time_plot <- overview_games %>%
  mutate(game_date = as_date(game_datetime)) %>%
  group_by(game_date) %>%
  summarise(total_minutes = sum(as.numeric(str_extract(time_eliminated, "\\d+")))) %>% 
  mutate(weekday = wday(game_date, 
                        label = TRUE)) %>% 
  plot_ly(
    x = ~game_date,
    y = ~total_minutes,
    type = "bar",
    marker = list(
      color = ~total_minutes,
      colorscale = list(c(0,1), c("#CC00FF", 
                                  "#0000FF")),  
      reversescale = TRUE,                                
      showscale = FALSE                                   
    ),
    hoverinfo = "text",
    hovertext = ~paste0(
      "Date: ", game_date, "<br>",
      "Weekday: ", weekday, "<br>",
      "Minutes Played: ", total_minutes
    )
  ) %>%
  layout(
    plot_bgcolor = "rgba(0,0,0,0)", 
    paper_bgcolor = "rgba(0,0,0,0)", 
    font = list(color = "white"), 
    xaxis = list(
      title = "Date",
      tickmode = "array",
      tickvals = ~game_date,
      tickformat = "%Y-%m-%d",
      tickangle = -45,
      tickfont = list(size = 10, 
                      color = "white"),
      titlefont = list(color = "white")
    ),
    yaxis = list(
      title = "Minutes Played",
      titlefont = list(color = "white"),
      tickfont = list(color = "white")
    ),
    margin = list(t = 50)
  )

### INLINE BAR PLOT FUNCTION ###
inline_game_bar <- function(total_games, top4_wins, max) {
  total_games <- as.integer(total_games[[1]])
  top4_wins   <- as.integer(top4_wins[[1]])
  losses <- total_games - top4_wins
  if (is.na(losses) || losses < 0) losses <- 0
  x_positions <- seq_len(total_games)
  dot_colors <- c(
    rep("#CC00FF", top4_wins),
    rep("#0000FF",   losses)
  )
  plot_ly(
    x = x_positions,
    y = rep(1, total_games),
    type = "scatter",
    mode = "markers",
    marker = list(color = dot_colors, 
                  size = 8),
    hoverinfo = "text",
    hovertext = paste0(
      top4_wins, "/", total_games,
      " top 4 (", 
      round(top4_wins / total_games * 100, 1), "%)"
    )) %>%
    layout(
      xaxis = list(
        showgrid = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE,
        range = c(0, max + 1)
      ),
      yaxis = list(
        showgrid = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      height = 20,
      paper_bgcolor = "rgba(0,0,0,0)", 
      plot_bgcolor  = "rgba(0,0,0,0)"    
    )}

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
      '<div style="width:35px; height:35px; background-color:black; border-radius:50%; display:flex; justify-content:center; align-items:center;">',
      '<img src="', trait_icon_link, '" height="24">',
      '</div>',
      '<span style="margin-left:8px;">', clean_name, '</span>',
      '</div>'
    ),
    win_rate = round(top4_wins / total_games * 100, 1)
  ) %>% 
  select(trait_icon, win_rate, total_games, top4_wins)

trait_tbl <- reactable(
  common_traits,
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
  ),
  compact = TRUE, 
  bordered = FALSE, 
  highlight = TRUE,
  sortable = TRUE,
  filterable = TRUE,
  pagination = TRUE,
  class = "transparent"
)

### COMMON UNITS ###
common_units <- match_details %>% 
  select(gameId, placement, units) %>%
  unnest(cols = units) %>%
  mutate(top4 = placement <= 4) %>%
  group_by(character_id, gameId) %>%
  summarise(game_top4 = any(top4), 
            .groups = "drop") %>%
  group_by(character_id) %>%
  summarise(
    total_games = n(),
    top4_wins = sum(game_top4),
    .groups = "drop"
  ) %>%
  mutate(
    win_rate = round(top4_wins / total_games * 100, 1),
    character_icon_link = paste0(
      "https://cdn.metatft.com/cdn-cgi/image/width=48,height=48,format=auto/https://cdn.metatft.com/file/metatft/champions/",
      tolower(character_id),
      ".png"
    ),
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
  ),
  compact = TRUE, 
  bordered = FALSE, 
  highlight = TRUE,
  sortable = TRUE,
  filterable = TRUE,
  pagination = TRUE,
  class = "transparent"
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
    top4_game = any(top4),
    .groups = "drop"
  ) %>%
  group_by(itemNames) %>%
  summarise(
    total_games = n(), 
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
  ),
  compact = TRUE, 
  bordered = FALSE, 
  highlight = TRUE,
  sortable = TRUE,
  filterable = TRUE,
  pagination = TRUE,
  class = "transparent"
)

##################### LAYOUT #################### 

ui <- page(
  title = "TFT-Overview",
  theme = bs_theme(
    fg = "#ffffff",
    bg = "#000000"
  ) |> bs_add_rules(sass::sass_file("styles.scss")),
  div(
    id = "dashboard-container",
    style = "display: flex; width: 100%; height: calc(100vh - 2rem); gap: 1rem;",  
    div(
      style = "flex: 0 0 25%; display: flex; flex-direction: column; gap: 1rem;",
      class = "hide-on-small-screens",
      card(
        class = "semi-transparent",
        style = "flex: 1;",
        value_box_summoner
      ),
      card(
        class = "semi-transparent",
        style = "flex: 1;",
        value_box_player_stats
      )
    ),
    # RIGHT COLUMN
    div(
      style = "flex: 0 0 75%; display: flex; flex-direction: column; overflow: auto;",
      class = "bigger-on-small-screens",
      navset_pill(
        nav_panel("Player History", card(class = "transparent", overview_games_tbl)),
        nav_panel("Game Time",      card(class = "transparent", time_plot)),
        nav_panel("Trait Details",  card(class = "transparent", trait_tbl)),
        nav_panel("Unit Details",   card(class = "transparent", unit_tbl)),
        nav_panel("Item Details",   card(class = "transparent", item_tbl))
        )
      )
    )
  )

# Save to HTML
save_html(ui, file = here(
  paste0(summoner_name, ".html")
  ))  
