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
  select(gameId, traits) %>%
  unnest(cols = traits) %>%
  group_by(name) %>%
  summarise(count = n_distinct(gameId), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(
    clean_name = sub(".*_(.*)", "\\1", name),
    trait_icon_link = paste0("https://cdn.metatft.com/file/metatft/traits/",
                             tolower(clean_name),
                             ".png"),
    # Wrap both circle and text in a single flex container
    trait_icon = paste0(
      '<div style="display:flex; align-items:center;">',
      '<div style="width:40px; height:40px; background-color:black; border-radius:50%; display:flex; justify-content:center; align-items:center;">',
      '<img src="', trait_icon_link, '" height="24">',
      '</div>',
      '<span style="margin-left:8px;">', clean_name, '</span>',
      '</div>'
    )
  ) %>% 
  select(trait_icon, count)

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
    count = colDef(
      name = "Game Count",
      align = "left",                  
      headerStyle = list(textAlign = "left"),
      cell = function(value) {
        plot <- inline_game_bar(value) |> 
          plotly::config(displayModeBar = FALSE)
        htmltools::tags$div(
          style = "width: 150px; display: flex; align-items: center;", 
          plot
        )}
    )))

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
  select(gameId, units) %>%
  unnest(cols = units) %>%
  mutate(itemNames = map_chr(itemNames, ~ ifelse(is.list(.), paste(unlist(.), collapse = ", "), .))) %>%
  unnest(cols = itemNames) %>% 
  filter(!is.na(itemNames)) %>%
  filter(!grepl("Core", itemNames, ignore.case = TRUE)) %>%
  group_by(itemNames) %>%
  summarise(count = n_distinct(gameId)) %>%
  arrange(desc(count)) %>%
  mutate(item_icon_link = paste0(
    "https://cdn.metatft.com/cdn-cgi/image/width=48,height=48,format=auto/https://cdn.metatft.com/file/metatft/items/",
    tolower(itemNames),
    ".png"),
         clean_names = sub(".*_(.*)", "\\1", itemNames),
         item_icon = paste0(
           '<div style="display:flex; align-items:center;">',
           '<div style="width:40px; height:40px; display:flex; justify-content:center; align-items:center;">',
           '<img src="', item_icon_link, '" height="24">',
           '</div>',
           '<span style="margin-left:8px;">', clean_names, '</span>',
           '</div>'
         )
  ) %>% 
  select(item_icon, count)

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
    count = colDef(
      name = "Game Count",
      align = "left",                  
      headerStyle = list(textAlign = "left"),
      cell = function(value) {
        plot <- inline_game_bar(value) |> 
          plotly::config(displayModeBar = FALSE)
        htmltools::tags$div(
          style = "width: 150px; display: flex; align-items: center;", 
          plot
        )}
    )))

### CHAMPION WIN RATES ###
champ_plot <- match_details %>%
  mutate(win = as.factor(win)) %>%
  select(gameId, win, units) %>%
  unnest(cols = units) %>%
  mutate(character_id = sub(".*_(.*)", "\\1", character_id)) %>%
  select(gameId, win, character_id) %>%
  group_by(character_id, win) %>%
  summarise(count = n_distinct(gameId)) %>%
  pivot_wider(names_from = win, values_from = count) %>%
  mutate(across(everything(), ~ if_else(is.na(.), 0, .))) %>%
  mutate(total = `FALSE` + `TRUE`) %>%
  mutate(win_rate = `TRUE`/total) %>%
  filter(win_rate != 0) %>%
  arrange(desc(win_rate)) %>%
  mutate(character_id = factor(character_id, levels = .$character_id[order(-.$win_rate)])) %>%
  plot_ly(x = ~character_id, y = ~win_rate, type = "bar",
          marker = list(color = ~win_rate, colorscale = 'Greens', reversescale = TRUE)) %>%
  layout(xaxis = list(title = "Champion"),
         yaxis = list(title = "Win rate"))
match_details %>%
  select(gameId, placement, units) %>%
  unnest(cols = units) %>%
  mutate(character_id = sub(".*_(.*)", "\\1", character_id),
         top4 = if_else(placement <= 4, TRUE, FALSE)) %>%
  group_by(character_id, top4) %>%
  summarise(count = n_distinct(gameId), .groups = "drop") %>%
  pivot_wider(names_from = top4, values_from = count, values_fill = 0) %>%
  mutate(total = `FALSE` + `TRUE`,
         win_rate = round(`TRUE` / total * 100, 1)) %>%
  arrange(desc(win_rate)) %>%
  mutate(character_id = factor(character_id, levels = character_id)) %>%
  plot_ly(x = ~character_id, y = ~`FALSE`, type = "bar", name = "Placement > 4",
          marker = list(color = "lightgray")) %>%
  add_trace(y = ~`TRUE`, name = "Top 4 Finish", marker = list(color = "steelblue"),
            text = ~paste0(win_rate, "%"), textposition = "inside") %>%
  layout(barmode = "stack",
         xaxis = list(title = "Champion", tickangle = -45),
         yaxis = list(title = "Number of Games"),
         legend = list(title = list(text = "Result")))
card_03 <- card(champ_plot)

### TRAITS WIN RATES ###
traits_plot <- match_details %>%
  mutate(win = as.factor(win)) %>%
  select(gameId, win, traits) %>%
  unnest(cols = traits) %>%
  mutate(name = sub(".*_(.*)", "\\1", name)) %>%
  select(gameId, win, name) %>%
  group_by(name, win) %>%
  summarise(count = n_distinct(gameId)) %>%
  pivot_wider(names_from = win, values_from = count) %>%
  mutate(across(everything(), ~ if_else(is.na(.), 0, .))) %>%
  mutate(total = `FALSE` + `TRUE`) %>%
  mutate(win_rate = `TRUE`/total) %>%
  filter(win_rate != 0) %>%
  arrange(desc(win_rate)) %>%
  mutate(name = factor(name, levels = .$name[order(-.$win_rate)])) %>%
  plot_ly(x = ~name, y = ~win_rate, type = "bar",
          marker = list(color = ~win_rate, colorscale = 'Greens', reversescale = TRUE)) %>%
  layout(xaxis = list(title = "Trait"),
         yaxis = list(title = "Win rate"))

card_04 <- card(traits_plot)

### ITEMS WIN RATE ###
items_plot <- match_details %>%
  mutate(win = as.factor(win)) %>%
  select(gameId, win, units) %>%
  unnest(cols = units) %>%
  mutate(itemNames = map_chr(itemNames, ~ ifelse(is.list(.), paste(unlist(.), collapse = ", "), .))) %>%
  unnest(cols = itemNames) %>% 
  filter(!is.na(itemNames) & itemNames != "") %>%
  mutate(itemNames = sub(".*_(.*)", "\\1", itemNames)) %>%
  select(gameId, win, itemNames) %>%
  group_by(itemNames, win) %>%
  summarise(count = n_distinct(gameId)) %>%
  pivot_wider(names_from = win, values_from = count) %>%
  mutate(across(everything(), ~ if_else(is.na(.), 0, .))) %>%
  mutate(total = `FALSE` + `TRUE`) %>%
  mutate(win_rate = `TRUE`/total) %>%
  filter(win_rate != 0) %>%
  arrange(desc(win_rate)) %>%
  mutate(itemNames = factor(itemNames, levels = .$itemNames[order(-.$win_rate)])) %>%
  plot_ly(x = ~itemNames, y = ~win_rate, type = "bar",
          marker = list(color = ~win_rate, colorscale = 'Greens', reversescale = TRUE)) %>%
  layout(xaxis = list(title = "Item"),
         yaxis = list(title = "Win rate"))

card_05 <- card(items_plot)

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
