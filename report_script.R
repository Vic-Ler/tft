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
library(here)
library(cli)

### PARAMETERS ###
api_key = "RGAPI-465f712d-4b89-4445-8805-db3341774632"
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
  showcase = bs_icon("person-fill"),
  theme = "gray",
  p(paste0("Region: ", region, " / ", sub_region))
)

### SUMMONER GENERAL PLAYER STATS ###
summoner_overview <- tft_overview_data %>% filter(queueType == game_type)

value_box_player_stats <- value_box(
  title = "Status",
  value = paste0(tft_overview_data$tier, " | ", tft_overview_data$rank),
  showcase = bs_icon("graph-up"),
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
  mutate(year = year(game_datetime), 
         month = month(game_datetime), 
         day = day(game_datetime)) %>%
  group_by(year, month, day) %>%
  summarise(placement = mean(placement)) %>%
  mutate(game_date = make_date(year, month, day)) %>%
  plot_ly(x = ~game_date, y = ~placement, 
          type = "scatter", mode = "lines+markers", 
          line = list(color = "gray"),  # Change line color
          marker = list(color = "black", size = 8) ) %>%
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Placement", autorange = "reversed"))

card_02 <- card(time_plot)

### COMMON TRAITS ###
common_traits <- match_details %>% 
  select(gameId, traits) %>%
  unnest(cols = traits) %>%
  group_by(name) %>%
  summarise(count = n_distinct(gameId)) %>%
  arrange(desc(count)) %>%
  mutate(name = sub(".*_(.*)", "\\1", name)) %>%
  slice_head(n = 3) %>%
  pull(name) %>%
  paste(collapse = ", ")

### COMMON UNITS ###
common_units <- match_details %>% 
  select(gameId, units) %>%
  unnest(cols = units) %>%
  group_by(character_id) %>%
  summarise(count = n_distinct(gameId)) %>%
  arrange(desc(count)) %>%
  mutate(character_id = sub(".*_(.*)", "\\1", character_id)) %>%
  slice_head(n = 3) %>%
  pull(character_id) %>%
  paste(collapse = ", ")

### COMMON ITEMS ###
common_items <- match_details %>% 
  select(gameId, units) %>%
  unnest(cols = units) %>%
  mutate(itemNames = map_chr(itemNames, ~ ifelse(is.list(.), paste(unlist(.), collapse = ", "), .))) %>%
  unnest(cols = itemNames) %>% 
  filter(!is.na(itemNames)) %>%
  group_by(itemNames) %>%
  summarise(count = n_distinct(gameId)) %>%
  arrange(desc(count)) %>%
  mutate(itemNames = sub(".*_(.*)", "\\1", itemNames)) %>%
  slice_head(n = 3) %>%
  pull(itemNames) %>%
  paste(collapse = ", ")

### COMMON PLAYER STYLE ###
value_box_player_style <- value_box(
  title = "Player Style (Top 3)",
  showcase = bs_icon("graph-up"),
  theme = "gray",
  p(common_traits), 
  p(paste0("Champions: ", common_units)),
  p(paste0("Items: ", common_items)))

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
