library(httr) #for API request
library(jsonlite) #for data extraction
library(dplyr) #for data manipulation
library(cli) #for spinner alert info

### OVERVIEW PARAMETERS ###
#parameters are defined in report_script and therefore commented here
#FYI: anyone can get an API key by creating an account on RIOT's developer platform 

#api_key = "RGAPI-465f712d-4b89-4445-8805-db3341774632"
#summoner_name = "LivingWeapon" #official player name as displayed in-game
#region = "europe" 
#sub_region = "euw1"
#tagline = "EUW"

### OVERVIEW API URLS ###
#API links are provided by RIOTs developer platform
#This script uses the following URLs: 
#/riot/account/v1/accounts/by-riot-id/{gameName}/{tagLine}
#/tft/league/v1/by-puuid/{puuid}
#/tft/match/v1/matches/by-puuid/{puuid}/ids
#/tft/match/v1/matches/{matchId}

#################
cli::cli_alert_info("Determining PUUID - Unique Player Key...")

url_puuid <- paste0("https://", 
                    region, 
                    ".api.riotgames.com/riot/account/v1/accounts/by-riot-id/",
                    summoner_name, "/", 
                    tagline)

response_uuid <- GET(url_puuid, 
                     add_headers("X-Riot-Token" = api_key))

if (status_code(response_uuid) == 200) {
  summoner_data <- fromJSON(content(response_uuid, 
                                    "text", 
                                    encoding = "UTF-8"))
  puuid <- summoner_data$puuid
  print(paste("PUUID obtained successfully:", puuid))
} else {
  print(paste("Error fetching data:", status_code(response_uuid)))
}

#################
cli::cli_alert_info("Fetching TFT League Data...")

url_league <- paste0("https://", sub_region,
                     ".api.riotgames.com/tft/league/v1/by-puuid/", 
                     puuid)

response_league <- GET(url_league, 
                       add_headers("X-Riot-Token" = api_key))

if (status_code(response_league) == 200) {
  tft_overview_data <- fromJSON(content(response_league, 
                                        "text", 
                                        encoding = "UTF-8"))
  print("TFT Overview data obtained successfully")
} else {
  print(paste("Error fetching data:", status_code(response_league)))
}
#################
cli::cli_alert_info("Fetching TFT Match IDs...")

url_match <- paste0("https://", 
                    region, 
                    ".api.riotgames.com/tft/match/v1/matches/by-puuid/", 
                    puuid, 
                    "/ids")

response_match <- GET(url_match, 
                      add_headers("X-Riot-Token" = api_key))

if (status_code(response_match) == 200) {
  match_ids <- fromJSON(content(response_match, 
                                        "text", 
                                        encoding = "UTF-8"))
  print("TFT Match IDs obtained successfully")
} else {
  print(paste("Error fetching data:", status_code(response_match)))
}
#################
cli::cli_alert_info("Fetching TFT Match Details...")

fetch_match_details <- function(match_id, puuid, region, api_key) {
  #returns match details as a tibble based on puuid
  url_match_details <- paste0("https://", 
                region, 
                ".api.riotgames.com/tft/match/v1/matches/", 
                match_id)
  
  response_match_details <- GET(url_match_details, 
                                add_headers("X-Riot-Token" = api_key))
  
  if (status_code(response_match_details) == 200) {
    match_data_raw <- fromJSON(content(response_match_details, 
                                       "text", 
                                       encoding = "UTF-8")) 
    
    match_participants <- match_data_raw$info$participants[match_data_raw$info$participants$puuid == puuid, ] %>% as_tibble()
    match_overview <- match_data_raw$info[c("game_datetime", "gameId", "game_length", "tft_game_type")] %>% as_tibble()
    match_data <- cbind(match_overview, match_participants)
    return(match_data)
  } else {
    print(paste("Error fetching data:", status_code(response_match_details)))
    return(NULL)
  }}

#looping through match ids to extract match data
match_details <- {} %>% as_tibble()
for (i in match_ids) {
  match_data <- fetch_match_details(i, puuid, region, api_key)
  match_details <- bind_rows(match_details, match_data)
}
