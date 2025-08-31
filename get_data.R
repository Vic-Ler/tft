library(httr) #for API request
library(jsonlite) #for data extraction
library(dplyr) #for data manipulation
library(cli) #for spinner alert info

### OVERVIEW PARAMETERS ###
#parameters are defined in report_script and therefore commented here
#FYI: anyone can get an API key by creating an account on RIOT's developer platform 

#api_key = Sys.getenv("RIOT_API")
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
#API FUNCTION 
fetch_data <- function(url, api_key) {
  #returns API data based on input
  response <- GET(url, 
                  add_headers("X-Riot-Token" = api_key))
  if (status_code(response) == 200) {
    data <- fromJSON(content(response,
                             "text", 
                             encoding = "UTF-8"))
    print("Data fetched successfully")
    return(data)
  } else {
    print(paste("Error fetching data:", status_code(response)))
  }}
#################
cli::cli_alert_info("Determining PUUID - Unique Player Key...")

summoner_data <- fetch_data(paste0("https://", 
                                   region, 
                                   ".api.riotgames.com/riot/account/v1/accounts/by-riot-id/",
                                   summoner_name, "/", 
                                   tagline),
                            api_key)

puuid <- summoner_data$puuid

#################
cli::cli_alert_info("Fetching TFT League Data...")

tft_overview_data <- fetch_data(paste0("https://", sub_region,
                                       ".api.riotgames.com/tft/league/v1/by-puuid/", 
                                       puuid),
                                api_key)
#################
cli::cli_alert_info("Fetching TFT Match IDs...")

match_ids <- fetch_data(paste0("https://", 
                               region, 
                               ".api.riotgames.com/tft/match/v1/matches/by-puuid/", 
                               puuid, 
                               "/ids"),
                        api_key)
#################
cli::cli_alert_info("Fetching TFT Match Details...")

match_details <- {} %>% as_tibble()
  
for (i in match_ids) {
  match_data_raw <- fetch_data(paste0("https://", 
                                      region, 
                                      ".api.riotgames.com/tft/match/v1/matches/", 
                                      i),
                               api_key)
  match_participants <- match_data_raw$info$participants[match_data_raw$info$participants$puuid == puuid, ] %>% as_tibble()
  match_overview <- match_data_raw$info[c("game_datetime", "gameId", "game_length", "tft_game_type")] %>% as_tibble()
  match_data <- cbind(match_overview, match_participants)
  match_details <- bind_rows(match_details, match_data)
}