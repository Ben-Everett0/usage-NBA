# Loading libraries
library(dplyr)
library(httr)

# Setting working directory
setwd("~/usage-NBA")

# Setting up headers to scrape NBA.com/stats
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# Function for getting tables from the urls
table_func <- function (x) {
  res <- httr::GET(url = x, httr::add_headers(.headers=headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet[1])
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  df$Season <- str_extract(x, "[:digit:]{4}\\-[:digit:]{2}")
  df
}

# Creating a vector for the seasons we 
SeasonYear <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25")

# Scraping assist tracking numbers
assist_urls <- paste0("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=",SeasonYear,"&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")

df <- lapply(assist_urls, table_func)
df <- do.call("rbind", df)
df[,6:(ncol(df)-1)] <- sapply(df[,6:(ncol(df)-1)], as.numeric)

# Scraping individual box numbers
box_urls <- paste0("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",SeasonYear,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=")

df2 <- lapply(box_urls, table_func)
df2 <- do.call("rbind", df2)
df2[,6:(ncol(df2)-1)] <- sapply(df2[,6:(ncol(df2)-1)], as.numeric)

# Scraping individual advanced numbers
adv_urls <- paste0("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",SeasonYear,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=")

df3 <- lapply(adv_urls, table_func)
df3 <- do.call("rbind", df3)
df3[,6:(ncol(df3)-1)] <- sapply(df3[,6:(ncol(df3)-1)], as.numeric)

# Scraping team box score stats
team_urls <- paste0("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",SeasonYear,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")

df4 <- lapply(team_urls, table_func)
df4 <- do.call("rbind", df4)
df4[,3:(ncol(df4)-1)] <- sapply(df4[,3:(ncol(df4)-1)], as.numeric)

# Scraping team advanced stats
team_adv_urls <- paste0("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",SeasonYear,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")

df5 <- lapply(team_adv_urls, table_func)
df5 <- do.call("rbind", df5)
df5[,3:(ncol(df5)-1)] <- sapply(df5[,3:(ncol(df5)-1)], as.numeric)

# Joining the tables
table <- left_join(df, df2, by = c("PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION", "Season"))
table <- left_join(table, df3, by = c("PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION", "Season"))
table <- left_join(table, df4, by = c("TEAM_ID", "Season"))
table <- left_join(table, df5, by = c("TEAM_ID", "Season"))

# Cleaning up the table to only include relevant columns and specify player vs. team columns
table$FGA_TEAM <- table$FGA
table$FGM_TEAM <- table$FGM
table <- table %>%
  mutate(GP = GP.x, TEAM_WINS = W, TEAM_LOSSES = L, TEAM_NAME = TEAM_NAME.x,
         MIN = MIN.x, POSS = POSS.x, SEASON = Season, AGE = AGE.x,
         FGM = FGM.x, FGA = FGA.x, FG3M = FG3M.x, FG3A = FG3A.x, FTM = FTM.x, FTA = FTA.x,
         OREB = OREB.x, DREB = DREB.x, AST = AST.x, TOV = TOV.x, STL = STL.x, BLK = BLK.x,
         PF = PF.x, PFD = PFD.x, PTS = PTS.x, PLUS_MINUS = PLUS_MINUS.x,
         OFF_RATING = OFF_RATING.x, DEF_RATING = DEF_RATING.x, NET_RATING = NET_RATING.x,
         AST_PCT = AST_PCT.x, AST_TO = AST_TO.x,
         OREB_PCT = OREB_PCT.x, DREB_PCT = DREB_PCT.x,
         TOV_PCT = TM_TOV_PCT.x, TS_PCT = TS_PCT.x,
         MIN_TEAM = MIN.y.y, POSS_TEAM = POSS.y,
         FGM_TEAM = FGM, FG3M_TEAM = FG3M.y, FG3A_TEAM = FG3A.y, FTM_TEAM = FTM.y, FTA_TEAM = FTA.y,
         OREB_TEAM = OREB.y, DREB_TEAM = DREB.y, AST_TEAM = AST, TOV_TEAM = TOV.y, STL_TEAM = STL.y, BLK_TEAM = BLK.y,
         PF_TEAM = PF.y, PFD_TEAM = PFD.y, PTS_TEAM = PTS.y, PLUS_MINUS_TEAM = PLUS_MINUS.y,
         OFF_RATING_TEAM = OFF_RATING.y, DEF_RATING_TEAM = DEF_RATING.y, NET_RATING_TEAM = NET_RATING.y,
         AST_PCT_TEAM = AST_PCT.y, AST_TO_TEAM = AST_TO.y,
         OREB_PCT_TEAM = OREB_PCT.y, DREB_PCT_TEAM = DREB_PCT.y,
         TOV_PCT_TEAM = TM_TOV_PCT.y, TS_PCT_TEAM = TS_PCT.y) %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME, SEASON, AGE, GP, TEAM_WINS, TEAM_LOSSES, MIN, MIN_TEAM, # General
         PASSES_MADE, PASSES_RECEIVED, AST, FT_AST, SECONDARY_AST, POTENTIAL_AST, AST_PTS_CREATED, AST_ADJ, # Assist table
         FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, TOV, STL, BLK, PF, PFD, PTS, PLUS_MINUS, # Individual box score
         OFF_RATING, DEF_RATING, NET_RATING, AST_PCT, AST_TO, OREB_PCT, DREB_PCT, TOV_PCT, TS_PCT, POSS, # Individual advanced
         FGM_TEAM, FGA_TEAM, FG3M_TEAM, FG3A_TEAM, FTM_TEAM, FTA_TEAM, OREB_TEAM, DREB_TEAM, TOV_TEAM, STL_TEAM, BLK_TEAM, PF_TEAM, PFD_TEAM, PTS_TEAM, PLUS_MINUS_TEAM, # Team box score
         OFF_RATING_TEAM, DEF_RATING_TEAM, NET_RATING_TEAM, AST_PCT_TEAM, AST_TO_TEAM, OREB_PCT_TEAM, DREB_PCT_TEAM, TOV_PCT_TEAM, TS_PCT_TEAM, POSS_TEAM) # Team advanced

# Calculating various usage stats
USG_data <- table %>%
  mutate(CHANCES = (MIN * (FGA_TEAM + 0.44 * FTA_TEAM + TOV_TEAM)),
         SCORING_USG = round(((FGA + 0.44*FTA) * (MIN_TEAM)) / CHANCES, 3),
         PLAYMAKING_USG = round(((POTENTIAL_AST + FT_AST) * (MIN_TEAM)) / CHANCES, 3),
         TOT_USG = round(((FGA + 0.44*FTA + TOV + POTENTIAL_AST) * (MIN_TEAM)) / CHANCES, 3))

# Removing unnecessary data
rm(list=setdiff(ls(), "USG_data"))

# Saving data
save.image("UsageData.RData")


