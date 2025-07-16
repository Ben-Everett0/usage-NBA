# Loading libraries
library(tidyverse)
library(httr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(showtext) # To use custom fonts easily

# Setting up scrape of NBA.com/stats
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

table_func <- function (x) {
  res <- httr::GET(url = x, httr::add_headers(.headers=headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet[1])
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  df$Season <- str_extract(x, "[:digit:]{4}\\-[:digit:]{2}")
  df
}

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

#Scraping team stats
team_urls <- paste0("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",SeasonYear,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")

df4 <- lapply(team_urls, table_func)
df4 <- do.call("rbind", df4)
df4[,3:(ncol(df4)-1)] <- sapply(df4[,3:(ncol(df4)-1)], as.numeric)

# Joining the tables
table <- left_join(df, df2, by = c("PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION", "Season"))
table <- left_join(table, df3, by = c("PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION", "Season"))
table <- left_join(table, df4, by = c("TEAM_ID", "Season"))

data <- table %>%
  mutate(Chances = (MIN.x * (FGA + 0.44 * FTA.y + TOV.y)),
         SCORING_USG = round(((FGA.x + 0.44*FTA.x) * (MIN.y.y)) / Chances, 3),
         PLAYMAKING_USG = round(((POTENTIAL_AST + FT_AST) * (MIN.y.y)) / Chances, 3),
         TOT_USG = round(((FGA.x + 0.44*FTA.x + TOV.x + POTENTIAL_AST) * (MIN.y.y)) / Chances, 3)) %>%
  filter(POSS > 2500)

# Font Setup
font_add_google("Montserrat", "montserratlight", regular.wt = 300) # Light version for general text
font_add_google("Lora", "lorabold", regular.wt = 700) # Bold for titles

showtext_auto() # Automatically use showtext for plots

USG_plot1 <- data %>%
  ggplot(aes(x = TOT_USG, y = TS_PCT, label = PLAYER_NAME)) +
  geom_point() +
  labs(
    title = "The Usage & Efficiency Spectrum",
    x = "Total Usage Rate",
    y = "True Shooting Percentage",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "lorabold", size = 22, face = "bold", color = "#333337", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "monserratlight",size = 15, color = "#555555", hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(family = "montserratlight",size = 13, color = "#666666"),
    axis.text = element_text(family = "montserratlight", size = 11, color = "#777777"),
    panel.grid.major = element_line(color = "#E0E0E0", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "#CCCCCC", size = 0.5)
  )

ggplotly(USG_plot1)

setwd("~/usage-NBA")
save.image(file = "UsageData.RData")



