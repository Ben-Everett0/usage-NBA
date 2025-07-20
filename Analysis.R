# Loading libraries
library(dplyr)
library(ggplot2)
library(plotly)

# Setting working directory
setwd("~/usage-NBA")

# Bringing in scraped data
load("UsageData.RData")


USG_plot1 <- USG_data %>%
  ggplot(aes(x = TOT_USG, y = TS_PCT, label = PLAYER_NAME)) +
  geom_point(color =  ) +
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


