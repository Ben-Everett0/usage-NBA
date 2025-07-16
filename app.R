library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rvest)
library(tidyr)
library(plotly)

load("UsageData.RData")

library(shiny)

ui <- fluidPage(
  navbarPage(title="NBA Usage",
             windowTitle = ("NBA Usage"),
             tabPanel("Home", fluid = TRUE, sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Season", label = "Season", choices = unique(data$Season)),
                 sliderInput(inputId = "PossMin", label = "Minimum Possessions", min = 0, max = 5000, value = 1000, step = 10),
                 tags$body("TBD"), width = 3),
               mainPanel(plotlyOutput("USG_plot1")))),
             tabPanel("Tab2"),
             tabPanel("Tab3"),
             )
  )

server <- function(input, output, session) {
  
  output$USG_plot1 <- renderPlotly({ data %>%
    filter(Season == input$Season) %>%
    filter(POSS > input$PossMin) %>%
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
  })
  

}


shinyApp(ui = ui, server = server)

