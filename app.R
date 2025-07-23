library(dplyr)
library(ggplot2)
library(plotly)

load("UsageData.RData")

library(shiny)

ui <- fluidPage(
  navbarPage(title="NBA Usage",
             windowTitle = ("NBA Usage"),
             tabPanel("Overall", fluid = TRUE, sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Season", label = "Season", choices = unique(USG_data$SEASON), selected = "2024-25"),
                 sliderInput(inputId = "PossMin", label = "Minimum Possessions", min = 0, max = 5000, value = 1000, step = 10),
                 tags$body("Filter for NBA Season and Minimum # of Possessions Played"), width = 3),
               mainPanel(
                 plotlyOutput("USG_plot1"),
                 tags$br(),
                 tags$body("Total Usage is a stat created by Seth Partnow that represents how often a player ends a possession for a team."), 
                 tags$br(),
                 tags$body("True Shooting Percentage indicates how efficient a player is at scoring."), 
                 tags$br(),
                 tags$body("Turnover Percentage indicates how often a player turns the ball over (possession ends with the other team gaining possession)."), 
                 tags$br(),
                 tags$body("The players in the top right quadrant (those who use a lot of possessions and are very efficient) are typically considered the best players in the league."), 
                 tags$body("Even better if those players have lower turnover rates."), 
                 tags$br(),
                 tags$br(),
                 tags$body("Total Usage can be further broken down into Scoring and Playmaking Usage (see additional tabs)."), 
                 width = 7))),
             tabPanel("Scoring", fluid = TRUE, sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Season2", label = "Season", choices = unique(USG_data$SEASON), selected = "2024-25"),
                 sliderInput(inputId = "PossMin2", label = "Minimum Possessions", min = 0, max = 5000, value = 2000, step = 10),
                 sliderInput(inputId = "ScoringUSG", label = "Minimum Scoring Usage", min = 0, max = 1, value = 0.2, step = 0.01),
                 tags$body("Filter for NBA Season, Minimum # of Possessions Played, and Minimum Scoring Usage"), width = 3),
               mainPanel(
                 plotlyOutput("USG_plot2"),
                 tags$br(),
                 tags$body("The players in the top right quadrant (those who take a lot of shots for their team and are very efficient) are typically considered the best scorers in the league."), 
                 width = 7))),
             tabPanel("Playmaking", fluid = TRUE, sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Season3", label = "Season", choices = unique(USG_data$SEASON), selected = "2024-25"),
                 sliderInput(inputId = "PossMin3", label = "Minimum Possessions", min = 0, max = 5000, value = 2000, step = 10),
                 sliderInput(inputId = "PlaymakingUSG", label = "Minimum Playmaking Usage", min = 0, max = 1, value = 0.1, step = 0.01),
                 tags$body("Filter for NBA Season, Minimum # of Possessions Played, and Minimum Playmaking Usage"), width = 3),
               mainPanel(
                 plotlyOutput("USG_plot3"),
                 tags$br(),
                 tags$body("The players in the lower right quadrant (those who create a lot of plays for teammates and commit less turnovers) are typically considered the best playmakers in the league."), 
                 width = 7))),
             )
  )

server <- function(input, output, session) {
  output$USG_plot1 <- renderPlotly({ USG_data %>%
    filter(SEASON == input$Season) %>%
    filter(POSS > input$PossMin) %>%
    ggplot(aes(x = TOT_USG, y = TS_PCT, label = PLAYER_NAME)) +
    geom_point(aes(color = TOV_PCT)) +
    scale_color_gradient2(low = "green", mid = "cornsilk3", high = "red", midpoint = 10.92, name = "Turnover %") +
    labs(
      title = "The Usage & Efficiency Spectrum",
      x = "Total Usage Rate",
      y = "True Shooting Percentage",
    ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 22, face = "bold", color = "darkblue", hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 15, color = "gray30", hjust = 0.5, margin = margin(b = 15)),
        axis.title = element_text(size = 13, color = "gray40"),
        axis.text = element_text(size = 11, color = "gray50"),
        panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "gray70", size = 0.5)
      )
  })
  output$USG_plot2 <- renderPlotly({ USG_data %>%
      filter(SEASON == input$Season2) %>%
      filter(POSS > input$PossMin2) %>%
      filter(SCORING_USG > input$ScoringUSG) %>%
      ggplot(aes(x = SCORING_USG, y = TS_PCT, label = PLAYER_NAME)) +
      geom_point(color = "red") +
      labs(
        title = "The Primary Scorers in the NBA",
        x = "Scoring Usage Rate",
        y = "True Shooting Percentage",
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 22, face = "bold", color = "darkblue", hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 15, color = "gray30", hjust = 0.5, margin = margin(b = 15)),
        axis.title = element_text(size = 13, color = "gray40"),
        axis.text = element_text(size = 11, color = "gray50"),
        panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "gray70", size = 0.5)
      )
  })
  output$USG_plot3 <- renderPlotly({ USG_data %>%
      filter(SEASON == input$Season3) %>%
      filter(POSS > input$PossMin3) %>%
      filter(PLAYMAKING_USG > input$PlaymakingUSG) %>%
      ggplot(aes(x = PLAYMAKING_USG, y = TOV_PCT, label = PLAYER_NAME)) +
      geom_point(color = "red") +
      labs(
        title = "The Primary Playmakers in the NBA",
        x = "Playmaking Usage Rate",
        y = "Turnover Rate",
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 22, face = "bold", color = "darkblue", hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 15, color = "gray30", hjust = 0.5, margin = margin(b = 15)),
        axis.title = element_text(size = 13, color = "gray40"),
        axis.text = element_text(size = 11, color = "gray50"),
        panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "gray70", size = 0.5)
      )
  })
}


shinyApp(ui = ui, server = server)

