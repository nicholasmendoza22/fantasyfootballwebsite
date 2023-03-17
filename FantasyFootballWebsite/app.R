library(shiny)
library(shinydashboard)
library(tidyverse)

data <- read_delim("fantasydata.csv")
getwd()

# average per game

data$averagetargetspergame <- data$Tgt/data$G
data$averagefantasypointspergame <- data$PPR/data$G
data$averagepassingattemptspergame <- data$PassAtt/data$G
data$averagerushingattemptspergame <- data$RushAtt/data$G
data$averagepassingyardspergame <- data$PassYds/data$G
data$averagerushingyardspergame <- data$RushYds/data$G

# filter by position
qb <- data %>% 
  filter(FantPos == "QB")

rb <- data %>% 
  filter(FantPos == "RB")

wr <- data %>% 
  filter(FantPos == "WR")

te <- data %>% 
  filter(FantPos == "TE")

# Define UI for app
ui <- dashboardPage(
  dashboardHeader(title = "Fantasy Football Stats"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quarterbacks", tabName = "Quarterbacks"),
      menuItem("Running Backs", tabName = "Running Backs"),
      menuItem("Wide Receivers", tabName = "Wide Receivers"),
      menuItem("Tight Ends", tabName = "Tight Ends"),
      menuItem("Kickers", tabName = "Kickers"),
      menuItem("Defense and Special Teams", tabName = "Defense and Special Teams"),
      menuItem("More Content", tabName = "More Content")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Quarterbacks",
              fluidRow(
                column(width = 12,
                       h2("Quarterbacks"),
                       radioButtons("qb_radio", 
                                    label = h3("Select a stat to plot:"),
                                    choices = c("Average Passing Attempts per Game" = "averagepassingattemptspergame",
                                                "Average Passing Yards per Game" = "averagepassingyardspergame",
                                                "Average Rushing Attempts per Game" = "averagerushingattemptspergame",
                                                "Average Rushing Yards per Game" = "averagerushingyardspergame"),
                                    selected = "averagepassingattemptspergame")
                )
              ),
              fluidRow(
                column(width = 12,
                       plotOutput("qb_plot")
                )
              )
      ),
      tabItem(tabName = "Running Backs",
              # Insert code for tab 2 here
      ),
      tabItem(tabName = "Wide Receivers",
              # Insert code for tab 3 here
      ),
      tabItem(tabName = "Tight Ends",
              # Insert code for tab 4 here
      ),
      tabItem(tabName = "Kickers",
              # Insert code for tab 5 here
      ),
      tabItem(tabName = "Defense and Special Teams",
              # Insert code for tab 6 here
      ),
      tabItem(tabName = "More Content",
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$qb_plot <- renderPlot({
    # Calculate correlation coefficient
    corr_coef <- cor(qb[[input$qb_radio]], qb$averagefantasypointspergame)
    
    ggplot(qb, aes_string(x = input$qb_radio, y = "averagefantasypointspergame")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      xlab(input$qb_radio) +
      ylab("Fantasy Points per Game") +
      ggtitle("Quarterback Stats") +
      # Add correlation coefficient as text to the plot
      geom_text(x = max(qb[[input$qb_radio]]) * 0.1,
                y = max(qb$averagefantasypointspergame) * 0.9,
                label = paste0("Correlation coefficient: ", round(corr_coef, 2)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

