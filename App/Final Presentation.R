
library(here)
library(dplyr)        
library(tidyverse)
library(shiny)
library(rsconnect)

#Loading in data
life_meaning <- read.csv("life_meaning.csv")

#Reorganizing the Position col
life_meaning <- life_meaning |> 
  mutate(
    new_position = case_match(position,
                              1 ~ 5,
                              2 ~ 4,
                              4 ~ 2,
                              5 ~ 1,
                              3 ~ 3
    )
  )

#Making the Graph for the app
ggplot(life_meaning, aes(x = nation, y = new_position, fill = life_aspect)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Importance of Life Aspects Across Nations",
       x = "Nation",
       y = "Importance with 5 being most important",
       fill = "Life Aspect") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Making the UI for the app
ui <- fluidPage(
  titlePanel("Importance of Different Life Aspects Across Nations"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("nation", "Select a nation:", choices = unique(life_meaning$nation))
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# setting up server 
server <- function(input, output) {
  
  selected_life_meaning <- reactive({
    filter(life_meaning, nation == input$nation)
  })
  
  output$barPlot <- renderPlot({
    ggplot(selected_life_meaning(), aes(x = nation, y = new_position, fill = life_aspect)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste("Importance of Life Aspects in", input$nation),
           x = "Nation",
           y = "Importance with 5 being most important",
           fill = "Life Aspect") +
      theme(axis.text.x = element_text(hjust = 1))
  })
}

# Running the app
shinyApp(ui = ui, server = server)
