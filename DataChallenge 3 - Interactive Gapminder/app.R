
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)

#Funzione per gestire lo scatterplot
continentPlot = function(cont, yearToSelect, XAxis, YAxis) {
  
  if(cont == "All")
  {
    gapminder |> 
      filter(year == yearToSelect) |>
      ggplot(aes(.data[[XAxis]], .data[[YAxis]], size = pop, colour = country)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) +
      scale_x_log10() +
      theme_minimal() 
  }
  else
  {
    gapminder |> 
      filter(continent == cont, year == yearToSelect) |>
      ggplot(aes(.data[[XAxis]], .data[[YAxis]], size = pop, colour = country)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) +
      scale_x_log10() +
      theme_minimal() 
  }
}

#Variabili di supporto

# User interface
ui <- fluidPage(
  titlePanel("Shiny Interactive Gapminder"),
  
  #Controlli a SX
  sidebarLayout(
    
    sidebarPanel(
  
      #Slider per determinare gli anni da visualizzare
      sliderInput(
        inputId = "yearsSlider",
        label = "Select the year to be displayed",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = min(gapminder$year),
        step = 5
      ),
      
      #Lista da cui scegliere il continente
      selectInput(
        inputId = "continentSelect",
        label = "Select the continent you want to focus on",
        choices = c("All", "Africa", "Americas", "Europe", "Oceania"),
        selected = "All"
      ),
      
      #Radiobuttons per scegliere cosa mostrare sugli assi
      radioButtons(
        inputId = "XAxisStat",
        label = "Select the stat you want to plot on the X axis",
        choiceNames = list("Life Expectancy", "Population", "GDP per Capita"),
        choiceValues = list("lifeExp","pop", "gdpPercap")
      ),
      
      radioButtons(
        inputId = "YAxisStat",
        label = "Select the stat you want to plot on the Y axis",
        choiceNames = list("Life Expectancy", "Population", "GDP per Capita"),
        choiceValues = list("lifeExp","pop", "gdpPercap")
      )
    ),
    
    mainPanel(
      plotOutput("scatter")
    )
  ),
)

# Server logic
server <- function(input, output, session) {
  output$scatter <- renderPlot({
    continentPlot(input$continentSelect, 
                  input$yearsSlider,
                  input$XAxisStat,
                  input$YAxisStat)
  })
  
}

# Launch application
shinyApp(ui, server)