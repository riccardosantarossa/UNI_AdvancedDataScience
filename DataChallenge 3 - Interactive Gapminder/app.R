
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)

#Funzione per gestire lo scatterplot
continentPlot = function(cont, year1, year2) {
  gapminder |> 
    filter(continent == cont, year >= year1, year <= year2) |>
    ggplot(aes(pop, gdpPercap, size = pop, colour = country)) +
    geom_point(alpha = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 20)) +
    scale_x_log10() +
    labs(title = paste(cont, year1, "-", year2)) +    
    theme_minimal() 
}

#Variabili di supporto


# User interface
ui <- fluidPage(
  titlePanel("Shiny Interactive Gapminder"),
  
  #Slider per determinare gli anni da visualizzare
  sliderInput(
    inputId = "yearsSlider",
    label = "Select the range of years to be displayed",
    min = min(gapminder$year),
    max = max(gapminder$year),
    value = min(gapminder$year),
    step = 5
  ),
  
  #Lista da cui scegliere il continente
  selectInput(
    inputId = "continentSelect",
    label = "Select the continent you want to focus on",
    choices = c("All", "Africa", "Americas", "Europe", "Oceania")
  ),
  
  #Radiobuttons per scegliere cosa mostrare sugli assi
  radioButtons(
    inputId = "XAxisStat",
    label = "Select the stat you want to plot on the X axis",
    choices = names(gapminder[, 4:ncol(gapminder)])
  ),
  
  plotOutput("scatter")
)

# Server logic
server <- function(input, output, session) {
  output$scatter <- renderPlot({
    continentPlot("Asia", input$yearsSlider, input$yearsSlider)
  })
  
}

# Launch application
shinyApp(ui, server)