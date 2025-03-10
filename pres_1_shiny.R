

library(tidyverse)
library(shiny)

df <- gapminder::gapminder %>%
  group_by(year) %>% mutate(percentiles = ntile(gdpPercap, 100))

ui <- fluidPage(
  titlePanel("Gappminder"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Help Text:
      # "Interactive plotting of gapminder data using R shiny",
      
      checkboxGroupInput("continent", 
                         "Choose a continent", 
                         choices = levels(df$continent),
                         selected = levels(df$continent)),
      
      # Slider for "Income Percentiles of interest". Hint check the `value` argument 
      # if you want to allow a range of values (instead of a single value).
      
      sliderInput("slider1", h3("Income Percentiles of interest"),
                  min = min(df$percentiles), max = max(df$percentiles), value = c(min(df$percentiles) + 24, max(df$percentiles)-25)),
      
      # Slider for "Year". Consider Looking at the animationOptions() to determine how fast
      # the animation flows.
      
      sliderInput("slider2", h3("Year"),
                  min = min(df$year), max = max(df$year), value = min(df$year),
                  round = 1, step = 5)
    ),
    
    mainPanel(plotOutput("gapminder_plot"))
  )
)

# server <- function(input, output) {}

server <- function(input, output) {
  output$gapminder_plot <- renderPlot({ 
    df %>%
      # Run some dplyr verbs that are informed by the inputs.
      filter(year == input$slider2) %>%
      # filter(percentiles >= input$slider2 & percentiles <= input$slider2) %>%
      filter(percentiles %in% input$slider1[1]:input$slider1[2]) %>%
      filter(continent %in% input$continent) %>%
      ggplot(aes(gdpPercap, lifeExp, color = continent)) +
      scale_x_log10(limits = c(100, 100000)
                    # Consider using the `limits` option.
      ) + 
      geom_point(aes(size = pop)) +
      ylim(limits = c(20, 90)
           # What should be the y axis range?
      ) +
      theme(legend.title = element_blank())
  })
}

shinyApp(ui, server)