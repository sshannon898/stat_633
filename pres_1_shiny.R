

library(tidyverse)
library(shiny)
library(maps)



county_data <- map_data("county")
county_match <- maps::county.fips %>%
  separate(polyname, into = c("region", "subregion"), sep = ",") 

county_shp <- left_join(county_data, county_match, by = c("region", "subregion")) %>%
  mutate(FIPS = str_pad(fips, width = 5, pad = "0")) %>% select(-5,-6,-7)
  

local_path <- "C:/Users/sshan/Desktop/umass amherst/sta633/stat_633/"

cra_data <- read.csv("cra_data.csv")

transmittal_data <- read.csv("transmittal.csv") %>% mutate(assets = assets)

acs_data <- read.csv("acs.csv") %>%
  mutate(FIPS = str_pad(FIPS, width = 5, pad = "0"))

cra_assets <- left_join(cra_data, transmittal_data, by = c("resp_id", "agency", "year")) %>%
  mutate(portfolio = case_when(
    # 10b CBO
    assets <= 10000000 ~ "CBO",
    # 10b - 100b RBO
    assets > 10000000 & assets <= 100000000 ~ "RBO",
    # 100b+ LBO
    assets > 100000000 ~ "LBO"
  )) %>% select(-1, -2)

county_data <- cra_assets %>%
  group_by(year, state, county, portfolio) %>%
  summarize(num_banks = n(),
            sum_loan_amt = sum(sum_loan_amt),
            sum_n_loans = sum(sum_n_loans)) %>% 
  mutate(state = str_pad(state, width = 2, pad = "0"),
         county = str_pad(county, width = 3, pad = "0")) %>% 
  unite(FIPS, state, county, sep = "")

county_acs_data <- left_join(county_data, acs_data, by = c("year" = "Year", "FIPS")) %>%
  select(-X, -County)

map_data <- left_join(county_acs_data, county_shp, by = c("FIPS")) %>%
  mutate(percentiles = ntile(Median_HH_Income, 100)) %>% filter(is.na(percentiles) != TRUE)

# x <- map_data %>%
#   filter(year == 2014) %>%
#   filter(substr(FIPS, 1, 2) == "25") %>%
#   # filter(percentiles >= input$slider2 & percentiles <= input$slider2) %>%
#   filter(percentiles %in% 1:100) %>%
#   filter(portfolio %in% "CBO") %>%
#   ggplot(aes(x = long, y = lat, group = group, fill = sum_loan_amt)) +
#   geom_polygon() +
#   # theme_void() +
#   # What should be the y axis range?
#   theme(legend.title = element_blank())





rm(cra_assets)
rm(acs_data)
rm(transmittal_data)
rm(cra_data)
rm(county_data)
rm(county_match)
rm(county_acs_data)
rm(county_shp)

ui <- fluidPage(
  titlePanel("Community Reinvestment Act"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Help Text:
      # "Interactive plotting of the Community Reinvestment Act data using R shiny",
      
      # checkboxGroupInput("portfolio", 
      #                    "Choose a map overlay", 
      #                    choices = levels(map_data$portfolio),
      #                    selected = levels(map_data$portfolio)),

               radioButtons("portfolio", h3("Portfolio"),
                            choices = list("CBO" = "CBO", "RBO" = "RBO",
                                           "LBO" = "LBO"),selected = "CBO"),
      
      
      
      # Slider for "Income Percentiles of interest". Hint check the `value` argument 
      # if you want to allow a range of values (instead of a single value).
      
      sliderInput("slider1", h3("Income Percentiles of interest"),
                  min = min(map_data$percentiles),
                  max = max(map_data$percentiles),
                  value = c(min(map_data$percentiles) + 24,
                            max(map_data$percentiles)-25)),
      
      # Slider for "Year". Consider Looking at the animationOptions() to determine how fast
      # the animation flows.
      
      sliderInput("slider2", h3("Year"),
                  min = min(map_data$year),
                  max = max(map_data$year),
                  value = min(map_data$year),
                  round = 1,
                  step = 1),
    ),
    mainPanel(plotOutput("cra_plot"))
  )
)

# server <- function(input, output) {}

server <- function(input, output) {
  output$cra_plot <- renderPlot({ 
    map_data %>%
      # Run some dplyr verbs that are informed by the inputs.
      filter(year == input$slider2) %>%
      # filter(substr(FIPS, 1, 2) == "25") %>%
      # filter(percentiles >= input$slider2 & percentiles <= input$slider2) %>%
      filter(percentiles %in% input$slider1[1]:input$slider1[2]) %>%
      filter(portfolio == input$portfolio) %>%
      ggplot(aes(x = long, y = lat, group = group, fill = sum_loan_amt)) +
      geom_polygon() +
      # theme_void() +
           # What should be the y axis range?
      theme(legend.title = element_blank())
  })
}

shinyApp(ui, server)
