# Setup environment ----
library(shiny)
library(ggvis)
library(ggmap)
library(maptools)

# Load data & resources ----
eco_indicators_df <- read.csv('data/Economic Indicators and Crime 2015.csv')
source('draw_map.R')

# Helper functions ----
scatter_indicators <- function(x,
                               y = 'Crimes.per.Capita',
                               color_by = 'Proportion.of.Violent.Crimes') {
  # # Using ggvis
  # xprop <- prop('x', as.symbol(x))
  # yprop <- prop('y', as.symbol(y))
  # eco_indicators_df %>%
  #   ggvis(xprop, yprop) %>%
  #   layer_points() %>%
  #   bind_shiny('plot')
  
  # Using ggplot
  ggplot(data = eco_indicators_df,
         aes_string(x, y, color = color_by)) +
    geom_point() +
    scale_color_continuous(
      # name = 'Proportion of Violent Crimes',
      low='white', high='red'
    ) +
    theme_minimal()
}



# UI ----
ui <- fluidPage(
  br(),
  fluidRow(
    h1("Crimes and Poverty"),
    p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')
  ),
  
  fluidRow(
    splitLayout(cellWidths = c("25%", "75%"),
                # Left column:
                verticalLayout(
                  plotOutput('map'),
                  # ggvisOutput('map'),
                  selectInput('indicator',
                              label = 'Socioeconomic Indicator',
                              choices = list(
                                '% Below Poverty Level' = "Below.Poverty.Level",
                                'Unemployment Rate' = 'Unemployment',
                                'Per Capita Income' = "Per.Capita.Income"
                              )
                  ),
                  selectInput('color_by',
                              label = 'Color by',
                              choices = list(
                                "Proportion of Arrests" = "Proportion.of.Arrests",
                                "Proportion of Violent Crimes" = "Proportion.of.Violent.Crimes"
                              ),
                              selected = "Proportion.of.Violent.Crimes"
                  )
                ),
                # Right column
                plotOutput('plot')     # for ggplot
                # ggvisOutput('plot')    # for ggvis
    )
  )
)

# Server logic ----
server <- function(input, output) {
  output$plot <- renderPlot({
    scatter_indicators(x = input$indicator,
                       color_by = input$color_by)
  })
  
  output$map <- renderPlot({
    draw_comm_areas()   # ggplot
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
