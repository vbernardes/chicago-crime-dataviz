# Setup environment ----
library(shiny)
library(plotly)
library(crosstalk)

# Load data & resources ----
eco_indicators_df <- read.csv('data/Economic Indicators and Crime 2011.csv')

# Helper functions ----
scatter_indicators <- function(shared_df,
                               x,
                               y = 'Crimes.per.Capita',
                               color_by = 'Proportion.of.Violent.Crimes') {
  # set plot labels
  y_label <- 'Crimes per Capita'
  x_label <- switch (x,
                     'Below.Poverty.Level' = 'Percentage of People Below Poverty Level',
                     'Unemployment' = 'Unemployment Rate (%)',
                     'Per.Capita.Income' = 'Per Capita Income (US$)')
  # set guide title
  guide_title <- switch (color_by,
                         'Proportion.of.Violent.Crimes' = 'Proportion of\nViolent Crimes',
                         'Proportion.of.Arrests' = 'Proportion\nof Arrests',
                         'Proportion.of.Financial.Crimes' = 'Proportion of\nFinancial Crimes')
  
  main_scatter <- plot_ly(shared_df,
                          x=~get(x), y=~get(y),
                          color=~get(color_by),
                          # name = guide_title,
                          colors='Reds',
                          hoverinfo = 'text',
                          text = ~paste(Community.Area.Name),
                          source = 'scatter') %>%
    add_trace(type='scatter') %>%
    layout(xaxis = list(title=x_label, zeroline=FALSE, fixedrange=TRUE),
           yaxis = list(title=y_label, zeroline=FALSE, fixedrange=TRUE)) %>%
    config(displayModeBar = F)
}


# UI ----
ui <- fluidPage(
  style = 'padding: 5%; font-size: small;',
  fluidRow(
    h1('Crime and Poverty in Chicago'),
    p('People in poor communities not only have to struggle with economic
      difficulties, but are also more exposed to crime. The plot below presents
      a snapshot of Chicago in 2011, and shows a strong association between the
      socioeconomic status of a community and crime activity. Each point
      represents a community and is placed according to the selected
      socioeconomic indicator and the number of crimes per capita.')
  ),

  fluidRow(
    style = 'height:600px;',
    splitLayout(cellWidths = c("25%", "75%"),
                # Left column:
                verticalLayout(
                  plotlyOutput('map'),
                  selectInput('indicator',
                              label = 'Socioeconomic Indicator',
                              choices = list(
                                '% Below Poverty Level' = 'Below.Poverty.Level',
                                'Unemployment Rate' = 'Unemployment',
                                'Per Capita Income' = "Per.Capita.Income"
                              )
                  ),
                  selectInput('color_by',
                              label = 'Color shows',
                              choices = list(
                                "Proportion of Arrests" = "Proportion.of.Arrests",
                                "Proportion of Violent Crimes" = "Proportion.of.Violent.Crimes",
                                "Proportion of Financial Crimes" = "Proportion.of.Financial.Crimes"
                              ),
                              selected = "Proportion.of.Violent.Crimes"
                  )
                ),
                # Right column
                plotlyOutput('plot')
    )
  )
)

# Server logic ----
server <- function(input, output, session) {
  
  # create shared data frame for all plots
  merged_shape <- merge(comm_shape, eco_indicators_df,
                        by.x='area_numbe',
                        by.y='Community.Area')
  shared_df <- SharedData$new(merged_shape)
  # load function to draw map
  source('draw_map.R')
  
  output$plot <- renderPlotly({
    scatter_indicators(shared_df,
                       x = input$indicator,
                       color_by = input$color_by)
  })
  
  output$map <- renderPlotly({
    draw_comm_areas(shared_df)
  })
}

# Run the app ----
shinyApp(ui, server)
