# Setup environment ----
library(shiny)
library(shinyBS)
library(SDMTools)

# Load data & resources ----
eco_indicators_df <- read.csv('data/Economic Indicators and Crime 2015.csv')
source('draw_map.R')

# Helper functions ----
scatter_indicators <- function(x,
                               y = 'Crimes.per.Capita',
                               color_by = 'Proportion.of.Violent.Crimes',
                               highlight) {
  
  main_scatter <- ggplot(data = eco_indicators_df,
                         aes_string(x, y, color = color_by)) +
    geom_point() +
    scale_color_continuous(
      # name = 'Proportion of Violent Crimes',
      low='white', high='red'
    ) +
    theme_minimal()
  
  if (nrow(highlight) == 0) {
    main_scatter
  }
  else {
    # highlight specific point
    main_scatter +
      geom_point(data = subset(eco_indicators_df,
                               Community.Area == highlight$Community.Area),
                 aes_string(x, y),
                 shape = 21,
                 alpha = 0.2,
                 fill = 'red',
                 # color = 'red',
                 size = 7) +
      guides(fill=FALSE) +
      scale_fill_continuous(low='white', high='red')
  }
}

point_in_comm <- function(point_coords) {
  # return community ID of point in point_coords.
  for (id in seq(1,77)) {
    comm_boundaries <- fortified_comm_areas[fortified_comm_areas$id==id, 1:2]
    if (pnt.in.poly(point_coords, comm_boundaries)$pip) {
      return(as.numeric(id))
    }
  }
  # if we leave the loop and don't find a community, return 0
  return(0)
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
                  plotOutput('map',
                             click = 'map_click'),
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
                verticalLayout(
                  plotOutput('plot', click = 'scatter_click'),
                  # test for reading user clicks on plot
                  verbatimTextOutput('scatter_click_info')
                )
    )
  )
)

# Server logic ----
server <- function(input, output) {
  highlighted_comm <- reactiveVal(data.frame()) # initialize w/ empty data frame
  
  output$plot <- renderPlot({
    scatter_indicators(x = input$indicator,
                       color_by = input$color_by,
                       highlight = highlighted_comm())
  })
  
  observeEvent(input$scatter_click, {
    highlighted_comm(nearPoints(eco_indicators_df,
                               input$scatter_click,
                               maxpoints = 1))
  })
  
  observeEvent(input$map_click, {
    clicked_coords <- data.frame(input$map_click[1], input$map_click[2])
    id <- point_in_comm(clicked_coords)
    if (id == 0) {
      highlighted_comm(data.frame())
    }
    else {
      highlighted_comm(eco_indicators_df[id,]) 
    }
  })
  
  output$scatter_click_info <- renderPrint({
    paste(highlighted_comm()$Community.Area,
          highlighted_comm()$Community.Area.Name,
          input$map_click[1],input$map_click[2])
  })
  
  output$scatter_hover_info <- renderPrint({
    paste(highlighted_comm()$Community.Area.Name)
  })
  
  output$map <- renderPlot({
    draw_comm_areas(highlighted_comm(),
                    input$indicator)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
