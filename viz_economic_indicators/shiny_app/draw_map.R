# Setup environment ----
library(sf)
library(ggplot2)

# Read Community Areas coordinates
comm_shape <- read_sf(dsn = 'data/Boundaries - Community Areas (current)',
                      layer = 'geo_export_5b358314-1bdf-449f-84dd-bb266d9a1459')


draw_comm_areas <- function(shared_df) {
  # main map with no highlighted communities
  main_plot <- plot_ly(shared_df,
                       split = ~community,
                       line = list(width = 1),
                       fillcolor = '#FFDED4',
                       color = '#FFCCBA',
                       hoveron = "fills",
                       hoverlabel = list(bgcolor="#B01117",
                                         font = list(color='white')),
                       hoverinfo = "text",
                       text = ~paste(community,
                                     '\n',
                                     '\nCrimes per Capita:', round(Crimes.per.Capita, digits = 2),
                                     '\n% Below Poverty Level:', round(Below.Poverty.Level, digits = 1),
                                     '\nUnemployment: ', round(Unemployment, digits = 1),
                                     '\nPer Capita Income: US$', round(Per.Capita.Income, digits = 1)),
                       source = 'map',
                       showlegend = FALSE) %>%
    config(displayModeBar = F)
  
  return(main_plot)
}
