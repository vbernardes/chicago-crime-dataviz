# Setup environment ----
library(sf)
library(ggplot2)

# Read Community Areas coordinates
comm_shape <- read_sf(dsn = 'data/Boundaries - Community Areas (current)',
                      layer = 'geo_export_5b358314-1bdf-449f-84dd-bb266d9a1459')
# shape_merc <- st_transform(comm_shape, 3857)
# simple_shape <- st_simplify(shape_merc, preserveTopology = TRUE, dTolerance = 1000)
# fortified_comms <- tidy(comm_shape, region = 'area_numbe')
# add community name to fortified_comms
# fortified_comms <- merge(fortified_comms,
#                          comm_shape@data[,c("area_numbe", "community")],
#                          by.x='id',
#                          by.y='area_numbe')


draw_comm_areas <- function(highlight_comm, indicator) {
  # this function receives a data.frame with 0 or 1 row
  
  # main map with no highlighted communities
  main_plot <- plot_ly(comm_shape,
                       split = ~community,
                       line = list(width = 1),
                       fillcolor = '#FFDED4',
                       color = '#FFCCBA',
                       hoveron = "fills",
                       hoverlabel = list(bgcolor="#B01117",
                                         font = list(color='white')),
                       hoverinfo = "text",
                       text = ~paste(community),
                       source = 'map',
                       showlegend = FALSE) %>%
    config(displayModeBar = F)
  
  if (nrow(highlight_comm) == 0) {
    main_plot
  }
  else {
    comm_id <- highlight_comm$Community.Area
    # plot highlighted community
    # main_plot +
    #   geom_polygon(aes(x = long,
    #                    y = lat,
    #                    group = group),
    #                    fill = '#FF8363',
    #                data = fortified_comms[fortified_comms$id == comm_id,]) +
    #   guides(fill=FALSE)
  }
}
