# Setup environment ----
library(broom)
library(rgdal)
library(rgeos)
library(ggplot2)
library(maptools)
library(maps)
library(mapproj)

# Read Community Areas coordinates
comm_areas_shape <- readOGR(dsn = '../data/Boundaries - Community Areas (current)',
                            layer = 'geo_export_5b358314-1bdf-449f-84dd-bb266d9a1459')
fortified_comms <- tidy(comm_areas_shape, region = 'area_numbe')


draw_comm_areas <- function(highlight_comm, indicator) {
  # this function receives a data.frame with 0 or 1 row
  
  # main map with no highlighted communities
  main_plot <- ggplot() +
    geom_polygon(data = fortified_comms,
                 aes(x = long,
                     y = lat,
                     group = group),
                 fill = '#FFDED4',
                 color = '#FFCCBA') +
    guides(fill=FALSE, color=FALSE) +
    coord_map('mercator') +
    theme_void()
  
  if (nrow(highlight_comm) == 0) {
    main_plot
  }
  else {
    comm_id <- highlight_comm$Community.Area
    # plot highlighted community
    main_plot +
      geom_polygon(aes(x = long,
                       y = lat,
                       group = group),
                       fill = '#FF8363',
                   data = fortified_comms[fortified_comms$id == comm_id,]) +
      guides(fill=FALSE)
  }
}
