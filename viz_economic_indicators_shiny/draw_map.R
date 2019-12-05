# Read Community Areas coordinates
comm_areas_shape <- readShapePoly('../data/Boundaries - Community Areas (current)/geo_export_5b358314-1bdf-449f-84dd-bb266d9a1459.shp')
comm_areas_points <- fortify(comm_areas_shape)

draw_comm_areas <- function() {
  # Using ggplot
  ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     alpha = 0.7),
                     # fill = Below.Poverty.Level),
                 data = comm_areas_points) +
    guides(alpha=FALSE) +
    coord_map('mercator') +
    theme_void()
  
  # # Using ggvis
  # comm_areas_points %>%
  #   group_by(group, id) %>%
  #   ggvis(~long, ~lat) %>%
  #   layer_paths(strokeOpacity:=0.15) %>%
  #   hide_legend('fill') %>%
  #   hide_axis("x") %>% hide_axis("y") %>%
  #   set_options(width=400, height=600, keep_aspect=TRUE) %>%
  #   bind_shiny('map')
}
