# Setup environment
library(ggplot2)
library(ggrepel)
setwd("~/Documents/Cursos/Mestrado UP/data_viz/project/viz_2008_crisis")

# Load data
unemp_crimes <- read.csv('data/Unemployment_and_Crimes_2001_2016.csv')

shift_unemp_left <- c(tail(unemp_crimes$Unemployment, n=-1), NA)
shift_crimes_left <- c(tail(unemp_crimes$Number.of.Crimes, n=-1), NA)

ggplot(data = unemp_crimes,
       aes(Unemployment, Number.of.Crimes)) +
  geom_point() +
  # first half of segment, with arrows
  geom_segment(aes(xend=(Unemployment + shift_unemp_left)/2,
                   yend=(Number.of.Crimes + shift_crimes_left)/2),
               arrow=arrow(angle = 30, length=unit(0.15, "cm"))) +
  # second half of segment, without arrows
  geom_segment(aes(x=(Unemployment + shift_unemp_left)/2,
                   y=(Number.of.Crimes + shift_crimes_left)/2,
                   xend=shift_unemp_left,
                   yend=shift_crimes_left)) +
  geom_label_repel(aes(label=Year)) +
  theme_minimal()
