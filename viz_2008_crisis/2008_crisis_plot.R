# Setup environment
library(ggplot2)
library(ggrepel)

# Load data
unemp_crimes <- read.csv('data/Unemployment_and_Crimes_2001_2016.csv')

# these will be used to split segments in half
shift_unemp_left <- c(tail(unemp_crimes$Unemployment, n=-1), NA)
shift_crimes_left <- c(tail(unemp_crimes$Number.of.Crimes, n=-1), NA)

label_x_offset = c(0,0,.1,-.05,0,0,0,0,0,-.1,-.2,-.1,.1,-.3,0,-.1)
label_y_offset = c(0,500,2000,5000,650,0,-4000,-500,500,0,0,0,0,0,0,0)

points_to_color <- unemp_crimes[7:8,]

ggplot(data = unemp_crimes,
       aes(Unemployment, Number.of.Crimes)) +
  # first half of segment, with arrows
  geom_segment(aes(xend=(Unemployment + shift_unemp_left)/2,
                   yend=(Number.of.Crimes + shift_crimes_left)/2),
               arrow=arrow(angle = 30, length=unit(0.15, "cm"))) +
  # second half of segment, without arrows
  geom_segment(aes(x=(Unemployment + shift_unemp_left)/2,
                   y=(Number.of.Crimes + shift_crimes_left)/2,
                   xend=shift_unemp_left,
                   yend=shift_crimes_left)) +
  geom_text_repel(aes(label=Year,
                       x=Unemployment+label_x_offset,
                       y=Number.of.Crimes+label_y_offset),
                   # force = .5,
                   segment.colour = NA,
                   seed = 2603) +
  xlab('Unemployment Rate (%)') +
  ylab('Number of Crimes') +
  scale_y_continuous(label=scales::label_number(big.mark = '.')) +
  theme_minimal() +
  theme(plot.title = element_text(size=18)) +
  labs(title = 'Crime and Unemployment During the Crisis',
       subtitle = 'According to the United Nations Office on Drugs and Crime, economically-motivated
crimes are more likely to increase during periods of economic downturn. This plot
includes crimes related to theft, robbery and burglary, and shows their relationship
with unemployment in the city of Chicago.') +
  annotate(
    'label', label = 'The downward trend in the number of crimes
was interrupted when the financial crisis
began in 2007 and reached its peak in 2008
with unemployment reaching as high as 10.5%
before the economic recovery.',
    x = 6, xend = 7.5, y = 125000, yend = 135000, size = 3,
    color = 'dimgrey'
  ) +
  # add color text on top of annotation. Really, ggplot?
  annotate(
    'text', label = '                                          
                                         
began in 2007 and reached its peak in 2008
                                           
                             ',
    x = 6, xend = 7.5, y = 125000, yend = 135000, size = 3,
    color = 'red'
  ) +
  
  # add colored segment
  geom_segment(data = points_to_color,
               aes(x=Unemployment[1],
                   y=Number.of.Crimes[1],
                   xend=(Unemployment[1] + Unemployment[2])/2,
                   yend=(Number.of.Crimes[1] + Number.of.Crimes[2])/2),
               color='red',
               arrow=arrow(angle = 30, length=unit(0.15, "cm"))) +
  geom_segment(data = points_to_color,
               aes(x=(Unemployment[1] + Unemployment[2])/2,
                   y=(Number.of.Crimes[1] + Number.of.Crimes[2])/2,
                   xend=Unemployment[2],
                   yend=Number.of.Crimes[2]),
               color='red') +
  # add points
  geom_point()
