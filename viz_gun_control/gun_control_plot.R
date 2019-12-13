library(ggplot2)

crimes <- read.csv('data/Gun Crimes in Chicago.csv')

rects <- data.frame(xstart = c(2000,2010.5,2013.58),
                    xend = c(2010.5,2013.58,2017),
                    col = c('period 1', 'period 2', 'period 3'))

ggplot() +
  geom_rect(aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col),
            data = rects) +
  geom_line(aes(Year, Proportion),
            data = crimes,
            color = 'dimgrey') +
  # xlim(c(2001,2016)) +
  theme_minimal() +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5)) +
  scale_fill_manual(values = alpha(c('#1F92BD', '#EF3E35', '#A5181D'), 0.2)) +
  ylab('Proportion of Firearm-Related Crimes') +
  labs(title = 'Are Gun Control Laws Effective?')
