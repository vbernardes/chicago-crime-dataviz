library(ggplot2)

crimes <- read.csv('data/Gun Crimes in Chicago.csv')

rects <- data.frame(xstart = c(2010.5,2013.58),
                    xend = c(2013.58,2017),
                    col = c('period 1', 'period 2'))

ggplot() +
  geom_rect(aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col),
            data = rects) +
  geom_line(aes(Year, Proportion),
            data = crimes,
            color = 'dimgrey') +
  theme_minimal() +
  theme(plot.title = element_text(size=20),
        axis.line = element_line(colour = 'dimgrey'),
        axis.ticks = element_line(colour = 'dimgrey'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5)) +
  scale_fill_manual(values = alpha(c('#EF3E35', '#A5181D'), 0.3)) +
  ylab('Percentage of Firearm-Related Crimes') +
  labs(title = 'The Loosening of Gun Control Laws in Chicago') +
  annotate(
    'text', label = ~underline('Historical restriction'),
    x = 2001.4, y = .043, size = 4,
    color = '#161616',
    hjust = 0
  ) +
  annotate(
    'text', label = 'Chicago had one of the most restrictive gun control laws
in the United States. The city required the registration
of all firearms but did not allow handguns to be registered,
effectively outlawing their possession.',
    x = 2001.5, y = .042, size = 3.5,
    color = '#161616',
    hjust = 0, vjust = 1
  ) +
  annotate(
    'text', label = ~underline('Supreme Court decision'),
    x = 2010.55, y = .0547, size = 2.9,
    color = '#161616',
    hjust = 0
  ) +
  annotate(
    'text', label = 'In June 2010, the ban
was reverted and
handguns started to be
allowed again.',
    x = 2010.65, y = .0525, size = 2.8,
    color = '#161616',
    hjust = 0
  ) +
  annotate(
    'text', label = ~underline('Concealed carry'),
    x = 2013.7, y = .0352, size = 3.2,
    color = '#161616',
    hjust = 0
  ) +
  annotate(
    'text', label = 'In July 2013, a concealed
carry law was passed, allowing
people to carry handguns. In
September, the laws requiring
the registration of firearms
and a city-issued permit
were repealed.',
    x = 2013.75, y = .032, size = 2.63,
    color = '#161616',
    hjust = 0
  )
