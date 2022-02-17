# pacotes ======================================================================
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
#===============================================================================

# importar dados ===============================================================
url = 'https://raw.githubusercontent.com/adamribaudo/'
url = paste0(url, 'storytelling-with-data-ggplot/master/data/FIG0209.csv')
df = read.csv(url) %>%
  clean_names
#===============================================================================

# manipulacao ==================================================================
df = df %>%
  unite('data_text', year, month, sep='-', remove = F) %>%
  mutate(data = ym(data_text))

#===============================================================================

# visualizacao =================================================================
p = df %>%
  filter(data == max(data))

df %>%
  ggplot(aes(
    x = data,
    y = avg,
    ymin = min,
    ymax = max
  )) +
  geom_ribbon(alpha = .5) +
  geom_line(size = 2, color = '#595959') +
  geom_point(data = p, size = 3.5, color = '#595959') +
  theme_minimal(25) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  labs(x = '', y = 'Wait time (minutes)') +
  scale_x_date(limits = ymd(c('2014-09-01', '2015-09-20')),
               breaks = 'month',
               # labels = \(m) month(m, label = T)) +
               date_labels = '%b') +
  annotate('text',
           x = as.Date('2014-09-15'),
           y = 10,
           label = 'MIN') +
  annotate('text',
           x = as.Date('2014-09-15'),
           y = 24.5,
           label = 'MAX') +
  annotate('text',
           x = as.Date('2014-09-20'),
           y = 19.5,
           label = 'AVG') +
  annotate('text',
           x = as.Date('2015-09-20'),
           y = 21,
           label = '21') +
  ggtitle('Passport control wait time', subtitle = 'Past 13 months') +
  theme(
    text = element_text(color = '#8c8c8c'),
    axis.text = element_text(color = '#bfbfbf'),
    axis.title.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    axis.line = element_line(size = .1, color = '#a6a6a6'),
    axis.ticks = element_line(size = .1, color = '#a6a6a6')
  )
#===============================================================================




















