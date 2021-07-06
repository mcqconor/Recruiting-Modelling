library(tidyverse)
library(ggthemes)
library(ggtext)

uf_commits <- cfbd_recruiting_player(team = 'Florida') %>% filter(year >= 2011 & year < 2022) %>% 
  group_by(year) %>% 
  summarise(
    num = n(),
    bcs = sum(stars > 3)
  ) %>% 
  ungroup() %>% 
  mutate(
    bc_per = (bcs/num)*100,
    school = 'Florida'
  )

ggplot(data = uf_commits)+
  annotate('rect', xmin = 2011, xmax = 2014, ymin = 20, ymax = 75, alpha = 0.2)+
  annotate('text', x = 2012.5, y = 40, label = 'Will \n Muschamp', size = 5)+
  annotate('rect', xmin = 2015, xmax = 2017, ymin = 20, ymax = 75, alpha = 0.2)+
  annotate('text', x = 2016, y = 55, label = 'Jim \n McElwain', size = 5)+
  annotate('rect', xmin = 2018, xmax = 2021, ymin = 20, ymax = 75, alpha = 0.2)+
  annotate('text', x = 2019.5, y = 50, label = 'Dan \n Mullen', size = 5)+
  geom_line(mapping = aes(x = year, y = bc_per), color = '#FA4616', size = 2)+
  geom_point(mapping = aes(x = year, y = bc_per), color = 'white', size = 5.5)+
  geom_point(mapping = aes(x = year, y = bc_per), color = '#0021A5', size = 5)+
  scale_x_continuous(breaks = c(2011:2021))+
  theme_fivethirtyeight()+
  theme(
    axis.title = element_text(),
    plot.title = element_markdown(lineheight = 1.2)
  )+
  labs(
    x = 'Recruiting Class',
    y = 'Class Blue Chip Percentage',
    title = "Has<span style = 'color:#FA4616;'> Florida\'s </span>Recruiting Gotten Worse Under <span style = 'color:#0021A5;'>Dan Mullen</span>?",
    subtitle = 'Doesn\'t really look like it.',
    caption = 'By Conor McQuiston @ConorMcQ5 | Data from @cfbfastR @CFB_Data'
  )

ggsave('uf_rec.png', height = 7, width = 13)